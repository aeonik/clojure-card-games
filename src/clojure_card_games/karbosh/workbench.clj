(ns clojure-card-games.karbosh.workbench
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.page :as page]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.hand-order :as hand-order]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.solver.play :as solve]
            [clojure-card-games.karbosh.trick-lab :as trick-lab]))

(defonce sessions* (atom {}))
(defonce bookmarks* (atom {}))

(def bookmark-schema :karbosh.workbench/bookmark.v1)
(def bookmark-record-type :workbench-bookmark)

(def default-monte-carlo-samples 300)
(def max-exact-remaining-cards 12)

(defn clear! []
  (reset! sessions* {})
  (reset! bookmarks* {}))

(defn sanitize-room [room]
  (-> room
      (dissoc :connections)
      (update :seats #(into {} %))))

(defn current-player [session]
  (get-in session [:room :game :current-player]))

(defn initial-session [room]
  {:room (sanitize-room room)
   :source-room (select-keys room [:id :seed :created-at :updated-at])
   :view-mode :god
   :observer (get-in room [:game :current-player])
   :undo []
   :redo []
   :analysis nil
   :message "Frozen from room state."})

(defn ensure-session! [room-id room]
  (get (swap! sessions*
              (fn [sessions]
                (if (contains? sessions room-id)
                  sessions
                  (assoc sessions room-id (initial-session room)))))
       room-id))

(defn push-room [session room']
  (-> session
      (update :undo conj (:room session))
      (assoc :room (sanitize-room room')
             :redo []
             :analysis nil)))

(defn set-message [session message]
  (assoc session :message message))

(defn reset-from-room [session source-room]
  (let [room' (sanitize-room source-room)]
    (if (= (:room session) room')
      (set-message session "Already matches room state.")
      (-> session
          (push-room room')
          (assoc :source-room (select-keys source-room [:id :seed :created-at :updated-at])
                 :observer (or (:observer session)
                               (get-in source-room [:game :current-player])))
          (set-message "Reset from room state.")))))

(defn reset-hand-start [session]
  (let [room' (update (:room session) :game game/rewind-current-hand)]
    (if (= (:room session) room')
      (set-message session "Already at the beginning of this hand.")
      (-> session
          (push-room room')
          (assoc :observer (or (:observer session)
                               (get-in room' [:game :current-player])))
          (set-message "Restarted frozen hand from its initial deal.")))))

(defn update-session! [room-id f & args]
  (get (apply swap! sessions* update room-id f args) room-id))

(defn parse-long* [x]
  (try
    (when (some? x)
      (Long/parseLong (str x)))
    (catch Exception _
      nil)))

(defn parse-keyword [x]
  (when-not (str/blank? (str x))
    (keyword (str x))))

(defn parse-card [s]
  (try
    (let [card (edn/read-string (str s))]
      (when (and (vector? card) (= 2 (count card)))
        card))
    (catch Exception _
      nil)))

(defn parse-suit [s]
  (parse-keyword s))

(defn parse-bid-event [{:keys [bid-type value]}]
  (let [bid-type (parse-keyword bid-type)]
    (cond-> {:type :bid :bid-type bid-type}
      (= :bid bid-type) (assoc :value (parse-long* value)))))

(defn legal-bid-events [state]
  (let [current (game/current-bid state)
        candidates (concat [{:type :bid :bid-type :pass}]
                           (map (fn [n]
                                  {:type :bid :bid-type :bid :value n})
                                (range 1 9))
                           [{:type :bid :bid-type :karbosh}
                            {:type :bid :bid-type :double-karbosh}])]
    (filterv #(rules/legal-bid? current %) candidates)))

(defn legal-card-events [state event-type]
  (let [player (:current-player state)
        hand (get-in state [:players player :hand])
        cards (case (:phase state)
                :trick-playing (rules/legal-cards hand
                                                  (:current-trick state)
                                                  (:trump state))
                (:karbosh-donation :karbosh-discard) (distinct hand)
                [])]
    (mapv #(assoc {:type event-type} :card %) cards)))

(defn manual-event [state params]
  (case (:phase state)
    :bidding
    (parse-bid-event params)

    :trump-selection
    {:type :trump-selection :suit (parse-suit (:suit params))}

    :karbosh-discard
    {:type :discard-card :card (parse-card (:card params))}

    :karbosh-donation
    {:type :donate-card :card (parse-card (:card params))}

    :trick-playing
    {:type :play-card :card (parse-card (:card params))}

    nil))

(defn player-bid-strategy [session player]
  (or (get-in session [:room :seats player :bid-strategy])
      bot/default-bid-strategy))

(defn player-play-strategy [session player]
  (or (get-in session [:room :seats player :play-strategy])
      (when (get-in session [:room :seats player :bot?])
        (room/bot-play-strategy (:room session) player))
      room/default-auto-play-strategy))

(defn player-ditch-policy [session player]
  (or (get-in session [:room :seats player :ditch-policy])
      (when (get-in session [:room :seats player :bot?])
        (room/bot-ditch-policy (:room session) player))
      bot/default-ditch-policy))

(defn player-play-config [session player]
  (assoc bot/default-play-config
         :ditch-policy (player-ditch-policy session player)))

(defn auto-event [session]
  (let [state (get-in session [:room :game])
        player (:current-player state)]
    (when player
      (binding [bot/*bid-strategy* (player-bid-strategy session player)
                bot/*play-strategy* (player-play-strategy session player)
                bot/*play-config* (player-play-config session player)]
        (bot/explained-action state player)))))

(defn model-recommendation [session]
  (let [state (get-in session [:room :game])
        player (:current-player state)]
    (when (and (= :trick-playing (:phase state)) player)
      (try
        (when-let [{:keys [type card ai] :as event} (auto-event session)]
          (when (and (= :play-card type) card)
            {:player player
             :card card
             :event event
             :ai ai}))
        (catch Exception _
          nil)))))

(defn apply-event-to-session [session event]
  (let [state (get-in session [:room :game])
        player (:current-player state)
        event (assoc event :player player)
        room' (assoc (:room session) :game (game/apply-event state event))]
    (-> session
        (push-room room')
        (set-message (str "Applied " (admin/kw-label (:type event))
                          " for " (name player) ".")))))

(defn apply-manual-action [session params]
  (if-let [event (manual-event (get-in session [:room :game]) params)]
    (apply-event-to-session session event)
    (set-message session "No manual action is available in this phase.")))

(defn apply-auto-action [session]
  (if-let [event (auto-event session)]
    (apply-event-to-session session event)
    (set-message session "Auto-play could not choose an action.")))

(defn undo-session [session]
  (if-let [previous (peek (:undo session))]
    (-> session
        (assoc :room previous
               :undo (pop (:undo session))
               :redo (conj (:redo session) (:room session))
               :analysis nil)
        (set-message "Undid one workbench action."))
    (set-message session "Nothing to undo.")))

(defn redo-session [session]
  (if-let [next-room (peek (:redo session))]
    (-> session
        (assoc :room next-room
               :redo (pop (:redo session))
               :undo (conj (:undo session) (:room session))
               :analysis nil)
        (set-message "Redid one workbench action."))
    (set-message session "Nothing to redo.")))

(defn update-strategies [session {:keys [player bid-strategy play-strategy ditch-policy]}]
  (let [player (parse-keyword player)
        bid-strategy (parse-keyword bid-strategy)
        play-strategy (parse-keyword play-strategy)
        ditch-policy (parse-keyword ditch-policy)]
    (if-not player
      (set-message session "No player selected.")
      (-> session
          (cond-> bid-strategy
            (assoc-in [:room :seats player :bid-strategy] bid-strategy)
            play-strategy
            (assoc-in [:room :seats player :play-strategy] play-strategy)
            ditch-policy
            (assoc-in [:room :seats player :ditch-policy] ditch-policy))
          (set-message (str "Updated strategy profile for " (name player) "."))))))

(defn set-view [session {:keys [view-mode observer]}]
  (cond-> session
    view-mode (assoc :view-mode (parse-keyword view-mode))
    observer (assoc :observer (parse-keyword observer))
    true (set-message "Updated workbench view.")))

(defn remaining-card-count [state]
  (reduce + (map #(count (get-in state [:players % :hand]))
                 (game/trick-players state))))

(defn monte-carlo-analysis [session params]
  (let [state (get-in session [:room :game])
        actor (:current-player state)
        samples (or (parse-long* (:samples params)) default-monte-carlo-samples)
        seed (or (parse-long* (:seed params)) 0)]
    (if-not (and (= :trick-playing (:phase state)) actor)
      {:kind :monte-carlo
       :error "Monte Carlo is currently available during trick play."}
      (let [candidates (vec (rules/legal-cards
                             (get-in state [:players actor :hand])
                             (:current-trick state)
                             (:trump state)))
            removed (vec (analysis/public-played-cards state))
            voids (trick-lab/void-constraints (:completed-tricks state)
                                              (:trump state))
            options (trick-lab/sample-options {:samples samples :seed seed})
            {:keys [worlds attempts]} (trick-lab/sample-worlds state
                                                               actor
                                                               removed
                                                               voids
                                                               options)]
        {:kind :monte-carlo
         :actor actor
         :actor-team (game/player-team state actor)
         :samples (:samples options)
         :accepted (count worlds)
         :attempts attempts
         :seed (:seed options)
         :baseline (mapv #(trick-lab/evaluate-candidate (:room session)
                                                        state
                                                        actor
                                                        %)
                         candidates)
         :results (trick-lab/summarize-monte-carlo (:room session)
                                                   state
                                                   actor
                                                   candidates
                                                   worlds)}))))

(defn exact-analysis [session]
  (let [state (get-in session [:room :game])
        actor (:current-player state)
        cards-left (remaining-card-count state)]
    (cond
      (not= :trick-playing (:phase state))
      {:kind :exact :error "Exact solving is currently available during trick play."}

      (> cards-left max-exact-remaining-cards)
      {:kind :exact
       :error (str "Exact solving skipped: " cards-left
                   " remaining active cards exceeds the "
                   max-exact-remaining-cards
                   "-card safety guard.")}

      :else
      (let [team (game/player-team state actor)
            candidates (rules/legal-cards (get-in state [:players actor :hand])
                                          (:current-trick state)
                                          (:trump state))]
        {:kind :exact
         :actor actor
         :actor-team team
         :team team
         :remaining-cards cards-left
         :results (mapv (fn [card]
                          (let [after (solve/play-card state actor card)]
                            {:card card
                             :future-tricks (solve/solve-future-tricks
                                             after
                                             team)}))
                        candidates)}))))

(defn current-hand-deals [state]
  (filterv #(= (:hand-index state) (:hand-index %))
           (:hand-deals state)))

(defn bookmark-coordinate [room]
  (let [state (:game room)
        deals (current-hand-deals state)
        completed-tricks (count (:completed-tricks state))
        current-trick-cards (count (:current-trick state))]
    {:room-id (:id room)
     :room-seed (:seed room)
     :game-index (:game-index room)
     :game-seed (:initial-seed state)
     :game-started-at (or (:game-started-at room)
                          (:created-at room))
     :hand-index (:hand-index state)
     :hand-number (inc (or (:hand-index state) 0))
     :hand-seed (some-> deals last :seed)
     :hand-deal-seeds (mapv :seed deals)
     :phase (:phase state)
     :current-player (:current-player state)
     :trick-index completed-tricks
     :trick-number (inc completed-tricks)
     :completed-tricks completed-tricks
     :current-trick-cards current-trick-cards}))

(defn bookmark-room-id [bookmark]
  (or (get-in bookmark [:coordinate :room-id])
      (:room-id bookmark)
      (get-in bookmark [:room :id])))

(defn bookmark-id [created-at coordinate]
  (str (:room-id coordinate)
       "-"
       (:game-seed coordinate)
       "-"
       (:hand-index coordinate)
       "-"
       (:trick-index coordinate)
       "-"
       (:current-trick-cards coordinate)
       "-"
       created-at))

(defn add-bookmark [session {:keys [note]}]
  (let [room (:room session)
        now (System/currentTimeMillis)
        coordinate (bookmark-coordinate room)
        bookmark {:id (bookmark-id now coordinate)
                  :created-at now
                  :coordinate coordinate
                  :note (str/trim (or note ""))
                  :analysis (:analysis session)
                  :room room}]
    (swap! bookmarks* update (:id room) (fnil conj []) bookmark)
    (-> session
        (assoc :last-bookmark-id (:id bookmark))
        (set-message "Bookmarked current workbench state."))))

(defn capture-from-room [session source-room params]
  (let [session' (reset-from-room session source-room)
        note (or (:note params)
                 "Captured from the game table.")]
    (-> session'
        (add-bookmark (assoc params :note note))
        (set-message "Sent current room state to workbench."))))

(defn bookmark-record [bookmark]
  {:schema bookmark-schema
   :type bookmark-record-type
   :logged-at (:created-at bookmark)
   :room-id (bookmark-room-id bookmark)
   :bookmark bookmark})

(defn bookmark-from-record [record]
  (when (and (= bookmark-record-type (:type record))
             (= bookmark-schema (:schema record)))
    (:bookmark record)))

(defn install-bookmark! [bookmark]
  (when-let [room-id (bookmark-room-id bookmark)]
    (swap! bookmarks*
           update
           room-id
           (fn [bookmarks]
             (let [bookmarks (vec (or bookmarks []))]
               (if (some #(= (:id bookmark) (:id %)) bookmarks)
                 bookmarks
                 (conj bookmarks bookmark))))))
  bookmark)

(defn bookmark-by-id [room-id bookmark-id]
  (some #(when (= bookmark-id (:id %)) %)
        (get @bookmarks* room-id)))

(defn restore-bookmark [session {:keys [bookmark-id]}]
  (if-let [bookmark (bookmark-by-id (get-in session [:room :id]) bookmark-id)]
    (-> session
        (push-room (:room bookmark))
        (set-message (str "Restored bookmark " bookmark-id ".")))
    (set-message session "Bookmark not found.")))

(defonce ^:private action-lock (Object.))

(defn handle-action! [room-id source-room params]
  ;; Actions read the session, compute, then write it back; the lock keeps
  ;; concurrent posts (e.g. rapid auto-saves) from losing updates.
  (locking action-lock
    (let [action (:action params)
          session (ensure-session! room-id source-room)]
      (try
        (let [updated (case action
                        "reset" (reset-from-room session source-room)
                        "reset-hand" (reset-hand-start session)
                        "manual" (apply-manual-action session params)
                        "auto" (apply-auto-action session)
                        "undo" (undo-session session)
                        "redo" (redo-session session)
                        "strategy" (update-strategies session params)
                        "view" (set-view session params)
                        "capture-room" (capture-from-room session
                                                          source-room
                                                          params)
                        "monte-carlo" (assoc session
                                             :analysis
                                             (monte-carlo-analysis session params)
                                             :message "Ran Monte Carlo from frozen state.")
                        "exact" (assoc session
                                       :analysis
                                       (exact-analysis session)
                                       :message "Ran exact solve guard from frozen state.")
                        "bookmark" (add-bookmark session params)
                        "restore-bookmark" (restore-bookmark session params)
                        (set-message session "Unknown workbench action."))]
          (swap! sessions* assoc room-id updated)
          updated)
        (catch Exception e
          (let [failed (set-message session (.getMessage e))]
            (swap! sessions* assoc room-id failed)
            failed))))))

(defn percent-label [x]
  (admin/probability-label x))

(defn card-analysis-state [state player actual-turn?]
  (if actual-turn?
    state
    (assoc state
           :current-player player
           :trick-leader player
           :current-trick [])))

(defn known-current-trick-risk [state player card]
  (when (seq (:current-trick state))
    (let [candidate {:player player :card card}
          winner (rules/winning-play (conj (vec (:current-trick state))
                                           candidate)
                                     (:trump state))]
      (when (not= winner candidate)
        {:winner (:player winner)
         :opponent? (not= (game/player-team state player)
                          (game/player-team state (:player winner)))}))))

(defn combine-known-risk [known-risk probability]
  (cond
    (= 1.0 known-risk)
    1.0

    (number? probability)
    (analysis/combine-event-probabilities [known-risk probability])

    (number? known-risk)
    known-risk

    :else
    nil))

(defn max-probability [& probabilities]
  (let [numbers (filter number? probabilities)]
    (when (seq numbers)
      (apply max numbers))))

(defn probabilistic-risks-from-analysis [analysis-state player card analysis]
  (let [trump (:trump analysis-state)
        trump-lead? (= trump (rules/effective-suit card trump))
        known-risk (known-current-trick-risk analysis-state player card)
        known-any-risk (when known-risk 1.0)
        known-opponent-risk (when (:opponent? known-risk) 1.0)
        future-opponent-risk (:prob-pending-opponent-can-beat-card analysis)
        future-any-risk (max-probability
                         (:prob-pending-player-can-beat-card analysis)
                         future-opponent-risk)
        control-burn (:expected-pending-partner-control-burn analysis)
        forced-follow (:prob-pending-partner-forced-higher-follow analysis)
        opponent-ruff-risk (analysis/combine-event-probabilities
                            (vals (or (:prob-pending-opponent-void-and-higher-trump
                                       analysis)
                                      {})))
        ruff-exposed-burn (* (double (or control-burn 0))
                             (double opponent-ruff-risk))]
    {:opponent (some-> (combine-known-risk known-opponent-risk
                                           future-opponent-risk)
                       bot/round-probability)
     :any (some-> (combine-known-risk known-any-risk future-any-risk)
                  bot/round-probability)
     :trump-control-burn
     (when trump-lead?
       (some-> control-burn bot/round-probability))
     :trump-control-burn-exact
     (when trump-lead?
       (some-> control-burn str))
     :ruff-exposed-control-burn
     (when-not trump-lead?
       (bot/round-probability ruff-exposed-burn))
     :opponent-ruff-risk
     (when-not trump-lead?
       (bot/round-probability opponent-ruff-risk))
     :suit-control-burn
     (when-not trump-lead?
       (some-> control-burn bot/round-probability))
     :partner-forced-follow
     (when (or trump-lead? (pos? ruff-exposed-burn))
       (some->> forced-follow
                (map (fn [[partner probability]]
                       [partner (bot/round-probability probability)]))
                (into {})))}))

(defn probabilistic-card-risk-map [state player cards actual-turn?]
  (when (:trump state)
    (let [analysis-state (card-analysis-state state player actual-turn?)
          cards (vec (distinct cards))
          analyses (try
                     (bot/card-analyses analysis-state player cards)
                     (catch Exception _
                       nil))]
      (into {}
            (map (fn [card]
                   [card (probabilistic-risks-from-analysis analysis-state
                                                            player
                                                            card
                                                            (get analyses card))]))
            cards))))

(defn probabilistic-card-risks [state player card actual-turn?]
  (get (probabilistic-card-risk-map state player [card] actual-turn?) card))

(defn exact-trick-risk [room state player card]
  (let [trick-finished (trick-lab/complete-trick-with-policy
                        room
                        (trick-lab/play-card state player card))
        trick (peek (:completed-tricks trick-finished))
        winner (rules/resolve-trick trick (:trump state))
        winner-team (game/player-team state winner)
        actor-team (game/player-team state player)]
    {:risk (bot/round-probability (if (= player winner) 0.0 1.0))
     :team-risk (bot/round-probability (if (= actor-team winner-team) 0.0 1.0))
     :winner winner
     :winner-team winner-team}))

(defn exact-card-risk-map [session player cards actual-turn?]
  (let [state (get-in session [:room :game])
        analysis-state (card-analysis-state state player actual-turn?)
        legal-cards (set (trick-lab/legal-cards analysis-state player))]
    (when (:trump analysis-state)
      (into {}
            (mapv
             (fn [card]
               [card
                (when (contains? legal-cards card)
                  (try
                    (exact-trick-risk (:room session)
                                      analysis-state
                                      player
                                      card)
                    (catch Exception _
                      nil)))])
             (distinct cards))))))

(defn exact-card-risk [session player card actual-turn?]
  (get (exact-card-risk-map session player [card] actual-turn?) card))

(defn card-risk-map [session player cards]
  (let [state (get-in session [:room :game])
        actual-turn? (= player (:current-player state))
        cards (vec (distinct cards))
        prob-risks (probabilistic-card-risk-map state player cards actual-turn?)
        exact-risks (exact-card-risk-map session player cards actual-turn?)]
    (into {}
          (map (fn [card]
                 [card {:prob (get prob-risks card)
                        :exact (get exact-risks card)}]))
          cards)))

(defn risk-class [risk]
  (cond
    (not (number? risk)) nil
    (>= risk 0.75) " high-risk"
    (<= risk 0.1) " low-risk"
    :else " medium-risk"))

(defn risk-line-html
  ([label risk]
   (risk-line-html label risk nil nil))
  ([label risk source]
   (risk-line-html label risk source nil))
  ([label risk source title]
   [:small (cond-> {:class (str "wb-risk-line"
                                (risk-class risk)
                                (when (= :god-eye source) " god-eye-risk")
                                (when (= :control-burn source) " control-risk"))}
             title (assoc :title title))
    [:span
     label]
    [:b (if (number? risk)
          (percent-label risk)
          "--")]]))

(defn partner-control-title [{:keys [trump-control-burn-exact
                                     partner-forced-follow]}]
  (let [forced (seq (sort-by (comp name key) partner-forced-follow))]
    (str "Partner trump-control burn"
         (when trump-control-burn-exact
           (str " exact " trump-control-burn-exact))
         (when forced
           (str " / forced follow "
                (str/join ", "
                          (map (fn [[partner probability]]
                                 (str (name partner)
                                      " "
                                      (percent-label probability)))
                               forced)))))))

(defn ruff-exposed-control-title
  [{:keys [suit-control-burn opponent-ruff-risk partner-forced-follow]}]
  (str "Ruff-exposed partner control burn"
       " / suit burn "
       (percent-label suit-control-burn)
       " / opponent ruff "
       (percent-label opponent-ruff-risk)
       (when-let [forced (seq (sort-by (comp name key) partner-forced-follow))]
         (str " / forced follow "
              (str/join ", "
                        (map (fn [[partner probability]]
                               (str (name partner)
                                    " "
                                    (percent-label probability)))
                             forced))))))

(defn risk-column-html [label lines]
  (into [:span {:class "wb-risk-column"}
         [:span {:class "wb-risk-column-label"} label]]
        lines))

(defn card-risk-lines-html [prob-risks exact-risk]
  [:span {:class "wb-risk-lines"
          :title (when exact-risk
                   (str "Exact winner: " (some-> (:winner exact-risk) name)
                        ", team risk "
                        (percent-label (:team-risk exact-risk))))}
   (risk-column-html "AI view"
                     [(risk-line-html "Opp" (:opponent prob-risks))
                      (risk-line-html "Any" (:any prob-risks))
                      (risk-line-html "T burn"
                                      (:trump-control-burn prob-risks)
                                      :control-burn
                                      (when (:trump-control-burn prob-risks)
                                        (partner-control-title prob-risks)))
                      (risk-line-html "R burn"
                                      (:ruff-exposed-control-burn prob-risks)
                                      :control-burn
                                      (when (:ruff-exposed-control-burn prob-risks)
                                        (ruff-exposed-control-title prob-risks)))])
   (risk-column-html "God's eye"
                     [(risk-line-html "Exact" (:risk exact-risk) :god-eye)
                      (risk-line-html "Team" (:team-risk exact-risk) :god-eye)])])

(defn recommended-card? [recommendation player card]
  (and (= player (:player recommendation))
       (= card (:card recommendation))))

(defn recommendation-title [recommendation]
  (let [ai (:ai recommendation)
        policy (some-> (:policy ai) admin/kw-label)
        reason (some-> (:reason ai) admin/kw-label)]
    (str "Model pick"
         (when policy (str ": " policy))
         (when reason (str " / " reason)))))

(defn card-risk-class [base recommendation player card]
  (str base
       (when (recommended-card? recommendation player card)
         " model-choice")))

(defn card-risk-attrs [base recommendation player card]
  (cond-> {:class (card-risk-class base recommendation player card)}
    (recommended-card? recommendation player card)
    (assoc :title (recommendation-title recommendation)
           :aria-label (recommendation-title recommendation))))

(defn card-risk-entry [session player card risks]
  (if (some? risks)
    (get risks card)
    (let [state (get-in session [:room :game])
          actual-turn? (= player (:current-player state))]
      {:prob (probabilistic-card-risks state player card actual-turn?)
       :exact (exact-card-risk session player card actual-turn?)})))

(defn card-with-risk-html
  ([session player card]
   (card-with-risk-html session player card nil))
  ([session player card recommendation]
   (card-with-risk-html session player card recommendation nil))
  ([session player card recommendation risks]
   (let [{:keys [prob exact]} (card-risk-entry session player card risks)]
     [:span (card-risk-attrs "wb-card-risk" recommendation player card)
      (admin/card-html card)
      (card-risk-lines-html prob exact)])))

(defn sorted-hand [state cards]
  (hand-order/sorted-hand cards (:trump state)))

(defn seat-name [session player]
  (or (get-in session [:room :seats player :name])
      (some-> player name)))

(defn player-short-label [session player]
  (let [label (seat-name session player)]
    (if (> (count label) 18)
      (str (subs label 0 17) "...")
      label)))

(defn latest-bid [state player]
  (last (filter #(= player (:player %))
                (game/bids-this-hand state))))

(defn board-play-html [session winning-play {:keys [player card] :as play}]
  [:li {:class (str "wb-board-play"
                    (when (= play winning-play) " is-winning"))}
   [:span {:class "wb-board-play-label"} (player-short-label session player)]
   (admin/card-html card)])

(defn board-trick-html [session trick]
  (let [state (get-in session [:room :game])
        winning-play (rules/winning-play trick (:trump state))]
    (if (seq trick)
      (into [:ol {:class "wb-board-trick"}]
            (map #(board-play-html session winning-play %) trick))
      [:ol {:class "wb-board-trick is-empty"}
       [:li "No cards played"]])))

(defn board-card-with-risk-html
  ([session player card]
   (board-card-with-risk-html session player card nil))
  ([session player card recommendation]
   (board-card-with-risk-html session player card recommendation nil))
  ([session player card recommendation risks]
   (let [{:keys [prob exact]} (card-risk-entry session player card risks)]
     [:span (card-risk-attrs "wb-board-card-risk" recommendation player card)
      (admin/card-html card)
      (card-risk-lines-html prob exact)])))

(defn board-hand-html
  ([session player hand]
   (board-hand-html session player hand nil))
  ([session player hand recommendation]
   (board-hand-html session player hand recommendation nil))
  ([session player hand recommendation risks]
   (let [state (get-in session [:room :game])
         visible? (or (not= :ai (:view-mode session))
                      (= player (:observer session)))]
     (if (seq hand)
       (if visible?
         (let [cards (sorted-hand state hand)
               risks (or risks (card-risk-map session player cards))]
           (into [:div {:class "wb-board-hand"}]
                 (map #(board-card-with-risk-html session player % recommendation risks)
                      cards)))
         (into [:div {:class "wb-board-hand is-hidden"}]
               (repeat (count hand) [:span {:class "wb-board-card-back"}])))
       [:div {:class "wb-board-hand is-empty"} "--"]))))

(defn board-void-chip-html [void-probs player suit]
  (let [probability (bot/round-probability
                     (get-in void-probs [player suit] 0))]
    [:span {:class (str "wb-void-chip"
                        (when (>= probability 0.65) " likely")
                        (when (= probability 1.0) " certain"))}
     [:span {:class "wb-void-chip-suit"} (admin/suit-html suit)]
     [:b (percent-label probability)]]))

(defn board-void-stats-html [{:keys [void-probs]} player]
  [:div {:class "wb-board-voids"}
   [:span {:class "wb-board-voids-label"} "Void odds"]
   (into [:span {:class "wb-board-void-chips"}]
         (map #(board-void-chip-html void-probs player %)
              cards/suits))])

(defn board-seat-html
  ([session player]
   (board-seat-html session player nil))
  ([session player inference]
   (board-seat-html session player inference nil))
  ([session player inference recommendation]
   (let [state (get-in session [:room :game])
         seat (get-in session [:room :seats player])
         hand (get-in state [:players player :hand])
         active? (contains? (set (game/trick-players state)) player)
         bid (latest-bid state player)
         selected? (and (= :ai (:view-mode session))
                        (= player (:observer session)))
         target-mode (if selected? :god :ai)]
     [:form {:class (str "wb-board-seat wb-board-" (name player)
                         (when (= player (:current-player state)) " current")
                         (when (= player (:dealer state)) " dealer")
                         (when selected? " observer")
                         (when (= :ai (:view-mode session)) " ai-view")
                         (when-not active? " inactive"))
             :method "post"}
      [:input {:type "hidden" :name "action" :value "view"}]
      [:input {:type "hidden" :name "view-mode" :value (name target-mode)}]
      [:input {:type "hidden" :name "observer" :value (name player)}]
      [:div {:class "wb-board-seat-content"}
       (when (= player (:dealer state))
         [:span {:class "wb-board-dealer"}])
       [:strong (player-short-label session player)]
       [:span (str "Team " (game/player-team state player)
                   " / "
                   (count hand)
                   " cards")]
       [:em (if bid (admin/bid-label bid) "--")]
       (when (and (= :ai (:view-mode session)) inference)
         (board-void-stats-html inference player))
       (board-hand-html session player hand recommendation)
       (when (:bot? seat)
         [:small "Bot"])]
      [:button {:class "wb-board-seat-button"
                :type "submit"
                :aria-label (if selected?
                              "Return to God's eye view"
                              (str "Inspect " (seat-name session player) " view"))
                :title (if selected?
                         "Return to God's eye view"
                         (str "Inspect " (seat-name session player) " view"))}]])))

(defn completed-trick-row-html [session idx trick]
  (let [state (get-in session [:room :game])
        winning-play (rules/winning-play trick (:trump state))
        winner (:player winning-play)]
    [:article {:class "wb-trick-history-card"}
     [:header
      [:strong (str "Trick " (inc idx))]
      [:span (if winner
               (str "Won by " (seat-name session winner))
               "--")]]
     (board-trick-html session trick)]))

(defn played-tricks-html [session]
  (let [state (get-in session [:room :game])
        completed (:completed-tricks state)]
    [:section {:class "wb-trick-history"}
     [:div {:class "section-heading"}
      [:div
       [:p "Play by play"]
       [:h3 "Played tricks"]]]
     (if (seq completed)
       [:div {:class "wb-trick-history-grid"}
        (map-indexed #(completed-trick-row-html session %1 %2) completed)]
       [:p {:class "empty"} "No completed tricks yet."])]))

(declare unseen-summary-html workbench-inference)

(defn workbench-board-html [session]
  (let [state (get-in session [:room :game])
        recommendation (model-recommendation session)
        inference (when (= :ai (:view-mode session))
                    (workbench-inference session))]
    [:section {:class "panel wb-panel wb-board-panel"}
     [:div {:class "section-heading"}
      [:div
       [:p "Frozen table"]
       [:h2 "Board state"]]]
     [:div {:class "stats room-stats"}
      (admin/stat-card "Phase" (admin/kw-label (:phase state)))
      (admin/stat-card "Current" (seat-name session (:current-player state)))
      (admin/stat-card "Trump" (admin/suit-html (:trump state)))
      (admin/stat-card "Bid" (admin/bid-label (game/current-bid state)))
      (admin/stat-card "Team 1 tricks" (get-in state [:tricks-this-hand 1] 0))
      (admin/stat-card "Team 2 tricks" (get-in state [:tricks-this-hand 2] 0))]
     [:div {:class "wb-risk-note"}
      [:span [:b "Opp"] "AI view: known or pending opponent can beat it"]
      [:span [:b "Any"] "AI view: known table card or pending player can beat it"]
      [:span [:b "T burn"] "AI view: trump lead may force partner to spend higher trump"]
      [:span [:b "R burn"] "AI view: off-suit partner control exposed to opponent ruffs"]
      [:span [:b "Exact"] "God's eye: this card loses the trick"]
      [:span [:b "Team"] "God's eye: this team loses the trick"]]
     (when inference
       (unseen-summary-html session inference))
     [:div {:class "wb-board"}
      [:div {:class "wb-felt"}]
      (for [player game/players]
        (board-seat-html session player inference recommendation))
      [:div {:class "wb-board-center"}
       [:span "Current trick"]
       (board-trick-html session (:current-trick state))]]
     (played-tricks-html session)]))

(defn player-row-html
  ([session player]
   (player-row-html session player nil))
  ([session player recommendation]
   (player-row-html session player recommendation nil))
  ([session player recommendation risks]
   (let [state (get-in session [:room :game])
         seat (get-in session [:room :seats player])
         hand (get-in state [:players player :hand])
         cards (sorted-hand state hand)
         risks (or risks (card-risk-map session player cards))]
     [:article {:class (str "wb-player"
                            (when (= player (:current-player state)) " current"))}
      [:header
       [:div
        [:strong (seat-name session player)]
        [:span (str (name player) " / Team " (game/player-team state player))]]
       [:span {:class "wb-policy"}
        (admin/kw-label (player-play-strategy session player))]]
      [:div {:class "wb-cards"}
       (if (seq hand)
         (for [card cards]
           (card-with-risk-html session player card recommendation risks))
         [:span {:class "empty"} "--"])]
      [:footer
       [:span (str (count hand) " cards")]
       [:span (if (:bot? seat) "Bot" "Human")]]])))

(defn void-label [voids player]
  (let [suits (sort-by cards/suit->str (get voids player))]
    (if (seq suits)
      (str/join " " (map cards/suit->str suits))
      "--")))

(defn exhausted-hidden-cards [state observer]
  (let [unseen-counts (analysis/unseen-card-counts state observer)]
    (hand-order/sorted-hand
     (filter #(zero? (get unseen-counts % 0))
             (distinct (cards/deck)))
     (:trump state))))

(defn compact-card-list-html [cards]
  (if (seq cards)
    (into [:span {:class "wb-card-strip"}]
          (map admin/card-html cards))
    "--"))

(defn hand-has-effective-suit? [state player suit]
  (boolean
   (some #(= suit (rules/effective-suit % (:trump state)))
         (get-in state [:players player :hand]))))

(defn exact-hand-void-probability [state player suit]
  (if (hand-has-effective-suit? state player suit)
    0.0
    1.0))

(defn void-probabilities [state observer]
  (let [unseen-counts (bot/unseen-card-counts state observer)
        voids (bot/known-voids state)]
    (into {}
          (map (fn [player]
                 [player
                  (into {}
                        (map (fn [suit]
                               [suit (if (= player observer)
                                       (exact-hand-void-probability
                                        state
                                        player
                                        suit)
                                       (bot/soft-void-confidence
                                        bot/action-inference-play-config
                                        state
                                        observer
                                        unseen-counts
                                        voids
                                        player
                                        suit))]))
                        cards/suits)]))
          game/players)))

(defn workbench-inference [session]
  (let [state (get-in session [:room :game])
        observer (:observer session)
        voids (bot/known-voids state)]
    {:voids voids
     :void-probs (void-probabilities state observer)}))

(defn void-odds-label [void-probs player]
  (str/join " "
            (map (fn [suit]
                   (str (cards/suit->str suit)
                        " "
                        (percent-label
                         (bot/round-probability
                          (get-in void-probs [player suit] 0)))))
                 cards/suits)))

(defn ai-view-player-html
  ([session player]
   (ai-view-player-html session player nil))
  ([session player recommendation]
   (ai-view-player-html session player recommendation nil))
  ([session player recommendation risks]
   (let [state (get-in session [:room :game])
         observer (:observer session)
         hand (get-in state [:players player :hand])
         cards (sorted-hand state hand)
         risks (when (= player observer)
                 (or risks (card-risk-map session player cards)))
         voids (bot/known-voids state)
         void-probs (void-probabilities state observer)]
     [:article {:class (str "wb-player"
                            (when (= player (:current-player state)) " current")
                            (when (= player observer) " observer"))}
      [:header
       [:div
        [:strong (seat-name session player)]
        [:span (str (name player) " / Team " (game/player-team state player))]]
       [:span {:class "wb-policy"}
        (if (= player observer) "Observer" "Hidden")]]
      (if (= player observer)
        [:div {:class "wb-cards"}
         (for [card cards]
           (card-with-risk-html session player card recommendation risks))]
        [:div {:class "wb-hidden-hand"}
         (repeat (count hand) [:span {:class "wb-card-back"}])])
      [:dl {:class "wb-facts"}
       [:div [:dt "Cards"] [:dd (count hand)]]
       [:div [:dt "Known voids"] [:dd (void-label voids player)]]
       [:div [:dt "Void odds"] [:dd (void-odds-label void-probs player)]]
       [:div [:dt "Team"] [:dd (game/player-team state player)]]]])))

(defn unseen-summary-html
  ([session]
   (unseen-summary-html session (workbench-inference session)))
  ([session _inference]
   (let [state (get-in session [:room :game])
         observer (:observer session)
         unseen (analysis/unseen-cards state observer)
         counts (analysis/effective-suit-counts (:trump state) unseen)
         exhausted (exhausted-hidden-cards state observer)]
     [:section {:class "wb-side-panel wb-ai-overview"}
      [:div {:class "wb-ai-overview-head"}
       [:div
        [:h3 (str "AI View: " (seat-name session observer))]
        [:p {:class "empty"}
         "Hidden card counts by effective suit. Player-specific void odds are shown on the table."]]]
      [:div {:class "wb-suit-counts wb-hidden-counts"}
       (for [suit cards/suits]
         [:span
          (admin/suit-html suit)
          [:strong (get counts suit 0)]])]
      [:dl {:class "wb-facts wb-ai-summary"}
       [:div
        [:dt "Hidden exhausted"]
        [:dd (compact-card-list-html exhausted)]]]])))

(defn workbench-table-html [session]
  (let [mode (:view-mode session)
        recommendation (model-recommendation session)]
    [:section {:class "panel wb-panel"}
     [:div {:class "section-heading"}
      [:div
       [:p "Hands"]
       [:h2 (if (= :ai mode) "AI view" "God's eye view")]]
      [:form {:class "wb-inline" :method "post"}
       [:input {:type "hidden" :name "action" :value "view"}]
       [:select {:name "view-mode"}
        [:option {:value "god" :selected (not= :ai mode)} "God's eye"]
        [:option {:value "ai" :selected (= :ai mode)} "AI view"]]
       [:select {:name "observer"}
        (for [player game/players]
          [:option {:value (name player)
                    :selected (= player (:observer session))}
           (seat-name session player)])]
       [:button {:type "submit"} "View"]]]
     (when (= :ai mode)
       (unseen-summary-html session))
     [:div {:class "wb-grid"}
      (for [player game/players]
        (if (= :ai mode)
          (ai-view-player-html session player recommendation)
          (player-row-html session player recommendation)))]]))

(defn strategy-option [selected strategy]
  [:option {:value (name strategy)
            :selected (= selected strategy)}
   (admin/kw-label strategy)])

(defn strategy-controls-html [session]
  [:section {:class "panel wb-panel"}
   [:div {:class "section-heading"}
    [:div
     [:p "Profiles"]
     [:h2 "AI strategy controls"]]]
   [:div {:class "wb-strategy-grid"}
    (for [player game/players]
      [:form {:class "wb-strategy-card" :method "post"}
       [:input {:type "hidden" :name "action" :value "strategy"}]
       [:input {:type "hidden" :name "player" :value (name player)}]
       [:strong (seat-name session player)]
       [:label
        [:span "Bid"]
        [:select {:name "bid-strategy"}
         (for [strategy (sort-by name (keys bot/bid-strategies))]
           (strategy-option (player-bid-strategy session player) strategy))]]
       [:label
        [:span "Play"]
        [:select {:name "play-strategy"}
         (for [strategy (sort-by name (keys bot/play-strategies))]
           (strategy-option (player-play-strategy session player) strategy))]]
       [:label
        [:span "Ditch"]
        [:select {:name "ditch-policy"}
         (for [policy (sort-by name bot/ditch-policies)]
           (strategy-option (player-ditch-policy session player) policy))]]])]])

(defn hidden-input [k v]
  [:input {:type "hidden" :name (name k) :value (str v)}])

(defn action-button [label params]
  [:form {:class "wb-action-form" :method "post"}
   (for [[k v] params]
     (hidden-input k v))
   [:button {:type "submit"} label]])

(defn bid-controls-html [state]
  [:div {:class "wb-action-row"}
   (for [{:keys [bid-type value] :as event} (legal-bid-events state)]
     (action-button (admin/bid-label event)
                    (cond-> {:action "manual"
                             :bid-type (name bid-type)}
                      value (assoc :value value))))])

(defn trump-controls-html []
  [:div {:class "wb-action-row suits"}
   (for [suit cards/suits]
     (action-button (cards/suit->str suit)
                    {:action "manual"
                     :suit (name suit)}))])

(defn card-controls-html [state event-type]
  (let [events (legal-card-events state event-type)]
    [:div {:class "wb-action-row cards"}
     (for [{:keys [card]} events]
       (action-button (cards/card->str card)
                      {:action "manual"
                       :card (pr-str card)}))]))

(defn manual-controls-html [session]
  (let [state (get-in session [:room :game])]
    [:section {:class "panel wb-panel"}
     [:div {:class "section-heading"}
      [:div
       [:p "Stepper"]
       [:h2 "Manual and profile play"]]
      [:div {:class "admin-actions"}
       (action-button "Auto current" {:action "auto"})
       (action-button "Undo" {:action "undo"})
       (action-button "Redo" {:action "redo"})
       (action-button "Restart hand" {:action "reset-hand"})
       (action-button "Reset from room" {:action "reset"})]]
     [:div {:class "stats room-stats"}
      (admin/stat-card "Phase" (admin/kw-label (:phase state)))
      (admin/stat-card "Current" (seat-name session (:current-player state)))
      (admin/stat-card "Trump" (admin/suit-html (:trump state)))
      (admin/stat-card "Bid" (admin/bid-label (game/current-bid state)))
      (admin/stat-card "Tricks" (admin/score-label (:tricks-this-hand state)))
      (admin/stat-card "Undo depth" (count (:undo session)))]
     [:h3 "Legal manual actions"]
     (case (:phase state)
       :bidding (bid-controls-html state)
       :trump-selection (trump-controls-html)
       :karbosh-discard (card-controls-html state :discard-card)
       :karbosh-donation (card-controls-html state :donate-card)
       :trick-playing (card-controls-html state :play-card)
       [:p {:class "empty"} "No manual action is available in this phase."])]))

(defn average-label [sum samples]
  (if (pos? (or samples 0))
    (format "%.2f" (/ (double (or sum 0)) samples))
    "--"))

(defn trick-counts-label [tricks]
  (str (get tricks 1 0) " / " (get tricks 2 0)))

(defn score-counts-label [scores]
  (str (get scores 1 0) " / " (get scores 2 0)))

(defn actor-team-label [team]
  (str "Actor team (" (admin/team-label team) ")"))

(defn analysis-actor-team [session analysis]
  (or (:actor-team analysis)
      (when-let [actor (:actor analysis)]
        (game/player-team (get-in session [:room :game]) actor))))

(defn bid-result-label [state final-hand]
  (let [{:keys [bid-type player value]} (game/current-bid state)
        bid-team (when player (game/player-team state player))
        bid-tricks (get-in final-hand [:tricks bid-team] 0)]
    (case bid-type
      :bid
      (str (admin/team-label bid-team)
           " "
           (if (>= bid-tricks value) "made" "failed")
           " bid "
           value
           " ("
           bid-tricks
           " tricks)")

      :karbosh
      (str (admin/team-label bid-team)
           " "
           (if (= 8 bid-tricks) "made" "failed")
           " karbosh ("
           bid-tricks
           " tricks)")

      :double-karbosh
      (str (admin/team-label bid-team)
           " "
           (if (= 8 bid-tricks) "made" "failed")
           " double karbosh ("
           bid-tricks
           " tricks)")

      "--")))

(defn mc-result-row-html
  [session {:keys [card
                   samples
                   team-wins
                   actor-wins
                   winners
                   actor-team-tricks-total]}]
  (let [view (game/admin-view (get-in session [:room :game])
                              (get-in session [:room :seats]))
        rate #(if (pos? samples)
                (format "%.1f%%" (* 100.0 (/ (double %) samples)))
                "--")
        [top-winner top-n] (first (sort-by (comp - val) winners))]
    [:tr
     (admin/table-cell "Card" (admin/card-html card))
     (admin/table-cell "Actor team wins current trick" (rate team-wins))
     (admin/table-cell "Actor wins" (rate actor-wins))
     (admin/table-cell "Avg actor-team tricks"
                       (average-label actor-team-tricks-total samples))
     (admin/table-cell "Top winner" (if top-winner
                                      (str (admin/player-label view top-winner)
                                           " x"
                                           top-n)
                                      "--"))]))

(defn baseline-row-html
  [session {:keys [card winner winner-team actor-team team-wins? final-hand]}]
  (let [state (get-in session [:room :game])
        view (game/admin-view state
                              (get-in session [:room :seats]))]
    [:tr
     (admin/table-cell "Card" (admin/card-html card))
     (admin/table-cell "Current trick winner" (admin/player-label view winner))
     (admin/table-cell "Current trick team" (admin/team-label winner-team))
     (admin/table-cell "Actor team takes current trick?"
                       (if team-wins? "Yes" "No"))
     (admin/table-cell "Final tricks (Team 1 / Team 2)"
                       (trick-counts-label (:tricks final-hand)))
     (admin/table-cell (str (actor-team-label actor-team) " final tricks")
                       (:actor-team-tricks final-hand))
     (admin/table-cell "Bid result"
                       (bid-result-label state final-hand))
     (admin/table-cell "Final score (Team 1 / Team 2)"
                       (score-counts-label (:scores final-hand)))]))

(defn baseline-table-html [session baseline]
  [:table {:class "admin-table wb-baseline-table"}
   [:thead
    [:tr
     [:th "Card"]
     [:th "Current trick winner"]
     [:th "Current trick team"]
     [:th "Actor team takes current trick?"]
     [:th "Final tricks (Team 1 / Team 2)"]
     [:th "Actor team final tricks"]
     [:th "Bid result"]
     [:th "Final score (Team 1 / Team 2)"]]]
   [:tbody
    (for [result baseline]
      (baseline-row-html session result))]])

(defn exact-result-row-html [{:keys [card future-tricks]}]
  [:tr
   (admin/table-cell "Card" (admin/card-html card))
   (admin/table-cell "Forced future tricks" future-tricks)])

(defn analysis-panel-html [session]
  (let [analysis (:analysis session)
        results (some-> analysis :results vals)
        actor-team (analysis-actor-team session analysis)]
    [:section {:class "panel wb-panel"}
      [:div {:class "section-heading"}
      [:div
       [:p "Frozen branch"]
       [:h2 "Monte Carlo and exact solve"]]
      [:div {:class "admin-actions wb-analysis-actions"}
        [:form {:class "wb-inline" :method "post"}
        [:input {:type "hidden" :name "action" :value "monte-carlo"}]
        [:label
         [:span "Samples"]
         [:input {:type "number"
                  :name "samples"
                  :min "1"
                  :max (str trick-lab/max-samples)
                  :value (str default-monte-carlo-samples)}]]
        [:label
         [:span "Seed"]
         [:input {:type "number" :name "seed" :value "0"}]]
        [:button {:type "submit"} "Monte Carlo"]]
       (action-button "Exact solve" {:action "exact"})]]
     (cond
       (nil? analysis)
       [:p {:class "empty"} "Freeze the state here, then run Monte Carlo or exact solve from this point."]

       (:error analysis)
       [:p {:class "empty"} (:error analysis)]

       (= :monte-carlo (:kind analysis))
       [:div
        [:div {:class "stats room-stats"}
         (admin/stat-card "Actor" (seat-name session (:actor analysis)))
         (admin/stat-card "Actor team" (admin/team-label actor-team))
         (admin/stat-card "Samples" (str (:accepted analysis) " / " (:samples analysis)))
         (admin/stat-card "Attempts" (:attempts analysis))
         (admin/stat-card "Seed" (:seed analysis))]
        [:h3 "Current-hand policy rollout"]
        (baseline-table-html session (:baseline analysis))
        [:h3 "Monte Carlo sampled worlds"]
        [:table {:class "admin-table"}
         [:thead
          [:tr
           [:th "Card"]
           [:th "Actor team wins current trick"]
           [:th "Actor wins"]
           [:th "Avg actor-team tricks"]
           [:th "Top winner"]]]
         [:tbody
          (for [result (sort-by (fn [{:keys [samples team-wins]}]
                                  (if (pos? samples)
                                    (- (/ (double team-wins) samples))
                                    0))
                                results)]
            (mc-result-row-html session result))]]]

       (= :exact (:kind analysis))
       [:div
        [:div {:class "stats room-stats"}
         (admin/stat-card "Actor" (seat-name session (:actor analysis)))
         (admin/stat-card "Team" (str "Team " (:team analysis)))
         (admin/stat-card "Remaining cards" (:remaining-cards analysis))]
        [:table {:class "admin-table"}
         [:thead
          [:tr
           [:th "Card"]
           [:th "Forced future tricks"]]]
         [:tbody
          (for [result (:results analysis)]
            (exact-result-row-html result))]]]

       :else
       [:p {:class "empty"} "Unknown analysis result."])]))

(defn bookmark-value [value]
  (cond
    (nil? value) "--"
    (keyword? value) (admin/kw-label value)
    (sequential? value) (if (seq value)
                          (str/join " -> " (map str value))
                          "--")
    :else (str value)))

(defn bookmark-fact-html [label value]
  [:div
   [:dt label]
   [:dd (bookmark-value value)]])

(defn bookmark-hand-url [{:keys [room-id game-seed game-started-at hand-index]}]
  (when (and room-id game-seed game-started-at (some? hand-index))
    (str "/karbosh/admin/history/"
         room-id
         "/"
         game-seed
         "/"
         game-started-at
         "/snapshot/hands/"
         hand-index)))

(defn bookmark-coordinate-html [{:keys [created-at coordinate]}]
  (let [{:keys [game-index game-seed hand-number hand-seed hand-deal-seeds
                phase current-player trick-number completed-tricks
                current-trick-cards room-seed game-started-at]} coordinate]
    [:dl {:class "wb-facts wb-bookmark-coordinate"}
     (bookmark-fact-html "Created"
                         (str (java.time.Instant/ofEpochMilli created-at)))
     (bookmark-fact-html "Room seed" room-seed)
     (bookmark-fact-html "Started" game-started-at)
     (bookmark-fact-html "Game" (str (inc (or game-index 0)) " / " game-seed))
     (bookmark-fact-html "Hand" hand-number)
     (bookmark-fact-html "Hand seed" hand-seed)
     (bookmark-fact-html "Deal seeds" hand-deal-seeds)
     (bookmark-fact-html "Phase" phase)
     (bookmark-fact-html "Current" current-player)
     (bookmark-fact-html "Trick"
                         (str trick-number
                              " ("
                              completed-tricks
                              " complete, "
                              current-trick-cards
                              " played)"))]))

(defn bookmark-row-html [{:keys [id note coordinate] :as bookmark}]
  [:li {:class "wb-bookmark"}
   [:div {:class "wb-bookmark-head"}
    [:strong (str "Game " (inc (or (:game-index coordinate) 0))
                  " / Hand " (:hand-number coordinate)
                  " / " (admin/kw-label (:phase coordinate)))]
    [:span id]]
   (bookmark-coordinate-html bookmark)
   (when-not (str/blank? note)
     [:p note])
   (when-let [analysis (:analysis bookmark)]
     [:p {:class "empty"}
      (case (:kind analysis)
        :monte-carlo
        (str "Saved Monte Carlo: "
             (:accepted analysis)
             " / "
             (:samples analysis)
             " samples, seed "
             (:seed analysis)
             ".")

        :exact
        (str "Saved exact solve: "
             (:remaining-cards analysis)
             " cards remaining.")

        "Saved analysis attached.")])
   [:div {:class "wb-bookmark-actions"}
    [:form {:class "wb-action-form wb-bookmark-restore" :method "post"}
     [:input {:type "hidden" :name "action" :value "restore-bookmark"}]
     [:input {:type "hidden" :name "bookmark-id" :value id}]
     [:button {:type "submit"} "Restore frozen point"]]
    (when-let [url (bookmark-hand-url coordinate)]
      [:a {:href url} "Immutable hand"])]])

(defn bookmark-panel-html [session]
  (let [room-id (get-in session [:room :id])
        bookmarks (get @bookmarks* room-id)]
    [:section {:class "panel wb-panel"}
     [:div {:class "section-heading"}
      [:div
       [:p "Review"]
       [:h2 "Bookmarks"]]]
     [:form {:class "wb-bookmark-form" :method "post"}
      [:input {:type "hidden" :name "action" :value "bookmark"}]
      [:textarea {:name "note"
                  :placeholder "Note why this hand/state is worth studying"}]
      [:button {:type "submit"} "Bookmark"]]
     (if (seq bookmarks)
       [:ol {:class "compact-list wb-bookmarks"}
        (for [bookmark (reverse bookmarks)]
          (bookmark-row-html bookmark))]
       [:p {:class "empty"} "No bookmarks in this local workbench session yet."])]))

(defn workbench-main [session]
  (let [room-id (get-in session [:room :id])]
    [:main {:id "admin-main"}
     [:div {:class "top"}
      [:div
       [:p "Karbosh AI debugging"]
       [:h1 (str "Workbench " room-id)]]
      [:div {:class "admin-actions"}
       [:a {:href (str "/karbosh/admin/rooms/" room-id "/snapshot")} "Snapshot"]
       [:a {:href (str "/karbosh/admin?room=" room-id)} "Dashboard"]
       [:a {:href "/karbosh/"} "Game"]
       [:a {:href "/karbosh/admin/logout"} "Logout"]]]
     [:section {:class "panel wb-message"}
      (:message session)]
     (workbench-board-html session)
     (manual-controls-html session)
     (strategy-controls-html session)
     (analysis-panel-html session)
     (bookmark-panel-html session)]))

(defn render [session]
  (page/render {:title (str "Karbosh Workbench " (get-in session [:room :id]))
                :stylesheets ["admin.css" "workbench.css?v=20260611-compact-hand-grid"]}
               (workbench-main session)
               [:script {:src "/karbosh/assets/js/workbench.js?v=20260611-queued-saves"}]))
