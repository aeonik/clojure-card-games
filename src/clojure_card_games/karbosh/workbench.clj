(ns clojure-card-games.karbosh.workbench
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure-card-games.karbosh.admin :as admin]
            [clojure-card-games.karbosh.analysis :as analysis]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.hiccup :as h]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.hand-order :as hand-order]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.solver.play :as solve]
            [clojure-card-games.karbosh.trick-lab :as trick-lab]))

(defonce sessions* (atom {}))
(defonce bookmarks* (atom {}))

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

(defn reset-session! [room-id room]
  (get (swap! sessions* assoc room-id (initial-session room))
       room-id))

(defn push-room [session room']
  (-> session
      (update :undo conj (:room session))
      (assoc :room (sanitize-room room')
             :redo []
             :analysis nil)))

(defn set-message [session message]
  (assoc session :message message))

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
         :samples (:samples options)
         :accepted (count worlds)
         :attempts attempts
         :seed (:seed options)
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
         :team team
         :remaining-cards cards-left
         :results (mapv (fn [card]
                          (let [after (solve/play-card state actor card)]
                            {:card card
                             :future-tricks (solve/solve-future-tricks
                                             after
                                             team)}))
                        candidates)}))))

(defn add-bookmark [session {:keys [note]}]
  (let [room (:room session)
        state (:game room)
        bookmark {:id (str (System/currentTimeMillis))
                  :created-at (System/currentTimeMillis)
                  :room-id (:id room)
                  :hand-index (:hand-index state)
                  :phase (:phase state)
                  :current-player (:current-player state)
                  :note (str/trim (or note ""))
                  :room room}]
    (swap! bookmarks* update (:id room) (fnil conj []) bookmark)
    (set-message session "Bookmarked current workbench state.")))

(defn handle-action! [room-id source-room params]
  (let [action (:action params)
        session (if (= action "reset")
                  (reset-session! room-id source-room)
                  (ensure-session! room-id source-room))]
    (try
      (let [updated (case action
                      "reset" session
                      "manual" (apply-manual-action session params)
                      "auto" (apply-auto-action session)
                      "undo" (undo-session session)
                      "redo" (redo-session session)
                      "strategy" (update-strategies session params)
                      "view" (set-view session params)
                      "monte-carlo" (assoc session
                                           :analysis
                                           (monte-carlo-analysis session params)
                                           :message "Ran Monte Carlo from frozen state.")
                      "exact" (assoc session
                                     :analysis
                                     (exact-analysis session)
                                     :message "Ran exact solve guard from frozen state.")
                      "bookmark" (add-bookmark session params)
                      (set-message session "Unknown workbench action."))]
        (swap! sessions* assoc room-id updated)
        updated)
      (catch Exception e
        (let [failed (set-message session (.getMessage e))]
          (swap! sessions* assoc room-id failed)
          failed)))))

(defn percent-label [x]
  (admin/probability-label x))

(defn maybe-risk [state player card actual-turn?]
  (when (:trump state)
    (let [analysis-state (if actual-turn?
                           state
                           (assoc state
                                  :current-player player
                                  :current-trick []))
          analyses (try
                     (bot/card-analyses analysis-state player [card])
                     (catch Exception _
                       nil))]
      (some-> (get analyses card)
              :prob-pending-opponent-can-beat-card
              bot/round-probability))))

(defn card-with-risk-html [state player card]
  (let [actual-turn? (= player (:current-player state))
        risk (maybe-risk state player card actual-turn?)]
    [:span {:class "wb-card-risk"}
     (admin/card-html card)
     [:small (if (number? risk)
               (percent-label risk)
               "--")]]))

(defn sorted-hand [state cards]
  (if-let [trump (:trump state)]
    (hand-order/sorted-hand cards trump)
    cards))

(defn seat-name [session player]
  (or (get-in session [:room :seats player :name])
      (some-> player name)))

(defn player-row-html [session player]
  (let [state (get-in session [:room :game])
        seat (get-in session [:room :seats player])
        hand (get-in state [:players player :hand])]
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
        (for [card (sorted-hand state hand)]
          (card-with-risk-html state player card))
        [:span {:class "empty"} "--"])]
     [:footer
      [:span (str (count hand) " cards")]
      [:span (if (:bot? seat) "Bot" "Human")]]]))

(defn void-label [voids player]
  (let [suits (sort-by cards/suit->str (get voids player))]
    (if (seq suits)
      (str/join " " (map cards/suit->str suits))
      "--")))

(defn ai-view-player-html [session player]
  (let [state (get-in session [:room :game])
        observer (:observer session)
        hand (get-in state [:players player :hand])
        voids (trick-lab/void-constraints (:completed-tricks state)
                                          (:trump state))]
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
        (for [card (sorted-hand state hand)]
          (card-with-risk-html state player card))]
       [:div {:class "wb-hidden-hand"}
        (repeat (count hand) [:span {:class "wb-card-back"}])])
     [:dl {:class "wb-facts"}
      [:div [:dt "Cards"] [:dd (count hand)]]
      [:div [:dt "Known voids"] [:dd (void-label voids player)]]
      [:div [:dt "Team"] [:dd (game/player-team state player)]]]]))

(defn unseen-summary-html [session]
  (let [state (get-in session [:room :game])
        observer (:observer session)
        unseen (analysis/unseen-cards state observer)
        counts (analysis/effective-suit-counts (:trump state) unseen)]
    [:section {:class "wb-side-panel"}
     [:h3 "AI View"]
     [:p {:class "empty"} "Cards not visible to the selected observer, grouped by effective suit."]
     [:div {:class "wb-suit-counts"}
      (for [suit cards/suits]
        [:span
         (admin/suit-html suit)
         [:strong (get counts suit 0)]])]]))

(defn workbench-table-html [session]
  (let [mode (:view-mode session)]
    [:section {:class "panel wb-panel"}
     [:div {:class "section-heading"}
      [:div
       [:p "Frozen table"]
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
          (ai-view-player-html session player)
          (player-row-html session player)))]]))

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
           (strategy-option (player-ditch-policy session player) policy))]]
       [:button {:type "submit"} "Apply"]])]])

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

(defn mc-result-row-html [session {:keys [card samples team-wins actor-wins winners]}]
  (let [view (game/admin-view (get-in session [:room :game])
                              (get-in session [:room :seats]))
        rate #(if (pos? samples)
                (format "%.1f%%" (* 100.0 (/ (double %) samples)))
                "--")
        [top-winner top-n] (first (sort-by (comp - val) winners))]
    [:tr
     (admin/table-cell "Card" (admin/card-html card))
     (admin/table-cell "Team wins" (rate team-wins))
     (admin/table-cell "Actor wins" (rate actor-wins))
     (admin/table-cell "Top winner" (if top-winner
                                      (str (admin/player-label view top-winner)
                                           " x"
                                           top-n)
                                      "--"))]))

(defn exact-result-row-html [{:keys [card future-tricks]}]
  [:tr
   (admin/table-cell "Card" (admin/card-html card))
   (admin/table-cell "Forced future tricks" future-tricks)])

(defn analysis-panel-html [session]
  (let [analysis (:analysis session)
        results (some-> analysis :results vals)]
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
         (admin/stat-card "Samples" (str (:accepted analysis) " / " (:samples analysis)))
         (admin/stat-card "Attempts" (:attempts analysis))
         (admin/stat-card "Seed" (:seed analysis))]
        [:table {:class "admin-table"}
         [:thead
          [:tr
           [:th "Card"]
           [:th "Team wins"]
           [:th "Actor wins"]
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
        (for [{:keys [id hand-index phase note]} (reverse bookmarks)]
          [:li
           [:strong (str "Hand " (inc hand-index) " / " (admin/kw-label phase))]
           [:span id]
           [:p note]])]
       [:p {:class "empty"} "No bookmarks in this local workbench session yet."])]))

(def workbench-styles
  (str
   ".wb-panel{overflow-x:visible}"
   ".wb-inline,.wb-action-form{display:inline-flex;gap:8px;align-items:end;margin:0}"
   ".wb-inline label{display:grid;gap:3px;color:rgba(255,255,255,.5);font-size:.58rem;font-weight:800;letter-spacing:.08em;text-transform:uppercase}"
   ".wb-inline select,.wb-inline input,.wb-strategy-card select{min-height:32px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:#111827;color:white;padding:0 8px}"
   ".wb-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px}"
   ".wb-player{border:1px solid rgba(255,255,255,.12);border-radius:8px;background:rgba(0,0,0,.16);padding:10px;min-width:0}"
   ".wb-player.current{border-color:rgba(245,200,91,.72);box-shadow:0 0 0 1px rgba(245,200,91,.18)}"
   ".wb-player.observer{border-color:rgba(111,208,199,.55)}"
   ".wb-player header,.wb-player footer{display:flex;justify-content:space-between;gap:8px;align-items:flex-start}"
   ".wb-player strong{display:block;color:white;font-size:.95rem;line-height:1.1}"
   ".wb-player header span,.wb-player footer span,.wb-policy{color:rgba(255,255,255,.52);font-size:.62rem;font-weight:800;letter-spacing:.06em;text-transform:uppercase}"
   ".wb-cards{display:flex;flex-wrap:wrap;gap:6px;margin:10px 0}"
   ".wb-card-risk{display:grid;justify-items:center;gap:2px}"
   ".wb-card-risk .card{margin:0}"
   ".wb-card-risk small{color:#f5c85b;font-size:.56rem;font-weight:900;line-height:1}"
   ".wb-hidden-hand{display:flex;flex-wrap:wrap;gap:6px;margin:10px 0}"
   ".wb-card-back{display:inline-block;width:38px;height:52px;border:1px solid rgba(255,255,255,.18);border-radius:6px;background:linear-gradient(135deg,#1c365e,#18213a)}"
   ".wb-facts{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:6px;margin:8px 0 0}"
   ".wb-facts div,.wb-suit-counts span{border:1px solid rgba(255,255,255,.1);border-radius:6px;background:rgba(255,255,255,.04);padding:6px}"
   ".wb-facts dt{color:rgba(255,255,255,.45);font-size:.52rem;font-weight:800;letter-spacing:.08em;text-transform:uppercase}"
   ".wb-facts dd{margin:2px 0 0;color:white;font-weight:800}"
   ".wb-side-panel{margin-bottom:10px}"
   ".wb-suit-counts{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:6px}"
   ".wb-suit-counts strong{float:right;color:white}"
   ".wb-strategy-grid{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px}"
   ".wb-strategy-card{display:grid;gap:7px;border:1px solid rgba(255,255,255,.1);border-radius:8px;background:rgba(0,0,0,.14);padding:10px}"
   ".wb-strategy-card label{display:grid;gap:3px}"
   ".wb-strategy-card label span{color:rgba(255,255,255,.48);font-size:.58rem;font-weight:800;letter-spacing:.08em;text-transform:uppercase}"
   ".wb-action-row{display:flex;flex-wrap:wrap;gap:8px}"
   ".wb-action-row.suits button{min-width:54px;font-size:1.5rem;color:#f7f8ff}"
   ".wb-action-row.suits form:nth-child(1) button,.wb-action-row.suits form:nth-child(2) button{color:#ff7d8b}"
   ".wb-action-row.cards button{min-width:48px;font-size:.95rem}"
   ".wb-bookmark-form{display:grid;gap:8px}"
   ".wb-bookmark-form textarea{min-height:86px;border:1px solid rgba(255,255,255,.16);border-radius:8px;background:#111827;color:white;padding:10px;font:inherit}"
   ".wb-bookmarks p{margin:3px 0 9px;color:rgba(255,255,255,.62)}"
   ".wb-message{border-color:rgba(111,208,199,.28);background:rgba(111,208,199,.09);color:#bdf4ef}"
   ".wb-analysis-actions{align-items:flex-end}"
   "@media(max-width:980px){.wb-grid,.wb-strategy-grid{grid-template-columns:repeat(2,minmax(0,1fr))}}"
   "@media(max-width:720px){.wb-grid,.wb-strategy-grid{grid-template-columns:1fr}.wb-inline{flex-wrap:wrap}.wb-player{padding:8px}.wb-card-back{width:30px;height:42px}.wb-facts{grid-template-columns:1fr}.wb-suit-counts{grid-template-columns:repeat(2,minmax(0,1fr))}}"))

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
     (manual-controls-html session)
     (workbench-table-html session)
     (strategy-controls-html session)
     (analysis-panel-html session)
     (bookmark-panel-html session)]))

(defn render [session]
  (str
   "<!doctype html>"
   (h/render
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width,initial-scale=1"}]
      [:title (str "Karbosh Workbench " (get-in session [:room :id]))]
      [:style (str admin/styles
                   admin/admin-layout-styles
                   admin/admin-card-styles
                   workbench-styles)]]
     [:body
      (workbench-main session)]])))
