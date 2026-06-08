(ns clojure-card-games.karbosh.trick-lab
  (:require [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.room :as rooms]
            [clojure-card-games.karbosh.shared.cards :as cards]
            [clojure-card-games.karbosh.shared.game :as game]
            [clojure-card-games.karbosh.shared.rules :as rules]
            [clojure-card-games.karbosh.solver.sample :as sample]))

(def default-samples 300)
(def max-samples 1200)
(def max-attempt-factor 40)

(defn parse-long* [x]
  (try
    (when (some? x)
      (Long/parseLong (str x)))
    (catch Exception _
      nil)))

(defn clamp [lo hi x]
  (max lo (min hi x)))

(defn sample-options [{:keys [samples seed]}]
  {:samples (clamp 1 max-samples (or (parse-long* samples)
                                     default-samples))
   :seed (or (parse-long* seed) 0)})

(defn playable-current-hand? [game]
  (not (contains? #{:hand-complete :game-over} (:phase game))))

(defn current-hand [game]
  {:hand-index (:hand-index game)
   :bid (game/current-bid game)
   :trump (:trump game)
   :initial-hands (:initial-hands game)
   :final-hands (game/player-hands game)
   :deals (:hand-deals game)
   :history (:history game)
   :completed-tricks (:completed-tricks game)
   :current-trick (:current-trick game)
   :tricks (:tricks-this-hand game)
   :scores-after (:scores game)
   :phase (:phase game)
   :current? true})

(defn room-hands [room]
  (let [state (:game room)]
    (cond-> (vec (:hand-history state))
      (playable-current-hand? state)
      (conj (current-hand state)))))

(defn room-hand [room hand-index]
  (first (filter #(= hand-index (:hand-index %)) (room-hands room))))

(defn special-contract? [{:keys [bid-type]}]
  (contains? #{:karbosh :double-karbosh} bid-type))

(defn player-states [hands]
  (into {}
        (map (fn [player]
               [player {:team (get (game/teams) player)
                        :hand (vec (get hands player []))}]))
        game/players))

(defn bid-events [hand]
  (if-let [bid (:bid hand)]
    [(assoc bid :hand-index (:hand-index hand))]
    []))

(defn base-state [hand]
  (let [state {:phase :trick-playing
               :hand-index (:hand-index hand)
               :players (player-states (:initial-hands hand))
               :bids (bid-events hand)
               :trump (:trump hand)
               :history []
               :completed-tricks []
               :current-trick []
               :tricks-this-hand {1 0 2 0}
               :scores {}
               :current-player nil
               :trick-leader nil}]
    (cond-> state
      (special-contract? (:bid hand))
      (assoc :active-players (game/lone-hand-players state
                                                     (get-in hand [:bid :player]))))))

(defn remove-card [state player card]
  (update-in state [:players player :hand] #(game/remove-first card %)))

(defn add-card [state player card]
  (update-in state [:players player :hand] (fnil conj []) card))

(defn setup-event? [{:keys [type]}]
  (contains? #{:trump-selection :discard-card :donate-card} type))

(defn setup-events [hand]
  (filter setup-event? (:history hand)))

(defn apply-setup-event [state {:keys [type player to card suit]}]
  (case type
    :trump-selection
    (assoc state :trump suit)

    :discard-card
    (remove-card state player card)

    :donate-card
    (-> state
        (remove-card player card)
        (add-card to card))

    state))

(defn previous-tricks [hand trick-index]
  (vec (take trick-index (:completed-tricks hand))))

(defn target-trick [hand trick-index]
  (nth (:completed-tricks hand) trick-index nil))

(defn completed-trick-score [state trick]
  (if-let [winner (rules/resolve-trick trick (:trump state))]
    (let [team (game/player-team state winner)]
      (update-in state [:tricks-this-hand team] (fnil inc 0)))
    state))

(defn remove-trick-cards [state tricks]
  (reduce (fn [state trick]
            (reduce (fn [state {:keys [player card]}]
                      (remove-card state player card))
                    state
                    trick))
          state
          tricks))

(defn score-previous-tricks [state tricks]
  (reduce completed-trick-score state tricks))

(defn pre-trick-state [hand trick-index]
  (when-let [trick (target-trick hand trick-index)]
    (when-not (:initial-hands hand)
      (throw (ex-info "Hand does not include initial hands"
                      {:hand-index (:hand-index hand)
                       :trick-index trick-index})))
    (let [previous (previous-tricks hand trick-index)
          leader (:player (first trick))]
      (-> (reduce apply-setup-event (base-state hand) (setup-events hand))
          (remove-trick-cards previous)
          (score-previous-tricks previous)
          (assoc :completed-tricks previous
                 :current-player leader
                 :trick-leader leader)))))

(defn removed-cards-before [hand trick-index]
  (vec (concat
        (keep (fn [{:keys [type card]}]
                (when (= :discard-card type) card))
              (setup-events hand))
        (mapcat #(map :card %) (previous-tricks hand trick-index)))))

(defn void-constraints [tricks trump]
  (reduce (fn [voids trick]
            (if-let [lead (rules/trick-lead trick trump)]
              (reduce (fn [voids {:keys [player card]}]
                        (if (= lead (rules/effective-suit card trump))
                          voids
                          (update voids player (fnil conj #{}) lead)))
                      voids
                      (rest trick))
              voids))
          {}
          tricks))

(defn legal-cards [state player]
  (rules/legal-cards (get-in state [:players player :hand])
                     (:current-trick state)
                     (:trump state)))

(defn strategy-for [room player]
  (if (get-in room [:seats player :bot?])
    (rooms/bot-play-strategy room player)
    rooms/default-auto-play-strategy))

(defn play-config-for [room player]
  (if (get-in room [:seats player :bot?])
    (rooms/bot-play-config room player)
    bot/default-play-config))

(defn choose-card [room state player]
  (or (binding [bot/*play-strategy* (strategy-for room player)
                bot/*play-config* (play-config-for room player)]
        (:card (bot/card-action state player)))
      (first (legal-cards state player))))

(defn play-card [state player card]
  (game/apply-event state {:type :play-card
                           :player player
                           :card card}))

(defn complete-trick-with-policy [room state]
  (let [completed-count (count (:completed-tricks state))]
    (loop [state state]
      (if (> (count (:completed-tricks state)) completed-count)
        state
        (let [player (:current-player state)
              card (choose-card room state player)]
          (if (and player card)
            (recur (play-card state player card))
            state))))))

(defn evaluate-candidate [room state player card]
  (let [finished (complete-trick-with-policy room (play-card state player card))
        trick (peek (:completed-tricks finished))
        winner (rules/resolve-trick trick (:trump state))
        winner-team (game/player-team state winner)]
    {:card card
     :winner winner
     :winner-team winner-team
     :actor-team (game/player-team state player)
     :actor-wins? (= player winner)
     :team-wins? (= (game/player-team state player) winner-team)
     :trick trick}))

(defn hand-sizes [state]
  (into {}
        (map (fn [player]
               [player (count (get-in state [:players player :hand]))]))
        game/players))

(defn with-hands [state hands]
  (reduce-kv (fn [state player hand]
               (assoc-in state [:players player :hand] (vec hand)))
             state
             hands))

(defn valid-world? [state voids]
  (let [trump (:trump state)]
    (every? (fn [[player suits]]
              (not-any? #(contains? suits (rules/effective-suit % trump))
                        (get-in state [:players player :hand])))
            voids)))

(defn sample-world [state actor known-cards seed]
  (let [hands (sample/sample-hands
               {:seed seed
                :players game/players
                :hand-sizes (hand-sizes state)
                :known-hands {actor (get-in state [:players actor :hand])}
                :known-cards known-cards})]
    (with-hands state hands)))

(defn sample-worlds [state actor known-cards voids {:keys [samples seed]}]
  (let [max-attempts (* max-attempt-factor samples)]
    (loop [attempt 0
           accepted []]
      (if (or (>= (count accepted) samples)
              (>= attempt max-attempts))
        {:worlds accepted
         :attempts attempt}
        (let [world (sample-world state actor known-cards (+ seed attempt))]
          (recur (inc attempt)
                 (cond-> accepted
                   (valid-world? world voids) (conj world))))))))

(defn increment-outcome [summary {:keys [winner team-wins? actor-wins?]}]
  (-> summary
      (update :samples (fnil inc 0))
      (update :team-wins (fnil + 0) (if team-wins? 1 0))
      (update :actor-wins (fnil + 0) (if actor-wins? 1 0))
      (update-in [:winners winner] (fnil inc 0))))

(defn summarize-monte-carlo [room state actor candidates worlds]
  (let [initial (into {}
                      (map (fn [card]
                             [card {:card card
                                    :samples 0
                                    :team-wins 0
                                    :actor-wins 0
                                    :winners {}}]))
                      candidates)]
    (reduce (fn [summary world]
              (reduce (fn [summary card]
                        (update summary card increment-outcome
                                (evaluate-candidate room world actor card)))
                      summary
                      candidates))
            initial
            worlds)))

(defn candidate-probabilities [state player candidates]
  (let [analyses (bot/card-analyses state player candidates)]
    (mapv #(bot/candidate-summary state player analyses %) candidates)))

(defn analyze [room hand-index trick-index raw-options]
  (let [options (sample-options raw-options)
        hand (room-hand room hand-index)]
    (when-not hand
      (throw (ex-info "Hand not found" {:hand-index hand-index})))
    (let [state (pre-trick-state hand trick-index)
          trick (target-trick hand trick-index)]
      (when-not state
        (throw (ex-info "Trick not found"
                        {:hand-index hand-index
                         :trick-index trick-index})))
      (let [actor (:current-player state)
            candidates (vec (legal-cards state actor))
            actual-winner (rules/resolve-trick trick (:trump state))
            known-results (mapv #(evaluate-candidate room state actor %)
                                candidates)
            removed-cards (removed-cards-before hand trick-index)
            voids (void-constraints (previous-tricks hand trick-index)
                                    (:trump state))
            {:keys [worlds attempts]} (sample-worlds state
                                                      actor
                                                      removed-cards
                                                      voids
                                                      options)]
        {:room-id (:id room)
         :hand hand
         :hand-index hand-index
         :trick-index trick-index
         :state state
         :actor actor
         :actor-team (game/player-team state actor)
         :trump (:trump state)
         :actual {:trick trick
                  :card (:card (first trick))
                  :winner actual-winner
                  :winner-team (game/player-team state actual-winner)}
         :known-results known-results
         :probabilities (candidate-probabilities state actor candidates)
         :monte-carlo {:requested-samples (:samples options)
                       :accepted-samples (count worlds)
                       :attempts attempts
                       :seed (:seed options)
                       :voids voids
                       :results (summarize-monte-carlo room
                                                       state
                                                       actor
                                                       candidates
                                                       worlds)}}))))
