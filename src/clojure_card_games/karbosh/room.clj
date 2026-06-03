(ns clojure-card-games.karbosh.room
  (:require [clojure.string :as str]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.game :as game]))

(def room-id-chars "ABCDEFGHJKLMNPQRSTUVWXYZ23456789")

(defn random-room-id []
  (apply str (repeatedly 6 #(rand-nth room-id-chars))))

(defn normalize-name [s]
  (let [s (str/trim (or s ""))]
    (if (str/blank? s) "Player" (subs s 0 (min 24 (count s))))))

(defn open-seat [seats]
  (first (remove seats game/players)))

(defn open-bot-seat [seats]
  (first (filter #(true? (get-in seats [% :bot?])) game/players)))

(defn new-room [room-id seed]
  {:id room-id
   :seed seed
   :created-at (System/currentTimeMillis)
   :game (game/init-game seed)
   :seats {}
   :connections {}})

(defn seat-player [room player name]
  (assoc-in room [:seats player]
            {:name (normalize-name name)
             :connected? true}))

(defn seat-bot [room player]
  (assoc-in room [:seats player]
            {:name (str "Bot " (last (name player)))
             :connected? true
             :bot? true}))

(defn bot-player? [room player]
  (true? (get-in room [:seats player :bot?])))

(defn fill-bots [room]
  (reduce (fn [room player]
            (if (contains? (:seats room) player)
              room
              (seat-bot room player)))
          room
          game/players))

(defn add-connection [room conn-id player out]
  (-> room
      (assoc-in [:connections conn-id] {:player player :out out})
      (assoc-in [:seats player :connected?] true)
      (dissoc :empty-since)))

(defn mark-empty [room now]
  (if (seq (:connections room))
    (dissoc room :empty-since)
    (assoc room :empty-since (or (:empty-since room) now))))

(defn remove-connection
  ([room conn-id]
   (remove-connection room conn-id (System/currentTimeMillis)))
  ([room conn-id now]
   (let [player (get-in room [:connections conn-id :player])
         room (update room :connections dissoc conn-id)
         bot? (bot-player? room player)
         still-connected? (some #(= player (:player %)) (vals (:connections room)))]
     (-> (cond-> room
           player (assoc-in [:seats player :connected?] (boolean (or bot? still-connected?))))
         (mark-empty now)))))

(defn player-for-join [room requested-player]
  (let [seats (:seats room)]
    (cond
      (and requested-player (some #{requested-player} game/players))
      requested-player

      requested-player
      nil

      :else
      (or (open-seat seats)
          (open-bot-seat seats)))))

(defn join-room [room {:keys [name player out conn-id]}]
  (let [player (player-for-join room player)]
    (when-not player
      (throw (ex-info "Room is full" {:room-id (:id room)})))
    (-> room
        (seat-player player name)
        (add-connection conn-id player out))))

(defn connection [room conn-id]
  (get-in room [:connections conn-id]))

(defn connection-player [room conn-id]
  (:player (connection room conn-id)))

(defn reshuffle-seed [room]
  (hash [(:seed room)
         (get-in room [:game :hand-index])
         (count (get-in room [:game :hand-deals]))
         (System/nanoTime)]))

(defn new-game-seed [room]
  (hash [(:seed room)
         :new-game
         (System/nanoTime)]))

(defn apply-player-event [room conn-id event]
  (let [player (connection-player room conn-id)
        game (:game room)
        event (case (:type event)
                :bid (assoc event :player player)
                :trump-selection (assoc event :player player)
                :donate-card (assoc event :player player)
                :discard-card (assoc event :player player)
                :play-card (assoc event :player player)
                :new-hand event
                :new-game (assoc event :seed (or (:seed event)
                                                 (new-game-seed room)))
                :reshuffle-hand (assoc event :seed (or (:seed event)
                                                       (reshuffle-seed room)))
                event)]
    (cond-> (assoc room :game (game/apply-event game event))
      (= :new-game (:type event))
      (assoc :seed (:seed event)))))

(def bot-advance-limit 96)

(def actionable-phases
  #{:bidding
    :trump-selection
    :karbosh-donation
    :karbosh-discard
    :trick-playing})

(defn active-bot [room]
  (let [player (get-in room [:game :current-player])]
    (when (and player
               (bot-player? room player)
               (actionable-phases (get-in room [:game :phase])))
      player)))

(defn apply-bot-event [room player event]
  (assoc room :game (game/apply-event (:game room) (assoc event :player player))))

(defn auto-play-player [room conn-id]
  (let [player (connection-player room conn-id)
        game (:game room)]
    (when-not player
      (throw (ex-info "Join a room first" {:conn-id conn-id})))
    (when-not (= player (:current-player game))
      (throw (ex-info "Not this player's turn"
                      {:expected (:current-player game)
                       :actual player})))
    (when-not (actionable-phases (:phase game))
      (throw (ex-info "Auto-play is not available in this phase"
                      {:phase (:phase game)})))
    (if-let [event (bot/action game player)]
      (apply-bot-event room player event)
      (throw (ex-info "Auto-play could not choose an action"
                      {:phase (:phase game)
                       :player player})))))

(defn bot-turn [room]
  (when-let [player (active-bot room)]
    (when-let [event (bot/action (:game room) player)]
      {:player player
       :event event})))

(defn advance-bot [room]
  (if-let [{:keys [player event]} (bot-turn room)]
    (apply-bot-event room player event)
    room))

(defn advance-bots
  ([room] (advance-bots room bot-advance-limit))
  ([room limit]
   (loop [room room
          remaining limit]
     (if-let [player (active-bot room)]
       (if (pos? remaining)
         (if-let [{:keys [event]} (bot-turn room)]
           (recur (apply-bot-event room player event) (dec remaining))
           room)
         (assoc room :bot-error "Bot turn limit reached"))
       room))))

(defn connection-views [room]
  (for [[conn-id {:keys [player out]}] (:connections room)]
    {:conn-id conn-id
     :out out
     :message {:op :state
               :room-id (:id room)
               :player player
               :view (game/public-view (:game room) (:seats room) player)}}))
