(ns clojure-card-games.karbosh.room
  (:require [clojure.string :as str]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.game :as game]))

(def room-id-chars "ABCDEFGHJKLMNPQRSTUVWXYZ23456789")

(def bot-personas
  [{:name "Deal-E" :icon "DE" :catchphrase "The adorable card-dealing bot."}
   {:name "Shuffleupagus" :icon "SH" :catchphrase "Ancient, chaotic shuffle beast."}
   {:name "Ace Ventura 3000" :icon "A3" :catchphrase "Pet detective, but for aces."}
   {:name "Sir Shufflesworth" :icon "SS" :catchphrase "Fancy-ass British card bot."}
   {:name "Cardi-Bot" :icon "CB" :catchphrase "Loud, flashy, probably wins."}
   {:name "Bot Flushmore" :icon "BF" :catchphrase "Presidential-level flush hunter."}
   {:name "Robo-Cop-a-Card" :icon "RC" :catchphrase "Enforces table rules poorly."}
   {:name "Trick-182" :icon "182" :catchphrase "Always takes one more trick than expected."}
   {:name "Bender the Rules" :icon "BR" :catchphrase "Absolutely cheats, somehow legally."}
   {:name "Clank Sinatra" :icon "CS" :catchphrase "Sings while bidding."}
   {:name "Optimus Prime Suit" :icon "OP" :catchphrase "Always calls trump."}
   {:name "The Termin-Dealer" :icon "TD" :catchphrase "I'll be back... after the redeal."}
   {:name "C-3P-Oh No" :icon "C3" :catchphrase "Catastrophic misplays only."}
   {:name "R2-Dealt-You" :icon "R2" :catchphrase "Cheerful little bastard."}
   {:name "HAL 52" :icon "52" :catchphrase "Calm voice, murders your strategy."}
   {:name "Bidney Spears" :icon "BS" :catchphrase "Oops, I bid it again."}
   {:name "Queen Latifah-Bot" :icon "QB" :catchphrase "Royal suit energy."}
   {:name "JackGPT" :icon "JG" :catchphrase "Confidently explains why its terrible play was optimal."}
   {:name "Trumpelstiltskin" :icon "TS" :catchphrase "Names trump, demands your firstborn."}
   {:name "Bot Damon" :icon "BD" :catchphrase "How do you like them apples?"}
   {:name "Mecha Streisand" :icon "MS" :catchphrase "Makes every hand dramatic."}
   {:name "Suit R2" :icon "SR" :catchphrase "Tiny robot obsessed with suits."}
   {:name "Deckard Cain't" :icon "DC" :catchphrase "Identifies cards, cannot win."}
   {:name "The Great Cardini" :icon "GC" :catchphrase "Magician bot who accidentally palms cards."}
   {:name "Karbosh Kardashian" :icon "KK" :catchphrase "Famous for going alone and causing drama."}
   {:name "Bid Zeppelin" :icon "BZ" :catchphrase "Heavy bids, louder losses."}
   {:name "Clubs Bunny" :icon "CL" :catchphrase "Cartoon menace in clubs."}
   {:name "Spade Invader" :icon "SI" :catchphrase "Retro arcade card killer."}
   {:name "Heart Vader" :icon "HV" :catchphrase "I find your lack of trump disturbing."}
   {:name "Diamond Dallas Page Fault" :icon "DD" :catchphrase "Wrestler/programmer/card pun abomination."}
   {:name "Rusty Shacklebot" :icon "RS" :catchphrase "Paranoid, overbuilt, probably running Arch."}
   {:name "Null Pointer Jackception" :icon "NP" :catchphrase "Crashes when dealt two jacks."}
   {:name "Stack Overflower" :icon "SO" :catchphrase "Asks the table how to play mid-hand."}
   {:name "Heap Trick" :icon "HT" :catchphrase "Memory-safe? Absolutely not."}
   {:name "Kenny Loggins' Danger Zone of No Trump" :icon "DZ" :catchphrase "Cursed long name, worth it."}
   {:name "The Bid Lebowski" :icon "BL" :catchphrase "The Dude abides... and passes."}
   {:name "Tony Starkboard" :icon "TSB" :catchphrase "Genius robot with a terrible poker face."}
   {:name "Johnny Five-Card Draw" :icon "J5" :catchphrase "Alive, but bad at trick-taking."}
   {:name "Megabyte Me" :icon "MM" :catchphrase "Bites off more bid than it can chew."}
   {:name "Cache Money" :icon "CM" :catchphrase "Wins now, forgets later."}
   {:name "Sudo Shuffle" :icon "SU" :catchphrase "Demands admin rights to deal."}
   {:name "Kernel Panic Jack" :icon "KP" :catchphrase "Folds under pressure."}
   {:name "Regex Rex" :icon "RX" :catchphrase "Matches every suit except the one you need."}
   {:name "Bitney Queers" :icon "BQ" :catchphrase "It's trick, bitch."}
   {:name "Sir Bids-a-Lot" :icon "SB" :catchphrase "Cannot lie, loves big contracts."}
   {:name "The Notorious R.O.B." :icon "ROB" :catchphrase "Steals tricks."}
   {:name "Cardashian Westworld" :icon "CW" :catchphrase "Too expensive, overly dramatic."}
   {:name "Trick Astley" :icon "TA" :catchphrase "Never gonna give you up, never gonna let you trump."}
   {:name "Decks Machina" :icon "DM" :catchphrase "Divine intervention, but with cards."}
   {:name "Botzilla" :icon "BZL" :catchphrase "Stomps the table when euchred."}
   {:name "Shufflin' Around and Find Out" :icon "FA" :catchphrase "Self-explanatory."}])

(defn random-room-id []
  (apply str (repeatedly 6 #(rand-nth room-id-chars))))

(defn normalize-name [s]
  (let [s (str/trim (or s ""))]
    (if (str/blank? s) "Player" (subs s 0 (min 24 (count s))))))

(defn seated-bot-persona-names [room]
  (set (keep (fn [[_ seat]]
               (when (:bot? seat)
                 (get-in seat [:persona :name])))
             (:seats room))))

(defn available-bot-personas [room]
  (let [used (seated-bot-persona-names room)
        available (remove #(contains? used (:name %)) bot-personas)]
    (vec (or (seq available) bot-personas))))

(defn random-bot-persona [room]
  (rand-nth (available-bot-personas room)))

(defn open-seat [seats]
  (first (remove seats game/players)))

(defn open-bot-seat [seats]
  (first (filter #(true? (get-in seats [% :bot?])) game/players)))

(defn joinable-seat? [seat]
  (or (nil? seat)
      (true? (:bot? seat))))

(defn human-seat? [seat]
  (and seat (not (:bot? seat))))

(defn joinable-player? [room player]
  (joinable-seat? (get-in room [:seats player])))

(defn available-seat-count [room]
  (count (filter #(joinable-player? room %) game/players)))

(defn human-player-count [room]
  (count (filter human-seat? (vals (:seats room)))))

(defn set-public [room public?]
  (assoc room :public? (true? public?)))

(defn new-room
  ([room-id seed]
   (new-room room-id seed false))
  ([room-id seed public?]
   {:id room-id
    :seed seed
    :created-at (System/currentTimeMillis)
    :owner nil
    :public? (true? public?)
    :game (game/init-game seed)
    :seats {}
    :connections {}}))

(defn seat-player [room player name]
  (assoc-in room [:seats player]
            {:name (normalize-name name)
             :connected? true}))

(defn seat-bot
  ([room player]
   (seat-bot room player (random-bot-persona room)))
  ([room player persona]
   (assoc-in room [:seats player]
             {:name (:name persona)
              :connected? true
              :bot? true
              :persona persona})))

(defn bot-player? [room player]
  (true? (get-in room [:seats player :bot?])))

(defn ensure-bot-personas [room]
  (reduce (fn [room player]
            (if (and (bot-player? room player)
                     (nil? (get-in room [:seats player :persona])))
              (seat-bot room player)
              room))
          room
          game/players))

(defn first-human-player [room]
  (first (filter #(human-seat? (get-in room [:seats %])) game/players)))

(defn ensure-owner [room]
  (if (:owner room)
    room
    (if-let [owner (first-human-player room)]
      (assoc room :owner owner)
      room)))

(defn ensure-room-metadata [room]
  (-> room
      (ensure-bot-personas)
      (ensure-owner)))

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

(defn player-connections [room player]
  (select-keys (:connections room)
               (for [[conn-id connection] (:connections room)
                     :when (= player (:player connection))]
                 conn-id)))

(defn occupied-player? [room player]
  (contains? (:seats room) player))

(defn kick-player
  ([room player]
   (kick-player room player (System/currentTimeMillis)))
  ([room player now]
   (when-not (some #{player} game/players)
     (throw (ex-info "Unknown player" {:player player})))
   (when-not (occupied-player? room player)
     (throw (ex-info "Seat is empty" {:player player})))
   (let [conn-ids (keys (player-connections room player))]
     (-> room
         (update :connections #(apply dissoc % conn-ids))
         (update :seats dissoc player)
         (mark-empty now)))))

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
               :view (assoc (game/public-view (:game room) (:seats room) player)
                            :owner (:owner room)
                            :can-kick? (= player (:owner room))
                            :public? (true? (:public? room)))}}))
