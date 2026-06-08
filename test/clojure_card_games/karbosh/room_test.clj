(ns clojure-card-games.karbosh.room-test
  (:require [clojure.test :refer [deftest is]]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.room :as room]
            [clojure-card-games.karbosh.shared.game :as game]))

(deftest fill-bots-test
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-player :player1 "Human")
                  (room/fill-bots))]
    (is (= (set game/players) (set (keys (:seats state)))))
    (is (not (room/bot-player? state :player1)))
    (is (room/bot-player? state :player2))
    (is (= "Human" (get-in state [:seats :player1 :name])))))

(deftest bot-personas-test
  (let [persona {:name "Deal-E"
                 :icon "DE"
                 :catchphrase "The adorable card-dealing bot."}
        normalized (assoc persona
                          :style :preservation
                          :play-strategy :hybrid-preservation
                          :ditch-policy :future-suit-equity)
        state (room/seat-bot (room/new-room "ABC123" 9) :player1 persona)]
    (is (= normalized (get-in state [:seats :player1 :persona])))
    (is (= "Deal-E" (get-in state [:seats :player1 :name])))
    (is (= normalized
           (-> (game/public-view (:game state) (:seats state) :player2)
               :players
               first
               :persona)))))

(deftest bidney-gears-persona-test
  (let [names (set (map :name room/bot-personas))
        bidney (some #(when (= "Bidney Gears" (:name %)) %)
                     room/bot-personas)]
    (is (= {:name "Bidney Gears"
            :icon "BG"
            :catchphrase "Oops! I Bid It Again"
            :style :preservation
            :play-strategy :hybrid-preservation
            :ditch-policy :future-suit-equity}
           bidney))
    (is (not (contains? names "Bidney Spears")))
    (is (not (contains? names "Bitney Queers")))))

(deftest bot-persona-strategy-test
  (let [deal-e (some #(when (= "Deal-E" (:name %)) %)
                     room/bot-personas)
        trumpelstiltskin (some #(when (= "Trumpelstiltskin" (:name %)) %)
                               room/bot-personas)
        heart-vader (some #(when (= "Heart Vader" (:name %)) %)
                          room/bot-personas)
        hal-52 (some #(when (= "HAL 52" (:name %)) %)
                     room/bot-personas)
        c-3p-oh-no (some #(when (= "C-3P-Oh No" (:name %)) %)
                         room/bot-personas)
        bid-zeppelin (some #(when (= "Bid Zeppelin" (:name %)) %)
                           room/bot-personas)
        trick-182 (some #(when (= "Trick-182" (:name %)) %)
                        room/bot-personas)
        tuned (room/normalize-bot-persona
               {:name "Deal-E"
                :icon "DE"
                :catchphrase "Tuned bot."
                :style :aggressive
                :play-strategy :hybrid})]
    (is (= :hybrid-preservation bot/default-play-strategy))
    (is (= :hybrid-ruff-invite room/default-auto-play-strategy))
    (is (= :preservation (:style deal-e)))
    (is (= :hybrid-preservation (:play-strategy deal-e)))
    (is (= :aggressive (:style trumpelstiltskin)))
    (is (= :hybrid (:play-strategy trumpelstiltskin)))
    (is (= :aggressive (:style heart-vader)))
    (is (= :hybrid-ruff-invite (:play-strategy heart-vader)))
    (is (= :preservation (:style hal-52)))
    (is (= :hybrid-ruff-invite (:play-strategy hal-52)))
    (is (= :future-suit-equity (:ditch-policy hal-52)))
    (is (= :preservation (:style c-3p-oh-no)))
    (is (= :hybrid-preservation (:play-strategy c-3p-oh-no)))
    (is (= :classic (:ditch-policy c-3p-oh-no)))
    (is (= :aggressive (:style bid-zeppelin)))
    (is (= :hybrid (:play-strategy bid-zeppelin)))
    (is (= :classic (:ditch-policy bid-zeppelin)))
    (is (= :preservation (:style trick-182)))
    (is (= :hybrid-ruff-invite (:play-strategy trick-182)))
    (is (= :classic (:ditch-policy trick-182)))
    (is (= :aggressive (:style tuned)))
    (is (= :hybrid (:play-strategy tuned)))))

(deftest bot-seats-carry-play-strategy-test
  (let [aggressive (some #(when (= "Trumpelstiltskin" (:name %)) %)
                         room/bot-personas)
        preservation (some #(when (= "Deal-E" (:name %)) %)
                           room/bot-personas)
        state (-> (room/new-room "ABC123" 9)
                  (room/seat-bot :player2 aggressive)
                  (room/seat-bot :player3 preservation))]
    (is (= :hybrid (get-in state [:seats :player2 :play-strategy])))
    (is (= :future-suit-equity (get-in state [:seats :player2 :ditch-policy])))
    (is (= :aggressive (get-in state [:seats :player2 :style])))
    (is (= :hybrid-preservation (get-in state [:seats :player3 :play-strategy])))
    (is (= :future-suit-equity (get-in state [:seats :player3 :ditch-policy])))
    (is (= :preservation (get-in state [:seats :player3 :style])))
    (is (= :hybrid (room/bot-play-strategy state :player2)))
    (is (= :future-suit-equity (room/bot-ditch-policy state :player2)))
    (is (= :hybrid-preservation (room/bot-play-strategy state :player3)))))

(deftest bot-turn-binds-seat-play-strategy
  (let [aggressive (some #(when (= "Trumpelstiltskin" (:name %)) %)
                         room/bot-personas)
        state (-> (room/new-room "ABC123" 9)
                  (room/seat-bot :player2 aggressive)
                  (assoc-in [:game :phase] :bidding)
                  (assoc-in [:game :current-player] :player2))]
    (with-redefs [bot/explained-action (fn [_ _]
                                         {:type :observed
                                          :play-strategy bot/*play-strategy*
                                          :ditch-policy (:ditch-policy
                                                         bot/*play-config*)})]
      (is (= {:player :player2
              :event {:type :observed
                      :play-strategy :hybrid
                      :ditch-policy :future-suit-equity}}
             (room/bot-turn state))))))

(deftest fill-bots-samples-distinct-personas
  (let [state (room/fill-bots (room/new-room "ABC123" 9))
        personas (keep #(get-in % [1 :persona]) (:seats state))]
    (is (= 6 (count personas)))
    (is (= (count personas) (count (distinct (map :name personas)))))))

(deftest legacy-bot-seats-get-personas
  (let [state (-> (room/new-room "ABC123" 9)
                  (assoc-in [:seats :player2]
                            {:name "Bot 2"
                             :connected? true
                             :bot? true})
                  (room/ensure-bot-personas))]
    (is (some? (get-in state [:seats :player2 :persona])))
    (is (some? (get-in state [:seats :player2 :play-strategy])))
    (is (some? (get-in state [:seats :player2 :ditch-policy])))
    (is (some? (get-in state [:seats :player2 :style])))
    (is (not= "Bot 2" (get-in state [:seats :player2 :name])))))

(deftest legacy-bot-personas-get-strategy-metadata
  (let [state (-> (room/new-room "ABC123" 9)
                  (assoc-in [:seats :player2]
                            {:name "Trumpelstiltskin"
                             :connected? true
                             :bot? true
                             :persona {:name "Trumpelstiltskin"
                                       :icon "TS"
                                       :catchphrase "Names trump, demands your firstborn."}})
                  (room/ensure-bot-personas))]
    (is (= :aggressive (get-in state [:seats :player2 :style])))
    (is (= :hybrid (get-in state [:seats :player2 :play-strategy])))
    (is (= :future-suit-equity
           (get-in state [:seats :player2 :ditch-policy])))
    (is (= :hybrid (room/bot-play-strategy state :player2)))))

(deftest legacy-ruff-invite-bot-personas-get-strategy-metadata
  (let [state (-> (room/new-room "ABC123" 9)
                  (assoc-in [:seats :player2]
                            {:name "Bender the Rules"
                             :connected? true
                             :bot? true
                             :persona {:name "Bender the Rules"
                                       :icon "BR"
                                       :catchphrase "Absolutely cheats, somehow legally."}})
                  (room/ensure-bot-personas))]
    (is (= :aggressive (get-in state [:seats :player2 :style])))
    (is (= :hybrid-ruff-invite
           (get-in state [:seats :player2 :play-strategy])))
    (is (= :future-suit-equity
           (get-in state [:seats :player2 :ditch-policy])))
    (is (= :hybrid-ruff-invite (room/bot-play-strategy state :player2)))))

(deftest legacy-preservation-ruff-invite-bot-personas-get-strategy-metadata
  (let [state (-> (room/new-room "ABC123" 9)
                  (assoc-in [:seats :player2]
                            {:name "HAL 52"
                             :connected? true
                             :bot? true
                             :persona {:name "HAL 52"
                                       :icon "52"
                                       :catchphrase "Calm voice, murders your strategy."}})
                  (room/ensure-bot-personas))]
    (is (= :preservation (get-in state [:seats :player2 :style])))
    (is (= :hybrid-ruff-invite
           (get-in state [:seats :player2 :play-strategy])))
    (is (= :future-suit-equity
           (get-in state [:seats :player2 :ditch-policy])))
    (is (= :hybrid-ruff-invite (room/bot-play-strategy state :player2)))))

(deftest room-visibility-defaults-to-private
  (is (false? (:public? (room/new-room "ABC123" 9))))
  (is (true? (:public? (room/new-room "ABC123" 9 true))))
  (is (true? (:public? (room/set-public (room/new-room "ABC123" 9) true))))
  (is (false? (:public? (room/set-public (room/new-room "ABC123" 9 true) false)))))

(deftest room-fast-mode-defaults-to-normal-speed
  (let [normal (room/new-room "ABC123" 9)
        fast (room/new-room "ABC123" 9 false true)
        ultra (room/set-speed-mode normal :ultra-fast)]
    (is (false? (:fast-mode? normal)))
    (is (= :normal (:speed-mode normal)))
    (is (true? (:fast-mode? fast)))
    (is (= :fast (:speed-mode fast)))
    (is (= :fast (room/speed-mode (assoc normal :fast-mode? true))))
    (is (true? (:fast-mode? (room/set-fast-mode normal true))))
    (is (false? (:fast-mode? (room/set-fast-mode fast false))))
    (is (= :ultra-fast (:speed-mode ultra)))
    (is (true? (:fast-mode? ultra)))))

(deftest room-seat-counts-ignore-bots-as-players
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-player :player1 "Human")
                  (room/seat-bot :player2))]
    (is (= 1 (room/human-player-count state)))
    (is (= 5 (room/available-seat-count state)))))

(deftest human-can-claim-bot-seat
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-player :player1 "Human")
                  (room/fill-bots))
        joined (room/join-room state {:conn-id :guest
                                      :out nil
                                      :name "Guest"})]
    (is (= :player2 (room/connection-player joined :guest)))
    (is (= "Guest" (get-in joined [:seats :player2 :name])))
    (is (not (room/bot-player? joined :player2)))))

(deftest human-can-choose-open-seat
  (let [joined (room/join-room (room/new-room "ABC123" 9)
                               {:conn-id :guest
                                :out nil
                                :player :player4
                                :name "Guest"})]
    (is (= :player4 (room/connection-player joined :guest)))
    (is (= "Guest" (get-in joined [:seats :player4 :name])))))

(deftest kick-player-clears-seat-and-connections
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/join-room {:conn-id :first
                                   :out nil
                                   :player :player1
                                   :name "First"})
                  (room/join-room {:conn-id :second
                                   :out nil
                                   :player :player2
                                   :name "Second"})
                  (room/kick-player :player2 1000))]
    (is (not (contains? (:seats state) :player2)))
    (is (not (contains? (:connections state) :second)))
    (is (contains? (:connections state) :first))))

(deftest kick-bot-clears-bot-seat
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-bot :player2)
                  (room/kick-player :player2 1000))]
    (is (not (contains? (:seats state) :player2)))
    (is (room/joinable-player? state :player2))))

(deftest ensure-owner-selects-first-human-seat
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-bot :player1)
                  (room/seat-player :player2 "Human")
                  (room/ensure-owner))]
    (is (= :player2 (:owner state)))))

(deftest connection-lifecycle-tracks-empty-room-time
  (let [state (-> (room/new-room "ABC123" 9)
                  (assoc :empty-since 10)
                  (room/join-room {:conn-id :human
                                   :out nil
                                   :name "Human"}))
        empty (room/remove-connection state :human 1234)]
    (is (not (contains? state :empty-since)))
    (is (= 1234 (:empty-since empty)))
    (is (false? (get-in empty [:seats :player1 :connected?])))))

(deftest advance-bots-stops-on-human-turn
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-player :player2 "Human")
                  (room/fill-bots)
                  (room/advance-bots))]
    (is (= :player2 (get-in state [:game :current-player])))
    (is (= :bidding (get-in state [:game :phase])))
    (is (= :player1 (-> state :game :history first :player)))))

(deftest human-action-advances-bot-turns
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/join-room {:conn-id :human
                                   :out nil
                                   :name "Human"})
                  (room/fill-bots))
        advanced (-> state
                     (room/apply-player-event :human {:type :bid :bid-type :pass})
                     (room/advance-bots))
        phase (get-in advanced [:game :phase])
        current-player (get-in advanced [:game :current-player])]
    (is
     (or (#{:hand-complete :game-over} phase)
         (not (room/bot-player? advanced current-player))))))

(deftest auto-play-applies-bot-action-for-human-turn
  (let [observed-strategy (atom nil)]
    (with-redefs [bot/explained-action (fn [_ _]
                                         (reset! observed-strategy bot/*play-strategy*)
                                         {:type :bid
                                          :bid-type :pass
                                          :ai {:policy bot/*play-strategy*}})]
      (let [state (room/join-room (room/new-room "ABC123" 9)
                                  {:conn-id :human
                                   :out nil
                                   :name "Human"})
            advanced (room/auto-play-player state :human)
            event (-> advanced :game :history first)]
        (is (= :bid (:type event)))
        (is (= :player1 (:player event)))
        (is (= :hybrid-ruff-invite (get-in event [:ai :policy])))
        (is (= :hybrid-ruff-invite @observed-strategy))))))

(deftest new-game-preserves-completed-game-history
  (let [completed (-> (room/new-room "ABC123" 9)
                      (assoc :game-started-at 1000)
                      (assoc-in [:game :phase] :game-over)
                      (assoc-in [:game :winner] 1)
                      (assoc-in [:game :scores] {1 52 2 10}))
        next-room (room/apply-player-event completed nil {:type :new-game
                                                          :seed 42})
        entry (first (:games next-room))]
    (is (= 1 (count (:games next-room))))
    (is (= 0 (:game-index entry)))
    (is (= 9 (:seed entry)))
    (is (= 1000 (:started-at entry)))
    (is (= :completed (:ended-reason entry)))
    (is (not (:abandoned? entry)))
    (is (= :game-over (get-in entry [:game :phase])))
    (is (= 1 (get-in entry [:game :winner])))
    (is (= 1 (:game-index next-room)))
    (is (= 42 (:seed next-room)))
    (is (= 42 (get-in next-room [:game :initial-seed])))
    (is (= :bidding (get-in next-room [:game :phase])))))

(deftest new-game-can-start-from-active-game
  (let [active (-> (room/new-room "ABC123" 9)
                   (assoc :game-started-at 1000)
                   (assoc-in [:game :phase] :trick-playing)
                   (assoc-in [:game :current-player] :player4)
                   (assoc-in [:game :scores] {1 12 2 9}))
        next-room (room/apply-player-event active nil {:type :new-game
                                                       :seed 42})
        entry (first (:games next-room))]
    (is (= 1 (count (:games next-room))))
    (is (= :trick-playing (get-in entry [:game :phase])))
    (is (= {1 12 2 9} (get-in entry [:game :scores])))
    (is (= :abandoned (:ended-reason entry)))
    (is (true? (:abandoned? entry)))
    (is (= 1 (:game-index next-room)))
    (is (= 42 (:seed next-room)))
    (is (= 42 (get-in next-room [:game :initial-seed])))
    (is (= :bidding (get-in next-room [:game :phase])))
    (is (= {1 0 2 0} (get-in next-room [:game :scores])))))

(deftest auto-play-requires-current-player
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-player :player2 "Human")
                  (room/add-connection :human :player2 nil))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Not this player's turn"
         (room/auto-play-player state :human)))))
