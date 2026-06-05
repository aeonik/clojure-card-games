(ns clojure-card-games.karbosh.room-test
  (:require [clojure.test :refer [deftest is]]
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
        state (room/seat-bot (room/new-room "ABC123" 9) :player1 persona)]
    (is (= persona (get-in state [:seats :player1 :persona])))
    (is (= "Deal-E" (get-in state [:seats :player1 :name])))
    (is (= persona
           (-> (game/public-view (:game state) (:seats state) :player2)
               :players
               first
               :persona)))))

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
    (is (not= "Bot 2" (get-in state [:seats :player2 :name])))))

(deftest room-visibility-defaults-to-private
  (is (false? (:public? (room/new-room "ABC123" 9))))
  (is (true? (:public? (room/new-room "ABC123" 9 true))))
  (is (true? (:public? (room/set-public (room/new-room "ABC123" 9) true))))
  (is (false? (:public? (room/set-public (room/new-room "ABC123" 9 true) false)))))

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
  (let [state (room/join-room (room/new-room "ABC123" 9)
                              {:conn-id :human
                               :out nil
                               :name "Human"})
        advanced (room/auto-play-player state :human)
        event (-> advanced :game :history first)]
    (is (= :bid (:type event)))
    (is (= :player1 (:player event)))))

(deftest auto-play-requires-current-player
  (let [state (-> (room/new-room "ABC123" 9)
                  (room/seat-player :player2 "Human")
                  (room/add-connection :human :player2 nil))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Not this player's turn"
         (room/auto-play-player state :human)))))
