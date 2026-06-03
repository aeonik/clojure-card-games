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
