(ns clojure-card-games.karbosh.shared.game-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.fixtures :as fixtures]
            [clojure-card-games.karbosh.shared.game :as game]))

(deftest bidding-test
  (let [state (-> (game/init-game 7)
                  (game/apply-event {:type :bid :player :player1 :bid-type :bid :value 4})
                  (game/apply-event {:type :bid :player :player2 :bid-type :pass})
                  (game/apply-event {:type :bid :player :player3 :bid-type :bid :value 6})
                  (game/apply-event {:type :bid :player :player4 :bid-type :pass})
                  (game/apply-event {:type :bid :player :player5 :bid-type :pass})
                  (game/apply-event {:type :bid :player :player6 :bid-type :pass}))]
    (is (= :trump-selection (:phase state)))
    (is (= :player3 (:current-player state)))
    (is (= {:type :bid :player :player3 :bid-type :bid :value 6}
           (game/current-bid state)))
    (is (not (contains? state :current-bid)))
    (is (= [{:player :player1 :bid-type :bid :value 4}
            {:player :player2 :bid-type :pass}
            {:player :player3 :bid-type :bid :value 6}
            {:player :player4 :bid-type :pass}
            {:player :player5 :bid-type :pass}
            {:player :player6 :bid-type :pass}]
           (:bids-this-hand (game/public-view state {} :player1))))))

(deftest remove-first-test
  (is (= [:a :b]     (game/remove-first :c [:a :b])))
  (is (= [:b :c]     (game/remove-first :a [:a :b :c])))
  (is (= [:a :c :b]  (game/remove-first :b [:a :b :c :b])))
  (is (= [:a :b :c]  (game/remove-first :x [:a :b :c]))))

(deftest turn-and-trump-guards-test
  (testing "out of turn bids are rejected"
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"turn"
         (game/apply-event (game/init-game 7)
                           {:type :bid :player :player2 :bid-type :bid :value 4}))))

  (testing "invalid trump suits are rejected"
    (let [state (reduce game/apply-event
                        (game/init-game 7)
                        (cons {:type :bid :player :player1 :bid-type :bid :value 4}
                              (map (fn [p] {:type :bid :player p :bid-type :pass})
                                   [:player2 :player3 :player4 :player5 :player6])))]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Invalid trump"
           (game/apply-event state {:type :trump-selection
                                    :player :player1
                                    :suit :stars}))))))

(deftest illegal-overcall-test
  (let [state (game/apply-event (game/init-game 7)
                                {:type :bid
                                 :player :player1
                                 :bid-type :bid
                                 :value 4})]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not beat"
         (game/apply-event state {:type :bid
                                  :player :player2
                                  :bid-type :bid
                                  :value 3})))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"does not beat"
         (game/apply-event state {:type :bid
                                  :player :player2
                                  :bid-type :bid
                                  :value 4})))
    (let [passed (game/apply-event state {:type :bid
                                          :player :player2
                                          :bid-type :pass})]
      (is (= :player3 (:current-player passed)))
      (is (= [{:player :player1 :bid-type :bid :value 4}
              {:player :player2 :bid-type :pass}]
             (:bids-this-hand (game/public-view passed {} :player1)))))))

(deftest reshuffle-hand-test
  (let [state (game/init-game 7)
        original-hands (game/player-hands state)
        reshuffled (game/apply-event state {:type :reshuffle-hand :seed 8})]
    (is (= :bidding (:phase reshuffled)))
    (is (not= original-hands (game/player-hands reshuffled)))
    (is (= (game/player-hands reshuffled) (:initial-hands reshuffled)))
    (is (= 2 (count (:hand-deals reshuffled))))
    (is (= 8 (-> reshuffled :hand-deals last :seed))))

  (let [state (-> (game/init-game 7)
                  (game/apply-event {:type :bid
                                     :player :player1
                                     :bid-type :pass}))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Cannot reshuffle"
         (game/apply-event state {:type :reshuffle-hand :seed 9})))))

(deftest bl32c2-seed-reproduces-regression-hand-test
  (let [fixture fixtures/bl32c2
        state (game/init-game (:seed fixture))]
    (is (= (:initial-hands fixture)
           (:initial-hands state)))
    (is (= (:seed fixture)
           (-> state :hand-deals first :seed)))))

(defn complete-with-passes [state]
  (reduce (fn [state player]
            (game/apply-event state {:type :bid
                                     :player player
                                     :bid-type :pass}))
          state
          (:bidding-order state)))

(deftest bidding-order-rotates-test
  (let [hand-1 (game/init-game 7)
        hand-2 (-> hand-1
                   complete-with-passes
                   (game/apply-event {:type :new-hand}))
        hand-3 (-> hand-2
                   complete-with-passes
                   (game/apply-event {:type :new-hand}))]
    (is (= :player1 (:current-player hand-1)))
    (is (= :player1 (:dealer hand-1)))
    (is (= :player1 (:dealer (game/public-view hand-1 {} :player1))))
    (is (= game/players (:bidding-order hand-1)))
    (is (= :player2 (:current-player hand-2)))
    (is (= :player2 (:dealer hand-2)))
    (is (= :player2 (:dealer (game/public-view hand-2 {} :player1))))
    (is (= [:player2 :player3 :player4 :player5 :player6 :player1]
           (:bidding-order hand-2)))
    (is (= :player3 (:current-player hand-3)))
    (is (= :player3 (:dealer hand-3)))
    (is (= :player3 (:dealer (game/public-view hand-3 {} :player1))))
    (is (= [:player3 :player4 :player5 :player6 :player1 :player2]
           (:bidding-order hand-3)))))

(deftest play-test
  (testing "players must follow suit"
    (let [state {:phase :trick-playing
                 :history []
                 :players {:player1 {:hand [[:A :♥]] :team 1}
                           :player2 {:hand [[:K :♥] [:A :♠]] :team 2}}
                 :current-player :player1
                 :current-trick []
                 :trump :♠}]
      (is (thrown-with-msg?
           clojure.lang.ExceptionInfo
           #"Illegal"
           (-> state
               (game/apply-event {:type :play-card :player :player1 :card [:A :♥]})
               (game/apply-event {:type :play-card :player :player2 :card [:A :♠]}))))))

  (testing "ai metadata stays in history but not public table state"
    (let [state {:phase :trick-playing
                 :history []
                 :players {:player1 {:hand [[:A :♥]] :team 1}
                           :player2 {:hand [[:K :♥]] :team 2}}
                 :active-players [:player1 :player2]
                 :current-player :player1
                 :current-trick []
                 :completed-tricks []
                 :trump :♠}
          ai {:policy :hybrid-ruff-invite
              :reason :lead-safe-card}
          updated (game/apply-event state {:type :play-card
                                           :player :player1
                                           :card [:A :♥]
                                           :ai ai})]
      (is (= ai (-> updated :history first :ai)))
      (is (= ai (-> updated :current-trick first :ai)))
      (is (nil? (-> (game/public-view updated {} :player1)
                    :current-trick
                    first
                    :ai))))))

(def karbosh-state
  {:phase :trump-selection
   :history []
   :bids [{:type :bid :player :player1 :bid-type :karbosh :hand-index 0}]
   :trumps []
   :hand-index 0
   :scores {1 0 2 0}
   :tricks-this-hand {1 0 2 0}
   :completed-tricks []
   :current-player :player1
   :players {:player1 {:hand [[:J :♠] [:A :♠] [9 :♥] [10 :♦] [:Q :♣] [:K :♥] [9 :♦] [10 :♣]]
                       :team 1}
             :player2 {:hand [[:Q :♠] [9 :♣] [10 :♥] [:K :♦] [:A :♣] [9 :♠] [:Q :♥] [:J :♦]]
                       :team 2}
             :player3 {:hand [[:K :♠] [9 :♥] [10 :♣] [:Q :♦] [:A :♥] [:J :♥] [9 :♦] [:K :♣]]
                       :team 1}
             :player4 {:hand [[:A :♠] [:K :♦] [10 :♦] [:Q :♣] [9 :♣] [:J :♦] [:A :♥] [10 :♥]]
                       :team 2}
             :player5 {:hand [[:Q :♠] [10 :♠] [9 :♠] [:A :♦] [:K :♥] [:Q :♥] [9 :♥] [:J :♣]]
                       :team 1}
             :player6 {:hand [[:K :♠] [:Q :♦] [10 :♠] [9 :♦] [:A :♦] [:J :♥] [:K :♣] [:A :♣]]
                       :team 2}}})

(deftest karbosh-flow-test
  (let [state (game/apply-event karbosh-state
                                {:type :trump-selection :player :player1 :suit :♠})]
    (is (= :karbosh-discard (:phase state)))
    (is (= :player1 (:current-player state)))
    (is (= [:player1 :player2 :player4 :player6] (:active-players state)))

    (let [state (-> state
                    (game/apply-event {:type :discard-card :player :player1 :card [9 :♥]})
                    (game/apply-event {:type :discard-card :player :player1 :card [10 :♦]}))]
      (is (= :karbosh-donation (:phase state)))
      (is (= :player3 (:current-player state)))
      (is (= 6 (count (get-in state [:players :player1 :hand]))))

      (let [state (-> state
                      (game/apply-event {:type :donate-card :player :player3 :card [:K :♠]})
                      (game/apply-event {:type :donate-card :player :player5 :card [:Q :♠]}))]
        (is (= :trick-playing (:phase state)))
        (is (= 8 (count (get-in state [:players :player1 :hand]))))
        (is (= [:player1 :player2 :player4 :player6] (game/trick-players state)))

        (let [state (-> state
                        (game/apply-event {:type :play-card :player :player1 :card [:J :♠]})
                        (game/apply-event {:type :play-card :player :player2 :card [:Q :♠]})
                        (game/apply-event {:type :play-card :player :player4 :card [:A :♠]})
                        (game/apply-event {:type :play-card :player :player6 :card [:K :♠]}))]
          (is (= 1 (count (:completed-tricks state))))
          (is (= :player1 (:current-player state)))
          (is (= {1 1 2 0} (:tricks-this-hand state))))))))

(deftest double-karbosh-skips-donation-test
  (let [state (-> (assoc karbosh-state
                         :bids [{:type :bid
                                 :player :player1
                                 :bid-type :double-karbosh
                                 :hand-index 0}])
                  (game/apply-event {:type :trump-selection
                                     :player :player1
                                     :suit :♠}))]
    (is (= :trick-playing (:phase state)))
    (is (= [:player1 :player2 :player4 :player6] (:active-players state)))
    (is (nil? (:donation-order state)))
    (is (= 8 (count (get-in state [:players :player1 :hand]))))))

(def one-trick-events
  [{:type :play-card :player :player1 :card [:A :♥]}
   {:type :play-card :player :player2 :card [:K :♥]}
   {:type :play-card :player :player3 :card [:Q :♥]}
   {:type :play-card :player :player4 :card [:J :♥]}
   {:type :play-card :player :player5 :card [10 :♥]}
   {:type :play-card :player :player6 :card [9 :♥]}])

(def one-trick-hands
  {:player1 [[:A :♥]]
   :player2 [[:K :♥]]
   :player3 [[:Q :♥]]
   :player4 [[:J :♥]]
   :player5 [[10 :♥]]
   :player6 [[9 :♥]]})

(def one-trick-state
  {:phase :trick-playing
   :history []
   :players (into {}
                  (map (fn [[player hand]]
                         [player {:hand hand
                                  :team (get (game/teams) player)}])
                       one-trick-hands))
   :active-players game/players
   :current-player :player1
   :current-trick []
   :trump :♠
   :scores {1 0 2 0}
   :tricks-this-hand {1 0 2 0}
   :completed-tricks []
   :bids [{:type :bid
           :player :player1
           :bid-type :bid
           :value 1
           :hand-index 0}]
   :hand-index 0
   :initial-hands one-trick-hands
   :hand-deals [{:hand-index 0
                 :seed 1
                 :hands one-trick-hands}]
   :hand-history []
   :tricks-per-hand []
   :points-per-hand []})

(deftest hand-history-test
  (let [state (reduce game/apply-event one-trick-state one-trick-events)
        summary (first (:hand-history state))]
    (is (= :hand-complete (:phase state)))
    (is (= 1 (count (:hand-history state))))
    (is (= (:initial-hands one-trick-state) (:initial-hands summary)))
    (is (= one-trick-events (:history summary)))
    (is (= 1 (count (:completed-tricks summary))))
    (is (= {1 1 2 0} (:tricks-this-hand state)))
    (is (= {1 1 2 0} (:tricks summary)))
    (is (= {1 1 2 0} (:points summary)))))

(deftest game-over-test
  (let [state (reduce game/apply-event
                      (assoc one-trick-state :scores {1 51 2 0})
                      one-trick-events)
        new-game (game/apply-event state {:type :new-game :seed 99})]
    (is (= :game-over (:phase state)))
    (is (= 1 (:winner state)))
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo
         #"Event is not valid"
         (game/apply-event state {:type :new-hand})))
    (is (= :bidding (:phase new-game)))
    (is (= {1 0 2 0} (:scores new-game)))
    (is (= 0 (:hand-index new-game)))
    (is (nil? (:winner new-game)))))

(deftest public-view-redacts-debug-state
  (let [state (game/init-game 1)
        public (game/public-view state {} :player1)
        admin (game/admin-view state {})]
    (is (not (contains? public :debug)))
    (is (= (get-in state [:players :player1 :hand])
           (:hand public)))
    (is (contains? (:debug admin) :hands))
    (is (= (game/player-hands state)
           (get-in admin [:debug :hands])))))
