(ns clojure-card-games.basic-test
  (:require [clojure.test :refer :all]
            [clojure-card-games.basic :as karbosh]
            [zprint.core :as zp]))

(defn play-trick
  "Helper to play a complete trick. cards is a vector of [player card] pairs."
  [game-state cards]
  (reduce (fn [state [player card]]
            (karbosh/apply-event state {:type :play-card
                                        :player player
                                        :card card}))
          game-state
          cards))

;; Game State Transition Tests
(deftest game-phase-tests
  (testing "Game initialization"
    (let [game (karbosh/init-game)]
      (is (= :bidding (:phase game)) "Game should start in bidding phase")
      (is (= [] (:history game)) "Game should start with empty history")
      (is (vector? (:deck game)) "Should have a deck")))  ;; Updated path

  (testing "Bid phase transitions"
    (let [initial-game (karbosh/init-game)
          bid-event {:type :bid
                     :player :player1
                     :bid-type :regular
                     :value 5}
          game-after-bid (karbosh/apply-event initial-game bid-event)]
      (is (= bid-event (last (:history game-after-bid))) "Event should be recorded in history")
      (is (= :player2 (:next-to-bid game-after-bid)) "Should track next bidder")))

  (testing "Complete bidding phase"
    (let [game (-> (karbosh/init-game)
                   (karbosh/apply-event {:type :bid :player :player1 :bid-type :regular :value 5})
                   (karbosh/apply-event {:type :bid :player :player2 :bid-type :pass})
                   (karbosh/apply-event {:type :bid :player :player3 :bid-type :pass})
                   (karbosh/apply-event {:type :bid :player :player4 :bid-type :pass})
                   (karbosh/apply-event {:type :bid :player :player5 :bid-type :pass})
                   (karbosh/apply-event {:type :bid :player :player6 :bid-type :pass}))]
      (println "\nFinal game state:")
      (zp/zprint game)
      (println "\nPhase:" (:phase game))
      (println "Current player:" (:current-player game))
      (println "History:" (:history game))
      (is (= :trump-selection (:phase game)) "Should move to trump selection after all bids")
      (is (= :player1 (:current-player game)) "Winning bidder should select trump"))))

;; Deck and Card Tests
(deftest deck-tests
  (testing "Deck initialization"
    (let [deck (karbosh/init-deck)]
      (is (= 48 (count deck)) "Deck should have 48 cards (2 decks, 6 cards per suit)")
      (is (= 2 (count (filter #(= [:A :♥] %) deck))) "Should have 2 Ace of Hearts")
      (is (= 12 (count (filter #(= (second %) :♥) deck))) "Should have 12 Hearts")))

  (testing "Card dealing"
    (let [deck (karbosh/init-deck)
          hands (karbosh/deal-hands deck)]
      (is (= 6 (count hands)) "Should deal to 6 players")
      (is (every? #(= 8 (count %)) hands) "Each hand should have 8 cards"))))

;; Event Application Tests
(deftest event-application-tests
  (testing "Karbosh declaration"
    (let [initial-game (karbosh/init-game)
          karbosh-event {:type :bid
                         :player :player1
                         :bid-type :karbosh}
          game-after-karbosh (karbosh/apply-event initial-game karbosh-event)]
      (is (= :card-exchange (:phase game-after-karbosh)) "Should move to card exchange after karbosh")
      (is (= karbosh-event (last (:history game-after-karbosh))) "Should record karbosh event")))

  (testing "Double karbosh response"
    (let [game-with-karbosh (-> (karbosh/init-game)
                                (karbosh/apply-event {:type :bid
                                                      :player :player1
                                                      :bid-type :karbosh})
                                (karbosh/apply-event {:type :bid
                                                      :player :player2
                                                      :bid-type :double-karbosh}))]
      (is (= :trump-selection (:phase game-with-karbosh)) "Should skip card exchange for double karbosh")
      (is (= :player2 (:current-player game-with-karbosh)) "Double karbosh bidder should select trump"))))

;; Card Play Tests
(deftest card-play-tests
  (testing "Legal play validation"
    (let [game-state {:phase :trick-playing
                      :current-trick [{:player :player1
                                       :card [:A :♥]}]
                      :players {:player2 {:hand [[:K :♥] [:A :♠]]}}}
          valid-play? (karbosh/legal-play? game-state :player2 [:K :♥])
          invalid-play? (karbosh/legal-play? game-state :player2 [:A :♠])]
      (is valid-play? "Should allow following suit")
      (is (not invalid-play?) "Should not allow breaking suit when able to follow"))))

;; Trick Resolution Tests
(deftest trick-resolution-tests
  (testing "Basic trick winning with trump"
    (let [trick [{:player :player1 :card [:J :♥]}
                 {:player :player2 :card [:A :♥]}
                 {:player :player3 :card [:K :♥]}]
          winner (karbosh/resolve-trick trick :♥)]
      (is (= :player1 winner) "Right bower should win"))))

(deftest trump-selection-tests
  (testing "Trump selection after regular bid"
    (let [game-state (-> (karbosh/init-game)
                         (karbosh/apply-event {:type :bid :player :player1 :bid-type :regular :value 5})
                         (karbosh/apply-event {:type :bid :player :player2 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player3 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player4 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player5 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player6 :bid-type :pass}))
          after-trump (karbosh/apply-event game-state
                                           {:type :trump-selection
                                            :player :player1
                                            :suit :♥})]
      (is (= :♥ (:trump after-trump)) "Should set selected trump suit")
      (is (= :trick-playing (:phase after-trump)) "Should move to trick playing phase")
      (is (= :player1 (:current-player after-trump)) "Bid winner should lead first trick")))

  (testing "Trump selection after double karbosh"
    (let [game-state (-> (karbosh/init-game)
                         (karbosh/apply-event {:type :bid :player :player1 :bid-type :karbosh})
                         (karbosh/apply-event {:type :bid :player :player2 :bid-type :double-karbosh}))
          after-trump (karbosh/apply-event game-state
                                           {:type :trump-selection
                                            :player :player2
                                            :suit :♠})]
      (is (= :♠ (:trump after-trump)) "Should set selected trump suit")
      (is (= :trick-playing (:phase after-trump)) "Should move to trick playing phase")
      (is (= :player2 (:current-player after-trump)) "Double karbosh player should lead")))

  (testing "Invalid trump selection"
    (let [game-state (-> (karbosh/init-game)
                         (karbosh/apply-event {:type :bid :player :player1 :bid-type :regular :value 5})
                         (karbosh/apply-event {:type :bid :player :player2 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player3 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player4 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player5 :bid-type :pass})
                         (karbosh/apply-event {:type :bid :player :player6 :bid-type :pass}))]
      ;; Wrong player tries to select trump
      (is (thrown? Exception
                   (karbosh/apply-event game-state
                                        {:type :trump-selection
                                         :player :player2
                                         :suit :♥}))
          "Should reject trump selection from wrong player")
      ;; Invalid suit
      (is (thrown? Exception
                   (karbosh/apply-event game-state
                                        {:type :trump-selection
                                         :player :player1
                                         :suit :X}))
          "Should reject invalid suit selection"))))

(deftest play-card-tests
  (testing "Complete trick resolution"
    (let [game-state {:phase :trick-playing
                      :trump :♥
                      :current-player :player1
                      :trick-count 0
                      :current-trick []
                      :completed-tricks []
                      :players {:player1 {:hand [[:A :♥]] :team 1}
                                :player2 {:hand [[:K :♥]] :team 2}
                                :player3 {:hand [[:Q :♥]] :team 1}
                                :player4 {:hand [[:J :♥]] :team 2}  ; Right bower
                                :player5 {:hand [[:10 :♥]] :team 1}
                                :player6 {:hand [[:9 :♥]] :team 2}}
                      :scores {1 0, 2 0}
                      :history []}
          ;; Play out the trick in sequence
          after-p1 (karbosh/apply-event game-state {:type :play-card :player :player1 :card [:A :♥]})
          after-p2 (karbosh/apply-event after-p1 {:type :play-card :player :player2 :card [:K :♥]})
          after-p3 (karbosh/apply-event after-p2 {:type :play-card :player :player3 :card [:Q :♥]})
          after-p4 (karbosh/apply-event after-p3 {:type :play-card :player :player4 :card [:J :♥]})
          after-p5 (karbosh/apply-event after-p4 {:type :play-card :player :player5 :card [:10 :♥]})
          final-state (karbosh/apply-event after-p5 {:type :play-card :player :player6 :card [:9 :♥]})]

      ;; Verify the complete trick
      (is (empty? (:current-trick final-state)) "Current trick should be cleared")
      (is (= [{:player :player1 :card [:A :♥]}
              {:player :player2 :card [:K :♥]}
              {:player :player3 :card [:Q :♥]}
              {:player :player4 :card [:J :♥]}
              {:player :player5 :card [:10 :♥]}
              {:player :player6 :card [:9 :♥]}]
             (first (:completed-tricks final-state)))
          "Completed trick should be stored")

      ;; Verify hands are empty
      (is (every? #(empty? (:hand %)) (vals (:players final-state)))
          "All hands should be empty")

      ;; Verify trick winner (player4 with Right Bower) leads next
      (is (= :player4 (:current-player final-state))
          "Right bower (J♥) should win when hearts is trump"))))

(deftest full-hand-tests
  (testing "Complete hand with 6 players and 8 tricks"
    (let [game-state {:phase :trick-playing
                      :trump :♥
                      :current-player :player1
                      :trick-leader :player1
                      :trick-count 0
                      :current-trick []
                      :completed-tricks []
                      :players {:player1 {:hand [[:A :♥] [:K :♥] [:Q :♥] [:J :♥] [:10 :♥] [:9 :♥] [:A :♠] [:K :♠]]
                                          :team 1}
                                :player2 {:hand [[:A :♦] [:K :♦] [:Q :♦] [:J :♦] [:10 :♦] [:9 :♦] [:A :♣] [:K :♣]]
                                          :team 2}
                                :player3 {:hand [[:Q :♣] [:J :♣] [:10 :♣] [:9 :♣] [:Q :♠] [:J :♠] [:10 :♠] [:9 :♠]]
                                          :team 1}
                                :player4 {:hand [[:A :♥] [:K :♥] [:Q :♥] [:J :♥] [:10 :♥] [:9 :♥] [:A :♠] [:K :♠]]
                                          :team 2}
                                :player5 {:hand [[:A :♦] [:K :♦] [:Q :♦] [:J :♦] [:10 :♦] [:9 :♦] [:A :♣] [:K :♣]]
                                          :team 1}
                                :player6 {:hand [[:Q :♣] [:J :♣] [:10 :♣] [:9 :♣] [:Q :♠] [:J :♠] [:10 :♠] [:9 :♠]]
                                          :team 2}}
                      :scores {1 0, 2 0}
                      :current-bid {:player :player1 :type :regular :value 5}  ; Added bid info
                      :history []}
          final-state (-> game-state
                          ;; Trick 1 - Player1 leads trump
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:A :♥]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:A :♦]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:Q :♣]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:A :♥]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:A :♦]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:Q :♣]})

                          ;; Trick 2 - Player1 wins and leads trump again
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:K :♥]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:K :♦]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:J :♣]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:K :♥]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:K :♦]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:J :♣]})

                          ;; Trick 3
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:Q :♥]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:Q :♦]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:10 :♣]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:Q :♥]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:Q :♦]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:10 :♣]})

                          ;; Trick 4
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:J :♥]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:J :♦]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:9 :♣]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:J :♥]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:J :♦]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:9 :♣]})

                          ;; Trick 5
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:10 :♥]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:10 :♦]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:Q :♠]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:10 :♥]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:10 :♦]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:Q :♠]})

                          ;; Trick 6
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:9 :♥]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:9 :♦]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:J :♠]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:9 :♥]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:9 :♦]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:J :♠]})

                          ;; Trick 7 - Switch to spades
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:A :♠]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:A :♣]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:10 :♠]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:A :♠]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:A :♣]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:10 :♠]})

                          ;; Trick 8 - Final trick
                          (karbosh/apply-event {:type :play-card :player :player1 :card [:K :♠]})
                          (karbosh/apply-event {:type :play-card :player :player2 :card [:K :♣]})
                          (karbosh/apply-event {:type :play-card :player :player3 :card [:9 :♠]})
                          (karbosh/apply-event {:type :play-card :player :player4 :card [:K :♠]})
                          (karbosh/apply-event {:type :play-card :player :player5 :card [:K :♣]})
                          (karbosh/apply-event {:type :play-card :player :player6 :card [:9 :♠]}))]

      ;; Basic completion checks
      (is (= 8 (count (:completed-tricks final-state))) "Should have 8 completed tricks")
      (is (empty? (:current-trick final-state)) "Current trick should be empty")
      (is (= 8 (:trick-count final-state)) "Should have counted 8 tricks")

      ;; Hand emptiness checks
      (is (every? (fn [[_ player]] (empty? (:hand player)))
                  (:players final-state))
          "All player hands should be empty")

      ;; Score checks - Team 1 (Player1's team) should win most tricks due to trump
      (let [team1-score (get-in final-state [:scores 1])
            team2-score (get-in final-state [:scores 2])]
        (is (= 8 (+ team1-score team2-score)) "Total tricks should be 8")
        (is (> team1-score team2-score) "Team 1 should win more tricks with trump control")

        ;; Check if bid was made
        (let [bid-value (get-in game-state [:current-bid :value])
              bidder-team 1]  ; Player1's team
          (is (>= team1-score bid-value)
              (str "Team 1 bid " bid-value " and took " team1-score " tricks")))))))

(deftest scoring-tests
  (testing "Regular bid scoring"
    (let [base-state {:phase :trick-playing
                      :current-bid {:player :player1 :type :regular :value 5}
                      :trick-count 8  ; Hand complete
                      :players {:player1 {:team 1} :player2 {:team 2}
                                :player3 {:team 1} :player4 {:team 2}
                                :player5 {:team 1} :player6 {:team 2}}
                      :scores {1 0, 2 0}}]

      ;; Made bid with exact tricks
      (let [final-state (assoc base-state
                               :scores {1 5, 2 3})]  ; Team 1 got 5 tricks, Team 2 got 3
        (is (= {1 5, 2 0}
               (karbosh/resolve-scoring final-state))
            "When making exact bid, bidding team gets points equal to tricks taken"))

      ;; Made bid with extra tricks
      (let [final-state (assoc base-state
                               :scores {1 6, 2 2})]  ; Team 1 got 6 tricks, Team 2 got 2
        (is (= {1 6, 2 0}
               (karbosh/resolve-scoring final-state))
            "When exceeding bid, bidding team gets points equal to tricks taken"))

      ;; Failed bid
      (let [final-state (assoc base-state
                               :scores {1 4, 2 4})]  ; Team 1 got 4 tricks, Team 2 got 4
        (is (= {1 -5, 2 4}
               (karbosh/resolve-scoring final-state))
            "When failing bid, bidding team goes down by bid amount, defenders get their tricks"))))

  (testing "Karbosh scoring"
    (let [base-state {:phase :trick-playing
                      :current-bid {:player :player1 :type :karbosh}
                      :trick-count 8
                      :players {:player1 {:team 1} :player2 {:team 2}
                                :player3 {:team 1} :player4 {:team 2}
                                :player5 {:team 1} :player6 {:team 2}}
                      :scores {1 0, 2 0}}]

      ;; Successful karbosh
      (let [final-state (assoc base-state
                               :scores {1 5, 2 3})]
        (is (= {1 15, 2 0}
               (karbosh/resolve-scoring final-state))
            "Successful karbosh awards 15 points"))

      ;; Failed karbosh
      (let [final-state (assoc base-state
                               :scores {1 3, 2 5})]
        (is (= {1 -15, 2 5}
               (karbosh/resolve-scoring final-state))
            "Failed karbosh loses 15 points, defenders get their tricks"))))

  (testing "Double karbosh scoring"
    (let [base-state {:phase :trick-playing
                      :current-bid {:player :player2 :type :double-karbosh}
                      :trick-count 8
                      :players {:player1 {:team 1} :player2 {:team 2}
                                :player3 {:team 1} :player4 {:team 2}
                                :player5 {:team 1} :player6 {:team 2}}
                      :scores {1 0, 2 0}}]

      ;; Successful double karbosh
      (let [final-state (assoc base-state
                               :scores {1 3, 2 5})]
        (is (= {1 0, 2 15}
               (karbosh/resolve-scoring final-state))
            "Successful double karbosh awards 15 points"))

      ;; Failed double karbosh
      (let [final-state (assoc base-state
                               :scores {1 5, 2 3})]
        (is (= {1 5, 2 -15}
               (karbosh/resolve-scoring final-state))
            "Failed double karbosh loses 15 points, defenders get their tricks")))))
