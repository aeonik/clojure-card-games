(ns clojure-card-games.core-test
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pp]
            [clojure-card-games.core :refer :all]
            [zprint.core :as zp]))

(deftest test-init-deck
  (testing "Deck initialization"
    (let [deck (karbosh-deck)]
      (is (= 48 (count deck)) "Deck should contain 48 cards (2 decks of 9-Ace in 4 suits)")
      (is (= 12 (count (filter #(= :♥ (second %)) deck))) "Each suit should have 12 cards"))))

(deftest test-shuffled-deck
  (testing "Shuffling the deck"
    (let [deck (karbosh-deck)
          shuffled (shuffle deck)]
      (println "Deck: " deck)
      (println "Shuffled: " shuffled)
      (is (= (set deck) (set shuffled)) "Shuffled deck should contain the same cards as the original deck")
      (is (not= deck shuffled) "Shuffled deck order should differ from the original deck"))))

(deftest test-deal-hands
  (testing "Dealing hands"
    (let [deck (karbosh-deck)
          hands (deal-hands deck)]
      (pp/pprint hands)
      (is (= 6 (count hands)) "Should deal 6 hands")
      (is (= 8 (count (first hands))) "Each hand should have 8 cards")
      (is (= (set deck) (set (apply concat hands))) "All cards should be distributed without duplicates")
      (is (empty? (drop (* 6 8) deck)) "No cards should remain undealt"))))

(deftest test-set-trump
  (testing "Setting trump suit"
    (let [game-state {:game {}}
          trump :♠
          updated-game (set-trump game-state trump)]
      (pp/pprint updated-game)
      (is (= trump (get-in updated-game [:game :trump])) "Trump suit should be set correctly"))))

(deftest test-set-bid
  (testing "Setting the bid"
    (let [game-state {:game {}}
          bid-value 7
          bidder :player3
          updated-game (set-bid game-state bidder bid-value)]
      (pp/pprint updated-game)
      (is (= {:player bidder, :value bid-value}
             (get-in updated-game [:game :bid]))
          "Bid should be recorded correctly"))))

(deftest test-get-player-order
  (testing "Getting player order"
    (let [players [:player1 :player2 :player3 :player4 :player5 :player6]]
      (is (= [:player4 :player5 :player6 :player1 :player2 :player3]
             (get-player-order players :player4))
          "Player order should rotate correctly starting from the given player"))))

(deftest test-start-trick
  (testing "Starting a trick"
    (let [game-state {:game {:players {:player1 {:hand [[9 :♥] [10 :♠]]}
                                       :player2 {:hand [[:J :♠] [:Q :♦]]}
                                       :player3 {:hand [[:K :♣] [:A :♠]]}}
                             :tricks []
                             :current-trick []}}
          updated-state (start-trick game-state)]
      (is (= [] (get-in updated-state [:game :current-trick]))
          "The current trick should initialize as an empty list")
      (is (= [] (get-in updated-state [:game :tricks])) "Completed tricks should remain unchanged when starting a new trick"))))

(deftest test-play-cards
  (testing "Playing a card updates the game state"
    (let [game-state {:game {:players {:player1 {:hand [[9 :♥] [10 :♠]]}
                                       :player2 {:hand [[:J :♠] [:Q :♦]]}
                                       :player3 {:hand [[:K :♣] [:A :♠]]}}
                             :tricks []
                             :current-trick []
                             :turn-order [:player1 :player2 :player3]
                             :current-turn :player1}}
          updated-state (play-card game-state :player1 [9 :♥])]
      (is (= [[10 :♠]] (get-in updated-state [:game :players :player1 :hand]))
          "Player1's hand should update after playing a card")
      (is (= {:player :player1, :card [9 :♥]} (last (get-in updated-state [:game :current-trick])))
          "The played card should be added to the current trick")
      (is (= :player2 (get-in updated-state [:game :current-turn]))
          "Turn order should move to the next player")
      (is (not (some #{[9 :♥]} (get-in updated-state [:game :players :player1 :hand])))
          "Played card should be removed from the player's hand")
      (is (= [[:J :♠] [:Q :♦]] (get-in updated-state [:game :players :player2 :hand]))
          "Other players' hands should remain unchanged"))))

(deftest test-score-trick
  (testing "Scoring a trick updates the correct team's score"
    (let [game-state {:game {:players {:player1 {:hand [[9 :♥] [10 :♠]]
                                                 :team 1
                                                 :score 0}
                                       :player2 {:hand [[:J :♠] [:Q :♦]]
                                                 :team 2
                                                 :score 0}
                                       :player3 {:hand [[:K :♣] [:A :♠]]
                                                 :team 1
                                                 :score 0}}
                             :scores {1 0, 2 0}
                             :trump :♠}}
          trick [{:player :player1 :card [9 :♥]}
                 {:player :player2 :card [10 :♠]}
                 {:player :player3 :card [:A :♠]}]
          updated-state (score-trick game-state trick)]
      (println "Scored trick state: ")
      (pp/pprint updated-state)
      (is (= 1 (:team (get-in updated-state [:game :players :player3])))
          "Team 1 should win the trick")
      (is (= 1 (get-in updated-state [:game :scores 1]))
          "Team 1's score should be incremented")
      (is (= 0 (get-in updated-state [:game :scores 2]))
          "Team 2's score should remain the same"))))

(deftest test-single-turn
  (testing "Playing a single turn updates state correctly"
    (let [game-state (init-game-state)
          player :player1
          card (first (get-in game-state [:game :players player :hand]))
          updated-state (play-card game-state player card)]
      (println "Initial Game State: ")
      (zp/zprint game-state)
      (println "Updated Game State: ")
      (zp/zprint updated-state)
      ;; Check card removal
      (is (not (some #{card} (get-in updated-state [:game :players player :hand])))
          "Card should be removed from player's hand")
      ;; Check turn advancement
      (let [expected-next-turn (->> (get-in game-state [:game :turn-order])
                                    (cycle)
                                    (drop-while #(not= % player))
                                    (second))]
        (is (= expected-next-turn (get-in updated-state [:game :current-turn]))
            "Turn should move to the next player")))))

(deftest test-game-over
  (testing "Game ends when a team reaches 52 points"
    (let [game-state {:game {:players {:player1 {:team 1 :score 30}
                                       :player2 {:team 2 :score 2}
                                       :player3 {:team 1 :score 25}
                                       :player4 {:team 2 :score 10}
                                       :player5 {:team 1 :score 5}
                                       :player6 {:team 2 :score 5}}}}
          result (game-over? game-state)]
      (pp/pprint result)
      (is (= true (:over result)) "Game should be over")
      (is (= 1 (:winner result)) "Team 2 should win"))))

(deftest test-resolve-trick
  (testing "Resolving a trick"
    (let [trump :♠
          trick [{:player :player1 :card [9 :♥]}
                 {:player :player2 :card [10 :♠]}
                 {:player :player3 :card [:J :♦]}]
          winner (resolve-trick trick trump)]
      (pp/pprint winner)
      (is (= winner :player2) "Player2 should win with the trump card"))))

(deftest test-generate-trick
  (testing "Generate a valid trick"
    (let [game-state (assoc-in (init-game-state) [:game :current-turn] :player1)
          updated-state (generate-trick game-state)
          trick (get-in updated-state [:game :current-trick])
          all-hands (mapcat :hand (vals (get-in game-state [:game :players])))]
      (pp/pprint trick)
      (is (= (count trick) (count (get-in game-state [:game :turn-order])))
          "Trick should include one card from each player")
      (is (every? #(some #{(:card %)} all-hands) trick)
          "Each card in the trick should come from a player's hand")
      (is (= (set (map :player trick)) (set (get-in game-state [:game :turn-order])))
          "Each player should play one card in the trick"))))

(deftest test-full-game
  (testing "Simulating a full game"
    (let [init-state      (init-game-state)
          current-hand    (get-in init-state [:game :current-hand])
          trump           (:trump current-hand)
          players         (:players current-hand)
          player-hands    (map :hand (vals players))
          first-hand      (:hand (first (vals players)))
          tricks          (:tricks current-hand)]

      ;; Print Initial State
      (println "Init-state:" (pr-str init-state))
      (println "Trump suit:" (pr-str trump))
      (println "Players in init-state:" (keys players))
      (println "Player Hands:" (pr-str player-hands))

      ;; Initial Setup Assertions
      (is (= 6 (count players)) "There should be 6 players in the game.")
      (is (= 8 (count first-hand)) "Each player should start with 8 cards.")
      (is (#{:♥ :♠ :♦ :♣} trump) "Trump suit should be set.")

      ;; Simulate Bidding
      (println "Simulating bidding...")
      (let [bidder         :player1
            bid-value      10
            state-after-bid (set-bid init-state bidder bid-value)]
        (println "Bid recorded for:" bidder "with value:" bid-value)
        (println "State after bid:" (pr-str state-after-bid))
        (is (= {:player bidder :value bid-value}
               (get-in state-after-bid [:game :bid]))
            "Bid should be recorded correctly."))

      ;; Play All Tricks
      (println "Simulating all tricks...")
      (let [state-after-tricks
            (loop [state init-state, trick-count 0]
              (if (>= trick-count 8)
                (do
                  (println "Test failed: Exceeded maximum of 8 tricks")
                  (is false "Test failed: Exceeded maximum of 8 tricks")
                  state)
                (let [remaining-players
                      (filter #(not-empty (:hand %))
                              (vals (get-in state [:game :current-hand :players])))]
                  (if (empty? remaining-players)
                    state
                    (let [updated-state (generate-trick state)]
                      (println "Current Trick State:" (pr-str (get-in updated-state [:game :current-hand :current-trick])))
                      (recur updated-state (inc trick-count)))))))]

        ;; Validate post-trick state
        (println "Completed Tricks:" (pr-str (get-in state-after-tricks [:game :current-hand :tricks])))
        (is (= 6 (count (get-in state-after-tricks [:game :current-hand :tricks])))
            "There should be one trick per player.")
        (is (every? #(empty? (:hand %))
                    (vals (get-in state-after-tricks [:game :current-hand :players])))
            "All players should have no cards left after playing tricks."))

      ;; Calculate Scores
      (println "Calculating scores...")
      (let [state-after-scores
            (reduce score-trick init-state tricks)]
        (println "Updated Scores:" (pr-str (get-in state-after-scores [:game :scores])))
        (is (= 0 (reduce + (vals (get-in state-after-scores [:game :scores]))))
            "Scores should be updated correctly after all tricks."))

      ;; Check Game Over
      (println "Checking game over state...")
      (let [final-state (game-over? init-state)]
        (println "Game Over State:" (pr-str final-state))
        (if (:over final-state)
          (is (some? (:winner final-state)) "A team should win when the game is over.")
          (is (nil? (:winner final-state)) "No winner if the game is not over."))))))
