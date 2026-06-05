(ns clojure-card-games.karbosh.bot-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.bot :as bot]))

(def team-game
  {:players {:player1 {:team 1 :hand []}
             :player2 {:team 2 :hand []}
             :player3 {:team 1 :hand []}
             :player4 {:team 2 :hand []}
             :player5 {:team 1 :hand []}
             :player6 {:team 2 :hand []}}
   :hand-index 0
   :bids []})

(def bid-5-hand
  [[:Q :♦] [9 :♥] [:J :♥] [:J :♥] [:Q :♥] [9 :♣] [10 :♦] [:Q :♣]])

(def bid-6-hand
  [[:A :♥] [:J :♣] [:J :♣] [10 :♠] [9 :♥] [:J :♠] [:A :♠] [:J :♠]])

(def bid-4-hand
  [[:K :♦] [10 :♥] [:Q :♥] [10 :♥] [:A :♥] [:A :♠] [:K :♠] [:J :♦]])

(def strong-non-karbosh-hand
  [[9 :♣] [:A :♠] [9 :♣] [:K :♠] [:K :♥] [:J :♠] [:Q :♠] [10 :♥]])

(def karbosh-hand
  [[:A :♦] [:K :♠] [:J :♣] [:A :♠] [:J :♠] [:K :♠] [10 :♠] [:Q :♠]])

(defn with-hand [game player hand]
  (assoc-in game [:players player :hand] hand))

(deftest bidding-policy-test
  (testing "bots make conservative numeric bids from tuned thresholds"
    (is (= {:type :bid :bid-type :bid :value 4}
           (bot/target-bid bid-4-hand)))
    (is (= {:type :bid :bid-type :bid :value 5}
           (bot/target-bid bid-5-hand)))
    (is (= {:type :bid :bid-type :bid :value 6}
           (bot/target-bid bid-6-hand))))

  (testing "bots do not turn a merely strong hand into karbosh"
    (is (not= :karbosh (:bid-type (bot/target-bid strong-non-karbosh-hand)))))

  (testing "bots use karbosh for credible sweep hands instead of numeric 7 or 8"
    (is (= {:type :bid :bid-type :karbosh}
           (bot/target-bid karbosh-hand))))

  (testing "a bot may overcall a partner's weak bid with a materially stronger hand"
    (let [game (-> team-game
                   (with-hand :player3 bid-5-hand)
                   (assoc :bids [{:type :bid
                                  :player :player1
                                  :bid-type :bid
                                  :value 4
                                  :hand-index 0}]))]
      (is (= {:type :bid :bid-type :bid :value 5}
             (bot/bid-action game :player3)))))

  (testing "a bot does not overcall a partner's six with another numeric bid"
    (let [game (-> team-game
                   (with-hand :player3 bid-6-hand)
                   (assoc :bids [{:type :bid
                                  :player :player1
                                  :bid-type :bid
                                  :value 6
                                  :hand-index 0}]))]
      (is (= {:type :bid :bid-type :pass}
             (bot/bid-action game :player3)))))

  (testing "a bot can overcall a partner's six only by going karbosh"
    (let [game (-> team-game
                   (with-hand :player3 karbosh-hand)
                   (assoc :bids [{:type :bid
                                  :player :player1
                                  :bid-type :bid
                                  :value 6
                                  :hand-index 0}]))]
      (is (= {:type :bid :bid-type :karbosh}
             (bot/bid-action game :player3))))))

(deftest card-policy-test
  (testing "bots count own cards and public played cards as seen"
    (let [game {:players {:player1 {:team 1
                                    :hand [[:A :♥]]}}
                :completed-tricks [[{:player :player2 :card [:A :♥]}]]
                :current-trick [{:player :player3 :card [:K :♥]}]}]
      (is (= 0 (get (bot/unseen-card-counts game :player1) [:A :♥] 0)))
      (is (= 1 (get (bot/unseen-card-counts game :player1) [:K :♥] 0)))))

  (testing "bots know when a trump card is good"
    (let [game {:players {:player1 {:team 1
                                    :hand [[:A :♠]]}}
                :trump :♠
                :completed-tricks [[{:player :player2 :card [:J :♠]}
                                    {:player :player3 :card [:J :♠]}]
                                   [{:player :player4 :card [:J :♣]}
                                    {:player :player5 :card [:J :♣]}]]
                :current-trick []}]
      (is (true? (bot/good-card? game :player1 [:A :♠])))
      (is (false? (bot/good-card?
                    (update game :completed-tricks pop)
                    :player1
                    [:A :♠])))))

  (testing "when leading, a bot uses the lowest guaranteed winner"
    (let [game {:players {:player1 {:team 1
                                    :hand [[:A :♠] [:K :♠]]}}
                :trump :♠
                :completed-tricks [[{:player :player2 :card [:J :♠]}
                                    {:player :player3 :card [:J :♠]}]
                                   [{:player :player4 :card [:J :♣]}
                                    {:player :player5 :card [:J :♣]}]
                                   [{:player :player6 :card [:A :♠]}]]
                :current-trick []}]
      (is (= {:type :play-card :card [:K :♠]}
             (bot/card-action game :player1)))))

  (testing "when a bot cannot win, it dumps the smallest legal card"
    (let [game {:players {:player3 {:team 1}
                          :player4 {:team 2
                                    :hand [[:A :♣] [10 :♣] [9 :♦]]}}
                :trump :♠
                :current-trick [{:player :player3 :card [10 :♥]}]}]
      (is (= {:type :play-card :card [9 :♦]}
             (bot/card-action game :player4)))))

  (testing "when the cheap winner is vulnerable, a bot secures the trick"
    (let [game {:players {:player1 {:team 1}
                          :player2 {:team 2
                                    :hand [[:A :♠] [:K :♠] [9 :♣]]}}
                :trump :♠
                :completed-tricks [[{:player :player3 :card [:J :♠]}
                                    {:player :player4 :card [:J :♠]}]
                                   [{:player :player5 :card [:J :♣]}
                                    {:player :player6 :card [:J :♣]}]]
                :current-trick [{:player :player1 :card [:Q :♠]}]}]
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player2)))))

  (testing "when the smallest winner is secure, a bot still uses it"
    (let [game {:players {:player1 {:team 1}
                          :player2 {:team 2
                                    :hand [[:A :♠] [:K :♠] [9 :♣]]}}
                :trump :♠
                :completed-tricks [[{:player :player3 :card [:J :♠]}
                                    {:player :player4 :card [:J :♠]}]
                                   [{:player :player5 :card [:J :♣]}
                                    {:player :player6 :card [:J :♣]}]
                                   [{:player :player3 :card [:A :♠]}]]
                :current-trick [{:player :player1 :card [:Q :♠]}]}]
      (is (= {:type :play-card :card [:K :♠]}
             (bot/card-action game :player2)))))

  (testing "when a partner is winning, a bot dumps low instead of overtaking"
    (let [game {:players {:player1 {:team 1}
                          :player3 {:team 1
                                    :hand [[:A :♥] [9 :♥] [9 :♣]]}}
                :trump :♠
                :current-trick [{:player :player1 :card [:Q :♥]}]}]
      (is (= {:type :play-card :card [9 :♥]}
             (bot/card-action game :player3))))))

(deftest karbosh-setup-actions-test
  (testing "bot donates its strongest card after trump is selected"
    (let [game {:phase :karbosh-donation
                :trump :♠
                :players {:player3 {:team 1
                                    :hand [[:A :♠] [9 :♣] [:K :♥]]}}}]
      (is (= {:type :donate-card :card [:A :♠]}
             (bot/action game :player3)))))

  (testing "bot discards its weakest card before lone play"
    (let [game {:phase :karbosh-discard
                :trump :♠
                :players {:player1 {:team 1
                                    :hand [[:A :♠] [9 :♣] [:K :♥]]}}}]
      (is (= {:type :discard-card :card [9 :♣]}
             (bot/action game :player1))))))
