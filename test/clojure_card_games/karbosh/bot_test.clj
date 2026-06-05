(ns clojure-card-games.karbosh.bot-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.shared.game :as game]))

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

(def right-only-karbosh-shape
  [[:J :♠] [:A :♠] [:K :♠] [:K :♠] [:Q :♠] [:Q :♠] [10 :♠] [:A :♥]])

(def hidden-hand-size (vec (repeat 8 [9 :♣])))

(defn with-hand [game player hand]
  (assoc-in game [:players player :hand] hand))

(defn with-hidden-hand-sizes [state]
  (reduce (fn [state player]
            (update-in state
                       [:players player]
                       #(merge {:team (get (game/teams) player)
                                :hand hidden-hand-size}
                               %)))
          state
          game/players))

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

  (testing "bots need multiple bowers before calling karbosh"
    (is (not= :karbosh (:bid-type (bot/target-bid right-only-karbosh-shape)))))

  (testing "karbosh evaluation prices the sweep explicitly"
    (let [game (-> team-game
                   (assoc :scores {1 0 2 0})
                   (with-hand :player1 karbosh-hand))
          evaluation (bot/karbosh-evaluation bot/default-bid-config
                                             game
                                             :player1
                                             :♠)]
      (is (true? (:call? evaluation)))
      (is (<= (:target-prob evaluation) (:make-prob evaluation)))
      (is (pos? (:diff-ev evaluation)))))

  (testing "score context changes the karbosh make target"
    (let [game (-> team-game
                   (assoc :scores {1 45 2 20})
                   (with-hand :player1 karbosh-hand))]
      (is (= (:karbosh-protect-target-prob bot/default-bid-config)
             (bot/karbosh-target-prob bot/default-bid-config
                                      game
                                      :player1)))))

  (testing "bid strategies are pluggable for old threshold comparison"
    (let [game (-> team-game
                   (assoc :scores {1 0 2 0})
                   (with-hand :player1 karbosh-hand))
          strict-config (assoc bot/default-bid-config
                          :karbosh-target-prob 0.99)]
      (binding [bot/*bid-config* strict-config]
        (is (= :karbosh
               (:bid-type (bot/bid-action game
                                           :player1
                                           :karbosh-threshold))))
        (is (not= :karbosh
                  (:bid-type (bot/bid-action game
                                             :player1
                                             :karbosh-probability)))))))

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
    (let [game (with-hidden-hand-sizes
                 {:players {:player1 {:team 1}
                            :player2 {:team 2
                                      :hand [[:A :♠] [:K :♠] [9 :♣]]}}
                  :trump :♠
                  :completed-tricks [[{:player :player3 :card [:J :♠]}
                                      {:player :player4 :card [:J :♠]}]
                                     [{:player :player5 :card [:J :♣]}
                                      {:player :player6 :card [:J :♣]}]]
                  :current-trick [{:player :player1 :card [:Q :♠]}]})]
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player2)))))

  (testing "when the smallest winner is secure, a bot still uses it"
    (let [game (with-hidden-hand-sizes
                 {:players {:player1 {:team 1}
                            :player2 {:team 2
                                      :hand [[:A :♠] [:K :♠] [9 :♣]]}}
                  :trump :♠
                  :completed-tricks [[{:player :player3 :card [:J :♠]}
                                      {:player :player4 :card [:J :♠]}]
                                     [{:player :player5 :card [:J :♣]}
                                      {:player :player6 :card [:J :♣]}]
                                     [{:player :player3 :card [:A :♠]}]]
                  :current-trick [{:player :player1 :card [:Q :♠]}]})]
      (is (= {:type :play-card :card [:K :♠]}
             (bot/card-action game :player2)))))

  (testing "play strategies are pluggable for same-position comparisons"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♠
                  :active-players game/players
                  :players {:player1 {:team 1
                                      :hand [[:A :♥] [9 :♠] [9 :♦] [10 :♦]
                                             [:Q :♦] [9 :♣] [10 :♣] [:Q :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player1 :card-counting)))
      (is (= {:type :play-card :card [:A :♥]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [:A :♥]}
             (bot/card-action game :player1 :hybrid)))))

  (testing "karbosh callers lead trump before cashing off-suit aces"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♠
                  :active-players game/players
                  :hand-index 0
                  :bids [{:type :bid
                          :player :player1
                          :bid-type :karbosh
                          :hand-index 0}]
                  :players {:player1 {:team 1
                                      :hand [[:A :♥] [:A :♠] [9 :♦] [10 :♦]
                                             [:Q :♦] [9 :♣] [10 :♣] [:Q :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player1 :hybrid)))))

  (testing "numeric trump callers pull with the right bower before off-suit aces"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♠
                  :active-players game/players
                  :hand-index 0
                  :bids [{:type :bid
                          :player :player1
                          :bid-type :bid
                          :value 5
                          :hand-index 0}]
                  :players {:player1 {:team 1
                                      :hand [[:J :♠] [:A :♥] [:A :♦]
                                             [9 :♠] [9 :♦] [10 :♦]
                                             [9 :♣] [10 :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [:J :♠]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [:J :♠]}
             (bot/card-action game :player1 :hybrid)))))

  (testing "unsafe trump-only leads bleed low trump instead of the ace"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♠
                  :active-players game/players
                  :hand-index 0
                  :bids [{:type :bid
                          :player :player1
                          :bid-type :bid
                          :value 5
                          :hand-index 0}]
                  :players {:player1 {:team 1
                                      :hand [[:A :♠] [9 :♠]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player1 :hybrid)))))

  (testing "when a partner is winning, a bot dumps low instead of overtaking"
    (let [game {:players {:player1 {:team 1}
                          :player3 {:team 1
                                    :hand [[:A :♥] [9 :♥] [9 :♣]]}}
                :trump :♠
                :current-trick [{:player :player1 :card [:Q :♥]}]}]
      (is (= {:type :play-card :card [9 :♥]}
             (bot/card-action game :player3)))))

  (testing "when a partner is winning with trump, a bot avoids overtrumping when possible"
    (let [game {:players {:player1 {:team 1}
                          :player3 {:team 1
                                    :hand [[:A :♠] [9 :♠]]}}
                :trump :♠
                :current-trick [{:player :player1 :card [:K :♠]}]}]
      (is (= {:type :play-card :card [9 :♠]}
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
