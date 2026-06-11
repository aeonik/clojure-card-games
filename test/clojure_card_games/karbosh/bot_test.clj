(ns clojure-card-games.karbosh.bot-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure-card-games.karbosh.bot :as bot]
            [clojure-card-games.karbosh.bot.play :as play]
            [clojure-card-games.karbosh.fixtures :as fixtures]
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

(def straight-double-karbosh-hand
  [[:J :♠] [:J :♠] [:J :♣] [:J :♣] [:A :♠] [:A :♠] [:K :♠] [:K :♠]])

(def forced-double-karbosh-hand
  [[:J :♠] [:J :♠] [:J :♣] [:J :♣] [:A :♠] [:A :♠] [9 :♠] [9 :♠]])

(def right-only-karbosh-shape
  [[:J :♠] [:A :♠] [:K :♠] [:K :♠] [:Q :♠] [:Q :♠] [10 :♠] [:A :♥]])

(def thin-right-bid-4-hand
  [[9 :♠] [:K :♣] [:Q :♦] [10 :♥] [9 :♠] [10 :♠] [:J :♥] [:K :♥]])

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

  (testing "bots do not turn ordinary karbosh hands into double karbosh"
    (let [game (-> team-game
                   (assoc :scores {1 0 2 0})
                   (with-hand :player1 karbosh-hand))]
      (is (= {:type :bid :bid-type :karbosh}
             (bot/bid-action game :player1)))))

  (testing "bots may call a straight double karbosh hand"
    (let [game (-> team-game
                   (assoc :scores {1 0 2 0})
                   (with-hand :player1 straight-double-karbosh-hand))]
      (is (true? (:straight?
                  (bot/double-karbosh-evaluation bot/default-bid-config
                                                 game
                                                 :player1
                                                 :♠))))
      (is (= {:type :bid :bid-type :double-karbosh}
             (bot/bid-action game :player1)))))

  (testing "bots only use forced non-straight double as a desperate enemy-karbosh overcall"
    (let [base (-> team-game
                   (with-hand :player1 forced-double-karbosh-hand)
                   (assoc :bids [{:type :bid
                                  :player :player2
                                  :bid-type :karbosh
                                  :hand-index 0}]))
          neutral (assoc base :scores {1 25 2 25})
          desperate (assoc base :scores {1 25 2 45})]
      (is (false? (:straight?
                   (bot/double-karbosh-evaluation bot/default-bid-config
                                                  desperate
                                                  :player1
                                                  :♠))))
      (is (true? (get-in (bot/double-karbosh-evaluation bot/default-bid-config
                                                        desperate
                                                        :player1
                                                        :♠)
                         [:features :forced?])))
      (is (= {:type :bid :bid-type :pass}
             (bot/bid-action neutral :player1)))
      (is (= {:type :bid :bid-type :double-karbosh}
             (bot/bid-action desperate :player1)))))

  (testing "bots do not overcall partner karbosh with non-straight double shapes"
    (let [game (-> team-game
                   (assoc :scores {1 25 2 45}
                          :bids [{:type :bid
                                  :player :player3
                                  :bid-type :karbosh
                                  :hand-index 0}])
                   (with-hand :player1 forced-double-karbosh-hand))]
      (is (= {:type :bid :bid-type :pass}
             (bot/bid-action game :player1)))))

  (testing "bots need multiple bowers before calling karbosh"
    (is (not= :karbosh (:bid-type (bot/target-bid right-only-karbosh-shape)))))

  (testing "conservative numeric bidding is pluggable without changing default behavior"
    (let [game (-> team-game
                   (assoc :scores {1 0 2 0})
                   (with-hand :player1 thin-right-bid-4-hand))]
      (is (= {:type :bid :bid-type :bid :value 4}
             (bot/bid-action game :player1 :karbosh-probability)))
      (is (= {:type :bid :bid-type :pass}
             (bot/bid-action game
                             :player1
                             :karbosh-probability-conservative)))
      (is (= {:type :bid :bid-type :bid :value 4}
             (bot/bid-action (with-hand game :player1 bid-4-hand)
                             :player1
                             :karbosh-probability-conservative)))))

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

  (testing "probability bidding accounts for karbosh donations"
    (let [fixture fixtures/bl32c2
          game (-> team-game
                   (assoc :scores {1 0 2 0})
                   (with-hand (:caller fixture)
                              (fixtures/hand fixture (:caller fixture))))
          evaluation (bot/karbosh-evaluation bot/default-bid-config
                                             game
                                             (:caller fixture)
                                             (:trump fixture))]
      (is (= {:type :bid :bid-type :bid :value 6}
             (bot/bid-action game :player1 :karbosh-threshold)))
      (is (= {:type :bid :bid-type :karbosh}
             (bot/bid-action game :player1 :karbosh-probability)))
      (is (= (:expected-discards fixture)
             (get-in evaluation [:donation :discards])))
      (is (= (:expected-wanted-donation-count fixture)
             (get-in evaluation [:donation :wanted-count])))
      (is (< 0.75 (get-in evaluation [:donation :prob-all-donors-helpful])))
      (is (<= (:target-prob evaluation) (:make-prob evaluation)))))

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
             (bot/bid-action game :player3 :karbosh-threshold)))
      (is (= {:type :bid :bid-type :karbosh}
             (bot/bid-action game :player3 :karbosh-probability)))))

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

(deftest control-burn-penalty-test
  (let [config (assoc bot/default-play-config
                      :partner-control-burn-penalty 100)
        game {:trump :♠}
        analyses {[:Q :♠] {:expected-pending-partner-control-burn 1/2}
                  [:Q :♥] {:expected-pending-partner-control-burn 1/2
                           :prob-pending-opponent-void-and-higher-trump
                           {:player2 1/2}}
                  [:K :♥] {:expected-pending-partner-control-burn 1/2}
                  [:A :♥] {:expected-pending-partner-control-burn 0
                           :prob-pending-opponent-void-and-higher-trump
                           {:player2 1/2}}}]
    (testing "trump control burn is penalized directly"
      (is (= 50.0
             (play/partner-control-burn-penalty config
                                                game
                                                analyses
                                                [:Q :♠]))))
    (testing "off-suit control burn is penalized only when exposed to ruff"
      (is (= 25.0
             (play/partner-control-burn-penalty config
                                                game
                                                analyses
                                                [:Q :♥])))
      (is (zero? (play/partner-control-burn-penalty config
                                                    game
                                                    analyses
                                                    [:K :♥]))))
    (testing "clean off-suit aces do not create burn penalty"
      (is (zero? (play/partner-control-burn-penalty config
                                                    game
                                                    analyses
                                                    [:A :♥]))))))

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

  (testing "when a bot cannot win with trump left, it ditches low singletons to short-suit"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♣
                  :active-players game/players
                  :players {:player4 {:team 2
                                      :hand [[9 :♦] [10 :♦] [9 :♠] [9 :♣]]}}
                  :current-trick [{:player :player1 :card [:A :♥]}
                                  {:player :player3 :card [:J :♣]}]})]
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player4 :hybrid)))
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player4 :card-counting)))))

  (testing "strategic ditching preserves singleton off-suit controls"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♣
                  :active-players game/players
                  :players {:player4 {:team 2
                                      :hand [[:A :♦] [:A :♠] [:A :♠]]}}
                  :current-trick [{:player :player1 :card [:A :♥]}
                                  {:player :player3 :card [:J :♣]}]})]
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player4 :hybrid)))
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player4 :card-counting)))))

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
             (binding [bot/*play-config* bot/classic-play-config]
               (bot/card-action game :player1 :hybrid-threshold))))
      (is (= {:type :play-card :card [:A :♥]}
             (bot/card-action game :player1 :hybrid)))
      (is (fn? (bot/resolve-play-strategy
                :hybrid-action-inference-team-ev)))))

  (testing "risk-adjusted fallback leads an ace over a doomed low card"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♥
                  :active-players game/players
                  :players {:player1 {:team 1
                                      :hand [[:A :♦] [10 :♦]
                                             [:Q :♣] [10 :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [10 :♦]}
             (bot/card-action game :player1 :probability-threshold)))
      (is (= {:type :play-card :card [:A :♦]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [:A :♦]}
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

  (testing "team EV discounts duplicate top trump and pulls before exposed off aces"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♥
                  :active-players game/players
                  :hand-index 0
                  :bids [{:type :bid
                          :player :player6
                          :bid-type :bid
                          :value 5
                          :hand-index 0}]
                  :players {:player6 {:team 2
                                      :hand [[:Q :♥] [9 :♣] [:K :♦]
                                             [:K :♣] [:J :♥] [:K :♣]
                                             [:A :♦] [:J :♥]]}}
                  :current-trick []})
          cards (bot/legal-cards game :player6)
          analyses (bot/card-analyses game :player6 cards)
          unseen-counts (bot/unseen-card-counts game :player6)
          right-value (:value (bot/team-ev-lead-breakdown
                               bot/default-play-config
                               game
                               :player6
                               analyses
                               unseen-counts
                               [:J :♥]))
          ace-value (:value (bot/team-ev-lead-breakdown
                             bot/default-play-config
                             game
                             :player6
                             analyses
                             unseen-counts
                             [:A :♦]))]
      (is (< ace-value right-value))
      (is (= {:type :play-card :card [:J :♥]}
             (bot/card-action game :player6 :probability-team-ev)))
      (is (= {:type :play-card :card [:J :♥]}
             (bot/card-action game
                              :player6
                              :hybrid-action-inference-team-ev)))))

  (testing "team EV pulls top trump before exposed aces while opponents may hold trump"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♠
                  :active-players game/players
                  :hand-index 11
                  :bids [{:type :bid
                          :player :player2
                          :bid-type :bid
                          :value 4
                          :hand-index 11}]
                  :players {:player2 {:team 2
                                      :hand [[:K :♥] [10 :♣] [:A :♦]
                                             [9 :♣] [:Q :♠] [10 :♠]
                                             [:J :♠] [10 :♠]]}}
                  :current-trick []})
          cards (bot/legal-cards game :player2)
          analyses (bot/card-analyses game :player2 cards)
          unseen-counts (bot/unseen-card-counts game :player2)
          right-value (:value (bot/team-ev-lead-breakdown
                               bot/default-play-config
                               game
                               :player2
                               analyses
                               unseen-counts
                               [:J :♠]))
          ace-value (:value (bot/team-ev-lead-breakdown
                             bot/default-play-config
                             game
                             :player2
                             analyses
                             unseen-counts
                             [:A :♦]))]
      (is (< ace-value right-value))
      (is (= {:type :play-card :card [:J :♠]}
             (bot/card-action game
                              :player2
                              :hybrid-action-inference-team-ev)))))

  (testing "team EV can cash an ace when opponents are exhausted of trump"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♠
                  :active-players game/players
                  :hand-index 11
                  :bids [{:type :bid
                          :player :player2
                          :bid-type :bid
                          :value 4
                          :hand-index 11}]
                  :players {:player2 {:team 2
                                      :hand [[:J :♠] [:A :♦] [10 :♣]
                                             [9 :♣]]}}
                  :completed-tricks [[{:player :player4 :card [9 :♠]}
                                      {:player :player5 :card [9 :♦]}
                                      {:player :player6 :card [:A :♠]}
                                      {:player :player1 :card [:K :♦]}
                                      {:player :player2 :card [:Q :♠]}
                                      {:player :player3 :card [:A :♥]}]]
                  :current-trick []})]
      (is (= {:type :play-card :card [:A :♦]}
             (bot/card-action game
                              :player2
                              :hybrid-action-inference-team-ev)))))

  (testing "team EV avoids winning now by burning a partner trump control"
    (let [game {:phase :trick-playing
                :trump :♣
                :active-players game/players
                :hand-index 4
                :bids [{:type :bid
                        :player :player1
                        :bid-type :bid
                        :value 5
                        :hand-index 4}]
                :players {:player1 {:team 1
                                    :hand [[:Q :♦] [:J :♦] [10 :♣]
                                           [9 :♥] [:K :♣]]}
                          :player2 {:team 2
                                    :hand [[10 :♦] [:A :♦] [:J :♥]
                                           [:A :♦] [:Q :♥]]}
                          :player3 {:team 1
                                    :hand [[:K :♥] [:K :♦] [:K :♦]
                                           [:J :♠] [10 :♦]]}
                          :player4 {:team 2
                                    :hand [[:Q :♥] [:J :♥] [:Q :♦]
                                           [:K :♥] [:A :♥]]}
                          :player5 {:team 1
                                    :hand [[:K :♠] [9 :♥] [:K :♠]
                                           [10 :♠] [10 :♥]]}
                          :player6 {:team 2
                                    :hand [[9 :♦] [:J :♦] [:A :♥]
                                           [10 :♥] [:Q :♠]]}}
                :completed-tricks [[{:player :player1 :card [:J :♣]}
                                    {:player :player2 :card [9 :♣]}
                                    {:player :player3 :card [9 :♣]}
                                    {:player :player4 :card [10 :♣]}
                                    {:player :player5 :card [:A :♣]}
                                    {:player :player6 :card [:A :♣]}]
                                   [{:player :player1 :card [:J :♣]}
                                    {:player :player2 :card [:Q :♣]}
                                    {:player :player3 :card [:K :♣]}
                                    {:player :player4 :card [:Q :♣]}
                                    {:player :player5 :card [9 :♦]}
                                    {:player :player6 :card [:J :♠]}]
                                   [{:player :player1 :card [:A :♠]}
                                    {:player :player2 :card [:A :♠]}
                                    {:player :player3 :card [:Q :♠]}
                                    {:player :player4 :card [9 :♠]}
                                    {:player :player5 :card [9 :♠]}
                                    {:player :player6 :card [10 :♠]}]]
                :tricks-this-hand {1 3 2 0}
                :current-trick []}
          cards (bot/legal-cards game :player1)
          analyses (bot/card-analyses game :player1 cards)
          unseen-counts (bot/unseen-card-counts game :player1)
          trump-breakdown (bot/team-ev-lead-breakdown
                           bot/default-play-config
                           game
                           :player1
                           analyses
                           unseen-counts
                           [10 :♣])
          diamond-breakdown (bot/team-ev-lead-breakdown
                             bot/default-play-config
                             game
                             :player1
                             analyses
                             unseen-counts
                             [:Q :♦])
          event (bot/card-action game
                                 :player1
                                 :hybrid-action-inference-team-ev)
          ruff-invite-event (bot/card-action game
                                             :player1
                                             :hybrid-ruff-invite)
          old-probability-event (bot/card-action game
                                                 :player1
                                                 :probability)]
      (is (= 1/4
             (get-in analyses
                     [[10 :♣] :expected-pending-partner-control-burn])))
      (is (= 750.0 (:control-burn-penalty trump-breakdown)))
      (is (< (:value trump-breakdown) (:value diamond-breakdown)))
      (is (= [:Q :♦] (:card event)))
      (is (= [:Q :♦] (:card ruff-invite-event)))
      (is (= [10 :♣] (:card old-probability-event)))))

  (testing "numeric callers without trump control pressure with off-suit aces"
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
                                      :hand [[:A :♥] [9 :♠] [9 :♦]
                                             [10 :♦] [9 :♣] [10 :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [:A :♥]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [:A :♥]}
             (bot/card-action game :player1 :hybrid)))))

  (testing "numeric callers without trump control can lead low trump to pull bowers"
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
                                      :hand [[9 :♠] [:A :♠] [9 :♦]
                                             [10 :♦] [9 :♣] [10 :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [9 :♠]}
             (bot/card-action game :player1 :hybrid)))))

  (testing "numeric callers without low trump use a non-trump exit"
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
                                      :hand [[:A :♠] [9 :♦] [10 :♦]
                                             [9 :♣] [10 :♣]]}}
                  :current-trick []})]
      (is (= {:type :play-card :card [9 :♦]}
             (bot/card-action game :player1 :probability-threshold)))
      (is (= {:type :play-card :card [10 :♣]}
             (bot/card-action game :player1 :probability)))
      (is (= {:type :play-card :card [10 :♣]}
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

  (testing "preservation policy spends lower trump before an unsafe left bower"
    (let [game {:phase :trick-playing
                :trump :♣
                :active-players game/players
                :hand-index 8
                :bids [{:type :bid
                        :player :player6
                        :bid-type :bid
                        :value 4
                        :hand-index 8}]
                :players {:player1 {:team 1
                                    :hand [[:Q :♥] [:A :♥] [:A :♦]
                                           [:J :♠] [:Q :♣] [:Q :♦]]}
                          :player2 {:team 2
                                    :hand [[:Q :♠] [9 :♦] [9 :♦]
                                           [9 :♥] [:J :♦] [:K :♥]]}
                          :player3 {:team 1
                                    :hand [[:Q :♠] [:K :♦] [:Q :♥]
                                           [10 :♥] [10 :♦] [:J :♠]]}
                          :player4 {:team 2
                                    :hand [[:K :♣] [:K :♠] [:A :♦]
                                           [:Q :♦] [9 :♥] [:J :♥]]}
                          :player5 {:team 1
                                    :hand [[:K :♠] [:A :♥] [:J :♦]
                                           [10 :♥] [:J :♣]]}
                          :player6 {:team 2
                                    :hand [[:K :♦] [:K :♥] [:K :♣]
                                           [:J :♥] [:Q :♣]]}}
                :completed-tricks [[{:player :player6 :card [:J :♣]}
                                    {:player :player1 :card [10 :♣]}
                                    {:player :player2 :card [10 :♣]}
                                    {:player :player3 :card [:A :♣]}
                                    {:player :player4 :card [9 :♣]}
                                    {:player :player5 :card [:A :♣]}]
                                   [{:player :player6 :card [:A :♠]}
                                    {:player :player1 :card [:A :♠]}
                                    {:player :player2 :card [9 :♠]}
                                    {:player :player3 :card [9 :♠]}
                                    {:player :player4 :card [10 :♠]}
                                    {:player :player5 :card [10 :♠]}]]
                :tricks-this-hand {1 0 2 2}
                :current-trick [{:player :player6 :card [9 :♣]}]}
          event (bot/card-action game :player1 :hybrid-ruff-invite)]
      (is (= {:type :play-card :card [:Q :♣]} event))
      (is (= :preserve-high-trump-winner
             (:reason (bot/explain-card-action
                       game
                       :player1
                       :hybrid-ruff-invite
                       event))))))

  (testing "preservation policy leads lower trump before an unsafe left bower"
    (let [game {:phase :trick-playing
                :trump :♣
                :active-players game/players
                :hand-index 11
                :bids [{:type :bid
                        :player :player1
                        :bid-type :bid
                        :value 5
                        :hand-index 11}]
                :players {:player1 {:team 1
                                    :hand [[:Q :♣] [:J :♠]]}
                          :player2 {:team 2
                                    :hand [[:Q :♠] [:A :♣]]}
                          :player3 {:team 1
                                    :hand [[:J :♠] [:A :♥]]}
                          :player4 {:team 2
                                    :hand [[:A :♠] [:K :♣]]}
                          :player5 {:team 1
                                    :hand [[:K :♦] [:Q :♠]]}
                          :player6 {:team 2
                                    :hand [[:A :♣] [:J :♣]]}}
                :completed-tricks [[{:player :player1 :card [:J :♣]}
                                    {:player :player2 :card [10 :♣]}
                                    {:player :player3 :card [9 :♣]}
                                    {:player :player4 :card [9 :♣]}
                                    {:player :player5 :card [:J :♥]}
                                    {:player :player6 :card [:K :♣]}]
                                   [{:player :player1 :card [:A :♦]}
                                    {:player :player2 :card [:J :♦]}
                                    {:player :player3 :card [9 :♦]}
                                    {:player :player4 :card [:J :♦]}
                                    {:player :player5 :card [10 :♦]}
                                    {:player :player6 :card [9 :♦]}]
                                   [{:player :player1 :card [:A :♥]}
                                    {:player :player2 :card [:K :♥]}
                                    {:player :player3 :card [10 :♥]}
                                    {:player :player4 :card [9 :♥]}
                                    {:player :player5 :card [9 :♠]}
                                    {:player :player6 :card [10 :♥]}]
                                   [{:player :player1 :card [9 :♥]}
                                    {:player :player2 :card [10 :♣]}
                                    {:player :player3 :card [:K :♥]}
                                    {:player :player4 :card [:J :♥]}
                                    {:player :player5 :card [10 :♠]}
                                    {:player :player6 :card [:Q :♥]}]
                                   [{:player :player2 :card [:A :♠]}
                                    {:player :player3 :card [:K :♠]}
                                    {:player :player4 :card [:K :♠]}
                                    {:player :player5 :card [10 :♠]}
                                    {:player :player6 :card [10 :♦]}
                                    {:player :player1 :card [9 :♠]}]
                                   [{:player :player2 :card [:A :♦]}
                                    {:player :player3 :card [:K :♦]}
                                    {:player :player4 :card [:Q :♦]}
                                    {:player :player5 :card [:Q :♦]}
                                    {:player :player6 :card [:Q :♥]}
                                    {:player :player1 :card [:Q :♣]}]]
                :tricks-this-hand {1 4 2 2}
                :current-trick []}
          event (bot/card-action game :player1 :hybrid-ruff-invite)]
      (is (= {:type :play-card :card [:Q :♣]} event))
      (is (= :lead-preserve-high-trump-winner
             (:reason (bot/explain-card-action
                       game
                       :player1
                       :hybrid-ruff-invite
                       event))))))

  (testing "defenders lead an off-suit ace before an unsafe left bower"
    (let [game {:phase :trick-playing
                :trump :♥
                :active-players game/players
                :hand-index 19
                :bids [{:type :bid
                        :player :player5
                        :bid-type :bid
                        :value 4
                        :hand-index 19}]
                :players {:player1 {:team 1
                                    :hand [[:J :♠] [:K :♦] [10 :♠]
                                           [10 :♦] [:Q :♠] [:K :♣]]}
                          :player2 {:team 2
                                    :hand [[:K :♠] [9 :♣] [:A :♣]
                                           [10 :♠] [10 :♣] [:A :♦]]}
                          :player3 {:team 1
                                    :hand [[:A :♠] [:J :♥] [:J :♣]
                                           [:J :♣] [:A :♠] [9 :♠]]}
                          :player4 {:team 2
                                    :hand [[:J :♦] [9 :♣] [:A :♣]
                                           [:J :♠] [9 :♠] [10 :♣]]}
                          :player5 {:team 1
                                    :hand [[:Q :♣] [:Q :♦] [:A :♥]
                                           [:Q :♣] [:Q :♠] [:Q :♥]]}
                          :player6 {:team 2
                                    :hand [[:A :♥] [:K :♣] [:K :♦]
                                           [:K :♠] [10 :♥] [:Q :♥]]}}
                :completed-tricks [[{:player :player5 :card [:J :♥]}
                                    {:player :player6 :card [9 :♥]}
                                    {:player :player1 :card [9 :♥]}
                                    {:player :player2 :card [:K :♥]}
                                    {:player :player3 :card [:J :♦]}
                                    {:player :player4 :card [10 :♥]}]
                                   [{:player :player5 :card [:A :♦]}
                                    {:player :player6 :card [:Q :♦]}
                                    {:player :player1 :card [9 :♦]}
                                    {:player :player2 :card [9 :♦]}
                                    {:player :player3 :card [10 :♦]}
                                    {:player :player4 :card [:K :♥]}]]
                :tricks-this-hand {1 1 2 1}
                :current-trick []}]
      (is (= {:type :play-card :card [:A :♣]}
             (bot/card-action game :player4 :probability)))
      (is (= {:type :play-card :card [:A :♣]}
             (bot/card-action game :player4 :hybrid)))))

  (testing "defenders preserve unsafe high trump with a low non-trump exit"
    (let [game {:phase :trick-playing
                :trump :♦
                :active-players game/players
                :hand-index 3
                :bids [{:type :bid
                        :player :player3
                        :bid-type :bid
                        :value 5
                        :hand-index 3}]
                :players {:player1 {:team 1
                                    :hand [[:K :♥] [:J :♠] [:Q :♣]
                                           [:K :♣] [:K :♠] [9 :♣]]}
                          :player2 {:team 2
                                    :hand [[:A :♠] [:A :♣] [10 :♠]
                                           [:A :♥] [10 :♣] [:A :♠]]}
                          :player3 {:team 1
                                    :hand [[10 :♥] [:K :♦] [:Q :♠]
                                           [:J :♥] [9 :♥] [:Q :♦]]}
                          :player4 {:team 2
                                    :hand [[:Q :♥] [10 :♠] [:J :♣]
                                           [:J :♠] [:A :♦] [:K :♥]]}
                          :player5 {:team 1
                                    :hand [[:Q :♥] [9 :♠] [:K :♠]
                                           [:K :♦] [:J :♣] [:Q :♣]]}
                          :player6 {:team 2
                                    :hand [[:A :♣] [10 :♥] [10 :♣]
                                           [:K :♣] [:A :♥] [:Q :♠]]}}
                :completed-tricks [[{:player :player3 :card [:J :♦]}
                                    {:player :player4 :card [:A :♦]}
                                    {:player :player5 :card [9 :♦]}
                                    {:player :player6 :card [10 :♦]}
                                    {:player :player1 :card [9 :♦]}
                                    {:player :player2 :card [:J :♥]}]
                                   [{:player :player3 :card [10 :♦]}
                                    {:player :player4 :card [:J :♦]}
                                    {:player :player5 :card [:Q :♦]}
                                    {:player :player6 :card [9 :♣]}
                                    {:player :player1 :card [9 :♥]}
                                    {:player :player2 :card [9 :♠]}]]
                :tricks-this-hand {1 1 2 1}
                :current-trick []}]
      (is (false? (bot/good-card? game :player4 [:A :♦])))
      (is (= {:type :play-card :card [:A :♦]}
             (bot/card-action game :player4 :hybrid)))
      (is (= {:type :play-card :card [10 :♠]}
             (bot/card-action game :player4 :probability-defender-exit)))
      (is (= {:type :play-card :card [10 :♠]}
             (bot/card-action game :player4 :hybrid-defender-exit)))
      (is (= {:type :play-card :card [10 :♠]}
             (bot/card-action game :player4 :probability-preservation)))
      (is (= {:type :play-card :card [10 :♠]}
             (bot/card-action game :player4 :hybrid-preservation)))))

  (testing "defenders exit low non-trump instead of leading vulnerable trump"
    (let [game {:phase :trick-playing
                :trump :♣
                :active-players game/players
                :hand-index 11
                :bids [{:type :bid
                        :player :player6
                        :bid-type :bid
                        :value 4
                        :hand-index 11}]
                :players {:player1 {:team 1
                                    :hand [[:Q :♥] [10 :♦] [10 :♣]
                                           [:Q :♦] [:K :♦] [:Q :♣]
                                           [9 :♠]]}
                          :player2 {:team 2
                                    :hand [[:Q :♦] [9 :♦] [:K :♠]
                                           [:K :♥] [10 :♠] [10 :♦]
                                           [:A :♥]]}
                          :player3 {:team 1
                                    :hand [[10 :♠] [:A :♣] [:K :♦]
                                           [:A :♦] [:A :♠] [:J :♦]
                                           [:K :♣]]}
                          :player4 {:team 2
                                    :hand [[:J :♣] [9 :♥] [:A :♦]
                                           [10 :♥] [:A :♥] [:Q :♥]
                                           [:Q :♠]]}
                          :player5 {:team 1
                                    :hand [[:K :♥] [:J :♦] [:A :♠]
                                           [9 :♥] [9 :♠] [:K :♠]
                                           [:J :♥]]}
                          :player6 {:team 2
                                    :hand [[:J :♥] [10 :♥] [:Q :♠]
                                           [:J :♠] [:A :♣] [:Q :♣]
                                           [:K :♣]]}}
                :completed-tricks [[{:player :player6 :card [9 :♣]}
                                    {:player :player1 :card [:J :♣]}
                                    {:player :player2 :card [10 :♣]}
                                    {:player :player3 :card [9 :♣]}
                                    {:player :player4 :card [:J :♠]}
                                    {:player :player5 :card [9 :♦]}]]
                :current-trick []}
          event (bot/card-action game :player1 :hybrid-ruff-invite)]
      (is (= {:type :play-card :card [9 :♠]} event))
      (is (= :defender-low-exit
             (:reason (bot/explain-card-action
                       game
                       :player1
                       :hybrid-ruff-invite
                       event))))))

  (testing "ruff invite strategy can lead a partner-void suit over a good trump"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♥
                  :active-players game/players
                  :hand-index 2
                  :bids [{:type :bid
                          :player :player1
                          :bid-type :bid
                          :value 5
                          :hand-index 2}]
                  :players {:player1 {:team 1
                                      :hand [[:J :♥] [:J :♥] [:Q :♣] [:K :♠]]}}
                  :completed-tricks [[{:player :player3 :card [:A :♥]}
                                      {:player :player4 :card [9 :♣]}
                                      {:player :player5 :card [:K :♥]}
                                      {:player :player6 :card [10 :♣]}
                                      {:player :player1 :card [9 :♥]}
                                      {:player :player2 :card [10 :♠]}]
                                     [{:player :player2 :card [:A :♣]}
                                      {:player :player3 :card [9 :♦]}
                                      {:player :player4 :card [:K :♣]}
                                      {:player :player5 :card [:Q :♦]}
                                      {:player :player6 :card [9 :♣]}
                                      {:player :player1 :card [10 :♣]}]]
                  :current-trick []})]
      (is (= {:type :play-card :card [:J :♥]}
             (bot/card-action game :player1 :hybrid-preservation)))
      (is (= {:type :play-card :card [:Q :♣]}
             (bot/card-action game :player1 :hybrid-ruff-invite)))
      (is (= {:type :play-card :card [:Q :♣]}
             (bot/card-action game :player1 :hybrid-team-ev)))))

  (testing "ruff invite waits until every opponent is known void in trump"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♥
                  :active-players game/players
                  :hand-index 2
                  :bids [{:type :bid
                          :player :player1
                          :bid-type :bid
                          :value 5
                          :hand-index 2}]
                  :players {:player1 {:team 1
                                      :hand [[:J :♥] [:J :♥] [:Q :♣] [:K :♠]]}}
                  :completed-tricks [[{:player :player3 :card [:A :♥]}
                                      {:player :player4 :card [9 :♣]}
                                      {:player :player5 :card [:K :♥]}
                                      {:player :player6 :card [10 :♣]}
                                      {:player :player1 :card [9 :♥]}
                                      {:player :player2 :card [10 :♥]}]
                                     [{:player :player2 :card [:A :♣]}
                                      {:player :player3 :card [9 :♦]}
                                      {:player :player4 :card [:K :♣]}
                                      {:player :player5 :card [:Q :♦]}
                                      {:player :player6 :card [9 :♣]}
                                      {:player :player1 :card [10 :♣]}]]
                  :current-trick []})]
      (is (= {:type :play-card :card [:J :♥]}
             (bot/card-action game :player1 :hybrid-ruff-invite)))))

  (testing "ruff invite can use high-discard inference before hard trump voids"
    (let [soft-void-game (fn [discards]
                           (with-hidden-hand-sizes
                             {:phase :trick-playing
                              :trump :♥
                              :active-players game/players
                              :hand-index 2
                              :bids [{:type :bid
                                      :player :player1
                                      :bid-type :bid
                                      :value 5
                                      :hand-index 2}]
                              :players {:player1 {:team 1
                                                  :hand [[:J :♥]
                                                         [:Q :♣]
                                                         [:K :♠]]}
                                        :player5 {:team 1
                                                  :hand []}}
                              :completed-tricks [[{:player :player1
                                                   :card [9 :♦]}
                                                  {:player :player2
                                                   :card (:player2 discards)}
                                                  {:player :player3
                                                   :card [10 :♦]}
                                                  {:player :player4
                                                   :card (:player4 discards)}
                                                  {:player :player5
                                                   :card [:Q :♦]}
                                                  {:player :player6
                                                   :card (:player6 discards)}]
                                                 [{:player :player2
                                                   :card [:A :♣]}
                                                  {:player :player3
                                                   :card [9 :♦]}
                                                  {:player :player4
                                                   :card [:K :♣]}
                                                  {:player :player5
                                                   :card [10 :♣]}
                                                  {:player :player6
                                                   :card [:Q :♣]}
                                                  {:player :player1
                                                   :card [9 :♣]}]]
                              :current-trick []}))
          high-discard-game (soft-void-game {:player2 [:A :♣]
                                             :player4 [:A :♠]
                                             :player6 [:K :♣]})
          low-discard-game (soft-void-game {:player2 [9 :♣]
                                            :player4 [9 :♠]
                                            :player6 [10 :♣]})
          high-event (bot/card-action high-discard-game
                                      :player1
                                      :hybrid-ruff-invite)]
      (is (= {:type :play-card :card [:Q :♣]} high-event))
      (is (= :partner-ruff-invite
             (:reason (bot/explain-card-action high-discard-game
                                               :player1
                                               :hybrid-ruff-invite
                                               high-event))))
      (is (= {:type :play-card :card [:J :♥]}
             (bot/card-action low-discard-game :player1 :hybrid-ruff-invite)))))

  (testing "action inference treats an expensive same-outcome trump as no cheap trump"
    (let [game {:phase :trick-playing
                :trump :♣
                :active-players game/players
                :players {:player1 {:team 1
                                    :hand [[:K :♦] [:A :♠] [9 :♦]
                                           [10 :♥] [:A :♦] [10 :♠]
                                           [:K :♥]]}
                          :player2 {:team 2
                                    :hand [[:Q :♥] [:J :♦] [:K :♦]
                                           [9 :♠] [9 :♦] [:A :♠]
                                           [:K :♣]]}
                          :player3 {:team 1
                                    :hand [[:J :♠] [:A :♥] [:Q :♦]
                                           [:K :♥] [9 :♥] [10 :♠]
                                           [:Q :♥]]}
                          :player4 {:team 2
                                    :hand [[10 :♦] [9 :♠] [:A :♥]
                                           [:A :♦] [:Q :♠] [9 :♥]
                                           [:J :♥]]}
                          :player5 {:team 1
                                    :hand [[:K :♠] [10 :♥] [9 :♣]
                                           [:A :♣] [:J :♦] [:Q :♣]]}
                          :player6 {:team 2
                                    :hand [[:Q :♠] [10 :♦] [:J :♥]
                                           [:K :♠] [:Q :♦] [:K :♣]]}}
                :completed-tricks [[{:player :player5 :card [:J :♣]}
                                    {:player :player6 :card [10 :♣]}
                                    {:player :player1 :card [:J :♠]}
                                    {:player :player2 :card [10 :♣]}
                                    {:player :player3 :card [9 :♣]}
                                    {:player :player4 :card [:A :♣]}]]
                :current-trick [{:player :player5 :card [:J :♣]}
                                {:player :player6 :card [:Q :♣]}]
                :current-player :player1
                :trick-leader :player5}
          unseen-counts (bot/unseen-card-counts game :player5)
          voids (bot/known-voids game)
          base-availability (bot/prob-hand-has-success
                             (bot/unseen-effective-suit-count
                              unseen-counts
                              :♣
                              :♣)
                             (bot/total-unseen-count unseen-counts)
                             (count (get-in game [:players :player1 :hand])))
          conditioned-availability (bot/suit-available-probability
                                    bot/action-inference-play-config
                                    game
                                    :player5
                                    unseen-counts
                                    voids
                                    :player1
                                    :♣)
          void-probability (bot/soft-void-confidence
                            bot/action-inference-play-config
                            game
                            :player5
                            unseen-counts
                            voids
                            :player1
                            :♣)]
      (is (< 0.50 base-availability 0.53))
      (is (< 0.20 conditioned-availability 0.24))
      (is (< 0.76 void-probability 0.80))))

  (testing "when following, a bot preserves an unsafe high trump with a partner pending"
    (let [game {:phase :trick-playing
                :trump :♦
                :active-players game/players
                :hand-index 9
                :bids [{:type :bid
                        :player :player6
                        :bid-type :bid
                        :value 5
                        :hand-index 9}]
                :players {:player1 {:team 1
                                    :hand [[10 :♣] [:A :♠] [10 :♦]
                                           [:J :♥] [:Q :♠] [10 :♥]
                                           [:K :♥]]}
                          :player2 {:team 2
                                    :hand hidden-hand-size}
                          :player3 {:team 1
                                    :hand hidden-hand-size}
                          :player4 {:team 2
                                    :hand hidden-hand-size}
                          :player5 {:team 1
                                    :hand hidden-hand-size}
                          :player6 {:team 2
                                    :hand hidden-hand-size}}
                :completed-tricks [[{:player :player6 :card [:J :♦]}
                                    {:player :player1 :card [9 :♦]}
                                    {:player :player2 :card [:Q :♦]}
                                    {:player :player3 :card [:K :♦]}
                                    {:player :player4 :card [9 :♠]}
                                    {:player :player5 :card [9 :♦]}]]
                :current-trick [{:player :player6 :card [10 :♦]}]}]
      (is (= {:type :play-card :card [:J :♥]}
             (bot/card-action game :player1 :hybrid)))
      (is (= {:type :play-card :card [10 :♦]}
             (bot/card-action game :player1 :hybrid-preservation)))))

  (testing "when a partner is safely winning, a bot dumps low instead of overtaking"
    (let [game {:players {:player1 {:team 1}
                          :player3 {:team 1
                                    :hand [[:K :♥] [9 :♥] [9 :♣]]}}
                :trump :♠
                :completed-tricks [[{:player :player2 :card [:A :♥]}]]
                :current-trick [{:player :player1 :card [:A :♥]}]}]
      (is (= {:type :play-card :card [9 :♥]}
             (bot/card-action game :player3)))))

  (testing "when a partner's king is vulnerable, a bot protects it with the ace"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♦
                  :active-players game/players
                  :players {:player1 {:team 1
                                      :hand [[:K :♠] [:A :♠]]}
                            :player5 {:team 1}
                            :player6 {:team 2}}
                  :current-trick [{:player :player5 :card [:K :♠]}
                                  {:player :player6 :card [:J :♣]}]})]
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player1)))
      (is (= {:type :play-card :card [:A :♠]}
             (bot/card-action game :player1 :card-counting)))))

  (testing "when the higher card has been seen, a bot preserves the partner's king"
    (let [game (with-hidden-hand-sizes
                 {:phase :trick-playing
                  :trump :♦
                  :active-players game/players
                  :players {:player1 {:team 1
                                      :hand [[:K :♠] [:A :♠]]}
                            :player5 {:team 1}
                            :player6 {:team 2}}
                  :completed-tricks [[{:player :player2 :card [:A :♠]}]]
                  :current-trick [{:player :player5 :card [:K :♠]}
                                  {:player :player6 :card [:J :♣]}]})]
      (is (= {:type :play-card :card [:K :♠]}
             (bot/card-action game :player1)))
      (is (= {:type :play-card :card [:K :♠]}
             (bot/card-action game :player1 :card-counting)))))

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

(deftest explained-action-test
  (let [game (with-hidden-hand-sizes
               {:phase :trick-playing
                :trump :♠
                :active-players game/players
                :players {:player1 {:team 1
                                    :hand [[:A :♠] [9 :♥]]}}
                :completed-tricks []
                :current-trick []})
        event (binding [bot/*play-strategy* :hybrid-ruff-invite]
                (bot/explained-action game :player1))
        action-inference-event
        (binding [bot/*play-strategy* :hybrid-action-inference-team-ev]
          (bot/explained-action game :player1))]
    (is (= :play-card (:type event)))
    (is (= :hybrid-ruff-invite (get-in event [:ai :policy])))
    (is (contains? #{:probability-ruff-invite :card-counting}
                   (get-in event [:ai :engine])))
    (is (some? (get-in event [:ai :reason])))
    (is (seq (get-in event [:ai :candidates])))
    (is (= :hybrid-action-inference-team-ev
           (get-in action-inference-event [:ai :policy])))
    (is (= :probability-action-inference-team-ev
           (get-in action-inference-event [:ai :engine])))))
