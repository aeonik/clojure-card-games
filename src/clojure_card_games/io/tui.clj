(ns clojure-card-games.io.tui
  (:require [clojure.string :as str]
            [clojure-card-games.cards :as cards :refer [char->rank char->suit]]
            [clojure-card-games.sorting :as sort]
            [clojure-card-games.state :as state]))

;; Pure string renderers
(defn card->str [[rank suit]]
  (let [ansi (str "\u001B[" (cards/suit->ansi suit) "m")
        reset "\u001B[0m"]
    (str ansi (cards/rank->str rank) (cards/suit->str suit) reset)))

(defn hand->str [hand & [sort?]]
  (let [cards (if sort? 
                (:sorted-hand (sort/find-best-trump hand))
                hand)]
    (str/join " " (map card->str cards))))

(defn print-hand! [hand]
  (println (hand->str hand)))

(defn print-game-state! [game & [sort-hands?]]
  (println
   (str
    "\n=== Game State ===\n"
    "Phase: " (:phase game) "\n"
    "Current Player: " (:current-player game) "\n"
    "Dealer: " (name (:dealer game)) "\n"
    (when (:current-bid game) (str "Current Bid: " (:current-bid game) "\n"))
    (when (:trump game) (str "Trump: " (:trump game) "\n"))
    "\nScores:\n"
    (str/join "\n" (map (fn [[team score]] (str "Team " team ": " score)) (:scores game)))
    (when (= (:phase game) :trick-playing)
      (str "\nTricks this hand: " (:tricks-this-hand game)))
    (when (seq (:current-trick game))
      (str "\nCurrent Trick:\n"
           (str/join "\n"
             (map (fn [{:keys [player card]}]
                    (str (name player) ": " (card->str card)))
                  (:current-trick game)))))
    "\n\nPlayer Hands:\n"
    (str/join "\n" (map (fn [[player {:keys [hand]}]]
                              (str (name player) ": " (hand->str hand sort-hands?)))
                            (:players game)))
    "\n")))

;; Pure phase parsers
(defn- bidding->action [game ch]
  (let [p (:current-player game)]
    (case ch
      \x {:type :quit}
      \p {:type :bid :player p :bid-type :pass}
      \k {:type :bid :player p :bid-type :karbosh}
      \d {:type :bid :player p :bid-type :double-karbosh}
      (when-let [n (try (Long/parseLong (str ch)) (catch Exception _ nil))]
        (when (<= 1 n 8)
          {:type :bid :player p :bid-type :bid :value n})))))

(defn- trump-selection->action [game ch]
  (let [p (:current-player game)
        suit (char->suit ch)]
    (cond
      (= ch \x) {:type :quit}
      suit      {:type :trump-selection :player p :suit suit})))

(defn- trick->action [game ch1 ch2]
  (let [p (:current-player game)
        hand (get-in game [:players p :hand])
        card (when (and ch1 ch2) (let [r (char->rank ch1)
                                       s (char->suit ch2)]
                                   (when (and r s) [r s])))]
    (cond
      (= ch1 \x) {:type :quit}
      (and card (some #(= card %) hand)) {:type :play-card :player p :card card})))

(def phase->parser
  {:bidding         bidding->action
   :trump-selection trump-selection->action
   :trick-playing   trick->action
   :hand-complete   (constantly {:type :new-hand})
   :game-over       (constantly {:type :quit})})

(defn parse-action [game chs]
  (let [phase (:phase game)
        f (phase->parser phase)]
    (cond
      (nil? f) {:type :quit}
      (= phase :trick-playing)
      (when (>= (count chs) 2)
        (f game (first chs) (second chs)))
      :else (f game (first chs)))))

(defn get-player-action!
  ([game] (get-player-action! game nil))
  ([game action-seq]
   (let [phase (:phase game)
         current-player (:current-player game)]
     (loop [chs (seq action-seq)]
       (if (seq chs)
         (if-let [action (parse-action game chs)]
           (let [consumed (if (= phase :trick-playing)
                            (apply str (take 2 chs))
                            (str (first chs)))]
             (assoc action
                    :next-seq (if (= phase :trick-playing) (nnext chs) (next chs))
                    :input consumed))
           (recur (next chs)))
         ;; Interactive mode
         (case phase
           :bidding
           (do
             (println (str "\n" (name current-player) " - Enter bid (1-8, p=pass, k=karbosh, d=double, x=quit): "))
             (let [input (read-line)]
               (if-let [action (bidding->action game (first input))]
                 (assoc action :input input)
                 (do (println "Invalid bid!") (recur nil)))))
           :trump-selection
           (do
             (println (str "\n" (name current-player) " - Select trump (h=♥, s=♠, d=♦, c=♣, x=quit): "))
             (let [input (read-line)]
               (if-let [action (trump-selection->action game (first input))]
                 (assoc action :input input)
                 (do (println "Invalid suit!") (recur nil)))))
           :trick-playing
           (let [hand (get-in game [:players current-player :hand])]
             (if (empty? hand)
               {:type :skip}
               (do
                 (println (str "\n" (name current-player) " - Play a card (rank then suit, e.g. Qh, 0d, x=quit): "))
                 (let [input (read-line)]
                   (if (>= (count input) 2)
                     (if-let [action (trick->action game (first input) (second input))]
                       (assoc action :input input)
                       (do (println "Invalid card!") (recur nil)))
                     (do (println "Invalid input!") (recur nil)))))))
           :hand-complete {:type :new-hand}
           :game-over {:type :quit}
           {:type :quit}))))))