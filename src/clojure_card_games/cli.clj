(ns clojure-card-games.cli
  (:require [clojure-card-games.basic :as game]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- rank-to-string [rank]
  (case rank
    :A "A"
    :K "K"
    :Q "Q"
    :J "J"
    (str rank)))

(defn- suit-to-string [suit]
  (case suit
    :♥ "♥"
    :♦ "♦"
    :♠ "♠"
    :♣ "♣"))

(defn- card-color [suit]
  (case suit
    (:♥ :♦) "\u001B[31m"  ; red
    (:♠ :♣) "\u001B[30m"  ; black
    "\u001B[0m"))         ; default

(defn print-card [[rank suit]]
  (let [color (card-color suit)
        reset "\u001B[0m"
        rank-str (rank-to-string rank)
        suit-str (suit-to-string suit)]
    (print (str color rank-str suit-str reset " "))))

(defn print-hand [hand game]
  (let [phase (:phase game)
        trump (:trump game)
        sorted-hand (case phase
                     :bidding (game/find-best-trump hand)
                     :trump-selection (game/find-best-trump hand)
                     :trick-playing (game/sort-hand hand trump)
                     hand)]
    (doseq [card (if (map? sorted-hand)
                   (:sorted-hand sorted-hand)
                   sorted-hand)]
      (print-card card))
    (println)))

(defn print-game-state [game]
  (println "\n=== Game State ===")
  (println "Phase:" (:phase game))
  (println "Current Player:" (:current-player game))
  (println "Dealer:" (name (:dealer game)))
  (when (:current-bid game)
    (println "Current Bid:" (:current-bid game)))
  (when (:trump game)
    (println "Trump:" (:trump game)))
  (println "\nScores:")
  (doseq [[team score] (:scores game)]
    (println "Team" team ":" score))
  (when (= (:phase game) :trick-playing)
    (println "Tricks this hand:" (:tricks-this-hand game)))
  (when (= (:phase game) :hand-complete)
    (println "\n=== Hand History ===")
    (let [hand-index (:hand-index game)
          bids (:bids game)
          trumps (:trumps game)
          tricks (:tricks-per-hand game)
          points (:points-per-hand game)]
      (doseq [i (range (inc hand-index))]
        (println (str "Hand " (inc i) ":"))
        (let [bids-for-hand (filter #(= (:hand-index %) i) bids)
              trump (get trumps i)
              tricks-map (get tricks i)
              points-map (get points i)]
          (when (seq bids-for-hand)
            (println "  Bids:")
            (doseq [b bids-for-hand]
              (println (str "    " (name (:player b)) ": " (:bid-type b) (when (:value b) (str " (" (:value b) ")")))))
            )
          (when trump (println "  Trump:" trump))
          (when tricks-map (println "  Tricks:" tricks-map))
          (when points-map (println "  Points:" points-map)))
        (println))))
  (when (= (:phase game) :game-over)
    (println "\n=== FINAL GAME SUMMARY ===")
    (let [hand-index (:hand-index game)
          bids (:bids game)
          trumps (:trumps game)
          tricks (:tricks-per-hand game)
          points (:points-per-hand game)
          winner (:winner game)]
      (doseq [i (range (inc hand-index))]
        (println (str "Hand " (inc i) ":"))
        (let [bids-for-hand (filter #(= (:hand-index %) i) bids)
              trump (get trumps i)
              tricks-map (get tricks i)
              points-map (get points i)]
          (when (seq bids-for-hand)
            (println "  Bids:")
            (doseq [b bids-for-hand]
              (println (str "    " (name (:player b)) ": " (:bid-type b) (when (:value b) (str " (" (:value b) ")")))))
            )
          (when trump (println "  Trump:" trump))
          (when tricks-map (println "  Tricks:" tricks-map))
          (when points-map (println "  Points:" points-map)))
        (println))
      (println "\nWINNER: Team" winner "!")))
  (println "\nCurrent Trick:")
  (if (empty? (:current-trick game))
    (println "No cards played yet")
    (do
      (println "Cards played this trick:")
      (doseq [{:keys [player card]} (:current-trick game)]
        (print (name player) "played ")
        (print-card card)
        (println))))
  (println "\nPlayer Hands:")
  (doseq [[player {:keys [hand]}] (:players game)]
    (print (name player) ": ")
    (print-hand hand game))
  (println))

(defn get-char
  "Read the next *visible* character from `reader`, skipping LF/CR.
  Returns nil on EOF."
  [^java.io.BufferedReader reader]
  (let [i (.read reader)]          ; int 0-65535 or -1 on EOF
    (cond
      (= -1 i) nil                ; end-of-stream – handle if you want
      (#{\newline \return} (char i)) (recur reader)  ; skip line breaks
      :else (char i))))           ; good char → return it


(defn- get-single-key-or-next-action
  [action-seq recorded-inputs]
  ;; If we're running a replay, consume the first stored action
  (if (seq action-seq)
    (let [action (first action-seq)]
      (print action) (flush)
      (when-not (#{\r \space} action)
        (swap! recorded-inputs conj action))
      [action (rest action-seq)])              ; ‹— keep rest of seq
    ;; Otherwise read a real keystroke
    (let [reader (java.io.BufferedReader. *in*)
          ch     (get-char reader)]            ; skips LF / CR
      (when-not (#{\r \space} ch)
        (swap! recorded-inputs conj ch))
      [ch nil])))


(defn print-prompt [text]
  "Prints a prompt and flushes stdout to ensure immediate display."
  (print text)
  (flush))

(defn parse-bid [input]
  (case input
    \p [:pass nil]
    \k [:karbosh nil]
    \d [:double-karbosh nil]
    (when-let [value (try (Character/digit input 10)
                         (catch NumberFormatException _ nil))]
      (when (and value (game/valid-bid? value))
        [:bid value]))))

(defn parse-card [rank suit]
  (let [rank (case (Character/toUpperCase rank)
               \A :A
               \K :K
               \Q :Q
               \J :J
               \1 1
               \2 2
               \3 3
               \4 4
               \5 5
               \6 6
               \7 7
               \8 8
               \9 9
               \0 10  ; Use 0 for 10 since it's easier to type
               nil)
        suit (case (Character/toLowerCase suit)
               \h :♥
               \s :♠
               \d :♦
               \c :♣
               nil)]
    (when (and rank suit)
      [rank suit])))

(defn get-player-action
  "Gets the next player action, either from stdin or from the action sequence.
   
   Args:
     game - map - The current game state
     action-seq - (optional) seq - Sequence of actions to use instead of stdin
     recorded-inputs - atom - Atom to store recorded inputs
   
   Returns: map - The action to take"
  ([game] (get-player-action game nil (atom [])))
  ([game action-seq] (get-player-action game action-seq (atom [])))
  ([game action-seq recorded-inputs]
   (let [current-player (:current-player game)
         phase (:phase game)]
     (case phase
       :bidding
       (let [_ (print-prompt (str "\n" (name current-player) " - Enter bid:\n"
                                 "1-8: Bid value\n"
                                 "p: Pass\n"
                                 "k: Karbosh\n"
                                 "d: Double\n"
                                 "r: Restart game\n"
                                 "x: Quit game\n"
                                 "Your choice: "))
             [input next-seq] (get-single-key-or-next-action action-seq recorded-inputs)]
         (case (Character/toLowerCase input)
           \r {:type :restart}
           \x {:type :quit}
           (let [bid (parse-bid input)]
             (if bid
               {:type :bid
                :player current-player
                :bid-type (first bid)
                :value (second bid)
                :next-seq next-seq}  ; Pass the next sequence along with the action
               (do
                 (println "\nInvalid bid!")
                 (recur game action-seq recorded-inputs))))))

       :trump-selection
       (let [_ (print-prompt (str "\n" (name current-player) " - Select trump:\n"
                                 "h: Hearts ♥\n"
                                 "s: Spades ♠\n"
                                 "d: Diamonds ♦\n"
                                 "c: Clubs ♣\n"
                                 "r: Restart game\n"
                                 "x: Quit game\n"
                                 "Your choice: "))
             [input next-seq] (get-single-key-or-next-action action-seq recorded-inputs)]
         (case (Character/toLowerCase input)
           \r {:type :restart}
           \x {:type :quit}
           (let [suit (case (Character/toLowerCase input)
                        \h :♥
                        \s :♠
                        \d :♦
                        \c :♣
                        nil)]
             (if suit
               {:type :trump-selection
                :player current-player
                :suit suit
                :next-seq next-seq}  ; Pass the next sequence along with the action
               (do
                 (println "\nInvalid suit!")
                 (recur game action-seq recorded-inputs))))))

       :trick-playing
       (let [player-hand (get-in game [:players current-player :hand])]
         (if (empty? player-hand)
           {:type :skip}
           (let [_ (print-prompt (str "\n" (name current-player) " - Play a card:\n"
                                     "First press: Rank\n"
                                     "  A/K/Q/J: Face cards\n"
                                     "  0: Ten\n"
                                     "  1-9: Other numbers\n"
                                     "  r: Restart game\n"
                                     "  x: Quit game\n"
                                     "Enter rank: "))
                 [rank next-seq] (get-single-key-or-next-action action-seq recorded-inputs)]
             (case (Character/toLowerCase rank)
               \r {:type :restart}
               \x {:type :quit}
               (let [_ (print-prompt "Enter suit: ")
                     [suit next-seq] (get-single-key-or-next-action next-seq recorded-inputs)
                     card (parse-card rank suit)
                     current-trick (get-in game [:current-trick] [])
                     trump (:trump game)
                     lead-suit (when (seq current-trick)
                                (second (get-in current-trick [0 :card])))
                     valid-cards (filter #(game/legal-play? game current-player %) player-hand)]
                 (cond
                   (not card)
                   (do
                     (println "\nInvalid card format!")
                     (recur game action-seq recorded-inputs))
                   (and lead-suit
                        (not= (game/effective-suit card trump) lead-suit)
                        (game/has-suit? player-hand lead-suit trump))
                   (do
                     (println (str "\nYou must follow the lead suit (" (name lead-suit) ")!"))
                     (println "Valid cards to play:")
                     (doseq [valid-card valid-cards]
                       (print "  ")
                       (print-card valid-card))
                     (println)
                     (recur game action-seq recorded-inputs))
                   (not (some #(= % card) player-hand))
                   (do
                     (println "\nYou don't have that card in your hand!")
                     (recur game action-seq recorded-inputs))
                   (game/legal-play? game current-player card)
                   {:type :play-card
                    :player current-player
                    :card card
                    :next-seq next-seq}  ; Pass the next sequence along with the action
                   :else
                   (do
                     (println "\nInvalid play!")
                     (recur game action-seq recorded-inputs))))))))

       :hand-complete
       {:type :new-hand}
       :game-over
       {:type :quit}
       (do
         (println "Unknown phase:" phase)
         (recur game action-seq recorded-inputs))))))

(defn- show-replay-message [seed recorded-inputs]
  "Shows the replay message if there were any recorded inputs."
  (when (seq @recorded-inputs)
    (println "\nTo replay this game, use:")
    (println "clj -M -m clojure-card-games.cli" seed "\"" (apply str @recorded-inputs) "\"")))

(defn play-round [seed & [action-seq]]
  (let [game (atom (game/init-game seed))
        recorded-inputs (atom [])]
    ;; Store seed for potential replay
    (System/setProperty "clojure.card.games.seed" (str seed))
    (loop [current-action-seq action-seq]
      (print-game-state @game)
      (let [action (get-player-action @game current-action-seq recorded-inputs)]
        (case (:type action)
          :skip
          (do
            ;; Just continue the loop, the game state will advance on next play
            (swap! game identity)
            (recur (:next-seq action)))
          :new-hand
          (do
            (swap! game game/apply-event {:type :new-hand})
            (recur (:next-seq action)))
          :restart
          (do
            (reset! game (game/init-game nil))  ; Use new random seed
            (recur nil))  ; Clear action sequence on restart
          :quit
          (do
            (println "\nThanks for playing!")
            (show-replay-message seed recorded-inputs)
            (System/exit 0))  ; Simple exit without shutdown hooks
          (do
            (swap! game game/apply-event action)
            (recur (:next-seq action))))))))

(defn -main [& args]
  (try
    (println "Welcome to Karbosh!")
    (println "Commands:")
    (println "  Bidding: Enter 1-8 for bid, p for pass, k for karbosh, d for double")
    (println "  Trump: Enter h (Hearts), s (Spades), d for Diamonds, or c for Clubs")
    (println "  Cards: Enter rank and suit (e.g. 'A H' for Ace of Hearts)")
    (println "  New hand: y for yes, n for no")
    (println "  Quit: Press 'x' at any time or Ctrl+C")
    (println "\nStarting new game...")
    (let [seed (when (seq args)
                 (try
                   (Long/parseLong (first args))
                   (catch NumberFormatException _
                     (println "Invalid seed, using random shuffle")
                     nil)))
          action-seq (when (> (count args) 1)
                       (seq (second args)))]
      (play-round seed action-seq))
    (catch Exception e
      (println "\nUnexpected error:" (.getMessage e))
      (.printStackTrace e)
      (System/exit 1)))) 