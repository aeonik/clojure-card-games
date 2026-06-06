(ns clojure-card-games.karbosh.fixtures)

(def bl32c2
  "Room BL32C2, captured from production after Dave called hearts Karbosh.

  This hand is useful for testing donation-aware Karbosh bidding: the raw hand
  has enough control to bid six, but the discard/donation phase pushes it into
  a credible Karbosh."
  {:room-id "BL32C2"
   :seed 1780766321306
   :caller :player1
   :trump :♥
   :initial-hands
   {:player1 [[:K :♥] [:J :♥] [9 :♥] [:J :♣]
              [10 :♠] [:J :♦] [:A :♥] [:J :♥]]
    :player2 [[:Q :♦] [9 :♥] [:K :♦] [10 :♦]
              [:Q :♣] [:K :♦] [:Q :♥] [:J :♠]]
    :player3 [[:Q :♠] [:K :♠] [:J :♦] [:J :♣]
              [9 :♠] [9 :♣] [9 :♠] [:Q :♦]]
    :player4 [[:A :♣] [10 :♠] [:A :♥] [:A :♣]
              [10 :♥] [10 :♣] [:Q :♥] [9 :♣]]
    :player5 [[:Q :♠] [:J :♠] [:A :♠] [9 :♦]
              [10 :♣] [:K :♣] [10 :♦] [:A :♦]]
    :player6 [[:K :♣] [:Q :♣] [:A :♦] [9 :♦]
              [:K :♥] [:K :♠] [:A :♠] [10 :♥]]}
   :expected-discards [[10 :♠] [:J :♣]]
   :expected-wanted-donation-count 11})

(defn hand [fixture player]
  (get-in fixture [:initial-hands player]))
