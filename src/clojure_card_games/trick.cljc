(ns clojure-card-games.trick
  "Generic trick-taking helpers shared by trick games.

  A trick is a vector of `{:player p :card c}` plays. Card strength is
  supplied by each game as a `value-fn` from card to number, so trump and
  lead-suit semantics stay in game-specific rules namespaces.")

(defn winning-play
  "Return the play that wins `trick` under `value-fn`.

  Ties keep the earlier play: in multi-deck games two copies of the same
  card can land in one trick, and by rule the first copy played wins."
  [trick value-fn]
  (when (seq trick)
    (reduce (fn [winner play]
              (if (> (value-fn (:card play))
                     (value-fn (:card winner)))
                play
                winner))
            (first trick)
            (rest trick))))

(defn resolve-trick
  "Return the player who wins `trick` under `value-fn`."
  [trick value-fn]
  (:player (winning-play trick value-fn)))
