# Karbosh core.logic experiment

Branch: `codex-karbosh-core-logic-experiments`

This branch tests whether `org.clojure/core.logic` is useful for Karbosh rules
and AI integration. The dependency is scoped to `:test` and the
`:karbosh-logic` alias, not the production server classpath.

## Implemented

- `clojure-card-games.karbosh.logic.rules`
  - relational same-color suit
  - relational right/left bower checks
  - relational effective suit
  - relational legal card generation
  - relation wrapper for winning play
- `clojure-card-games.karbosh.logic.ai`
  - a pluggable strategy function:
    ```clojure
    (bot/card-action game player
                     clojure-card-games.karbosh.logic.ai/card-action)
    ```
- `clojure-card-games.karbosh.logic.bench`
  - local benchmark comparing pure legal-card generation to relation-based
    legal-card generation

## Local commands

```sh
clojure -M:test
clojure -M:karbosh-logic 10000
```

## Initial result

For 10,000 sampled trick contexts:

```clojure
{:contexts 10000
 :pure {:card-count 33004, :elapsed-ms 67.304416}
 :core-logic {:card-count 33004, :elapsed-ms 1774.739712}
 :logic-over-pure-ratio 26.36884498039475
 :mismatch-count 0}
```

The relation implementation agrees with the pure rules for sampled legal-card
generation, but it is much slower in this direct use.

## Takeaway

core.logic is promising for local analysis tools, rule exploration, constraint
queries, and maybe generating counterexamples. It does not look appropriate for
hot-path bot play or perfect-information search where legal move generation is
called constantly.

The clean integration point is a function strategy passed to `bot/card-action`.
That lets us test relation-backed AI locally without registering a production
strategy keyword or changing live bot behavior.

## Possible next experiments

- Use core.logic for hidden-hand constraint queries instead of play selection.
- Ask inverse questions such as "which cards could make this play legal?"
- Generate minimal counterexample deals for rule bugs.
- Compare core.logic hidden-hand sampling against the current random sampler.
- Keep the pure rule functions as the production oracle unless relation queries
  solve a problem the direct rules cannot express cleanly.
