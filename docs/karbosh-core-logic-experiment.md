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
{:contexts 10000,
 :tasks
 [{:task :effective-suit,
   :pure {:elapsed-ms 29.420875},
   :core-logic {:elapsed-ms 553.986553},
   :logic-over-pure-ratio 18.82971029923481,
   :mismatch-count 0}
  {:task :legal-cards,
   :pure {:elapsed-ms 71.207813},
   :core-logic {:elapsed-ms 1695.007304},
   :logic-over-pure-ratio 23.80367030791972,
   :mismatch-count 0}
  {:task :winning-play,
   :pure {:elapsed-ms 17.33195},
   :core-logic {:elapsed-ms 80.7244},
   :logic-over-pure-ratio 4.657548631284997,
   :mismatch-count 0}
  {:task :simple-card-action,
   :pure {:elapsed-ms 101.687198},
   :core-logic {:elapsed-ms 1961.953848},
   :logic-over-pure-ratio 19.29401032369876,
   :mismatch-count 0}]}
```

For 50,000 sampled trick contexts:

```clojure
{:contexts 50000,
 :tasks
 [{:task :effective-suit,
   :pure {:elapsed-ms 68.167658},
   :core-logic {:elapsed-ms 2087.747126},
   :logic-over-pure-ratio 30.62665180605149,
   :mismatch-count 0}
  {:task :legal-cards,
   :pure {:elapsed-ms 224.809509},
   :core-logic {:elapsed-ms 7806.643637},
   :logic-over-pure-ratio 34.72559355574234,
   :mismatch-count 0}
  {:task :winning-play,
   :pure {:elapsed-ms 64.998776},
   :core-logic {:elapsed-ms 336.100163},
   :logic-over-pure-ratio 5.17086910990447,
   :mismatch-count 0}
  {:task :simple-card-action,
   :pure {:elapsed-ms 398.83729},
   :core-logic {:elapsed-ms 9619.135222},
   :logic-over-pure-ratio 24.11794349018869,
   :mismatch-count 0}]}
```

The relation implementation agrees with the pure rules for sampled like-for-like
tasks, but it is much slower in direct use.

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
