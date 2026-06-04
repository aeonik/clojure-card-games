# clojure-card-games

A small Clojure implementation of Karbosh, a six-player double-deck Bid Euchre variant.

The code is organized around a pure game engine:

- `clojure-card-games.cards`: card data and display helpers
- `clojure-card-games.deck`: deck creation, shuffling, and dealing
- `clojure-card-games.rules`: trick ordering, bid validation, legal plays, and scoring
- `clojure-card-games.state`: pure state transitions from game events
- `clojure-card-games.io.*`: terminal rendering and interaction

## Run

```sh
clojure -M:run
```

With a seed and optional replay input:

```sh
clojure -M:run 12345 "4ppppphAhKhQhJh0h9h"
```

## Karbosh Web App

The multiplayer web app copied from `dc3systems-new` lives under
`clojure-card-games.karbosh`.

```sh
clojure -M:karbosh-server
```

The static client is served from `karbosh/` by default. Rebuild the ClojureScript
bundle with:

```sh
clojure -M:karbosh-cljs
```

Deployment and production REPL notes are in `deploy/README.md`. The normal
no-restart Karbosh deploy flow is:

```sh
clojure -T:build test
clojure -T:build cljs
clojure -T:build package-static
clojure -T:build deploy-compatible
```

## Test

```sh
clojure -M:test
```

## Probability Helpers

Exact hypergeometric helpers are available through:

```sh
clojure -M:prob prob-hg 9 31 16 4
clojure -M:prob follow 8 --float
```

## Status

This is still a work in progress. The current priority is keeping the game engine pure,
small, and covered by focused tests before expanding UI or bot behavior.
