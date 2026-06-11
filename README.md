# clojure-card-games

A Clojure library of card-game engines. The first game is Karbosh, a
six-player double-deck Bid Euchre variant with a multiplayer web app and a
heuristic AI; the architecture is designed so future games (Oh Hell, Euchre,
Hearts, Bridge, Poker, Blackjack) can share the same core.

## Architecture

The code is layered so game-agnostic pieces sit at the root and each game
owns its own rules and state machine:

- `clojure-card-games.cards` — generic card model: a card is a logical
  `[rank suit]` tuple; rank/suit tables, display, and parsing.
- `clojure-card-games.deck` — deck construction from a spec
  (`{:ranks .. :suits .. :copies ..}`), seeded shuffling, dealing.
- `clojure-card-games.trick` — generic trick resolution parameterized by a
  card-value function. Ties keep the earlier play, which matters for
  multi-deck games.
- `clojure-card-games.probability.*` — exact hypergeometric helpers.
- `clojure-card-games.karbosh.shared.*` — **the canonical Karbosh engine**
  (`.cljc`, shared with the browser client): `cards` (deck spec), `rules`
  (bowers, bids, scoring), `game` (event-driven state machine).
- `clojure-card-games.karbosh.*` — the Karbosh application: web server,
  client, rooms, analysis, bot, simulation harness, solvers.
- `clojure-card-games.io.*` — terminal rendering and interaction, running on
  the canonical engine.

### Karbosh rules notes

- The deck is two physical copies of each 9–A card (48 cards). When both
  copies of a card land in one trick, the first copy played wins.
- Karbosh and double Karbosh both score the same 15 points made or set, by
  house rule. Double Karbosh differs only in bid rank (it outranks Karbosh)
  and in being played solo without partner donations.

### Bot policy

The bot is a deterministic heuristic policy stack over exact hypergeometric
hidden-card analysis — not a trained model. It is split into focused modules
under `clojure-card-games.karbosh.bot.*` (`config`, `cards`, `inference`,
`bid`, `play`, `explain`), with `clojure-card-games.karbosh.bot` as the
facade that owns strategy registries and dynamic config selection.

All bid/play weights live in `src/clojure_card_games/karbosh/policy.edn`,
validated at load time. Tuning is a data change: edit the EDN, run seeded
matchups (`clojure-card-games.karbosh.sim`), and commit the diff with the
results. Soft inference is advisory only — it never changes legality or
exact card-count facts.

## Run

```sh
clojure -M:run
```

With a seed and optional replay input:

```sh
clojure -M:run 12345 "4ppppphAhKhQhJh0h9h"
```

## Karbosh Web App

```sh
clojure -M:karbosh-server
```

The static client is served from `karbosh/` by default. Rebuild the ClojureScript
bundle with:

```sh
clojure -M:karbosh-cljs
```

Deployment and production REPL notes are in `deploy/README.md`. The normal
contributor-safe no-restart Karbosh deploy flow is:

```sh
bin/karbosh-deploy check
bin/karbosh-deploy plan-compatible
git push
bin/karbosh-deploy deploy-compatible
```

## Test

```sh
clojure -M:test
```

The suite includes invariant gates in
`clojure-card-games.karbosh.invariants-test`: deck multiset invariants,
per-action legality across seeded bot self-play, score conservation, and
same-seed determinism. Policy changes must keep these green.

## Probability Helpers

Exact hypergeometric helpers are available through:

```sh
clojure -M:prob prob-hg 9 31 16 4
clojure -M:prob follow 8 --float
```

Karbosh trick analysis keeps exact ratios internally where practical. One
important derived metric is
`:expected-pending-partner-control-burn`, computed in
`clojure-card-games.karbosh.analysis/forced-higher-follow-probability-for`.
It counts labeled hidden-hand deals that satisfy hard public void facts before
asking whether a pending partner must play a higher follow-suit card because
they have no lower/equal follow card. This catches control-collision positions,
for example leading a medium trump that can only win by forcing partner's left
bower. It is downstream of the usual void-and-trump analysis: public follow-suit
failures first constrain who can still hold the led suit, and the control-burn
ratio is computed inside that smaller exact universe. Policy scoring converts
the exact ratio to a decimal only at the final weighting step.

Bot play scoring distinguishes two control-burn costs. Trump-control burn is
the direct expected partner control burn when leading trump. Ruff-exposed burn
is used for off-suit leads and multiplies the partner control-burn expectation
by opponent ruff risk, so clean off-suit Aces can still be preferred while low
off-suit leads that force a partner Ace into a likely ruff are penalized.

## Status

Actively developed. Current priorities are in `TODO.md`; AI policy history
and regression hands are in `docs/karbosh-ai-notes.md`.
