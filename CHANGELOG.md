# Change Log

## Unreleased

### Structural refactor

- Added a contributor-friendly Karbosh deploy wrapper (`bin/karbosh-deploy`),
  dry-run build tasks, SSH port/RSync transport configuration, and local deploy
  environment docs so deploys are reproducible outside one shell history.
- Declared `clojure-card-games.karbosh.shared.*` the canonical Karbosh
  engine and removed the stale root `rules`/`state` duplicates. The TUI now
  runs on the canonical engine and supports the karbosh discard and
  donation phases.
- Added a game-agnostic core for future games: `clojure-card-games.cards`
  (card model), `clojure-card-games.deck` (deck specs, seeded shuffle), and
  `clojure-card-games.trick` (value-fn trick resolution with earlier-play
  tie semantics). The Karbosh shared cards/rules delegate to it.
- Split `clojure-card-games.karbosh.bot` (1987 lines) into focused
  submodules — `bot.config`, `bot.cards`, `bot.inference`, `bot.bid`,
  `bot.play`, `bot.explain` — behind a facade that preserves the existing
  public API, dynamic config vars, and strategy registries.
- Externalized all bid/play policy weights into
  `src/clojure_card_games/karbosh/policy.edn`, validated at load time
  (missing, unknown, or mistyped keys fail fast).
- Added invariant regression tests: deck multiset invariants, per-action
  legality across seeded bot self-play, termination, score conservation,
  and same-seed determinism.
- Documented the house rule that Karbosh and double Karbosh score the same
  15 points; double Karbosh differs only in bid rank and solo play.
- Fixed Unicode card glyph offsets: queen and king previously rendered as
  knight and queen.

### Earlier

- Consolidated around the pure `cards`/`deck`/`rules`/`state` engine.
- Removed stale template-era engine, CLI, GUI, and tests.
- Updated project aliases and focused tests.
