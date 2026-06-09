# TODO

## Karbosh Analysis

- Improve perfect-information solver performance for full six-player, eight-card
  hand audits. Current exact solving can time out on complete deals; investigate
  stronger transposition keys, suit symmetry, better move ordering, and
  endgame tablebases before relying on it as a routine admin oracle. As a
  concrete benchmark, BL32C2 donation-aware Karbosh PIMC took about 220 seconds
  for 20 samples.
- Compare bot policy changes with seeded Monte Carlo runs and record the
  aggregate results alongside the change. Keep old policies pluggable so
  suspected improvements can be validated against the same deal set.
- Tighten ruff-risk hypergeometric calculations to use exact joint labeled-hand
  probabilities instead of combining per-player probabilities as independent
  events. `ZXD4P4` archived game `6729679244203831760` / `1781044733784`,
  hand 2 is the reference case: after `J♦` pulls trump, exact immediate ruff
  risk for the second lead is about `18.74%` for `A♣` and `24.93%` for
  singleton `A♥`, while the current approximation reports `18.0%` and `23.7%`.
  The direction is right, but the hand is important enough to use as a
  regression/benchmark for exact odds.
- Validate defender low-exit lead policy with seeded Monte Carlo. The first
  fix was motivated by `ZXD4P4` hand 11: after winning trick 1 in the maker's
  trump suit, preservation/defender-exit policies now prefer a low non-trump
  exit when no safe card or off-suit ace is available.

## Karbosh UI

- Investigate mobile vertical-view animation resets when tapping player
  chiclets or other controls. Also audit possible display bugs where the wrong
  card is shown during animation or cards briefly disappear.
