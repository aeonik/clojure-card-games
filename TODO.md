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
- Study defender trump-lead discipline using `ZXD4P4` hand 11 as a concrete
  case. Autoplay won trick 1 with the right bower after the maker led low
  clubs, then led vulnerable `Q♣` on trick 2 and lost to the duplicate right.
  Evaluate whether defenders should prefer non-trump exits or lower trump in
  this shape before changing defaults.

## Karbosh UI

- Investigate mobile vertical-view animation resets when tapping player
  chiclets or other controls. Also audit possible display bugs where the wrong
  card is shown during animation or cards briefly disappear.
