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
- Validate defender low-exit lead policy with seeded Monte Carlo. The first
  fix was motivated by `ZXD4P4` hand 11: after winning trick 1 in the maker's
  trump suit, preservation/defender-exit policies now prefer a low non-trump
  exit when no safe card or off-suit ace is available.

## Karbosh UI

- Investigate mobile vertical-view animation resets when tapping player
  chiclets or other controls. Also audit possible display bugs where the wrong
  card is shown during animation or cards briefly disappear.
