# TODO

## Karbosh Analysis

- Improve perfect-information solver performance for full six-player, eight-card
  hand audits. Current exact solving can time out on complete deals; investigate
  stronger transposition keys, suit symmetry, better move ordering, and
  endgame tablebases before relying on it as a routine admin oracle.
- Compare bot policy changes with seeded Monte Carlo runs and record the
  aggregate results alongside the change. Keep old policies pluggable so
  suspected improvements can be validated against the same deal set.

## Karbosh UI

- Investigate mobile vertical-view animation resets when tapping player
  chiclets or other controls. Also audit possible display bugs where the wrong
  card is shown during animation or cards briefly disappear.
