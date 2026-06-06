# Karbosh AI Notes

## 2026-06-05 Lead Policy

VQS26P showed a recurring lead-policy weakness: when no card met the safe-lead
threshold, bots dumped the lowest card even if an off-suit ace had much better
risk-adjusted value. Example pattern: lead `10♦` while holding `A♦` because the
ace was risky, not because the `10♦` was useful.

Change:

- Keep the old behavior available as `:probability-threshold` and
  `:hybrid-threshold`.
- Preserve the old 32% lead-risk threshold as `classic-play-config`.
- Make default `:hybrid` use risk-adjusted fallback leads with a 5% safe-lead
  threshold.
- Numeric callers still preserve vulnerable high trump when they have non-trump
  exits; they should not casually lead an unsupported trump ace.

Seeded Monte Carlo, 100 seeds with teams swapped for 200 total games:

| Matchup | New wins | Old wins | Read |
| --- | ---: | ---: | --- |
| Same old 32% threshold | 99 | 101 | Essentially even. |
| Same low 5% threshold | 121 | 79 | Strongly favors risk-adjusted fallback. |
| True before/after: old threshold policy + 32% config vs new risk-adjusted + 5% config | 102 | 98 | Slightly positive, not decisive. |

Next step: run larger matchups after solver/sim performance improves and inspect
hand-level deltas, not just game win rate. The current change is mainly justified
because the local decisions are more coherent and the low-threshold A/B result is
promising, not because 200 games proves a large global win-rate gain.
