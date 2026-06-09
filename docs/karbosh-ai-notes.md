# Karbosh AI Notes

## Regression Hands

- `BL32C2`: production room captured on 2026-06-06.
  - Seed: `1780766321306`
  - Fixture: `clojure-card-games.karbosh.fixtures/bl32c2`
  - Scenario: Dave/player1 has a hearts Karbosh candidate where discard plus
    partner donations are essential to evaluate the bid correctly.

- `ZXD4P4` hand 11: production room captured on 2026-06-08.
  - Game seed: `1780918954859`
  - Hand seed: `-2008906192`
  - Scenario: clubs are trump, Bender/player6 made `4`, then led `9♣`.
    Dave/player1 used autoplay, won trick 1 with `J♣`, and then led `Q♣`
    on trick 2. The lead was beaten by the duplicate `J♣` from
    Sir Shufflesworth/player4.
  - Initial player1 hand: `Q♥ 10♦ 10♣ Q♦ J♣ K♦ Q♣ 9♠`.
  - Trick 1: `9♣ J♣ 10♣ 9♣ J♠ 9♦`; team 1 won.
  - Trick 2 candidate risks from player1's view:
    `Q♣ 99.8%`, `10♣ 99.9%`, `K♦ 89.6%`, `Q♦ 96.2%`,
    `Q♥ 98.4%`, `10♦ 99.9%`, `9♠ 100%`.
  - Exact continuation with actual hidden hands and current bot policies:
    `Q♥` and `9♠` win the trick for team 1; `10♦`, `10♣`, `Q♦`,
    `K♦`, and `Q♣` lose the trick to team 2.
  - Single-trick Monte Carlo from the post-trick-1 state, keeping player1 and
    player6 hands fixed, randomizing player2-player5 hands, and rejecting
    player5 hands with clubs because player5 threw off on the club-led first
    trick:

    | Lead | Team 1 trick win rate | Main winners |
    | --- | ---: | --- |
    | `9♠` | 36.88% | player2 47.35%, player3 32.62%, player4 15.78% |
    | `Q♥` | 36.81% | player2 46.26%, player3 31.47%, player4 16.93% |
    | `10♣` | 33.35% | player2 33.19%, player3 33.35%, player4 33.47% |
    | `Q♣` | 33.35% | player2 33.19%, player3 33.35%, player4 33.47% |
    | `10♦` | 1.18% | player2 46.99%, player6 35.10%, player4 16.74% |
    | `Q♦` | 0.88% | player2 46.99%, player6 34.19%, player4 17.95% |
    | `K♦` | 0.64% | player2 46.38%, player6 33.19%, player4 19.80% |

    The history-consistent hidden-hand space is still about
    191,834,371,872,000 ordered deals, so this was sampled rather than
    exhaustively enumerated.
  - Read: the current preservation lead formula preferred `Q♣` because its
    score-risk value beat the off-suit exits, but this is strategically
    questionable for a defender after taking the first trick in opponent-made
    trump. The one-trick sampled result says `Q♣` was materially worse than
    the best off-suit exits, but not a disaster relative to `10♣`; diamonds
    were clearly poor exits. Study whether defender lead policy should avoid
    vulnerable trump leads unless they are safe, intentionally pulling trump,
    or materially better in rollout/PIMC evaluation.
  - Initial fix: preservation and defender-exit lead policies now use
    `:defender-low-exit` when a defender has no safe card, no off-suit ace,
    and at least one non-trump exit. In this hand, autoplay now leads `9♠`
    instead of `Q♣`. Plain aggressive `:hybrid` remains allowed to spend trump.

- `ZXD4P4` hand 2 (Game 10): external Monte Carlo review logged.
  - Date: 2026-06-09
  - Snapshot link: `https://dc3systems.com/karbosh/admin/rooms/ZXD4P4/snapshot/hands/2`
  - Note: another Codex model completed monte-carlo sims for this game; keep this
    on file for follow-up analysis.

- `ZXD4P4` hand 2, archived game `6729679244203831760` /
  `1781044733784`: singleton ace clearance study.
  - Date: 2026-06-09
  - Snapshot link: `https://dc3systems.com/karbosh/admin/history/ZXD4P4/6729679244203831760/1781044733784/snapshot/hands/2`
  - Scenario: Dave/player1 made `5`, selected diamonds, opened with `J♦`, then
    autoplay led `A♣` over the singleton `A♥`. At that point `J♥` had already
    followed as the left bower, and effective hidden suit counts were
    `{:♥ 9, :♠ 10, :♦ 6, :♣ 10}`. The hypergeometric model correctly treated
    `J♥` as trump, not as a heart; it preferred `A♣` because immediate ruff
    risk was lower (`18.0%` vs `23.7%`).
  - Paired Monte Carlo setup: keep player1's initial hand fixed, randomize the
    other five hands, force player1 to lead `J♦`, let current policy finish
    trick 1, then force either `A♣` or `A♥` as the next lead and let current
    policy play out the rest of the hand. Player1 used `:hybrid-ruff-invite`;
    the other seats used `:hybrid-action-inference-team-ev`.
  - Combined 25,000 paired samples:

    | Forced second lead | Make rate | Avg team 1 tricks | Avg team 1 points |
    | --- | ---: | ---: | ---: |
    | `A♣` | 55.70% | 4.783 | 1.120 |
    | `A♥` | 56.02% | 4.806 | 1.156 |

    `A♥` led by `+0.036` points and `+0.023` tricks per hand. This supports
    studying a modest singleton-control/short-suit lead bonus, but the edge is
    small enough that it should be validated across broader seeded policy
    matchups before changing defaults.

- `ZXD4P4` hand 10 (Game 10): canonical candidate for deeper study.
  - Date: 2026-06-09
  - Snapshot link: `https://dc3systems.com/karbosh/admin/history/ZXD4P4/2008994620/1781027426968/snapshot/hands/10`
  - Hypothesis: if partner is known to be the only player left with trump, leading
    the `9♦` may be a weak/non-optimal choice and should be re-evaluated with
    dedicated Monte Carlo rollout.

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

## 2026-06-07 Conservative Numeric Bidding Experiment

R7ERG4 hand 6 showed a thin numeric bid shape: right bower plus weak support,
no left, no trump ace, and no off-suit ace. The current default strategy bids
that as a `4`, and the result was a hard failure.

Change:

- Added `:karbosh-probability-conservative` as an experimental bid strategy.
- Default remains `:karbosh-probability`.
- The conservative strategy keeps the same Karbosh/double-Karbosh logic, but
  gates numeric bids using configurable control counts derived from high trump,
  off-suit aces, and extra bower depth.
- Added bid-strategy-by-team simulation plumbing so A/B tests can swap bidding
  policies across teams on identical seeds.

Seeded simulation, 500 seeds:

| Strategy | Hands | Bid 4 make | Bid 5 make | Karbosh make | All-pass hands |
| --- | ---: | ---: | ---: | ---: | ---: |
| Default | 8,874 | 81.9% | 76.7% | 55.5% | 4 |
| Conservative | 8,825 | 82.9% | 77.7% | 55.6% | 16 |

Fair head-to-head bid-policy matchup, same 500 seeds with team assignments
swapped for 1,000 games:

| Policy | Wins | Win rate |
| --- | ---: | ---: |
| Default | 512 | 51.2% |
| Conservative | 488 | 48.8% |

Read: the conservative gate improves numeric-bid make rates in isolation, but it
did not beat the default policy head-to-head. Keep it pluggable for comparison;
do not make it default without stronger evidence.

## 2026-06-07 Partner Ruff Invite Experiment

Added a rare lead tactic for situations where the leader has a guaranteed-good
trump but can prove a partner ruff is available:

- All opponents with cards left are known void in trump from prior play.
- Unseen trump still exists, so with opponents void it must belong to partners.
- Every partner with cards left is known void in the off-suit being led.
- The leader has a secure trump winner available, but leads the lowest card in
  the partner-void off-suit instead.

The tactic is available as:

- `:probability-ruff-invite`
- `:hybrid-ruff-invite`

Default remains `:hybrid-preservation`.

Mixed live bot assignment:

- Aggressive-style ruff invite bots: Bender the Rules, Heart Vader, Spade
  Invader, The Notorious R.O.B., Botzilla.
- Preservation-style ruff invite bots: Trick-182, HAL 52, Queen Latifah-Bot,
  The Great Cardini, Tony Starkboard, Cache Money, Decks Machina.

Seeded opportunity scan with default play, 500 games:

| Games | Opportunities | Games with any | Avg per game |
| ---: | ---: | ---: | ---: |
| 500 | 1 | 1 | 0.002 |

Fair head-to-head play-policy matchup, same 500 seeds with team assignments
swapped for 1,000 games:

| Policy | Wins | Win rate |
| --- | ---: | ---: |
| Default `:hybrid-preservation` | 500 | 50.0% |
| Ruff invite `:hybrid-ruff-invite` | 500 | 50.0% |

Score quality in the same 1,000-game matchup:

| Policy | Avg final score | Avg score diff |
| --- | ---: | ---: |
| Default `:hybrid-preservation` | 39.682 | -0.001 |
| Ruff invite `:hybrid-ruff-invite` | 39.683 | +0.001 |

Read: after tightening the requirement so both partners are known void in the
led suit, the tactic appears almost never under current inference rules. It has
no measurable win-rate edge and only a negligible score-differential edge in
this sample. Keep it pluggable and assign it to a mixed set of live bot personas
for long-run tracking; do not make it global default yet.
