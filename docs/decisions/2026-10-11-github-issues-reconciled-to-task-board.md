# 2026-10-11 — Open GitHub issues reconciled to the rebuild task board

**Context.** Eleven GitHub issues were open on 2026-10-11, created between
December 2025 and July 2026, all before the rebuild specification
(`ch4-causal-analysis/rebuild/`) and its task board (`tasks.md`) existed.
Some restated a task-board item, some were already settled by a
`DECISIONS.md` entry, and some described work the rebuild had replaced
wholesale. Two records of the same queue drifting apart is a
contradiction between homes; the task board is the authoritative home for
pipeline work, and the issues had to be mapped onto it or retired.

**Decision.** Each issue was mapped as follows.

| Issue | Maps to | Action |
|---|---|---|
| #20 fix: electoral off-by-one | task 1.5 (phase 2 step 2.3); D-2026-10-10-d | kept open, comment |
| #19 fix: MT quota outliers | task 1.4 (phase 2 step 2.1) | kept open, comment |
| #18 feat: Legal Amazon coverage with biome flag | task 1.1 (phase 2 step 2.9) | kept open as the canonical issue, comment |
| #17 fix: price index normalization | task 1.3 (phase 2 step 2.4); D-2026-10-10-h | kept open, comment |
| #15 style: palette and graph design | new task 4.4 (descriptive stage) | kept open, comment |
| #8 fix: Mojuí dos Campos | task 1.6; D-2026-10-05-l, D-2026-10-07-d | kept open, comment |
| #10 feat: DiD/RDD blueprint | stage 12 | left untouched by the author's choice |
| #14 feat: include all Legal Amazon municipalities | task 1.1 | closed as duplicate of #18 |
| #11 fix: PIA instead of PEA | D-2026-10-05-e | closed as implemented |
| #13 fix: PIA/PEA changes into the panel | D-2026-10-05-e; task 2.2 | closed as implemented |
| #12 fix: rewrite data-description sections 3 and 4 | stage 10, phase 3 annex | closed as superseded |

The drop of Mojuí dos Campos (D-2026-10-05-l) is treated as provisional:
the author may reverse it if the municipality's data can be reconstructed,
and the phase 1 scripts already handle Mojuí's data needs with that in
mind. Issue #8 therefore stays open against task 1.6 rather than closing
as resolved.

**Rationale.** The six kept issues each correspond to work not yet done
and gain a stable pointer into the task board, which scripts, decision
entries and commit messages cite. #14 duplicates #18, and #18 carries the
fuller specification (Grajaú as the only ever-listed municipality outside
the 502). #11 and #13 asked for PIA-based employment rates and
population- or quota-normalised transfer intensities; the recovered panel
already carries `emp_pia_rate_*`, `bf_transfers_pc_brl_2024` and
`bf_transfers_quota_brl_2024` with the PEA variant kept for robustness,
and D-2026-10-05-e records the choice; whether all three transfer
normalisations stay stored belongs to the storage audit (task 2.2). #12
asked to reconstruct the closing sections of the parked data-description
appendix (`_annex/parked/`); phase 3 writes the single annex from the
final state per `rebuild/03-annex-variable-template.md`, so the appendix
is replaced rather than revised. Closing #8 as resolved was considered
and rejected: the decision it refers to is one the author intends to
revisit.

**Consequences.** `tasks.md` gains three details carried only by the
issues: task 1.4 notes that the fourteen outliers sit above the median
coverage split, so the stratification must be rechecked after the fix;
task 1.1 notes that Grajaú (MA) is the only ever-listed municipality
outside the 502 in 2004-2020, making the extension an infrastructure
investment rather than a power fix; task 1.6 records that the Mojuí drop
is provisional and that the parent-rule handling must not foreclose
re-inclusion. A new task 4.4 (figure style convention) holds #15. Issue
comments point each kept issue to its task ID and each closed one to the
record that settles it.
