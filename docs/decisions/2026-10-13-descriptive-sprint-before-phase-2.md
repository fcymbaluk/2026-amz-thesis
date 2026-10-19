# 2026-10-13 — Descriptive sprint for Chapter 4 runs before phase 2, on its own branch

**Context.** Phase 1 of the rebuild closed on 2026-10-12 and phase 2
(fixes 2.1-2.9, then the rename and the RAIS reform) was next. On
2026-10-13 the supervisor asked for Chapter 4 by 2026-10-18 with an
improved descriptive opening: trajectories of deforestation against the
social and income series 2004-2020, social expenditure by function and
its federal transfers, municipalities compared with states. Five days do
not allow any phase 2 fix, and the chapter text had not yet been
migrated from the August Word version into the Quarto book.

**Decision.** The chapter is reproduced and extended on the phase 1
panel, as it stands, on a single branch `ch4-descriptive-sprint` that
touches only the manuscript, the analysis layer (scripts 07 onward,
`utils/`, one audit notebook), new raw sources with provenance rows and
the guidance records. Pipeline scripts 01-06 and their outputs are not
touched. The sprint is merged into `main` at delivery and phase 2
resumes at step 2.1 in its original order. The new sources (SICONFI
expenditure and revenue by function, Atlas Brasil, value added by
activity) are joined in an analysis-layer script for now and promoted
into the pipeline as script 02 sources after step 2.9 (task 4.8). Atlas
Brasil is re-acquired by the author on a separate short branch merged to
`main` first, since it closes an open stage 5 item regardless of the
sprint. The chapter keeps its two footnotes on the price-index and
electoral defects, which still hold. Plan:
`ch4-causal-analysis/rebuild/06-descriptive-sprint.md`.

**Rationale.** The deadline is fixed and the descriptive opening does not
depend on the phase 2 fixes: none of the known defects (price
normalization, electoral indexing, MT quota outliers, post-2000
municipalities, NA versus zero) changes the aggregate trajectories the
supervisor asked for, and the text already acknowledges the two that
touch the causal step. Running the sprint inside phase 2 was rejected
because each phase 2 step changes panel columns and would force the new
figures to be regenerated and re-read mid-sprint. Parking the sprint
branch unmerged was rejected because the chapter text is the
dissertation's current state and a parked branch rots against the
rename. Joining the new sources in the analysis layer rather than in
script 02 now was chosen because a pipeline change would reopen the
phase 1 gate; the follow-up keeps the pipeline the single home of panel
columns.

**Consequences.** The stage sequence in `docs/execution-path.md` records
stage 11 (descriptive stage) as partly advanced out of order. Task board
items 4.5-4.8 added. The `siconfi` source token joins the interim
vocabulary. `manuscript/figures` and `manuscript/tables` are symlinks
into `ch4-causal-analysis/output/`, and the chapter's figure PNGs are
committed as the deliverable. A gitignored `ch4-causal-analysis/
literature/` holds the PDFs read for the chapter. After the merge, the
rename (phase 2R) must also migrate scripts 09-11 and the chapter qmd,
which read panel columns by their current names.

Delivered 2026-10-18: docx rendered from the sprint branch (tag `ch4-descriptive-2026-10-18`) after `main` was merged in; the sprint sources stay in the analysis layer until task 4.8; phase 2 resumes at step 2.1.
