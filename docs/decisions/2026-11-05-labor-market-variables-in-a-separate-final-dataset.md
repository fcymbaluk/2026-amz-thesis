# 2026-11-05 — Labor-market variables of the RAIS track stored in a separate final dataset

**Context.** The RAIS track (tasks 3.A to 3.C of
`ch4-causal-analysis/rebuild/upstream-reform-rais.md`) produces on the
order of 80 to 100 new municipality-year columns: the wage distribution
in two sectors (34 columns in `rais_wage_distribution_1999_2024`),
private-sector variants of the twelve existing employment variables plus
the public count, and counts and shares for about twenty CNAE sections.
The final panel carries 76 columns after phase 2R. The author asked,
at the task 3.A session, whether the new variables would make the final
dataset too large and whether a separate dataset would be wiser. The
phase table (`rebuild/00-rebuild-phases.md`) and the chapter guidance
name a single final panel, `panel_amz_2000_2024_final`.

**Decision.** The labor-market variables of the RAIS track are stored in
a separate final-layer dataset, keyed by geocode and year and built by
the pipeline from the RAIS interim files, which analysis scripts join
when they need it. The main panel takes only the few labor-market
columns the chapter uses, chosen at the script 02 re-point under the
storage-verdict rule of the codebook. The sector counts of task 3.C are
stored long (one row per municipality, year and section), not wide.
Names and the exact column set are fixed at the re-point session, with
the final names entered in the crosswalk.

**Rationale.** Rows are not the constraint (807 municipalities over 25
years); readability and documentation are. A 150-column panel defeats
the codebook and the annex, and most of the new columns serve
descriptive or robustness uses that no estimation script needs in the
panel. The repository already separates the layers (interim files by
source, the final panel by content), so a second final dataset follows
the existing grammar (`panel_<scope>_<content>_<years>`) rather than
breaking it. Keeping everything in one panel was rejected for the reason
above; keeping the new variables interim-only was rejected because the
chapter's data description must document them as deliverables with the
same provenance and codebook discipline as the panel.

**Consequences.** A new output of the pipeline in `_data/final/`, built
by script 04 or a sibling, with codebook rows (`dataset` column) and an
annex section; the phase-3 acceptance ("done when
`panel_amz_2000_2024_final` exists and is fully documented") extends to
the labor-market dataset. Guidance edits proposed with this memo, for
the author's approval: `rebuild/00-rebuild-phases.md` (phase 3 outputs),
`docs/execution-path.md` stage 10 ("Done when" line),
`docs/repo-conventions.md` (final datasets: one example of a second
content-first final dataset), `rebuild/naming-convention.md` (public and
private tokens for the sector slot). Approved and applied 2026-11-05
(branch guidance-labor-dataset).
