# Dataset rebuild: phases, order, and control rules

Chapter 4 pipeline, scripts 01 to 06. All paths in this file and its companions are relative to `ch4-causal-analysis/`, the chapter folder inside the dissertation repository. This document tells the agent what to do in which order and what counts as done at each step. The four companion files define the targets: `01-script-template.md`, `02-audit-notebook-template.md`, `03-annex-variable-template.md`, `04-codebook-and-decisions.md`.

## Principle

Never change structure and behavior in the same edit. The rebuild has three phases that must not overlap within a commit: refactor with output equivalence (phase 1), fixes with a diff report (phase 2), documentation from the final state (phase 3). Phase 0 builds the instrument that makes phases 1 and 2 verifiable.

## Input and reference status (salvage, Sept 2026)

The original machine was lost with all local data; the pipeline scripts survived in git. Inputs therefore come in two classes, and the class decides the acceptance criterion applied to every variable downstream:

- **Recovered originals** (pinned versions; placed in `_data/raw/<provider>/` under their original filenames, provenance from Drive/Claude Project metadata and internal version markers): MapBiomas deforestation export, IBGE biome list, PPCDAm list compilation, IBGE municipality IDs, census PIA extracts (2000/2010/2022), IPCA deflator. The RAIS extraction survives in BigQuery (`social_employment_1`, read-only).
- **Re-acquired sources** (fresh downloads, fresh provenance rows; version drift relative to the original run is possible and must be documented): MDS Bolsa Família and quotas, TSE, PAM/PPM, IBGE PIB, Pink Sheet prices, Atlas, and anything else not listed above.

The only recovered output is the final panel (`dataset_final_v6`), which is the end-to-end reference. No intermediate outputs survive, so there is no per-script reference.

## Phase 0. Setup (one session)

Goal: a repository in which every later change can be checked against a frozen reference.

1. Folder layout:

   ```
   _data/raw/          raw files, never modified, one subfolder per provider
   _data/interim/      outputs of scripts 01 to 05
   _data/final/        output of script 06 (the analysis panel)
   _data/reference/    frozen copies of every current .rds and final_dataset_6.csv
   _data/aux/          deflators, crosswalks, codebook.csv
   _audit/             one .qmd per source plus rendered output
   _scripts/           01 to 06 pipeline, 07 onward analysis
   _scripts/utils/     compare_outputs.R and shared functions
   _scripts/models/    one script per candidate causal design
   _scripts/robustness/  Python cross-checks only
   _scripts/sql/       SQL logs from the RAIS track
   output/             figures and tables, regenerable
   rebuild/            this specification and its companions
   DECISIONS.md
   README.md
   ```

1b. Naming: folders and files follow `docs/repo-conventions.md` (interim datasets source-first; the final panel is `panel_amz_2000_2024`).

2. Freeze the oracle. Copy the recovered final panel (`dataset_final_v6`) into `_data/reference/` under its recovered filename; it is read-only from now on. Build the salvage register in `_data/aux/provenance.csv`: one provenance row per recovered file (source, version markers, recovery location, Drive/Project timestamps, sha256) and one per re-acquired source as downloads happen; the README provenance table (phase 3) is rendered from it. Mirror `_data/` to Google Drive immediately; the mirror is refreshed at every phase acceptance from here on.

3. renv is managed at the repository root. Run `renv::snapshot()` after any approved package install and commit the lockfile.

4. Create `compare_outputs.R` in `_scripts/utils/`. It takes a new output and its reference, joins on `geocode` and `year`, and reports per variable: number of cells that differ beyond a tolerance, number of rows present in one file only, and the list of municipalities and years affected. Type mismatches (character vs numeric) are reported separately, not silently coerced. This script is the acceptance test for phases 1 and 2.

5. Create an empty `DECISIONS.md` and a `codebook.csv` with the current 85 variables of `final_dataset_6.csv` listed by name only. Labels and sources are filled in phase 3.

Acceptance: `compare_outputs.R` run on a reference file against itself reports zero differences. This is a self-test of the comparator, not of the data: identical inputs can only differ if the script is defective (join keys, duplicate handling, tolerance, type comparison), so a nonzero report here means the tool is fixed before anything else proceeds. Commit.

## Phase 1. Refactor only, one script per session

Goal: scripts that follow `01-script-template.md` and reproduce the reference outputs exactly.

Order: 01, 02, 03, 04, 05, 06. Run order is dependency order, so each script receives clean inputs when its turn comes.

For each script the agent:

1. States in plain text, before touching code, what will move where: which blocks become the pipeline, which blocks go to the audit notebook, which constants are extracted, which prose leaves the script entirely (source descriptions go to the annex in phase 3 and are parked in `_annex/parked/` until then). A monolithic script whose `rm(list = ls())`-delimited sessions cover independent sources may be split into separate scripts in the same plan, each with the template's single environment, chained through `_data/interim/`; the acceptance below then applies to the split scripts' combined outputs, and new intermediate files are permitted so long as every output the reference has is reproduced exactly.
2. Rewrites the script to the template. Exploration code (`glimpse`, `view`, `print(n = 100)`, plots, `install.packages`, `rm(list = ls())`) is removed from the pipeline. Checks whose result is currently reported in prose become `stopifnot()` assertions.
3. Moves the exploration code into `_audit/<source>.qmd` following `02-audit-notebook-template.md`, one section per question.
4. Runs the script and `compare_outputs.R` against the reference.

Acceptance, in two tiers, because only the final panel survives as a reference. Per script: the template is satisfied, every prose fact has become an assertion, the script runs end-to-end, and the audit notebook answers its standard questions; this acceptance is provisional. Phase 1 gate, after script 06: the full pipeline runs and the final panel is compared with `compare_outputs.R` against the recovered `dataset_final_v6`, read by column class. Variables built from recovered-original raw (and BigQuery RAIS) must match exactly; a difference there is triaged as before: (a) the refactor changed behavior, so the refactor is corrected; (b) the environment differs (package versions), a lockfile problem; (c) the reference value has no derivation from raw (manual edit or nondeterminism in the original run), in which case the rebuilt value is accepted only with audit-notebook evidence, a decision entry, and the discrepancy listed in the gate commit. Variables built from re-acquired sources may drift with source versions; drift is never silently accepted: the gate report quantifies it (variables, cells, municipalities, magnitude) and each drifting source gets a version decision entry. After the gate passes, the phase 1 final panel becomes the working reference, as phase 2 already assumes.

Known defects split by origin. Code-borne defects (price-index normalization, electoral off-by-one) live in the surviving scripts and are reproduced on purpose in phase 1, listed under `KNOWN DEFECTS`, and fixed in phase 2. Source-borne defects (MT quota outliers, 2004 zero-quota rows) are verified against the re-acquired source in the audit notebook before any fix: a defect absent upstream is closed as resolved-by-source with its entry; a defect still present proceeds to its phase 2 step.

Expect type problems. Several `bf_` and `gdp_` columns are stored as character in `final_dataset_6.csv`. Where the reference itself carries a coercion error, the agent reports it, the decision is logged with an ID, and the assertion for that column is written against the corrected type. This is the one permitted behavior change in phase 1, and it is logged as such.

Commit per script. Six commits.

## Phase 2. Fixes, one per session, one per commit

Goal: the open data tasks resolved, each with a traceable diff.

Every fix follows the same protocol:

1. A decision entry is written in `DECISIONS.md` before code changes, with status `open`.
2. The evidence for the fix, if any is needed, is produced in the relevant audit notebook section.
3. The script is changed. The change is expressed as a constant or a clearly delimited block, and an assertion encodes the expected state after the fix.
4. `compare_outputs.R` is run against the phase 1 output (not the original reference) and the report is attached to the commit message: which variables changed, how many cells, which municipalities.
5. The decision entry is closed with the commit hash.

Order of fixes. Value-level fixes on the existing sample first, most upstream first, so each diff reads as "same rows, these values moved". The row-level change comes last. This costs no rework: fixes are changes to scripts, not to rows, so when the extension lands and the pipeline reruns, every earlier fix applies to the new municipalities automatically. Running the extension last preserves what matters instead: each fix's diff is evaluated on the audited 502-municipality sample, where its expected footprint can be stated in advance, and the ~270 unaudited municipalities enter at one controlled point, immediately followed by the audit-notebook re-render that surfaces their source problems as new decision entries.

| Step | Task | Script | What changes |
|---|---|---|---|
| 2.1 | 1.4 quota source errors (14 MT municipalities, ratio > 3) | 02 | values of `bf_quota`, `bf_families_quota_ratio` |
| 2.2 | 1.2 BF 2020 endpoint | 02 | rows for 2020 in `bf_*` columns |
| 2.3 | 1.5 electoral timing (election year vs governing period) | 03 | values of `elec_*_gov` columns |
| 2.4 | 1.3 price index normalization (anchor 2000 = 1) | 06 | values of `price_index_*` |
| 2.5 | 1.6 municipalities created after the 2000 census (NA not zero, population estimates, new flags, recomputed ratios) | 01-03 | values and flags for the nine MT municipalities and their parents |
| 2.6 | 1.7 census-interpolated series: store raw anchors in `muni_baseline`, mark interpolated columns | 05 | new baseline columns, codebook flags |
| 2.7 | 1.8 NA versus zero audit (denominator counts, documented rules) | 02, 04 | values and new `_n` denominator columns |
| 2.8 | 1.9 unit pattern: rescale all shares to 0-1, record units in the codebook | 06 | values of percent-scaled columns |
| 2.9 | 1.1 Legal Amazon extension (`biome_amazon`, `legal_amazon` flags) | 01, 04, 05 | rows: new municipalities added, all `N_MUNIC_EXPECTED` assertions updated once |

Task 1.10 (character to numeric coercion) is handled inside phase 1 under the permitted type correction, logged per script.

Open definitional question raised in phase 1 (script 01, 2026-10-06): which level-1 classes compose the four MapBiomas transitions the pipeline sums. `area_forest` is primary plus secondary vegetation of every class (Forest, Non Forest Natural Formation, Water, Non vegetated area, Not Observed, and Farming under secondary vegetation), and `area_deforestation` includes suppression of non-forest natural formation. For the biome municipalities in 2020 the Forest class is 95 percent of `area_forest` and 98 percent of `area_deforestation`. Phase 1 reproduces the aggregate as inherited (D-2026-10-06-b; evidence `_audit/mapbiomas.qmd` Q3). **CONFIRM:** the author decides whether both aggregates are restricted to class 1 (Forest). If so, the change is a new phase 2 step after 2.9 (values of every environmental column, `defor_norm_*` included), with its own decision entry and the comparator report; if not, D-2026-10-06-b is confirmed and the annex states the definition.

After 2.9 every audit notebook is re-rendered on the extended sample. New municipalities may expose source problems the biome sample never had, and those become new decision entries.

## Phase 2R. Rename and storage audit

After phase 2 passes its equivalence checks, the naming convention is applied per `rebuild/naming-convention.md`: crosswalk file first, rename through the crosswalk, convention guard assertions at the end of script 06, downstream scripts and appendices updated by grep against the crosswalk. While the crosswalk is built, every stored derived variable is also classified against the governing rule for derived variables (`GUIDANCE.md`); task 2.2 in `rebuild/tasks.md` specifies the audit, and a variable that fails the rule moves to the analysis scripts with a decision entry. Equivalence for this phase is checked with `compare_outputs.R` mapping names through the crosswalk: values must not move, and the only permitted schema changes are the rename and the documented removals from task 2.2.

## Parallel track: RAIS reform

The RAIS re-extraction (`rebuild/upstream-reform-rais.md`, tasks A to C) runs as a parallel track once its section 0 decisions are confirmed. It merges into the pipeline through its regression rule: the rebuilt extraction must reproduce the current `_data/data_employment_rais` exactly before any new variable enters script 02. New RAIS variables land before phase 3 so the annex documents them.

After phase 2, the rename and the RAIS merge, the analysis scripts (`08-data-analysis-2WFE-CS.R` at minimum) are rerun so the new stratum ATTs are known before any annex text is written.

## Phase 3. Documentation from the final state

Goal: the annex, codebook and README describe the dataset that exists.

1. `codebook.csv` is completed for every variable in the final panel. The annex codebook table is rendered from this file.
2. The annex is written per variable following `03-annex-variable-template.md`, in the section order: sample, outcome, treatment, moderators, covariates, codebook, provenance. The parked prose from phase 1 is the raw material, not the text.
3. `README.md` is written: layout, run order, provenance table, how to reproduce.

The one document written during rather than after is `DECISIONS.md`.

## Control rules for the agent

- One phase per commit. One script (phase 1) or one fix (phase 2) per session.
- Every session opens with a plan in plain text: what will change, what the acceptance check is. Work starts after the plan is approved.
- Every session closes with the `compare_outputs.R` report and the commit hash.
- A decision without an ID in the script header, in `DECISIONS.md` and, from phase 3, in the annex, is incomplete.
- Raw files under `_data/raw/` and `_data/reference/` are never written to.
- Every phase acceptance includes refreshing the Google Drive mirror of `_data/`. No phase closes with the mirror stale.
- The agent does not proceed to the next script or fix on its own.
- Decisions marked CONFIRM in the companion files, and section 0 of the RAIS brief, are resolved with the author before implementation. The agent never resolves one silently.
- Any variable added during phase 2 or the RAIS track lands in the pipeline or in an analysis script according to the governing rule for derived variables in `GUIDANCE.md` (Methodological standards).
