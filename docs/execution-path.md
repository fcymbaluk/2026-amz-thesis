---
role: execution-path
current_stage: "0 — machine setup"
updated: 2026-10-01
---

# Execution path — from blank machine to defended empirical chapter

Master tracker, above the per-chapter checklists: this file orders the stages; the chapter GUIDANCE checklists and `rebuild/` specs hold the detail. Stages 0-4 are strictly sequential; stage 5 runs parallel to 6; stage 9 is a parallel track; the writing tracks at the end interleave throughout. Update `current_stage` and tick items as they close; each stage names its proof of completion.

## Stage 0 — Machine setup (author, \~half a day)

- [x] Install: Git, R + RStudio/Positron, Quarto + TinyTeX, Claude Code, gcloud CLI, Zotero + Better BibTeX; sign into Google Drive.
- [x] Clone `fcymbaluk/2026-amz-thesis`.
- [x] Done when `quarto check` and `Rscript --version` run.

## Stage 1 — Salvage consolidation (author, before touching the repo)

- [x] Download the five Claude Project files (`dataset_final_v6`, `ppcdam_lists`, `ibge_bioma`, `ibge_munic_id`, `mapbiomas_deforestation`) into a staging folder OUTSIDE the repo.
- [x] Download the full Drive `2025_AMZ/2. Database` folder into the same staging folder.
- [x] For files present in both sources (`mapbiomas_deforestation`, `ibge_munic_id`): keep the version the scripts actually read; note the duplicate in the salvage register at phase 0.
- [x] Open every file once to verify integrity.
- [x] Create the Drive mirror destination folder (in Google Drive, outside the repo; the repo is never placed inside a Drive-synced folder). 2026-AMZ/data-mirror/
- [x] Done when every salvaged file is local and openable.

## Stage 2 — Reorg branch (author + Claude Code, 1-2 sessions)

- [x] Commit and push anything outstanding; `git tag pre-reorg && git push --tags`.
- [ ] Branch `reorg-structure`.
- [ ] Unzip the consolidated scaffold at the repo root; commit.
- [ ] `git rm` the empty chapter qmds, `board-tasks.md`, and superseded appendix qmds (empty ones deleted; any with prose go to `ch4-causal-analysis/_annex/parked/`).
- [ ] `git mv`: root `_scripts/` → `ch4-causal-analysis/_scripts/`; `references.bib` + both CSLs → `manuscript/`.
- [ ] Merge the old `_quarto.yml` formatting (scrreprt, APA CSL, appendix mechanism) into `manuscript/_quarto.yml` (produced in chat from both files).
- [ ] Merge `.gitignore`s, with the `!**/_data/aux/` exception so codebook and crosswalk stay versioned.
- [ ] Commit the moves WITHOUT editing script contents (in-script path fixes belong to phase 1).
- [ ] Review the branch diff; merge to `main`; delete the branch.
- [ ] `renv::init()`, `renv::snapshot()`, commit the lockfile.
- [ ] Open Claude Code once and confirm the settings.json deny rules fire.
- [ ] Done when `main` holds the new tree and the `pre-reorg` tag is on GitHub.

## Stage 3 — Author decision gate (author, one sitting)

- [ ] Resolve the CONFIRM items in `ch4-causal-analysis/rebuild/naming-convention.md`.
- [ ] Resolve the section 0 decisions in `ch4-causal-analysis/rebuild/upstream-reform-rais.md`.
- [ ] Set the BigQuery budget alert on `amz-data-dissertation`.
- [ ] Optionally settle the Chapter 3 protocol TODOs.
- [ ] Done when ch4's checklist item 1 is ticked. Nothing in the pipeline starts before this stage closes.

## Stage 4 — Phase 0 (Claude Code, one session, plan mode)

- [ ] Salvaged originals placed in `_data/raw/<provider>/` under original filenames; salvage register written.
- [ ] `dataset_final_v6` frozen in `_data/reference/` under its recovered name.
- [ ] `_data/` mirrored to Drive.
- [ ] `compare_outputs.R` built and self-tested (zero diff on a file against itself).
- [ ] `DECISIONS.md` seeded with the known-history entries; `codebook.csv` skeleton from the recovered panel's columns.
- [ ] Commit. Done per `rebuild/00-rebuild-phases.md` phase 0 acceptance.

## Stage 5 — Re-acquisition (author; parallel with stage 6)

- [ ] MDS Bolsa Família monthly files through 2024 + municipal quotas (front-load: feeds script 02 and task 1.2).
- [ ] TSE electoral data (front-load: feeds script 03).
- [ ] PAM/PPM; IBGE PIB dos Municípios incl. value added by activity (task 4.1); Pink Sheet prices; Atlas Brasil.
- [ ] One provenance row per download, at download time.
- [ ] Done when every script's inputs exist before its phase 1 session.

## Stage 6 — Phase 1: refactor (Claude Code, six sessions + gate)

- [ ] Script 01 refactored to template; provisional acceptance (assertions + audit notebook); commit.
- [ ] Script 02 — same.
- [ ] Script 03 — same.
- [ ] Script 04 — same.
- [ ] Script 05 — same.
- [ ] Script 06 — same.
- [ ] Phase 1 gate: full pipeline run; compare against the recovered panel (exact match for recovered-input columns; drift quantified and logged for re-acquired sources); phase 1 panel becomes the working reference; Drive mirror refreshed.

## Stage 7 — Phase 2: fixes (Claude Code, nine sessions, order 2.1-2.9)

- [ ] 2.1 quota source (verify upstream first) · \[ \] 2.2 BF endpoint/extension · \[ \] 2.3 electoral timing · \[ \] 2.4 price normalization · \[ \] 2.5 post-2000 municipalities · \[ \] 2.6 census anchors · \[ \] 2.7 NA-vs-zero · \[ \] 2.8 units · \[ \] 2.9 Legal Amazon extension.
- [ ] Each: decision entry opened → evidence in notebook → fix as constant/delimited block + assertion → compare vs phase 1 output in the commit → entry closed with hash.
- [ ] After 2.9: all audit notebooks re-rendered on the extended sample; new municipalities' source problems become new entries. Mirror refreshed.

## Stage 8 — Phase 2R: rename + storage audit (1-2 sessions)

- [ ] Crosswalk built with task 2.2 storage-audit verdicts in the codebook.
- [ ] Rename applied through the crosswalk; guard assertions in script 06; downstream grep.
- [ ] Equivalence through the name mapping; only rename + documented removals as schema changes.

## Stage 9 — RAIS track (parallel, any time after stage 3)

- [ ] `gcloud auth login` + application-default; budget alert confirmed.
- [ ] Task 3.A → \[ \] 3.B → \[ \] 3.C, under the brief's rules and CLAUDE.md's BigQuery section.
- [ ] Regression rule passed before any new variable enters script 02; merged before stage 10 closes.

## Stage 10 — Rerun + Phase 3: documentation

- [ ] Analysis scripts rerun (08b at minimum); new stratum ATTs known.
- [ ] The single annex written from the final state per `rebuild/03-annex-variable-template.md` (H3 section-order amendment decided here).
- [ ] Codebook §6 and provenance §7 rendered; chapter README written.
- [ ] Done when `panel_amz_2000_2024` exists and is fully documented.

## Stage 11 — Descriptive stage

- [ ] Tasks 4.1-4.3 (land-based dependence; rural/urban; trajectories), then the broader descriptive program; outputs under the `fig-`/`tab-` convention.

## Stage 12 — Causal stage

- [ ] Candidate designs as separate scripts in `_scripts/models/`, per the methodological standards; trade-offs presented across designs.
- [ ] Task 5.1: persistence framed as the sharper test of mechanism A; the disconfirmation condition stated before estimation.
- [ ] Results into the chapter.

## Parallel writing tracks (interleave with pipeline sessions throughout)

- [ ] **Ch1 (unblocked):** gap-section cluster (checklist items 1/3/4, per the revision instructions), then the epistemological section (item 2), in `manuscript/ch1-theoretical.qmd`.
- [ ] **Ch3 (unblocked):** protocol decisions, preregistration decision, protocol freeze (work-plan items 1-3); searches once frozen; screening interleaves with stages 6-8.
- [ ] **Ch2 (unblocked):** Zotero-highlights workflow decided (no ATLAS.ti); codebook (tag vocabulary) → corpus → coding in Zotero → export → matrix → outline → sections, per its work plan.
- [ ] **Infra, once:** Better BibTeX auto-export re-pointed at `manuscript/references.bib`; `quarto preview manuscript/` verified.

## Standing cadence

Plan before work. One script or one fix per session. Commits proposed, never chained. Frontmatter updated and durable items promoted at session end. The Drive mirror is never stale at a phase close.