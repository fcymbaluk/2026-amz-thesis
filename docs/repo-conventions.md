---
role: repo-conventions
updated: 2026-09-26
---

# Repository conventions: folders and file names

One rule set for the whole repository. `CLAUDE.md` points here; chapter-specific grammars (variable names in `ch4-causal-analysis/rebuild/naming-convention.md`) build on top of this, never against it.

## Folders: three classes

1. **Workflow sequences** are numbered with a single digit and a hyphen: `1-protocol`, `2-search`, ... Numbers are reserved for folders that represent a fixed, ordered workflow (the ch3 PRISMA stages). A sequence that could grow past 9 or be reordered is not numbered, because renumbering breaks every path reference at once.
2. **Infrastructure and taxonomy folders** carry the underscore prefix and plain nouns: `_data`, `_audit`, `_scripts`. Their subfolders (`raw`, `interim`, `final`, `reference`, `aux`; `utils`, `models`, `robustness`, `sql`) are unnumbered nouns, because they are a taxonomy, not a sequence.
3. **Content folders** are plain nouns: `docs/`, `manuscript/`, `output/`, `rebuild/`, `notes/`.

Chapter folders carry their chapter number (`ch1-theoretical` ... `ch4-causal-analysis`).

## Files

- **Sequenced files** use zero-padded two-digit prefixes: pipeline scripts `01-` to `06-`, analysis scripts `07-` onward, the rebuild docs `00-` to `04-`. Folders use single digits (small fixed sets); files use two digits (sets that can grow).
- **Raw data files keep the provider's original filename, always.** The source is carried by the provider subfolder (`_data/raw/mds_bolsa_familia/`), and the original name is provenance: it must match the provenance table.
- **Interim datasets are source-first**, snake_case: `<source>_<content>[_<scope>][_<years>].rds`. The source token comes from the audit-notebook vocabulary, so file, notebook and provenance table share one dictionary: `mapbiomas`, `ibge_biome`, `ppcdam`, `rais`, `bf`, `ibge_pib`, `tse`, `pam_ppm`, `prices`, `census`, `atlas`. Examples: `bf_panel.rds`, `mapbiomas_deforestation_municipal.rds`, `rais_wage_distribution_2006_2024.rds`.
- **Final datasets are content-first**, because they have no single source: `panel_<scope>_<years>` — the analysis panel is `panel_amz_2000_2024.rds` / `.csv`. Script-numbered names (`final_dataset_6.csv`) are retired.
- **Auxiliary files are content-first**: `deflator_ipca_2024.csv`, `codebook.csv`, `crosswalk.csv`.
- **Output files** use Quarto's cross-reference prefixes so file names and `@fig-`/`@tbl-` labels align: `fig-<slug>.png`, `tab-<slug>.csv`.
- **Documents** follow their local convention: `GUIDANCE.md` per chapter, numbered specs in `rebuild/`, dated memos in `docs/decisions/` (`YYYY-MM-DD-slug.md`), audit notebooks named by source token (`bf.qmd`).

## Where scripts live (Chapter 4)

Scripts have no "old" folder: the pre-rebuild scripts are moved into `_scripts/` under their current names and transformed in place by phase 1; prior versions live in git history (the `pre-reorg` tag and the phase commits), and what phase 1 needs frozen is the outputs, held in `_data/reference/`. New scripts route by kind: a new pipeline stage takes the next number (or a documented split); analysis is `07-` onward; candidate designs in `_scripts/models/`; Python cross-checks in `_scripts/robustness/`; functions used by more than one script in `_scripts/utils/` (definitions only, no side effects at source time; `compare_outputs.R` lives here and is never sourced by pipeline scripts); SQL logs from the RAIS track in `_scripts/sql/`; throwaway code in the gitignored `scratch/` at the repository root, never in `_scripts/`.

## Change discipline

Renames are migrations: the rename and every reference to the old path (guidance files, `CLAUDE.md`, `README.md`, `.claude/settings.json`, `.gitignore`, scripts) change in one commit, via `git mv` so history follows. A convention change that leaves stale references anywhere is worse than either convention alone.
