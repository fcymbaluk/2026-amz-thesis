# Codebook, decision log, and README

Three files that keep the pipeline, the annex and the history from drifting apart. `DECISIONS.md` supersedes the earlier `data/data-decisions.md` one-line log for this chapter; `docs/decisions/` at the repository root keeps dissertation-level choices outside the pipeline. The README specified here is `ch4-causal-analysis/README.md`, the chapter folder's own. The codebook is read by both the pipeline and the annex, so the annex cannot describe a variable that does not exist. The decision log is the append-only history that a replicator does not need and future-you does. The README is the current state.

## 1. `_data/aux/codebook.csv`

One row per variable in the final panel, plus one per column of the municipality store `panel_amz_baseline` that is not in the panel (the census anchors from 2026-10-26, the baseline summaries, the pressure scores and the alternative treatment flags from 2026-10-30; their `script` is 05 and their `construction` names the store), plus one per variable the storage audit moved to the analysis scripts (from 2026-10-30, so the verdict is on record). The `dataset` column tells the three apart. Variable names are the `new_name` of `_data/aux/crosswalk.csv` (phase 2R, D-2026-10-30-a). Read by script 06, which asserts that the panel's columns and the codebook's `panel` rows coincide and that every column holds its declared `type`, and by the annex to render section 6.

Columns:

| Column | Content |
|---|---|
| `variable` | Name as it appears in the final dataset |
| `label` | Short human label for tables and figures |
| `role` | One of: `id`, `sample`, `outcome`, `treatment`, `moderator`, `covariate`, `auxiliary` |
| `type` | R type the CSV must round-trip to: `integer`, `double`, `character`, `logical` |
| `unit` | One value per row, filled for every row at phase 2, step 2.8 (D-2026-10-28-k): `text`, `year`, `0/1`, `TRUE/FALSE`, `count`, `km2`, `head per km2`, `share (0-1)`, `rate (0-1)`, `ratio`, `BRL (nominal)`, `BRL 2024` and their per-capita and log forms, `index (...)` with its base stated, `SD (...)` with its reference population stated, `log km2`. A column rescaled from a source on another scale records that scale in parentheses (naming-convention.md §3.4) |
| `source` | Provider short name as used in the provenance table |
| `years` | Years with non-missing values, e.g. `2004-2019` |
| `construction` | One line, or `see annex §x.y` |
| `script` | Script that creates the variable |
| `interpolated` | `TRUE` when the column carries, or divides by, a series interpolated between census anchors; `FALSE` otherwise (added at phase 2, step 2.6, D-2026-10-26-a) |
| `anchor_years` | The census years of the anchors when `interpolated` is `TRUE`, e.g. `2000/2010/2022`; empty otherwise |
| `missing_rule` | One sentence: when the variable is NA and what 0 means, or that neither occurs, citing the decision that set the rule (added at phase 2, step 2.7, D-2026-10-28-f) |
| `dataset` | Where the variable lives: `panel` (the final panel, whether or not the store also carries it), `store` (the municipality store only) or `analysis` (left the pipeline at the storage audit; built in the analysis scripts). Added at phase 2R, D-2026-10-30-f |
| `storage_verdict` | The task 2.2 classification against the governing rule for derived variables: `key`, `stored: source value`, `stored: <condition>` naming the storage condition met, `store only: <reason>` or `analysis: <reason>`, with the decision ID. Added at phase 2R, D-2026-10-30-f |

Example rows:

```
variable,label,role,type,unit,source,years,construction,script
geocode,IBGE municipality code,id,character,,IBGE,2000-2020,7-digit code,01
year,Year,id,integer,,,2000-2020,,04
enf_ppcdam_listed_dum,PPCDAm priority list,treatment,integer,0/1,MMA ordinances,2008-2020,1 if listed in year t,01
env_defor_area_z_bl0007,Deforestation z-score (2000-07 base),outcome,double,SD,MapBiomas c9,2000-2020,see annex §2.2,06
sop_bf_fam_quota_ratio,BF coverage ratio,moderator,double,ratio,MDS,2004-2019,families in June / municipal quota,02
ecn_gdp_pc_brl24_log,Log GDP per capita (2024 BRL),covariate,double,log BRL,IBGE PIB,2002-2020,log(gdp_brl_2024 / population),03
mkt_price_crop_idx,Crop price index,covariate,double,index (exposure x relative price),World Bank Pink Sheet + PAM,2000-2020,see annex §5.5,03
smp_atrisk_narrow_dum,At-risk sample flag,sample,logical,TRUE/FALSE,,2000-2020,see annex §1.4,05
```

The `type` column is the contract that fixed the phase 1 problem of `bf_` and `gdp_` columns arriving as character in the CSV; it is filled for every row since 2026-10-30 (`typeof` of the column: `integer`, `double`, `character`, `logical`) and script 06 asserts that every panel column matches it before writing.

## 2. `DECISIONS.md`

Append-only. Never edited retroactively except to close an entry. One entry per decision that could have been made differently. Written during the work, not after.

Entry format:

```markdown
## D-2026-02-20  Moderator baseline window fixed at 2006-2007

Status: closed (commit a3f9c21)
Scope: script 05, script 08-data-analysis-2WFE-CS.R, annex §4.1
Context: wider windows were tested (2006-2010, 2006-2012) to gain
         precision on the moderator.
Reason: a DiD of listing on coverage returns -0.13 to -0.18 with the
        wider windows, so coverage after 2007 is post-treatment.
Evidence: _audit/bf.qmd, Q5
Affects: bf_families_quota_ratio baseline, stratum split, all 08-data-analysis-2WFE-CS.R rows
Supersedes: none
```

Rules:

- The ID is `D-YYYY-MM-DD` with a letter suffix when several decisions are logged on the same day.
- `Status` is `open` when the entry is created and `closed (commit ...)` when the implementing commit lands. Phase 2 fixes are created as `open` before code changes.
- `Supersedes` links to the earlier entry when a decision is reversed. The earlier entry is not deleted. Its status becomes `superseded by D-...`.
- Every decision ID cited in a script header or an annex paragraph must exist here.

Entries to create in phase 0 from what is already known, so the log starts with the history rather than with the rebuild:

- Reference month June, February for robustness
- BF panel truncated at 2019
- Deflation to 2024 BRL, IPCA
- Wage brackets anchored to R$1,412 (2024 minimum wage)
- PIA as primary denominator, PEA as robustness
- PIA interpolated from 2000, 2010, 2022 Census anchors
- Moderator window 2006-2007
- Outcome `defor_norm_ref0007`, at-risk narrow sample
- Median split at 0.96 for the stratified C&S
- `pov_2000` reserved for falsification
- Colniza dropped in adjusted specifications (missing pre-2010 GDP)
- Legal Amazon extension via `biome_amazon` flag (open, Task 1.1)
- Quota source correction (open, Task 1.4)
- Electoral governing-period mapping (open, Task 1.5)
- Price index anchored 2000 = 1 (open, Task 1.3)
- BF 2020 endpoint (open, Task 1.2)

## 3. `README.md`

Current state only. No history.

Sections:

1. **What this repository builds.** Two sentences.
2. **Layout.** The folder tree from `00-rebuild-phases.md`.
3. **Run order.** Scripts 01 to 06 with one line each, then analysis scripts 07 onward.
4. **Reproduce.** `renv::restore()`, then `source()` each script in order, then `compare_outputs.R` against `_data/reference/` to confirm the build.
5. **Provenance table.** One row per raw file, rendered from `_data/aux/provenance.csv` (the salvage register phase 0 creates and stage 5 appends to, one row per download at download time; it carries the columns below plus `size_bytes`, `sha256`, `recovery_location`, `recovery_timestamp`, `class` and `script_path_expected`).

Provenance table columns:

| File | Provider | Dataset | Version / release | Downloaded | Size | Notes |
|---|---|---|---|---|---|---|
| `raw/mapbiomas/deforestation-municipalities.xlsx` | MapBiomas | Deforestation and Secondary Vegetation statistics | Collection 9, Aug 2024 | 2025-xx-xx | 14.2 MB | Sheet 2 |
| `raw/mds_bolsa_familia/bf-2004.txt` ... `bf-2026.txt` | MDS | Bolsa Família municipal monthly | March 2026 release | 2026-03-10 | | one file per year |
| `raw/ibge/biome-predominante.xlsx` | IBGE | Bioma predominante por município | June 2024 | | | |
| `raw/ppcdam/ppcdam_list.xlsx` | Own compilation from MMA ordinances | | | | | Decreto 6.321/2007, Portarias MMA 28, 102, ... |

The annex renders section 7 from this table.
