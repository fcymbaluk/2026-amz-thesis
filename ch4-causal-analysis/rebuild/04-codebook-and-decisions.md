# Codebook, decision log, and README

Three files that keep the pipeline, the annex and the history from drifting apart. `DECISIONS.md` supersedes the earlier `data/data-decisions.md` one-line log for this chapter; `docs/decisions/` at the repository root keeps dissertation-level choices outside the pipeline. The README specified here is `ch4-causal-analysis/README.md`, the chapter folder's own. The codebook is read by both the pipeline and the annex, so the annex cannot describe a variable that does not exist. The decision log is the append-only history that a replicator does not need and future-you does. The README is the current state.

## 1. `_data/aux/codebook.csv`

One row per variable in the final panel. Read by script 06 to set labels and by the annex to render section 6. Adding a variable to the panel without a codebook row fails an assertion in script 06.

Columns:

| Column | Content |
|---|---|
| `variable` | Name as it appears in the final dataset |
| `label` | Short human label for tables and figures |
| `role` | One of: `id`, `sample`, `outcome`, `treatment`, `moderator`, `covariate`, `auxiliary` |
| `type` | R type the CSV must round-trip to: `integer`, `double`, `character`, `logical` |
| `unit` | `km2`, `ratio`, `BRL 2024`, `SD`, `count`, `share`, `index (2000 = 1)`, ... |
| `source` | Provider short name as used in the provenance table |
| `years` | Years with non-missing values, e.g. `2004-2019` |
| `construction` | One line, or `see annex §x.y` |
| `script` | Script that creates the variable |

Example rows:

```
variable,label,role,type,unit,source,years,construction,script
geocode,IBGE municipality code,id,character,,IBGE,2000-2020,7-digit code,01
year,Year,id,integer,,,2000-2020,,04
ppcdam_list,PPCDAm priority list,treatment,integer,0/1,MMA ordinances,2008-2020,1 if listed in year t,01
defor_norm_ref0007,Deforestation z-score (2000-07 base),outcome,double,SD,MapBiomas c9,2000-2020,see annex §2.2,06
bf_families_quota_ratio,BF coverage ratio,moderator,double,ratio,MDS,2004-2019,families in June / municipal quota,02
gdp_per_capita_2024_log,Log GDP per capita (2024 BRL),covariate,double,log BRL,IBGE PIB,2002-2020,log(gdp_brl_2024 / population),03
price_index_crop,Crop price index,covariate,double,index (2000 = 1),World Bank Pink Sheet + PAM,2000-2020,see annex §5.5,06
at_risk_narrow,At-risk sample flag,sample,logical,0/1,,2000-2020,see annex §1.4,05
```

The `type` column is the contract that fixes the current problem of `bf_` and `gdp_` columns arriving as character in the CSV. Script 06 asserts that every column matches its declared type before writing.

## 2. `DECISIONS.md`

Append-only. Never edited retroactively except to close an entry. One entry per decision that could have been made differently. Written during the work, not after.

Entry format:

```markdown
## D-2026-02-20  Moderator baseline window fixed at 2006-2007

Status: closed (commit a3f9c21)
Scope: script 05, script 08b, annex §4.1
Context: wider windows were tested (2006-2010, 2006-2012) to gain
         precision on the moderator.
Reason: a DiD of listing on coverage returns -0.13 to -0.18 with the
        wider windows, so coverage after 2007 is post-treatment.
Evidence: _audit/bf.qmd, Q5
Affects: bf_families_quota_ratio baseline, stratum split, all 08b rows
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
5. **Provenance table.** One row per raw file.

Provenance table columns:

| File | Provider | Dataset | Version / release | Downloaded | Size | Notes |
|---|---|---|---|---|---|---|
| `raw/mapbiomas/deforestation-municipalities.xlsx` | MapBiomas | Deforestation and Secondary Vegetation statistics | Collection 9, Aug 2024 | 2025-xx-xx | 14.2 MB | Sheet 2 |
| `raw/mds_bolsa_familia/bf-2004.txt` ... `bf-2026.txt` | MDS | Bolsa Família municipal monthly | March 2026 release | 2026-03-10 | | one file per year |
| `raw/ibge/biome-predominante.xlsx` | IBGE | Bioma predominante por município | June 2024 | | | |
| `raw/ppcdam/ppcdam_list.xlsx` | Own compilation from MMA ordinances | | | | | Decreto 6.321/2007, Portarias MMA 28, 102, ... |

The annex renders section 7 from this table.
