# Variable naming convention

**Project:** Dissertation Chapter 4 (causal inference), Amazon eco-social policy panel **Applies to:** `final_dataset_*.csv` (municipal panel, 2000–2020) and `outcome_atlas_poverty_geocoded.csv` (Atlas Brasil, census vintages) **Status:** proposal, version 1 (2026-09-24). Items marked **CONFIRM** require a decision from Fernando before implementation.

## 1. Purpose

Variable names must make it possible to tell, from the name alone, what domain a variable belongs to, what concept it measures, over which denominator, in which unit, and with which transforms and timing applied. Names describe **what a variable measures**, never its role in a given model. Roles (treatment, moderator, outcome, control) change across specifications and belong in the codebook and the analysis scripts.

## 2. Grammar

```         
domain_concept[_qualifier...][_denominator]_unit[_transform...][_timing]
```

- `domain`, `concept` and `unit` are required.
- `qualifier`, `denominator`, `transform` and `timing` are optional.
- The slot order never changes. Do not add slots. Do not pad short names to reach a fixed number of slots.
- Panel keys carry no prefix: `geocode`, `year`, `municipality`, `state`, `uf`.

Examples:

| Name | Parsed as |
|------------------------------------|------------------------------------|
| `env_defor_area_km2` | env / deforestation / area / km2 |
| `sp_bf_transf_pc_brl24` | sop / BF transfers / per capita / real BRL 2024 |
| `lab_emp_formal_lowwage_pia_rate` | lab / employment / formal, low-wage bracket / over PIA / rate |
| `mkt_price_crop_idx_z_lag1` | mkt / crop price / index / standardized / lagged one year |
| `env_defor_area_z_bl0007` | env / deforestation / area / z-score / relative to 2000–2007 baseline |

## 3. Controlled vocabularies

Only tokens listed here may be used. To add a token, add it to this file and to the codebook dictionary first.

### 3.1 Domain prefixes

| Prefix | Content | Place in the argument |
|------------------------|------------------------|------------------------|
| `env_` | Forest stock, deforestation, forest-based pressure measures | Environmental outcomes |
| `enf_` | PPCDAm priority-list status and its coding variants | Environmental policy (enforcement) |
| `sop_` | Bolsa Família coverage and transfers | Social policy provision |
| `soc_` | Poverty, extreme poverty, vulnerability, inequality (Atlas Brasil) | Social conditions |
| `lab_` | RAIS formal employment, wage brackets, PEA, informality | Labor market (Mechanism 4) |
| `dem_` | Population | Demography |
| `eco_` | GDP | Economy |
| `agr_` | Crop area, cattle | Land-use drivers |
| `mkt_` | Commodity price indices | Exogenous market shocks |
| `pol_` | Electoral variables (mayoral) | Politics (Mechanism 3) |
| `geo_` | Municipal area and other time-invariant geography | Fixed attributes |
| `smp_` | Sample-definition flags and composites used only for sample construction | Analytical design artifacts |

Design rationale: the theory distinguishes policy instruments (`enf_`, `sop_`) from the conditions and outcomes they act on (`env_`, `soc_`, `lab_`). The prefixes keep that distinction visible in every formula. `smp_` is the one prefix that names a role rather than a measurement, because those flags are not data and must be visibly separate from it.

### 3.2 Concept and qualifier tokens (abbreviation dictionary)

| Token | Meaning |
|------------------------------------|------------------------------------|
| `defor` | deforestation (MapBiomas, primary + secondary vegetation suppression) |
| `forest` | forest stock / forest area |
| `exposure` | forest exposure measure |
| `pressure` | pressure composite |
| `ppcdam` | PPCDAm priority municipality list |
| `listed` | on the PPCDAm list in that year |
| `ever` | ever listed within the panel window |
| `bf` | Bolsa Família |
| `fam` | families |
| `quota` | administrative BF quota |
| `transf` | transfers (monetary) |
| `emp` | employment |
| `formal` | formal sector (RAIS) |
| `informal` | informality |
| `agri` | agricultural sector |
| `lowwage`, `midwage`, `highwage` | wage brackets anchored to the 2024 real minimum wage (R\$1,412) |
| `pea` | população economicamente ativa (IBGE) |
| `pia` | população em idade ativa (IBGE, interpolated 2000/2010/2022 anchors) |
| `pop` | population |
| `gdp` | gross domestic product |
| `soy`, `corn`, `rice`, `cotton` | crops (PAM) |
| `planted` | planted area |
| `cattle` | cattle herd (PPM) |
| `price` | commodity price (World Bank Pink Sheet, MUV-deflated) |
| `mayor` | mayoral election |
| `enc` | effective number of candidates (**CONFIRM** that `elec_enc` means this) |
| `margin` | margin of victory |
| `winner` | winning candidate |
| `cand` | candidates |
| `competitive`, `uncontested` | election-type flags |
| `pov`, `extpov` | poverty, extreme poverty (Atlas) |
| `vuln` | vulnerability to poverty (Atlas) |
| `gini` | Gini index |
| `r1040` | income ratio, richest 10% to poorest 40% |
| `inc` | income |
| `atrisk` | at-risk sample flag |
| `avail` | data availability flag |

Rules for tokens: lowercase ASCII, no accents, no digits inside a concept token (digits are reserved for units, baselines and vintages). Official acronyms (`bf`, `ppcdam`, `pia`, `pea`) stay as they are because they trace directly to IBGE and MDS sources. All other roots are English. Never let two tokens mean the same thing.

### 3.3 Denominator qualifiers

Placed immediately before the unit.

| Token      | Denominator                    |
|------------|--------------------------------|
| `_pc`      | per capita (population)        |
| `_pia`     | working-age population         |
| `_pea`     | economically active population |
| `_fam`     | per beneficiary family         |
| `_quota`   | per BF quota slot              |
| `_forest`  | forest area                    |
| `_munic`   | municipal area                 |
| `_per_km2` | per square kilometre           |

### 3.4 Unit and measure suffixes

| Suffix | Meaning | Range |
|------------------------|------------------------|------------------------|
| `_n` | count | integer |
| `_km2` | area |  |
| `_brl` | nominal BRL |  |
| `_brl24` | real BRL at 2024 prices |  |
| `_brl10` | real BRL at 2010 prices (Atlas income) |  |
| `_share` | part of a whole in the same unit | 0–1 |
| `_rate` | incidence over a named at-risk population | 0–1 |
| `_ratio` | quotient that may exceed 1 | ≥ 0 |
| `_idx` | index or composite score |  |
| `_d` | binary dummy | 0/1 |
| `_cat` | categorical (character or factor) |  |
| `_id` | identifier |  |
| `_gyear` | first-treatment (cohort) year, `Inf` or `0` for never-treated per estimator convention | year |

Every 0-1 variable must be `_share` or `_rate`. There is no percent suffix: sources delivered in percent are rescaled to 0-1 at construction, and the codebook `unit` column records the source's original scale.

### 3.5 Transforms

Appended after the unit, in the order they were applied.

| Suffix | Meaning |
|------------------------------------|------------------------------------|
| `_log` | natural log |
| `_z` | standardized; the codebook states the reference population (pooled, within-municipality, or baseline) |

### 3.6 Timing

Appended last.

| Suffix | Meaning |
|------------------------------------|------------------------------------|
| `_lag1`, `_lag2` | lagged k years |
| `_bl0607` | baseline value, window 2006–2007 (replace digits as needed) |
| `_c1991`, `_c2000`, `_c2010` | census vintage (Atlas) |
| `_term` | electoral values aligned to the governing term rather than the election year |

Fixed order when several apply: unit → transform(s) → timing. So `mkt_price_crop_idx_z_lag1`, not `mkt_price_crop_idx_lag1_z`.

## 4. General rules

1.  Names are lowercase snake_case, ASCII only, at most 40 characters. If a name exceeds 40 characters, shorten a token via the dictionary rather than dropping a slot.
2.  Do not encode the data source in the name. The source lives in the codebook. Exception: when two sources measure the same concept, append a source token after the unit (for example `env_defor_area_km2_mb` versus `env_defor_area_km2_prodes`).
3.  Do not encode analytical roles (`treat`, `mod`, `outcome`, `ctrl`) in names.
4.  A variable with a denominator must name it. `emp_rate` is not acceptable; `emp_formal_pia_rate` is.
5.  "avg", "total" and "mean" are not tokens. "avg" almost always hides a denominator; name the denominator instead. "total" is the default and needs no token.
6.  Derived variables keep the full name of their parent and add suffixes. `eco_gdp_pc_brl24_log` is derived from `eco_gdp_pc_brl24`.
7.  Baseline covariates used in analysis scripts follow the same grammar with `_blYYYY` instead of an ad hoc `0`. Replace `gdp0`, `informal0`, `agri0` with `eco_gdp_pc_brl24_bl0607`, `lab_informal_share_bl0607`, `lab_emp_formal_agri_share_bl0607` (**CONFIRM** the actual baseline window and the exact parent variable of each).
8.  One name, one meaning, across all datasets and scripts. The crosswalk (section 6) is the single source of truth.

## 5. Crosswalk: current → proposed

### 5.1 Panel (`final_dataset_6.csv`)

| Current | Proposed | Note |
|------------------------|------------------------|------------------------|
| `geocode` | `geocode` | key |
| `municipality` | `municipality` | key |
| `state` | `state` | key |
| `state_abbreviation` | `uf` | key |
| `year` | `year` | key |
| `ppcdam_list` | `enf_ppcdam_listed_d` |  |
| `ppcdam_main` | `enf_ppcdam_listed_main_d` | **CONFIRM** coding. If it stores a cohort year, use `enf_ppcdam_main_gyear` |
| `ppcdam_robust` | `enf_ppcdam_listed_robust_d` | **CONFIRM** as above |
| `ppcdam_ever_full` | `enf_ppcdam_ever_d` | **CONFIRM** as above |
| `forest_area_km2` | `env_forest_area_km2` |  |
| `deforestation_area_km2` | `env_defor_area_km2` |  |
| `deforestation_forest_rate` | `env_defor_forest_share` | deforested area over forest stock |
| `defor_norm_ref0003` | `env_defor_area_z_bl0003` | within-municipality z, 2000–2003 baseline |
| `defor_norm_ref0007` | `env_defor_area_z_bl0007` | within-municipality z, 2000–2007 baseline |
| `log_forest_exposure` | `env_forest_exposure_log` |  |
| `pressure_defor` | `env_defor_pressure_idx` |  |
| `pressure_agri` | `agr_pressure_idx` |  |
| `pressure_score` | `smp_pressure_idx` | composite used only for sample definition. **CONFIRM** |
| `bf_families_n` | `sop_bf_fam_n` |  |
| `bf_quota` | `sop_bf_quota_n` |  |
| `bf_transfers_total_brl_2024` | `sop_bf_transf_brl24` |  |
| `bf_transfers_avg_brl_2024` | `sop_bf_transf_fam_brl24` | per family |
| `bf_families_quota_ratio` | `sop_bf_fam_quota_ratio` |  |
| `bf_transfers_pea_brl_2024` | `sop_bf_transf_pea_brl24` |  |
| `bf_transfers_quota_brl_2024` | `sop_bf_transf_quota_brl24` |  |
| `bf_transfers_pc_brl_2024` | `sop_bf_transf_pc_brl24` |  |
| `emp_pia_rate_total` | `lab_emp_formal_pia_rate` |  |
| `emp_pia_rate_agric` | `lab_emp_formal_agri_pia_rate` |  |
| `emp_pia_rate_low` | `lab_emp_formal_lowwage_pia_rate` |  |
| `emp_pia_rate_mid` | `lab_emp_formal_midwage_pia_rate` |  |
| `emp_pia_rate_high` | `lab_emp_formal_highwage_pia_rate` |  |
| `emp_share_low_workers` | `lab_emp_formal_lowwage_share` | share of all formal workers. **CONFIRM** |
| `emp_share_mid_workers` | `lab_emp_formal_midwage_share` |  |
| `emp_share_high_workers` | `lab_emp_formal_highwage_share` |  |
| `emp_share_agri_low_workers` | `lab_emp_formal_agri_lowwage_share` | **CONFIRM** denominator: all formal workers, or agricultural formal workers only. If the latter, use `lab_emp_formal_lowwage_agri_share` and document |
| `emp_share_agri_mid_workers` | `lab_emp_formal_agri_midwage_share` | same |
| `emp_share_agri_high_workers` | `lab_emp_formal_agri_highwage_share` | same |
| `emp_share_agri_workers` | `lab_emp_formal_agri_share` |  |
| `pea` | `lab_pea_n` |  |
| `informal` | `lab_informal_share` | **CONFIRM** unit, source and denominator |
| `population` | `dem_pop_n` |  |
| `gdp_brl` | `eco_gdp_brl` | nominal |
| `gdp_per_capita` | `eco_gdp_pc_brl` | nominal |
| `gdp_per_capita_log` | `eco_gdp_pc_brl_log` |  |
| `gdp_brl_2024` | `eco_gdp_brl24` |  |
| `gdp_per_capita_2024` | `eco_gdp_pc_brl24` |  |
| `gdp_per_capita_2024_log` | `eco_gdp_pc_brl24_log` |  |
| `elec_enc` | `pol_mayor_enc_idx` | **CONFIRM** meaning of `enc` |
| `elec_mov` | `pol_mayor_margin_share` |  |
| `elec_winner_share` | `pol_mayor_winner_share` |  |
| `elec_n_candidates` | `pol_mayor_cand_n` |  |
| `elec_winner_party` | `pol_mayor_winner_party_cat` |  |
| `elec_winner_id` | `pol_mayor_winner_id` |  |
| `elec_competitive` | `pol_mayor_competitive_d` |  |
| `elec_uncontested` | `pol_mayor_uncontested_d` |  |
| `election_year` | `pol_elec_year_d` | **CONFIRM**: if it is a 0/1 flag for election years, keep `_d`; if it stores the year of the most recent election, use `pol_mayor_elec_year` |
| `elec_enc_gov` | `pol_mayor_enc_idx_term` | **CONFIRM** that `_gov` means "aligned to governing term". If it means gubernatorial, use `pol_governor_*` instead |
| `elec_mov_gov` | `pol_mayor_margin_share_term` | same |
| `elec_winner_share_gov` | `pol_mayor_winner_share_term` | same |
| `elec_n_candidates_gov` | `pol_mayor_cand_n_term` | same |
| `elec_winner_party_gov` | `pol_mayor_winner_party_cat_term` | same |
| `elec_winner_id_gov` | `pol_mayor_winner_id_term` | same |
| `elec_competitive_gov` | `pol_mayor_competitive_d_term` | same |
| `elec_uncontested_gov` | `pol_mayor_uncontested_d_term` | same |
| `munic_area_km2` | `geo_area_km2` |  |
| `crop_soy_area_planted_km2` | `agr_soy_planted_km2` |  |
| `crop_corn_area_planted_km2` | `agr_corn_planted_km2` |  |
| `crop_rice_area_planted_km2` | `agr_rice_planted_km2` |  |
| `crop_cotton_area_planted_km2` | `agr_cotton_planted_km2` |  |
| `crop_soy_munic_area_share` | `agr_soy_planted_munic_share` |  |
| `crop_corn_munic_area_share` | `agr_corn_planted_munic_share` |  |
| `crop_rice_munic_area_share` | `agr_rice_planted_munic_share` |  |
| `crop_cotton_munic_area_share` | `agr_cotton_planted_munic_share` |  |
| `cattle_heads` | `agr_cattle_n` |  |
| `cattle_per_km2` | `agr_cattle_per_km2` |  |
| `price_index_crop` | `mkt_price_crop_idx` |  |
| `price_index_cattle` | `mkt_price_cattle_idx` |  |
| `price_index_crop_lag1` | `mkt_price_crop_idx_lag1` |  |
| `price_index_crop_z` | `mkt_price_crop_idx_z` |  |
| `price_index_crop_lag1_z` | `mkt_price_crop_idx_z_lag1` |  |
| `price_index_cattle_z` | `mkt_price_cattle_idx_z` |  |
| `price_index_cattle_lag1_z` | `mkt_price_cattle_idx_z_lag1` |  |
| `avail_ok` | `smp_avail_d` |  |
| `at_risk_broad` | `smp_atrisk_broad_d` |  |
| `at_risk_narrow` | `smp_atrisk_narrow_d` |  |

### 5.2 Atlas Brasil (`outcome_atlas_poverty_geocoded.csv`)

Wide file, one row per municipality, three census vintages. Apply `_cYYYY` for the vintage.

| Current pattern | Proposed pattern | Note |
|------------------------|------------------------|------------------------|
| `name` | `atlas_name` | source name string, kept for audit only |
| `key` | `atlas_key` | join key used in script 09 |
| `geocode` | `geocode` | key |
| `r1040_YYYY` | `soc_r1040_ratio_cYYYY` |  |
| `gini_YYYY` | `soc_gini_idx_cYYYY` |  |
| `rpc_YYYY` | `soc_inc_pc_brl10_cYYYY` | **CONFIRM** price base of Atlas income (2010 BRL in the standard release) |
| `ext_YYYY` | `soc_extpov_share_cYYYY` | rescale to 0-1 at construction if the source is percent |
| `pov_YYYY` | `soc_pov_share_cYYYY` | same |
| `vul_YYYY` | `soc_vuln_share_cYYYY` | same |

If the Atlas file is later reshaped to long format, drop the vintage suffix and add a `census_year` key column instead.

## 6. Implementation instructions for the agent

1.  **Create the crosswalk file** at `codebook/crosswalk.csv` with columns `old_name, new_name, domain, label, unit, denominator, transform, timing, source, note`. Populate it from section 5. This file is the single source of truth for both the rename and the codebook.

2.  **Do not rename inside the existing pipeline scripts 01–06 during the equivalence phase.** The equivalence tests compare the rebuilt output against the frozen `final_dataset_6.csv`, which uses the old names. Either run the rename as a final step after equivalence passes, or map names through the crosswalk inside the test so both sides are compared under the same names.

3.  **Apply the rename from the crosswalk**, never by hand:

    ``` r
    xwalk <- readr::read_csv("codebook/crosswalk.csv", show_col_types = FALSE)
    panel <- panel |>
      dplyr::rename(dplyr::any_of(setNames(xwalk$old_name, xwalk$new_name)))
    ```

4.  **Guard the convention** at the end of the pipeline so any variable that escapes it fails loudly:

    ``` r
    keys <- c("geocode", "year", "municipality", "state", "uf")
    prefix_ok <- grepl("^(env|enf|sop|soc|lab|dem|eco|agr|mkt|pol|geo|smp)_", names(panel))
    stopifnot(all(names(panel) %in% keys | prefix_ok))
    stopifnot(all(nchar(names(panel)) <= 40))
    stopifnot(!any(duplicated(names(panel))))
    ```

5.  **Generate the codebook from the crosswalk**, not by hand, so names and documentation cannot drift.

6.  **Resolve every CONFIRM item with Fernando before writing the crosswalk.** Do not guess. Where the answer changes a name, update this file and the crosswalk together.

7.  **Do not create new variables with old-style names.** Any variable added during the rebuild follows the grammar from the start.

8.  **Update downstream scripts (07–10) and the Quarto appendices** after the rename, using the crosswalk to find and replace old names. Grep for any remaining old name before closing the task.

## 7. Known data issue to fix in the same pass

Roughly 60 of the 85 panel columns are stored as character even though they hold numeric values (`bf_families_n`, `population`, `gdp_brl` and others). This usually means NA strings or decimal commas upstream. Coerce to numeric at the source in scripts 01–03, record the cause in `DECISIONS.md`, and confirm that no non-NA value is lost in the coercion.

## 8. Things not to do

- Do not add a fifth slot or a "role" slot to the grammar.
- Do not use `avg`, `total`, `mean`, `rate` without a denominator, or `gov` as a suffix.
- Do not store percentages. Rescale to 0-1 at construction.
- Do not translate `bf`, `ppcdam`, `pia`, `pea` into English.
- Do not put the data source in a name unless two sources measure the same concept.
- Do not rename by hand in individual scripts. All renaming goes through the crosswalk.
- Do not silently resolve a CONFIRM item.