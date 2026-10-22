# DECISIONS.md — Chapter 4 data-pipeline decision log

Append-only. One entry per decision that could have been made differently,
written during the work. Format and rules: `rebuild/04-codebook-and-decisions.md`
§2. IDs are `D-YYYY-MM-DD` plus a letter when several are logged on one day;
the date is the logging date. Entries are never edited retroactively except to
close them (`Status`) or to mark them superseded. Every decision ID cited in a
script header or an annex paragraph must exist here.

The entries below were seeded in phase 0 (2026-10-05) from what the surviving
scripts, the task board and the rebuild brief already record, so the log opens
with the pre-rebuild history. Script line references point to the scripts as
they stand at tag `pre-reorg`; the audit-notebook evidence for the closed
entries is produced in phase 1.

## D-2026-10-05-a  Bolsa Família reference month June, February for robustness

Status: closed (pre-rebuild; script 02 at tag pre-reorg; refactored in phase 1, 02-social-bf.R)
Scope: script 02 (lines 1028, 1200), bf_* columns, annex §4
Context: MDS publishes monthly municipal counts and transfers; the panel is
         annual, so one month must stand for the year.
Reason: June was chosen on stability, centrality and completeness of the
        monthly series; February is kept as the alternative (outcome_bf_feb).
Evidence: pre-rebuild; audit notebook pending (phase 1, _audit/bf.qmd)
Affects: bf_families_n, bf_quota, bf_transfers_*, bf_families_quota_ratio
Supersedes: none

## D-2026-10-05-b  Bolsa Família panel truncated at 2019

Status: superseded by D-2026-10-22-a (phase 2, step 2.2, 2026-10-22)
Scope: script 02 (lines 876, 1028), bf_* columns 2004-2019
Context: the MDS series continues past 2019, but 2020 carries the COVID
         emergency transfers and the 2021-2023 Auxílio Brasil substitution.
Reason: the monthly series breaks in 2020; until the break is handled, 2019 is
        the last comparable year.
Evidence: pre-rebuild; audit notebook pending (phase 1)
Affects: all bf_* columns are NA in 2020
Supersedes: none

## D-2026-10-05-c  Monetary values deflated to 2024 BRL with the IPCA

Status: closed (pre-rebuild; scripts 02, 03 at tag pre-reorg)
Scope: script 02 (lines 1106-1119), script 03 (line 29), _data/aux/deflator_ipca_2024.csv
Context: transfers and GDP are nominal in the sources and span 2002-2020.
Reason: the IPCA is the official consumer-price index; 2024 is the latest full
        year at the time of the original run. The lookup is hand-built from the
        IBGE series (raw/ibge/ipca_202509SerieHist.xlsx); placement decided
        2026-10-05 (execution path, stage 3).
Evidence: pre-rebuild; construction of the lookup to be documented in the annex
Affects: bf_transfers_*_brl_2024, gdp_brl_2024, gdp_per_capita_2024(_log)
Supersedes: none

## D-2026-10-05-d  RAIS wage brackets anchored to R$ 1,412 (2024 minimum wage)

Status: closed (pre-rebuild; BigQuery extraction and script 02 at tag pre-reorg; refactored in phase 1, 02-social-rais.R)
Scope: BigQuery query 2 (rebuild/upstream-reform-rais.md), script 02 (lines 396-470)
Context: RAIS reports wages in nominal BRL; bracket membership must be
         comparable across years.
Reason: wages are deflated to 2024 BRL and cut at multiples of the 2024
        minimum wage (R$ 1,412) into six codes; script 02 groups them as low
        (code 1), mid (2-4) and high (5-6).
Evidence: pre-rebuild; regression rule in the RAIS brief guards the extraction
Affects: emp_pia_rate_low/mid/high, emp_share_*_workers
Supersedes: none

## D-2026-10-05-e  PIA as primary employment denominator, PEA as robustness

Status: closed (pre-rebuild; script 02 at tag pre-reorg); reopened by D-2026-10-07-i (PEA as denominator)
Scope: script 02 (employment block, lines 365-697), emp_pia_rate_*, pea
Context: formal employment counts need a population denominator; the census
         gives working-age population (PIA) and economically active population
         (PEA).
Reason: PIA is defined identically across censuses and does not depend on
        labour-force participation, which is itself an outcome of interest;
        PEA is retained in the panel for robustness.
Evidence: pre-rebuild; argument in the chapter methods section
Affects: emp_pia_rate_total/agric/low/mid/high, pea
Supersedes: none

## D-2026-10-05-f  PIA interpolated linearly between the 2000, 2010 and 2022 censuses

Status: closed (pre-rebuild; script 02 at tag pre-reorg; refactored in phase 1, 02-social-census.R); storage of anchors reopened by Task 1.7; the "extrapolated flat" wording is corrected by D-2026-10-07-f
Scope: script 02 (lines 217-334), raw/ibge/data-censo{2000,2010,2022}-pia.xlsx
Context: PIA exists only at census years; the panel is annual.
Reason: linear interpolation between anchors, extrapolated flat outside them,
        is the simplest defensible fill. Task 1.7 will store the raw anchors in
        a baseline table and flag interpolated columns in the codebook without
        changing the interpolation.
Evidence: pre-rebuild; audit notebook pending (phase 1, _audit/census.qmd)
Affects: pia (denominator), emp_pia_rate_*, and by the same method population, pea, informal
Supersedes: none

## D-2026-10-05-g  Moderator baseline window 2006-2007

Status: closed (pre-rebuild; analysis scripts at tag pre-reorg); window parameterised per
        docs/decisions/2026-10-05-baseline-window-in-analysis-scripts.md
Scope: 07-data-analysis-eda.R (lines 944, 1277, 1461), 08-data-analysis-2WFE-CS.R
Context: Bolsa Família coverage after the 2008 listing is post-treatment; the
         moderator must be measured before it.
Reason: 2006-2007 is after the programme's 2004-2005 rollout and before the
        first listing (2008). Wider windows (2006-2010, 2006-2012) were tested
        and rejected: a DiD of listing on coverage returns -0.13 to -0.18, so
        coverage after 2007 is post-treatment.
Evidence: pre-rebuild; to be reproduced in _audit/bf.qmd
Affects: stratum split in 08, Block B/C figures in 07-eda
Supersedes: none
Note: 08-data-analysis-2WFE-CS.R currently averages the moderator over
      BASE_YEARS = 2002:2007 (line 67), not 2006-2007. The two records
      disagree; the 2026-10-05 memo makes the window a per-script parameter,
      so the contradiction is resolved at the causal stage, not here.

## D-2026-10-05-h  Outcome `defor_norm_ref0007`; estimation on the at-risk narrow sample

Status: closed (pre-rebuild; scripts 05, 06 at tag pre-reorg; refactored in phase 1, 05-sample-definition.R and 06-normalized-outcome.R, DEFOR_REF_MAIN_YEARS)
Scope: script 06 (lines 36-66), script 05 (lines 122-132), 08 (SAMPLE_FLAG)
Context: deforestation levels are heteroskedastic across municipalities, and
         municipalities with no forest or no pre-period clearing cannot carry
         a treatment effect.
Reason: the outcome is the within-municipality z-score of the annual
        deforestation increment against the 2000-2007 pre-listing mean and
        sd (defor_norm_ref0003 kept as the short-window alternative); the
        at-risk narrow sample keeps municipalities above the forest-share floor
        and the narrow baseline-deforestation cut (at_risk_broad is the looser
        cut, avail_ok the floor only).
Evidence: pre-rebuild; script 05 balance tables (_output/at_risk_balance.csv)
Affects: defor_norm_ref0003/0007, log_forest_exposure, avail_ok, at_risk_broad/narrow,
         pressure_defor/agri/score
Supersedes: none

## D-2026-10-05-i  Median split of the moderator for the stratified Callaway-Sant'Anna

Status: closed (pre-rebuild; 08-data-analysis-2WFE-CS.R at tag pre-reorg)
Scope: 08 (lines 65, 133-135)
Context: the moderation hypothesis (H2) is tested by estimating the listing
         effect separately in high- and low-coverage strata.
Reason: a median split on the baseline quota ratio (threshold 0.96 on the
        current sample) gives two strata of equal size, each with its own
        treated and never-treated units; the tercile split is the robustness
        alternative. The threshold is sample-dependent, so it is computed in
        the analysis script, never stored in the panel (governing rule for
        derived variables, GUIDANCE.md).
Evidence: pre-rebuild
Affects: stratum assignment in 08; ATTs by stratum
Supersedes: none

## D-2026-10-05-j  `pov_2000` reserved for falsification

Status: closed (pre-rebuild; 07-data-analysis-EDA-poverty.R at tag pre-reorg)
Scope: 07-data-analysis-EDA-poverty.R; Atlas Brasil join (not in the panel)
Context: the 2000 poverty rate predates both the programme and the listing.
Reason: a pre-programme poverty measure cannot be moved by Bolsa Família
        coverage, so a stratification on it that reproduced the coverage result
        would show the moderation is structural, not programme-driven. It is
        therefore held back from the main specification and used only as a
        falsification contrast.
Evidence: pre-rebuild
Affects: EDA poverty blocks; no panel column
Supersedes: none

## D-2026-10-05-k  Colniza drops from covariate-adjusted specifications

Status: closed (pre-rebuild; consequence of missing pre-2010 GDP per capita); reopened by Task 1.6
Scope: script 03 (population and GDP), 08 (xformla rows)
Context: Colniza (5103254) was created after the 2000 census; population is NA
         until 2009, so gdp_per_capita_* is NA from 2002 to 2008 and the unit
         is dropped by `did` in every specification with covariates.
Reason: not a choice but a known consequence of D-2026-10-05-f applied to a
        municipality without a 2000 anchor; logged so the adjusted rows of the
        results are read correctly. Task 1.6 replaces the missing anchor with
        official annual estimates.
Evidence: rebuild/tasks.md task 1.6
Affects: adjusted ATT rows in 08; Colniza is a treated unit (2008 cohort, low-coverage stratum)
Supersedes: none

## D-2026-10-05-l  Mojuí dos Campos dropped from the panel

Status: closed (pre-rebuild; script 05 at tag pre-reorg; refactored in phase 1, 05-sample-definition.R, MOJUI_GEOCODE); the statement that the parent's series are not adjusted holds for PIA, population and deforestation only, see D-2026-10-07-d
Scope: script 05 (line 63), N_MUNIC_EXPECTED = 502 in the reference
Context: Mojuí dos Campos (1504752) was installed in 2013 by emancipation from
         Santarém; it has no series before 2013 and its parent's series are
         not adjusted.
Reason: with no pre-treatment observations the unit cannot enter any design;
        it is removed in script 05, which is why the reference panel has 502
        municipalities against the 503 of the IBGE biome list.
Evidence: pre-rebuild; to be confirmed in _audit/ibge_biome.qmd
Affects: row count assertions in scripts 05-06; the 502 count in the reference
Supersedes: none

## D-2026-10-05-m  Legal Amazon extension via `biome_amazon` and `legal_amazon` flags

Status: open (Task 1.1; phase 2 step 2.9)
Scope: scripts 01, 04, 05; all N_MUNIC_EXPECTED assertions
Context: the panel covers the 502 municipalities of the Amazon biome; the
         Legal Amazon adds the non-biome municipalities of the nine states.
Reason: extending the sample with constant flags allows a biome-versus-Legal
        Amazon robustness check without permanently excluding units. Row-level
        change, so it runs last in phase 2.
Evidence: pending
Affects: new rows; biome_amazon, legal_amazon
Supersedes: none

## D-2026-10-05-n  Quota source correction for 14 Mato Grosso municipalities

Status: superseded by D-2026-10-21-a (phase 2, step 2.1, 2026-10-21)
Scope: script 02; bf_quota, bf_families_quota_ratio
Context: 14 MT municipalities show 2006-07 quota ratios above 3 (max 11.9)
         while MT's median is 0.96, which points to a source error for
         specific codes rather than a statewide problem.
Reason: verify against the re-acquired quota source before any fix; a defect
        absent upstream is closed as resolved-by-source.
Evidence: pending (_audit/bf.qmd)
Affects: bf_quota, bf_families_quota_ratio for the listed municipalities
Supersedes: none

## D-2026-10-05-o  Electoral values mapped to the governing period

Status: open (Task 1.5; phase 2 step 2.3)
Scope: script 03; elec_*_gov columns
Context: the stated rule is that an election's values apply to the governing
         years (2004 election → 2005-2008), but the stored columns change in
         the election year (2004 → 2004-2007), so 2008 carries the October-2008
         result.
Reason: align storage with the stated rule; the governing rule then applies to
        every derived electoral variable.
Evidence: pending (_audit/tse.qmd)
Affects: elec_enc_gov, elec_mov_gov, elec_winner_share_gov, elec_n_candidates_gov,
         elec_winner_party_gov, elec_winner_id_gov, elec_competitive_gov, elec_uncontested_gov
Supersedes: none

## D-2026-10-05-p  Price indices anchored at 2000 = 1 after the panel join

Status: open (Task 1.3; phase 2 step 2.4)
Scope: script 06 (normalization moves here from script 03); price_index_*
Context: price_index_crop (~0.014-0.03) and price_index_cattle (~26-51) are
         on incompatible scales and neither equals 1 in 2000, against the
         documented Pink Sheet specification (MUV-deflated, 2000 = 1), because
         normalization ran before the final join.
Reason: normalize once, on the joined panel, in the script devoted to
        normalized variables.
Evidence: pending (_audit/prices.qmd)
Affects: price_index_crop/cattle, their lags and z-scores
Supersedes: none

## D-2026-10-05-q  Bolsa Família endpoint extended past 2020

Status: closed (phase 2, step 2.2, 2026-10-22: implemented by D-2026-10-22-a, -b and -c)
Scope: script 02; bf_* columns 2020-2024
Context: all bf_* columns are missing for 2020-2024 while PRODES, RAIS and
         prices are present (D-2026-10-05-b).
Reason: the programme ran to late 2021, was replaced by Auxílio Brasil and
        returned in 2023; the break must be handled or documented so the panel
        can reach 2024.
Evidence: pending (_audit/bf.qmd)
Affects: bf_* columns 2020-2024
Supersedes: none (extends D-2026-10-05-b)

## D-2026-10-06-a  MapBiomas year window 1987-2023

Status: closed (pre-rebuild; script 01 at tag pre-reorg, line 88; refactored in phase 1, script 01)
Scope: 01-env-deforestation.R (MAPBIOMAS_YEARS); area_forest, area_deforestation and derived columns
Context: the Collection 9 municipal statistics cover 1985-2023, but both
         suppression classes are zero for every municipality in 1985 and
         1986, the first two years of the Landsat series.
Reason: two all-zero years would enter the municipality means and standard
        deviations as artificial zeros; the window starts at the first year
        with observed suppression.
Evidence: _audit/mapbiomas.qmd, Q2
Affects: every row of the interim deforestation panel; deforestation_mean,
         deforestation_sd, deforestation_z
Supersedes: none

## D-2026-10-06-b  Forest and deforestation aggregated over all level-1 classes

Status: closed (pre-rebuild; script 01 at tag pre-reorg, lines 86-145; refactored in phase 1, script 01)
Scope: 01-env-deforestation.R (MAPBIOMAS_FOREST_TRANSITIONS, MAPBIOMAS_DEFOR_TRANSITIONS);
       area_forest, area_deforestation
Context: the source reports areas by transition class (primary vegetation,
         secondary vegetation, their suppression, and others) crossed with
         four levels of land-cover class.
Reason: forest is the sum of primary and secondary vegetation and
        deforestation the sum of their suppression, with no restriction on
        class_level_1, as in the surviving script. The audit shows that the
        vegetation rows include non-forest natural formation, water and
        other classes. Whether to restrict both aggregates to class 1
        (Forest) is left open for the author; it is not a phase 2 task.
Evidence: _audit/mapbiomas.qmd, Q3
Affects: area_forest, area_deforestation, deforestation_rate; downstream
         forest_area_km2, deforestation_area_km2, defor_norm_*
Supersedes: none

## D-2026-10-06-c  Sample rule in script 01: nine states, then IBGE predominant biome Amazônia

Status: closed (pre-rebuild; script 01 at tag pre-reorg, lines 91, 376; refactored in phase 1, script 01); reopened by D-2026-10-05-m (task 1.1)
Scope: 01-env-deforestation.R (AMZ_STATES, BIOME_KEEP, N_MUNIC_*_EXPECTED)
Context: MapBiomas covers all 5,571 municipalities; the study population is
         the Amazon.
Reason: the IBGE list gives a standard criterion (the biome covering the
        largest share of the municipal area): 503 municipalities, Mojuí dos
        Campos included at this stage and dropped in the panel scripts
        (D-2026-10-05-l). The nine-state subset (808 municipalities) is
        built in full before the biome filter, which is one delimited block
        governed by BIOME_KEEP, so that step 2.9 can replace the filter with
        the biome_amazon and legal_amazon flags.
Evidence: _audit/ibge_biome.qmd, Q2; _audit/mapbiomas.qmd, Q1
Affects: the row set of every downstream dataset
Supersedes: none

## D-2026-10-06-d  deforestation_rate = deforestation / contemporaneous forest x 100

Status: closed (pre-rebuild; script 01 at tag pre-reorg, lines 251-258; refactored in phase 1, script 01); reopened by task 1.9 (phase 2 step 2.8)
Scope: 01-env-deforestation.R (RATE_SCALE); deforestation_rate, downstream deforestation_forest_rate
Context: municipalities differ in size, so the cleared area is also
         expressed relative to the forest stock.
Reason: the denominator is the same year's forest area, not the lagged one;
        the value is a percentage; NA when forest is zero. Task 1.9 rescales
        to 0-1 and records the denominator timing in the codebook.
Evidence: _audit/mapbiomas.qmd, Q8
Affects: deforestation_forest_rate
Supersedes: none

## D-2026-10-06-e  Municipality-level deforestation mean, sd and z-score stored in the interim panel

Status: closed (pre-rebuild; script 01 at tag pre-reorg, lines 259-265; refactored in phase 1, script 01); flagged for task 2.2
Scope: 01-env-deforestation.R; deforestation_mean, deforestation_sd, deforestation_z
Context: computed within municipality over 1987-2023; mean and sd are
         dropped in script 04 and z in script 06, so none reaches the final
         panel.
Reason: kept in phase 1 so that script 04 receives the same input it had;
        whether they remain in the interim file is decided in the storage
        audit (task 2.2), not here.
Evidence: scripts 04 (line 23) and 06 (line 115) at tag pre-reorg
Affects: interim file only
Supersedes: none

## D-2026-10-06-f  PPCDAm geocodes attached from the IBGE municipality lookup

Status: closed (commit 34d0d1f)
Scope: 01-env-ppcdam.R (PPCDAM_STATE_UF); geocode of ppcdam_list
Context: the surviving script read a data_ppcdam.xlsx with a hand-entered
         Geocode column, lost with the machine; the recovered
         ppcdam_lists.xlsx (sheet Lista) carries municipality name and state
         only.
Reason: all 92 names match raw/ibge/ibge_munic_id.xlsx on (name, state)
        with no duplicates, and the joined list reproduces the reference
        panel's ppcdam_list column exactly for 502 municipalities x
        2000-2020 (compare_outputs, n_diff 0). Behavior change relative to
        the lost script, confirmed by the author 2026-10-06.
Evidence: _audit/ppcdam.qmd, Q2 and Q3; compare_outputs report in the
          phase 1 script 01 commit message
Affects: ppcdam_list and every ppcdam_* variable downstream
Supersedes: none

## D-2026-10-06-g  Sheet Lista is the PPCDAm indicator; the missing 2024 cell stays NA

Status: closed (phase 1, script 01)
Scope: 01-env-ppcdam.R (PPCDAM_SHEET, N_PPCDAM_NA_EXPECTED)
Context: the workbook holds Lista (0/1), Lista_dummy (identical), Lista_cat
         (-1/0/1 status coding) and Decretos (normative record); Grajaú (MA)
         has no value for 2024.
Reason: Lista is the sheet the surviving script read (first sheet);
        Lista_cat is left for a possible status coding later. The NA is
        outside the reference window and recoding it to 0 would be a silent
        change.
Evidence: _audit/ppcdam.qmd, Q1 and Q5
Affects: ppcdam_list for Grajaú 2024 (outside the current panel)
Supersedes: none

## D-2026-10-06-h  2010 PEA anchor not reproducible from the re-acquired census tables

Status: closed (phase 1, script 02, 2026-10-07: D-2026-10-07-b builds the 2010 anchor from Tabela 616 PEA 15+; drift quantified in commit 41a1e99)
Scope: script 02 (PEA interpolation), pea, bf_transfers_pea_brl_2024; raw/ibge/sidra/
Context: the lost pea-2000/2010/2022.xlsx extracts were re-acquired from the
         SIDRA API on 2026-10-06. The 2000 anchor is PEA aged 15 and over from
         Tabela 616 (450 of 493 municipalities within one person of the
         reference, the rest SIDRA cell rounding plus the Mojuí allocation at
         Santarém); the 2022 anchor is the labour force aged 14 and over from
         Tabela 6580 (exact for all 502). No cut of the 2010 tables (616, 1572:
         ages 10+, 14+, 15+, 16+, 18+, occupied only) reproduces the reference
         2010 values, which sit 2-7 percent below PEA 15+ with a dispersion
         that rules out a constant rescaling.
Reason: the author decided (2026-10-06) not to chase the lost 2010 source:
        PIA is the primary denominator (D-2026-10-05-e) and PEA is retained
        only for robustness. The inconsistency is registered here; script 02
        will build the 2010 anchor from Tabela 616 under the same rule as 2000
        (PEA 15+) and the phase 1 gate will report the resulting drift in pea
        and bf_transfers_pea_brl_2024 for 2001-2021 as re-acquired-source
        drift, not as a refactor error.
Evidence: scratch comparison 2026-10-06 (to be reproduced in _audit/census.qmd)
Affects: pea (all years, through interpolation), bf_transfers_pea_brl_2024
Supersedes: none

## D-2026-10-07-a  PIA = resident population aged 14 and over, summed from the census age groups

Status: closed (phase 1, script 02)
Scope: 02-social-census.R (PIA_AGE_GROUPS_*); pia (interim), emp_pia_rate_*
Context: the lost script read three hand-tidied PIA tables (id, total,
         state); the recovered workbooks are the SIDRA exports behind them
         (2000 Dados do Universo, Tabela 1378, Tabela 9514), each listing a
         "Total" row and the age groups from 14 upward.
Reason: the "Total" is the population of all ages (it equals the panel's
        2010 population), so the PIA the lost tables held is the sum of the
        14-and-over groups: nineteen five-year groups in 2000 and 2022,
        thirteen in 2010. The rebuilt tidy step reproduces the twelve emp_*
        panel columns exactly (compare_outputs, 0 differing cells at 1e-8,
        502 x 2000-2020), which pins the definition.
Evidence: _audit/census.qmd, Q1; compare_outputs report in commit 41a1e99
Affects: pia, emp_pia_rate_total/agric/low/mid/high
Supersedes: none

## D-2026-10-07-b  PEA anchors from SIDRA Tabela 616 (15+) for 2000 and 2010 and Tabela 6580 (14+) for 2022

Status: closed (phase 1, script 02); closes D-2026-10-06-h
Scope: 02-social-census.R; pea, bf_transfers_pea_brl_2024
Context: the lost pea-YYYY.xlsx extracts were re-acquired from the SIDRA
         API (D-2026-10-06-h); the candidate cuts differ by a few percent.
Reason: PEA 10+ minus PEA 10-14 from Tabela 616 reproduces the recovered
        2000 anchors to within one person for 460 of 502 sample
        municipalities (208 exact; API cell rounding and the Mojuí
        allocation explain the rest) and the Tabela 6580 labour force 14+
        reproduces 2022 exactly. No cut of the 2010 tables reproduces the
        2010 anchors; the same rule is applied to 2010 and the difference
        (median 3.4 percent above the reference, 10th-90th percentiles 2.0
        to 4.9 percent) is registered as re-acquired-source drift, per the
        author's decision of 2026-10-06 not to chase the lost 2010 source.
        It propagates to pea in 2001-2021 and to bf_transfers_pea_brl_2024
        (8,016 cells, max 16.0 BRL).
Evidence: _audit/census.qmd, Q2; compare_outputs report in commit 41a1e99
Affects: pea (2001-2021 drift), bf_transfers_pea_brl_2024 (2004-2019 drift)
Supersedes: none (closes D-2026-10-06-h)

## D-2026-10-07-c  Informality = share of the occupied population outside the formal categories, stored unrounded

Status: closed (phase 1, script 02)
Scope: 02-social-census.R (INFORMAL_*_IDS_*, INFORMAL_SCALE); informal
Context: the lost informality-YYYY.xlsx extracts held a percentage rounded
         to two decimals; the re-acquired tables are Tabela 2031 (2000,
         2010: position in occupation, no CNPJ split) and Tabela 10261
         (2022: with CNPJ split).
Reason: formal in 2000 and 2010 = employees with a signed card, military
        and statutory public servants, employers; formal in 2022 = the
        IBGE set (employees with a signed card in the private, domestic,
        public non-statutory and state-company categories, military,
        statutory servants, employers with CNPJ, self-employed with CNPJ);
        informal = (occupied - formal) / occupied x 100. The 2022 set
        reproduces the anchor the recovered panel implies for every sample
        municipality; the 2000 and 2010 sets reproduce the recovered
        anchors to within 0.01 pp for 296 and 331 of 502 municipalities and
        within 0.05 pp for all but 19 and 8 (max 0.19 pp), the residue
        being the two-decimal rounding of the lost extract and cell
        differences of a few persons between the 2025 web export and the
        API. The author chose (2026-10-07) to store the unrounded share.
        Scale stays 0-100 as inherited (task 1.9).
Evidence: _audit/census.qmd, Q3; compare_outputs report in commit 41a1e99 (informal: 10,532 cells differ, max
          0.20 pp)
Affects: informal (all years, through interpolation)
Supersedes: none

## D-2026-10-07-d  Mojuí dos Campos 2000 and 2010 PEA allocated from Santarém; informality copied

Status: closed (pre-rebuild; script 02 at tag pre-reorg, lines 1521-1612; refactored in phase 1, 02-social-census.R)
Scope: 02-social-census.R (MOJUI_GEOCODE, SANTAREM_GEOCODE); pea, informal for 1504752 and 1506807
Context: Mojuí dos Campos was emancipated from Santarém in 2013 and has no
         2000 or 2010 census row.
Reason: as inherited, Mojuí's 2000 and 2010 PEA anchors are Santarém's
        times Mojuí's share of the pair's 2022 labour force (5.76 percent),
        Santarém's anchors are reduced by the complement, and Mojuí takes
        Santarém's informality share; both series are then interpolated.
        The PIA is not adjusted (Mojuí has no PIA before 2022). The
        recovered panel carries the adjusted Santarém PEA (92,426 against
        98,076 unadjusted in 2000), so the statement in D-2026-10-05-l that
        the parent's series are not adjusted holds for PIA, population and
        deforestation only.
Evidence: _audit/census.qmd, Q4
Affects: pea and informal for Santarém (2000-2012) and Mojuí (2000-2022);
         bf_transfers_pea_brl_2024 for both
Supersedes: none

## D-2026-10-07-e  SIDRA "..." read as zero in the 2000 anchors of municipalities installed after 2000

Status: closed (phase 1, script 02; reproduces the inherited defect, task 1.6 replaces it)
Scope: 02-social-census.R (SIDRA_ZERO_SYMBOLS); pea, informal, 2000-2009, 58 municipalities
Context: SIDRA prints "-" for a true zero and "..." for a value not
         available; the 58 municipalities installed after the 2000 census
         (the nine Mato Grosso ones of task 1.6 among them) read "..." in
         the 2000 tables, and the lost extracts coded them 0.
Reason: the recovered panel carries pea = 0 and informal = 0 in 2000 for
        these municipalities, with both series rising linearly to the 2010
        value; phase 1 reproduces that by reading "..." as zero and
        setting informal to 0 where the occupied total is 0. Task 1.6
        changes the rule to NA with no backcasting. The same municipalities
        have no 2000 PIA; their PIA for 2000-2009 is the inherited backward
        extrapolation from the 2010-2022 slope (D-2026-10-07-f).
Evidence: _audit/census.qmd, Q5
Affects: pea, informal (2000-2009) and pia (2000-2009) of 58
         municipalities; the five municipalities created after 2010 carry
         NA before 2022
Supersedes: none

## D-2026-10-07-f  PIA extrapolated to 1999 and 2023-2024 with the slope of the adjacent census decade

Status: closed (pre-rebuild; script 02 at tag pre-reorg, lines 272-299; refactored in phase 1, 02-social-census.R)
Scope: 02-social-census.R (PIA_YEARS; utils/interpolate_census.R); pia outside 2000-2022
Context: D-2026-10-05-f describes the fill outside the anchors as flat;
         the surviving code extends the series linearly with the slope of
         the first (1999) and last (2023-2024) anchor pair, and applies the
         same rule backward to municipalities without a 2000 anchor.
Reason: reproduce the code, not the summary: the panel window is
        2000-2020, so only the backward extrapolation of post-2000
        municipalities touches the reference, and it is reproduced on
        purpose (task 1.6). The wording of D-2026-10-05-f is corrected
        here rather than edited there.
Evidence: _audit/census.qmd, Q5 and Q6
Affects: pia in 1999 and 2023-2024 (outside the panel) and in 2000-2009
         for municipalities without a 2000 anchor
Supersedes: the "extrapolated flat" wording of D-2026-10-05-f

## D-2026-10-07-g  Municipal quota per year from the four period columns of the LAI workbook

Status: closed (phase 1, script 02)
Scope: 02-social-bf.R (BF_QUOTA_PERIOD_YEARS); bf_quota, bf_families_quota_ratio, bf_transfers_quota_brl_2024
Context: the lost bf-quotas.xlsx was a hand-tidied derivative (7-digit
         geocode, quota_YYYY columns) of the SENARC LAI response, which
         carries four estimates of poor families per municipality: until
         2005, 2006-2008, 2009-2011, 2012 onward.
Reason: the recovered panel's quota is constant within those periods and
        equal in 2004 and 2005, so the periods map to 2004-2005, 2006-2008,
        2009-2011 and 2012-2019. The rebuilt step reproduces bf_quota and
        the two quota ratios exactly (0 differing cells). The fourteen
        Mato Grosso outliers of task 1.4 are in the source values.
Evidence: _audit/bf.qmd, Q4; compare_outputs report in commit 41a1e99
Affects: bf_quota, bf_families_quota_ratio, bf_transfers_quota_brl_2024
Supersedes: none

## D-2026-10-07-h  MDS six-digit codes mapped to seven-digit geocodes through the IBGE lookup

Status: closed (phase 1, script 02)
Scope: 02-social-bf.R (geocode_map); geocode of bf_panel
Context: the MDS monthly files identify municipalities by the six-digit
         code (no check digit); the lost script took the seven-digit code
         from its join with the quota table.
Reason: the seventh IBGE digit is a check digit, so the six-digit prefix is
        unique in raw/ibge/ibge_munic_id.xlsx (5,571 of 5,571) and every
        MDS code matches one; mapping through the lookup removes the
        dependence of the key on the quota source. The bf_* columns
        reproduce the reference exactly (bf_families_n and bf_quota with 0
        differing cells; the deflated transfers within 4e-9).
Evidence: _audit/bf.qmd, Q5; compare_outputs report in commit 41a1e99
Affects: geocode of every bf_* row
Supersedes: none

## D-2026-10-07-i  PEA-normalised and agricultural-by-wage employment rates not rebuilt; PIA-only denominator pending

Status: open (author's intent recorded 2026-10-07; settled at the storage audit, task 2.2, or by a docs/decisions memo)
Scope: 02-social-rais.R; interim rais_employment columns; D-2026-10-05-e
Context: the lost script also built emp_rate_*_pea (eight rates over PEA)
         and emp_rate_agri_{low,mid,high}_pia; none reached the panel, and
         script 04 kept the PIA rates and the shares only.
Reason: the author stated (2026-10-07) that PIA is the intended sole
        denominator: it is demographic and predetermined with respect to
        the treatment, whereas PEA is behavioral and could become an
        endogenous, price-responsive denominator baked into the variable.
        The interim file therefore carries the PIA rates and the shares
        only. pea and bf_transfers_pea_brl_2024 are still built because the
        reference panel carries them; whether they stay is decided when the
        intent becomes a decision.
Evidence: author's answer at the phase 1 script 02 planning step
Affects: interim rais_employment (fewer columns than the lost
         outcome_employment); no panel column
Supersedes: none (reopens D-2026-10-05-e)

## D-2026-10-07-j  Task 1.10 at script 02: no character-to-numeric coercion needed at source

Status: closed (phase 1, script 02)
Scope: 02-social-census.R, 02-social-rais.R, 02-social-bf.R; bf_*, emp_*, pea, informal
Context: task 1.10 expected NA strings or decimal commas upstream behind
         the character columns of final_dataset_6.csv.
Reason: every numeric input of script 02 parses clean with explicit column
        types (the only non-numeric cells are the SIDRA symbols handled by
        D-2026-10-07-e and the "-" zero of the workbooks); the interim
        outputs set geocode as character, year and bf_ref_month as integer,
        counts as integer and the rest as double. The character storage in
        the reference CSV is therefore a write-side artefact of the lost
        scripts, to be confirmed at the phase 1 gate.
Evidence: raw-state and output assertions of the three scripts
Affects: none (types of the interim outputs)
Supersedes: none

## D-2026-10-10-a  Pink Sheet input is the September 2026 release; MUV re-deflation drift accepted

Status: closed (phase 1, script 03, 03-controls-prices.R). Drift at the script 03 acceptance: price_index_crop 9,144 cells (483 municipalities) within -0.15/+0.19 percent of the reference, price_index_cattle 9,941 cells (497) within ±0.15 percent, the lag accordingly; no refactor error
Scope: script 03 (price index section, lines 1081-1210 at tag pre-reorg); price_index_crop, price_index_cattle, price_index_crop_lag1, the _z variants of script 06; raw/worldbank/
Context: the lost raw-prices-world bank-pink sheet.xlsx was re-acquired on
         2026-10-10 as CMO-Historical-Data-Annual.xlsx, header "Updated on
         September 02, 2026". Script 03 reads the real series (2010 US
         dollars), which the World Bank recomputes over the whole history at
         every revision of the MUV deflator; the nominal series do not move.
         Normalising to 2000 = 1 does not cancel the revision.
Reason: the author accepts the current release as the input (rebuild/05-reacquisition-controls.md section
        2.4, 2026-10-09). The beef index recovered from the reference
        price_index_cattle (ratio to its 2000 value) is reproduced within
        0.15 percent on every year 2000-2020 (mean +0.03 percent); the lost
        release is not reconstructed, and the phase 1 gate reports the
        resulting differences in the price indices as re-acquired-source
        drift, not as a refactor error.
Evidence: scratch/staging/worldbank/check_pink_sheet_vs_reference.R
          (2026-10-10; to be reproduced in the script 03 audit notebook);
          provenance row raw/worldbank/CMO-Historical-Data-Annual.xlsx
Affects: price_index_crop, price_index_cattle, price_index_crop_lag1 (all
         years); downstream price_index_*_z
Supersedes: none

## D-2026-10-10-b  Municipal GDP from the SIDRA API in integer thousands of reais

Status: closed (phase 1, script 03, 03-controls-gdp.R). At the script 03 acceptance gdp_brl differs on 9,434 of the 9,534 non-missing cells, every difference at most R$ 500 (half the integer unit); the Santarém and Mojuí cells reproduce once the split (D-2026-10-10-e) runs on the same source; the derived columns follow
Scope: script 03 (PIB section; legacy input _data/raw_ibge_pib/pib_munic.csv); gdp_brl, gdp_per_capita, gdp_per_capita_log, gdp_brl_2024, gdp_per_capita_2024, gdp_per_capita_2024_log; raw/ibge/sidra/sidra_5938_*
Context: the lost PIB export carried decimals in the "mil reais" column; the
         SIDRA agregados API (and apisidra) serve Tabela 5938 variable 37 as
         integers. Re-acquired 2026-10-09 as 68 JSON extracts (seven
         variables, 2002-2023).
Reason: the author chose the API route over the FTP release workbook (rebuild/05-reacquisition-controls.md
        section 4, decision 2, 2026-10-09). gdp_brl reproduces the reference
        within R$ 1,000 on 9,521 of 9,532 cells; the 11 remaining cells are
        Santarém 2002-2012, the legacy Mojuí dos Campos split (D-2026-10-05
        seed entry). The rounding difference is accepted as a source
        version change and is not corrected; the gate reports it as drift.
Evidence: scratch/staging/ibge_sidra/check_controls_vs_reference.R
          (2026-10-09; to be reproduced in _audit/ibge_pib.qmd);
          provenance rows raw/ibge/sidra/sidra_5938_*
Affects: gdp_brl and the five derived GDP columns (all years)
Supersedes: none

## D-2026-10-10-c  Mayoral candidates keyed on sequencial_candidato; id_candidato_bd abandoned

Status: closed (phase 1, script 03, 03-controls-elections.R; author decision 2026-10-10 on the key). Implementation: votes are summed by ballot number (numero_candidato) and party within municipality, year and round, the legacy aggregation less the lost id, and elec_winner_id is the winner's smallest sequencial_candidato. The eight other elec_* columns and election_year reproduce the reference on all 10,542 cells, so the "9 residual cells" of the stage 5 spot-check were an artefact of that check; elec_winner_id differs on every cell by definition (60 reference NAs are now filled)
Scope: script 03 (electoral section, legacy input _data/raw-tse-results-2.csv, line 409 at tag pre-reorg); elec_enc, elec_mov, elec_winner_share, elec_n_candidates, elec_winner_party, elec_winner_id, elec_competitive, elec_uncontested, election_year and the _gov lags; raw/tse/
Context: the lost TSE file carried id_candidato_bd, the Base dos Dados
         cross-election person identifier, which the legacy script used as
         the candidate key and as elec_winner_id. Base dos Dados has since
         dropped the column from resultados_candidato_municipio_zona; the
         re-acquired pull (2026-10-09) carries sequencial_candidato (the TSE
         per-election candidate number) and numero_candidato.
Reason: the author confirmed (2026-10-09) that a cross-election person id
        is not needed by the analysis. Candidates are therefore keyed on
        sequencial_candidato within municipality, election year and turno;
        elec_winner_id changes meaning to a per-election identifier. Note
        for the audit: the source has 36 duplicate rows in 2000 and 2004
        (same municipality and ballot number, two sequencial_candidato,
        identical votes) that the legacy pipeline double-counted; phase 1
        reproduces the legacy count and phase 2 opens a separate entry to
        dedupe. 9 residual 2004 cells carry one candidate fewer than the
        reference (rows since removed upstream; the Dahis et al. 2026 file
        of mid-2024 agrees with the 2026 pull).
Evidence: provenance rows raw/tse/tse_mayoral_results_2000_2024.csv and
          raw/dahis-2026/bq_resultados_candidato_municipio_v2024.csv;
          _scripts/sql/2026-10-09-tse-mayoral-results.sql; spot-check 2,990
          of 3,007 municipality-elections reproduced as served, 2,998 when
          votes are summed by numero_candidato
Affects: elec_winner_id (definition); the other elec_* columns only through
         the duplicate rows (phase 2)
Supersedes: none

## D-2026-10-10-d  Electoral competitiveness: first round, ordinary elections, ENC, margin, 10-point threshold, uncontested coding

Status: closed (pre-rebuild; script 03 at tag pre-reorg, lines 487-534 and 583-606; refactored in phase 1, 03-controls-elections.R)
Scope: 03-controls-elections.R; elec_enc, elec_mov, elec_winner_share, elec_n_candidates, elec_winner_party, elec_winner_id, elec_competitive, elec_uncontested, election_year
Context: mayoral results are kept for the first round of ordinary elections
         only (second rounds and the supplementary elections of the file
         are dropped); votes are summed to one row per candidate; the
         effective number of candidates is 1 over the sum of squared vote
         shares, the margin of victory the first minus the second share,
         the winner's share and party those of the first; a race is
         competitive when the margin is below 0.10 and uncontested when it
         has one candidate, in which case the margin is NA, the ENC is 1
         and the competitive flag is NA. Values are stored from the
         election year and carried forward to the next election
         (D-2026-10-05-o, task 1.5); the carry-forward also overwrites the
         NA margin and flag of an uncontested race with the previous
         election's values, as inherited.
Reason: pre-rebuild definitions, reproduced; the first round keeps the
        measure comparable across municipalities with and without a
        second round. The window 2000-2020 is a constant (ELEC_YEARS); the
        2024 election already in the raw file enters with tasks 1.2 and
        1.5.
Evidence: _audit/tse.qmd Q1, Q3, Q4, Q5
Affects: the elec_* columns and election_year (all years)
Supersedes: none

## D-2026-10-10-e  Santarém's GDP and population before 2013 split with Mojuí dos Campos by the 2022 shares

Status: closed (pre-rebuild; script 03 at tag pre-reorg, lines 215-315; refactored in phase 1, 03-controls-gdp.R)
Scope: 03-controls-gdp.R; population, gdp_brl, gdp_brl_2024 and the per-capita columns of 1506807 and 1504752
Context: Mojuí dos Campos was installed in 2013 and has no GDP or census
         population before that year. For 2002-2012 the legacy script gives
         Mojuí a share of Santarém's gdp_brl, gdp_brl_2024 and population,
         each by the pair's 2022 share of that same variable, and reduces
         Santarém by the same amount; Mojuí's population gap 2013-2021 is
         then interpolated linearly between the allocated 2012 value and the
         2022 census. Santarém's 2013-2021 population keeps the unadjusted
         2010-2022 interpolation. The same device allocates PEA in script 02
         (D-2026-10-07-d); script 05 drops Mojuí from the panel
         (D-2026-10-05-l).
Reason: pre-rebuild choice, reproduced; the pair's totals are unchanged
        (assertion in the script).
Evidence: _audit/ibge_pib.qmd Q3
Affects: population, gdp_* of 1506807 and 1504752, 2002-2012 (Mojuí's
         population also 2013-2021)
Supersedes: none

## D-2026-10-10-f  Crop and cattle exposure weights: 2000-2002 means of the crop share and of the cattle density, over the 2025 municipal area

Status: closed (pre-rebuild; script 03 at tag pre-reorg, lines 866-884 and 974-983; refactored in phase 1, 03-controls-agro.R)
Scope: 03-controls-agro.R; crop_*_munic_area_share, cattle_per_km2, the interim weights crop_w_* and cattle_weight; through them the price indices
Context: planted area (ha) is converted to km2 and divided by the municipal
         area of the 2025 IBGE release, the only vintage in the raw layer,
         for every year; the share may exceed 1 where successive crops are
         planted in one year. Cattle density is heads per km2 of the same
         area. The weights are the 2000-2002 means of the share and of the
         density; a window with no observed value gives NA (the legacy CSV
         held NaN). Weights do not sum to one.
Reason: pre-rebuild choice, reproduced: the window predates the 2004
        policy period, so the weights are not a response to later prices
        or enforcement. Flag for phase 2 (task 1.8): the 2025 grid already
        carries Boa Esperança do Norte (MT, installed 2025), so its parent
        municipalities' areas are the post-split ones in every year.
Evidence: _audit/pam_ppm.qmd Q3, Q4
Affects: crop_*_munic_area_share, cattle_per_km2 (all years); price indices
Supersedes: none

## D-2026-10-10-g  SIDRA symbols in PAM, PPM and PIB: "-" is zero in the surveys, every other symbol is NA

Status: closed (pre-rebuild; script 03 at tag pre-reorg, lines 752-756, 931-936 and 42-46; refactored in phase 1, 03-controls-agro.R and 03-controls-gdp.R)
Scope: 03-controls-agro.R, 03-controls-gdp.R; crop_*, cattle_*, gdp_*
Context: the PAM (5457) and PPM (3939) extracts carry "-" and "..."; the
         PIB extract (5938) carries "..." and one negative value (Guamaré,
         RN, 2012, outside the sample). The legacy script read "-" as zero
         and "...", ".." and "X" as NA in PAM and PPM, and every symbol as
         NA in the PIB export. Census anchors follow a different rule
         (D-2026-10-07-e). IBGE's legend (Notas sheet of every SIDRA
         workbook in raw/ibge/): "-" absolute zero, not from rounding;
         "0" zero from rounding; "X" value suppressed to protect the
         informant; ".." not applicable; "..." not available, "the product
         was not surveyed in the municipality or the municipality did not
         exist in the survey year".
Reason: pre-rebuild rule, reproduced; it matches the legend for PAM and
        PPM (zero kept as zero, suppressed, not-applicable and unavailable
        as missing). For the PIB export the legacy also read "-" as NA,
        against the legend, but no PIB cell carries "-" and the raw-state
        assertion fails if one appears. Whether a "..." means not surveyed
        or not yet installed, the two cases IBGE's own definition joins, is
        settled in task 1.8.
Evidence: IBGE legend in raw/ibge/Ibge_pop_1970-80-91-00-10.xlsx and
          Ibge_pop_2022 (Censo - Sidra).xlsx, sheet Notas;
          _audit/pam_ppm.qmd Q1, Q2; _audit/ibge_pib.qmd Q1
Affects: crop_*_area_planted_km2, crop_*_munic_area_share, cattle_heads,
         cattle_per_km2, gdp_* (cells with a symbol)
Supersedes: none

## D-2026-10-10-h  Price indices: base-window weights times real prices relative to 2000; crop index lagged one year

Status: closed (pre-rebuild; script 03 at tag pre-reorg, lines 1033-1189; refactored in phase 1, 03-controls-prices.R)
Scope: 03-controls-prices.R; price_index_crop, price_index_cattle, price_index_crop_lag1
Context: the Pink Sheet real series (2010 US dollars) of Soybeans, Maize,
         Rice Thai 5%, Cotton A Index and Beef are divided by their 2000
         value; the crop index is the sum over the four crops of the
         base-window share (D-2026-10-10-f) times the relative price, with
         NA weights dropped, so a municipality whose four weights are NA
         reads 0; the cattle index is the base-window density times the
         relative beef price, NA where the density is NA; the crop lag is
         the previous year within the municipality. Window 2000-2020
         (PRICES_YEARS).
Reason: pre-rebuild construction, reproduced with its scale defect
        (task 1.3, D-2026-10-05-p).
Evidence: _audit/prices.qmd Q2, Q3, Q4, Q5
Affects: the three price columns (all years)
Supersedes: none

## D-2026-10-10-i  Municipal area stored for every municipality from the 2025 workbook; Santa Cruz do Arari keeps its area and its cattle index

Status: closed (phase 1, script 03)
Scope: 03-controls-agro.R (pam_ppm_munic_area_2025), 03-controls-prices.R (full join of the indices), script 04 (area join); munic_area_km2 and price_index_cattle of 1506401
Context: the reference has NA munic_area_km2, crop columns and both price
         indices for Santa Cruz do Arari (PA) in every year, although the
         areas workbook carries 1076.652 km2 and PPM carries its herd.
         Stage 5 read this as a join loss; the audit shows the cause: the
         municipality has no PAM Tabela 5457 series (5,564 of the 5,570
         municipalities have one; the other six are urban municipalities of
         RJ and SP), and the legacy panel took the area through the PAM
         table and the cattle index through a join on the crop index.
Reason: author decision 2026-10-10 (restore the value; phase 1 spec, case
        c: a reference value without derivation from raw). The area is
        read from the workbook for the 5,571 municipalities of the 2025
        grid and stored on its own; the price panel is a full join of the
        two indices. In the panel: munic_area_km2 = 1076.652 (21 cells, was
        NA) and price_index_cattle = 33.8178 x beef index (21 cells, was
        NA); the crop columns and crop index stay NA (no PAM series; the
        coding of absence is task 1.8).
Evidence: _audit/pam_ppm.qmd Q1, Q3, Q5; _audit/prices.qmd Q5;
          compare_outputs at the script 03 acceptance (munic_area_km2 21
          cells, price_index_cattle 21 reference-NA cells)
Affects: munic_area_km2, price_index_cattle of 1506401 (all years);
         cattle_per_km2 unchanged (the legacy density joined the area
         directly)
Supersedes: none

## D-2026-10-10-j  Task 1.10 at script 03: typed outputs; no coercion needed at source

Status: closed (phase 1, script 03)
Scope: 03-controls-agro.R, 03-controls-elections.R, 03-controls-gdp.R, 03-controls-prices.R; the 30 panel columns of script 03
Context: as for script 02 (D-2026-10-07-j), every numeric input parses
         clean: SIDRA symbols under D-2026-10-10-g, the Pink Sheet units
         row skipped by position, TSE votes as digits. The interim outputs
         set geocode, elec_winner_party and elec_winner_id as character;
         year, elec_n_candidates, elec_total_votes, elec_competitive,
         elec_uncontested and election_year as integer; the rest as double.
         All-NA base windows are stored as NA where the legacy CSV held
         NaN (same missingness).
Reason: the character storage of the reference is a write-side artefact,
        as found for scripts 01 and 02.
Evidence: output assertions of the four scripts; compare_outputs at the
          script 03 acceptance, no type mismatch on 30 columns
Affects: none (types of the interim outputs)
Supersedes: none

## D-2026-10-10-k  Population stored only for the GDP years 2002-2022

Status: open (tasks 1.6 and 1.7; phase 2 steps 2.5 and 2.6)
Scope: 03-controls-gdp.R; population
Context: the population series is interpolated on 2000-2022 from the three
         census anchors, joined onto the GDP rows (2002 onward) and capped
         at the last census, so the panel carries population NA in 2000 and
         2001 for every municipality although the 2000 anchor exists
         (stage 5 finding, rebuild/05-reacquisition-controls.md section 5).
         The nine post-2000 MT municipalities are NA through 2009
         (D-2026-10-05-k).
Reason: inherited storage, reproduced in phase 1; phase 2 stores
        population for 2000-2024 (official estimates per task 1.6, anchors
        per task 1.7) independently of the GDP years.
Evidence: _audit/ibge_pib.qmd Q4, Q6
Affects: population 2000-2001 (all municipalities); bf per-capita columns
         of script 04 in those years
Supersedes: none

## D-2026-10-11-a  Panel assembled by left joins onto the MapBiomas biome sample, 2000-2020

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 170-204; refactored in phase 1, 04-panel-assemble.R)
Scope: 04-panel-assemble.R; every panel column; N_MUNIC_EXPECTED = 503
Context: the legacy script joined the per-source outputs onto the
         deforestation table with left joins on geocode and year (the
         municipal area on geocode only) and kept 2000-2020. Municipalities
         absent from a source carry NA in that source's columns; sources
         with municipalities outside the biome sample lose them at the join.
Reason: the MapBiomas sample (D-2026-10-06-c) defines the panel's rows;
        the outcome must exist for every row, the covariates need not. The
        panel keeps Mojuí dos Campos (503 rows per year); script 05 drops it
        (D-2026-10-05-l), so the reference has 502.
Evidence: _audit/panel.qmd Q1, Q7
Affects: all columns; row count 503 x 21 = 10,563 (reference 10,542)
Supersedes: none

## D-2026-10-11-b  ppcdam_list 0 where the list does not cover the municipality-year; listed municipalities outside the biome sample leave the panel

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 261-269; refactored in phase 1, 04-panel-assemble.R)
Scope: 04-panel-assemble.R; ppcdam_list
Context: the list file (01-env-ppcdam.R) carries the 92 ever-listed
         municipalities for 2004-2024. After the left join, 2000-2003 and
         every never-listed municipality are NA; the legacy script recoded
         NA to 0. Four listed municipalities are not in the biome sample
         and leave the panel: Grajaú (MA, listed from 2011) and Comodoro,
         Paranatinga and Ribeirão Cascalheira (MT, listed 2023-2024 only).
Reason: a municipality-year the list does not name is not listed; the
        indicator must be 0/1 for every row. Grajaú returns with the Legal
        Amazon extension (task 1.1).
Evidence: _audit/panel.qmd Q2
Affects: ppcdam_list (0 for 2000-2007 and for all never-listed rows)
Supersedes: none

## D-2026-10-11-c  Forest and deforestation areas converted from hectares to km2 at the join

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 41-63; refactored in phase 1, 04-panel-assemble.R)
Scope: 04-panel-assemble.R (HA_PER_KM2); forest_area_km2, deforestation_area_km2
Context: 01-env-deforestation.R stores the MapBiomas aggregates in
         hectares as exported; every other area in the panel (municipal
         area, crop areas, cattle density) is in km2.
Reason: one unit for every area column. Inherited placement: the
        conversion belongs upstream with the unit pattern (task 1.9).
        Nova Ubiratã (MT) 2000-2003 has a converted forest area above its
        2025 municipal area (up to 18 percent): MapBiomas Collection 9
        maps the pre-split municipality (12,461 km2) while the 2025 area
        (8,721 km2) excludes Boa Esperança do Norte, the mesh-vintage
        mismatch of D-2026-10-10-f; quantified in task 1.8.
Evidence: _audit/panel.qmd Q3
Affects: forest_area_km2, deforestation_area_km2 (all rows)
Supersedes: none

## D-2026-10-11-d  Price indices z-scored on the pooled panel; level and lag share one centre and spread; the cattle lag is built from the z-score

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 457-491; refactored in phase 1, 04-panel-assemble.R); flagged for task 2.2
Scope: 04-panel-assemble.R; price_index_crop_z, price_index_crop_lag1_z, price_index_cattle_z, price_index_cattle_lag1_z
Context: the crop and cattle indices sit three orders of magnitude apart
         (D-2026-10-05-p). The legacy script standardised each over all
         municipality-years of the 503-municipality panel (na.rm), scaled
         the crop lag with the level's mean and sd, and built the cattle
         lag as the within-municipality lag of the cattle z-score.
Reason: inherited; reproduced for equivalence. The pooled standardisation
        is sample-dependent and therefore fails the governing rule for
        derived variables; its removal from the panel is decided at the
        storage audit (task 2.2), not in phase 1. Phase 1 drift: the Pink
        Sheet release (D-2026-10-10-a) passes through, and Santa Cruz do
        Arari's restored cattle index (D-2026-10-10-i) enters the pool
        and shifts every cattle z-score by at most about 0.004 sd.
Evidence: _audit/panel.qmd Q4; compare_outputs at the script 04 acceptance
Affects: the four z-score columns (all rows)
Supersedes: none

## D-2026-10-11-e  Electoral block lagged one year within municipality (_gov); election_year not lagged

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 495-517; refactored in phase 1, 04-panel-assemble.R)
Scope: 04-panel-assemble.R (ELEC_GOV_VARS, ELEC_GOV_LAG); the eight elec_*_gov columns
Context: the stored elec_* columns change in the election year
         (D-2026-10-05-o, open). The legacy script added a one-year lag of
         the eight measures with the _gov suffix so each calendar year
         carries the result of the mayor in office; 2000 is NA (the 1996
         election is outside the data). election_year flags when an
         election happened and is not lagged.
Reason: inherited; the lag compensates for the storage defect of task 1.5.
        When step 2.3 moves the storage to the governing period, the _gov
        block becomes redundant and its fate is decided there.
Evidence: _audit/panel.qmd Q5
Affects: elec_enc_gov, elec_mov_gov, elec_winner_share_gov,
         elec_n_candidates_gov, elec_winner_party_gov, elec_winner_id_gov,
         elec_competitive_gov, elec_uncontested_gov
Supersedes: none

## D-2026-10-11-f  bf_transfers_pc_brl_2024 = transfers / population, NA where population is missing or zero

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 529-536; refactored in phase 1, 04-panel-assemble.R)
Scope: 04-panel-assemble.R; bf_transfers_pc_brl_2024
Context: the methodology sets total population (or the quota) as the
         primary transfer-intensity denominator (D-2026-10-05-e); script 02
         builds the PEA- and quota-normalised versions, and the per-capita
         version needs the population series of script 03, so it is built
         at the join.
Reason: the variable needs an upstream join and is sample-independent, so
        it is stored. NA where transfers (before 2004, after 2019) or
        population (2000-2001; the nine post-2000 MT municipalities through
        2009, D-2026-10-05-k) are missing.
Evidence: _audit/panel.qmd Q6
Affects: bf_transfers_pc_brl_2024 (2004-2019)
Supersedes: none

## D-2026-10-11-g  Columns not carried into the panel at the join

Status: closed (pre-rebuild; script 04 at tag pre-reorg, lines 22-30 and 342-356; refactored in phase 1, 04-panel-assemble.R)
Scope: 04-panel-assemble.R; interim columns absent from the panel
Context: the interim files carry columns the legacy join dropped:
         bf_ref_month (the February rows and the month marker,
         D-2026-10-05-a), biome, deforestation_mean, deforestation_sd and
         deforestation_z (D-2026-10-06-e), the eight workers_* counts and
         pia (02-social-rais.R), elec_total_votes (03-controls-elections.R),
         and the base-window weights file (D-2026-10-10-f) in full.
Reason: inherited selection; the panel is the reference's 73 columns.
        Whether the counts and anchors enter the panel is decided by tasks
        1.7 and 1.8 and by the storage audit (task 2.2); D-2026-10-07-i
        records the author's intent on the PEA-normalised rates.
Evidence: _audit/panel.qmd Q1
Affects: no panel column; interim files unchanged
Supersedes: none

## D-2026-10-11-h  Task 1.10 at script 04: typed output; the xlsx copy retired

Status: closed (phase 1, script 04)
Scope: 04-panel-assemble.R (PANEL_INT_COLS, PANEL_CHR_COLS); every panel column
Context: the legacy script wrote csv, rds and xlsx without setting types;
         the reference csv therefore carries integers and doubles as the
         writer rendered them. The refactored script fixes nine integer
         columns (year, ppcdam_list, four electoral flags and counts and
         their three _gov lags), eight character columns (keys, names,
         party and candidate ids and their lags) and doubles for the rest,
         and asserts them before writing. The xlsx copy is not produced.
Reason: the type contract of rebuild/04-codebook-and-decisions.md; the
        comparator reports no type mismatch on the 71 compared columns.
Evidence: compare_outputs at the script 04 acceptance
Affects: column classes of the interim panel; no value
Supersedes: none

## D-2026-10-12-a  Baseline summaries over 2000-2003; NA, never NaN, where a source has no baseline value

Status: closed (pre-rebuild; script 05 at tag pre-reorg, lines 68-86; refactored in phase 1, 05-sample-definition.R)
Scope: 05-sample-definition.R (SAMPLE_BASE_YEARS); panel_amz_baseline: defor_area_b, defor_rate_b, cattle_dens_b, crop_share_b, forest_share_2000, forest_km2_2000
Context: the legacy script averaged clearing area, clearing rate, cattle
         density and the summed share of the four crops over 2000-2003 per
         municipality (na.rm) and took the 2000 forest share (forest over
         the municipal area) and forest area, as the pre-treatment
         characteristics the sample rule and the pressure scores read.
         Every row of the municipality file is pre-PPCDAm by
         construction. The legacy mean of an all-NA window returned NaN,
         which the csv writer rendered as NA.
Reason: inherited; the window predates the plan (2004) and the list
        (2008). The refactor returns NA explicitly for an all-NA window so
        the typed file carries no NaN; the csv is unchanged. Three
        municipalities have no agricultural baseline: Ipiranga do Norte
        and Itanhangá (MT) have no PAM/PPM rows before their first survey
        year (D-2026-10-10-g) and Santa Cruz do Arari (PA) has no crop
        series (D-2026-10-10-i). Nova Ubiratã's forest share and cattle
        density are computed on the post-split 2025 area (task 1.8).
        The municipality file is the store task 1.7 names muni_baseline;
        the census anchors join it at phase 2 step 2.6.
Evidence: _audit/panel-sample.qmd Q1, Q2
Affects: the six baseline columns of panel_amz_baseline (502 rows);
         through them every column of D-2026-10-12-b…d
Supersedes: none

## D-2026-10-12-b  Treatment flags defined on the analysis window: main 2004-2010, robust 2004-2012, ever over the full panel

Status: closed (pre-rebuild; script 05 at tag pre-reorg, lines 90-104; refactored in phase 1, 05-sample-definition.R); flagged for task 2.2
Scope: 05-sample-definition.R (PPCDAM_WINDOW_START, PPCDAM_WINDOW_END_MAIN, PPCDAM_WINDOW_END_ROBUST); ppcdam_main, ppcdam_robust, ppcdam_ever_full
Context: a municipality counts as treated in a design if it was listed in
         any year of that design's window; one first listed after the
         window is a not-yet-treated control there. The main window
         (2004-2010) holds the 2008 and 2009 cohorts (36 + 7 = 43) with
         no exit; the robust window adds the 2011-2012 cohorts (51) and
         the first exits; 61 municipalities are ever listed over
         2000-2020.
Reason: inherited; positivity diagnostics and the stratified estimates
        must use the same treated set as the design they serve. The three
        flags are stored as integers 0/1 (the legacy ppcdam_ever_full was
        a double). The window is a design parameter varied in robustness
        checks, so whether the flags stay in the panel or move to the
        analysis scripts is decided at the storage audit (task 2.2).
Evidence: _audit/panel-sample.qmd Q3
Affects: ppcdam_main, ppcdam_robust, ppcdam_ever_full (constant within
         municipality)
Supersedes: none

## D-2026-10-12-c  Pressure scores: logged baselines z-scored across the 502 municipalities; pressure_agri a half-half mix, not re-standardised

Status: closed (pre-rebuild; script 05 at tag pre-reorg, lines 106-122; refactored in phase 1, 05-sample-definition.R); flagged for task 2.2
Scope: 05-sample-definition.R; pressure_defor, pressure_agri, pressure_score
Context: pressure_defor = z(log1p(defor_area_b)); pressure_agri =
         0.5 z(log1p(cattle_dens_b)) + 0.5 z(log1p(crop_share_b));
         pressure_score = z(pressure_defor + pressure_agri), each z over
         the 502 municipalities (na.rm). pressure_agri and
         pressure_score are NA for the three municipalities without an
         agricultural baseline (D-2026-10-12-a).
Reason: inherited; reproduced for equivalence (exact against the
        reference). A cross-sectional standardisation is sample-dependent
        and therefore fails the governing rule for derived variables
        (restandardising on the broad at-risk sample moves the scores by
        up to several tenths of a standard deviation); removal from the
        panel is decided at the storage audit (task 2.2), with
        D-2026-10-11-d.
Evidence: _audit/panel-sample.qmd Q7
Affects: the three pressure columns (constant within municipality)
Supersedes: none

## D-2026-10-12-d  Santa Cruz do Arari passes the availability floor with its restored area

Status: closed (phase 1, script 05)
Scope: 05-sample-definition.R (N_AVAIL_EXPECTED = 500); avail_ok of 1506401
Context: the recovered panel has no municipal area for Santa Cruz do Arari
         (restored at script 03, D-2026-10-10-i), so its 2000 forest share
         could not be computed there; the reference nonetheless carries
         avail_ok = FALSE for it, a value with no derivation from a missing
         share (NA >= 0.10 is NA). With the restored area the share is
         0.94 and the flag is TRUE; with zero baseline clearing the
         municipality stays outside both at-risk gates, as in the
         reference.
Reason: triage class (c) of rebuild/00-rebuild-phases.md: the reference
        value has no derivation from raw; the rebuilt value is accepted
        with this entry and the comparator report.
Evidence: _audit/panel-sample.qmd Q8; compare_outputs at the script 05
          acceptance (21 cells of avail_ok, one municipality)
Affects: avail_ok (500 TRUE against the reference's 499); no at-risk
         flag, no pressure score
Supersedes: none

## D-2026-10-12-e  Task 1.10 at script 05: typed output; the diagnostics leave the pipeline; the municipality lookup moves to _data/interim/

Status: closed (phase 1, script 05)
Scope: 05-sample-definition.R (SAMPLE_LGL_COLS, SAMPLE_INT_COLS, MUNI_COLS); panel_amz_2000_2020_sample, panel_amz_baseline
Context: the legacy script wrote final_dataset_5.csv untyped, and two
         tables under _output/: at_risk_indicator.csv (the municipality
         lookup) and at_risk_balance.csv (the balance table of the main
         window); it printed the retention, balance and overlap
         diagnostics for both windows to the console with the appendix
         prose in comments.
Reason: the type contract of rebuild/04-codebook-and-decisions.md: the
        three sample flags are logical, the three treatment flags
        integer, the pressure scores double, asserted before writing.
        The diagnostics are evidence, not data, so they live in the
        rendered audit notebook (_audit/panel-sample.qmd Q4-Q6) and
        the pipeline writes no table under output/. The municipality
        lookup is an interim dataset (panel_amz_baseline, the task
        1.7 store) and carries forest_share_2000 and defor_area_b, which
        the legacy panel carried and legacy script 06 dropped; the panel
        output holds the 73 script 04 columns plus the nine the reference
        carries from this script, so script 06 has nothing to drop. The
        prose is parked in _annex/parked/05-sample-definition-prose.md.
Evidence: compare_outputs at the script 05 acceptance (no type mismatch
          on the 80 compared columns)
Affects: column classes and the file layout of the script 05 outputs; no
         value
Supersedes: none

## D-2026-10-12-f  Normalized outcome: within-municipality z on a pre-treatment reference, main 2000-2007 and short 2000-2003; a degenerate reference sd gives NA

Status: closed (phase 1, script 06); flagged for task 2.2
Scope: 06-normalized-outcome.R (DEFOR_REF_MAIN_YEARS, DEFOR_REF_SHORT_YEARS, DEFOR_SD_FLOOR); defor_norm_ref0007, defor_norm_ref0003
Context: the annual clearing increment varies across municipalities by
         orders of magnitude; the legacy script centred and scaled it on
         each municipality's own mean and sd over 2000-2007 (eight years
         before the first listing) and, as the short alternative, over
         2000-2003 (before PPCDAm), and set the outcome to NA where the
         reference sd was at or below 1e-9.
Reason: inherited as is (phase 1 reproduces). The scale is set on
        pre-policy years so that it carries no effect of the policy whose
        effect is estimated; the centre is immaterial under municipality
        fixed effects (_audit/panel-outcome.qmd Q4, numerical identity).
        A municipality without reference variation cannot be normalised:
        Santa Cruz do Arari (PA, no clearing over 2000-2007) under both
        windows and Afuá (PA) under the short one, 21 cells each per
        window, both outside the at-risk samples. The column names are
        generated from the window constants and tied by assertion. The
        reference window is a robustness parameter, like the treatment
        windows (D-2026-10-12-b): the storage audit (task 2.2) decides
        whether both columns stay in the panel.
Evidence: _audit/panel-outcome.qmd Q2-Q4; compare_outputs at the script 06
          acceptance (defor_norm_ref0003 and defor_norm_ref0007 exact, max
          abs diff 4.6e-13)
Affects: defor_norm_ref0003 (42 NA cells), defor_norm_ref0007 (21 NA
         cells); no value changed
Supersedes: none (detail of D-2026-10-05-h)

## D-2026-10-12-g  Exposure offset: log of the previous year's forest area, NA in the first panel year

Status: closed (phase 1, script 06)
Scope: 06-normalized-outcome.R; log_forest_exposure
Context: the Poisson specifications take the clearing area in levels and
         need the stock at risk as an offset; the legacy script used the
         forest standing at the start of the year, the lag of
         forest_area_km2, logged.
Reason: inherited as is. Forest area is positive in every cell (minimum
        about 35 km²), so the log is defined wherever the lag exists; the
        first panel year has no lag and is NA (502 cells), which drops one
        year per municipality from the Poisson sample and no municipality.
Evidence: _audit/panel-outcome.qmd Q1, Q5; compare_outputs at the script 06
          acceptance (exact, max abs diff 5.3e-15)
Affects: log_forest_exposure
Supersedes: none

## D-2026-10-12-h  Task 1.10 at script 06: typed final output; the diagnostics leave the pipeline; helper columns never stored

Status: closed (phase 1, script 06)
Scope: 06-normalized-outcome.R (OUTCOME_COLS); _data/final/panel_amz_2000_2020_final
Context: the legacy script read final_dataset_5.csv, wrote
         final_dataset_6.csv untyped, printed the degenerate-sd count and
         a quantile table on the broad sample to the console, and dropped
         its helper columns together with deforestation_z,
         forest_share_2000 and defor_area_b, which script 05 no longer
         carries on the panel (D-2026-10-12-e).
Reason: the three outcome columns are double, asserted before writing;
        the final panel is written to _data/final/ under the content-first
        name of docs/repo-conventions.md with the _final token (author's
        choice, 2026-10-12), in the reference's column order (the 82
        sample-panel columns followed by defor_norm_ref0003,
        defor_norm_ref0007 and log_forest_exposure). The diagnostics live
        in _audit/panel-outcome.qmd; the helper mean and sd columns are
        computed inside a function and never join the panel. The prose is
        parked in _annex/parked/06-normalized-outcome-prose.md. The price
        re-anchoring that D-2026-10-05-p assigns to this script is a phase
        2 fix and is listed under KNOWN DEFECTS until then.
Evidence: compare_outputs at the script 06 acceptance (same column order
          as the reference; no type mismatch on the 83 compared columns)
Affects: column classes and the file layout of the final panel; no value
Supersedes: none

## D-2026-10-12-i  Phase 1 gate passed: the rebuilt pipeline reproduces the recovered panel on every recovered-raw column; re-acquired drift closed under version entries; the phase 1 panel is the working reference

Status: closed (phase 1 gate, 2026-10-12)
Scope: scripts 01-06 (12 files); _data/final/panel_amz_2000_2020_final; _data/reference/panel_amz_2000_2020_final_phase1
Context: rebuild/00-rebuild-phases.md, phase 1 acceptance, second tier:
         after script 06 the full pipeline runs and the final panel is
         compared with compare_outputs.R against the recovered
         dataset_final_v6, read by column class.
Reason: the run. The 12 scripts (01-env-deforestation, 01-env-ppcdam,
        02-social-census, 02-social-rais, 02-social-bf, 03-controls-agro,
        03-controls-elections, 03-controls-gdp, 03-controls-prices,
        04-panel-assemble, 05-sample-definition, 06-normalized-outcome)
        ran in order from the repository root (Rscript, renv, R 4.6.1,
        dplyr 1.2.1, readr 2.2.0, tidyr 1.3.2, purrr 1.2.2, readxl
        1.5.0.1, jsonlite 2.0.0), every exit 0, 51 seconds; the 36
        interim and final files are byte-identical (md5) to the outputs of
        the per-script sessions of 2026-10-06 to 2026-10-12.
        The comparison. 85 columns, 10,542 joined rows, same column order,
        no row only in one file, no type mismatch on the 83 compared
        columns (tol 1e-8).
          Recovered-original raw, 19 columns, all exact: municipality,
          state, state_abbreviation (IBGE biome list); forest_area_km2,
          deforestation_area_km2, deforestation_forest_rate (MapBiomas
          Collection 9); ppcdam_list (MMA ordinances); the 12 emp_*
          (RAIS BigQuery export with the recovered census PIA).
          Re-acquired sources, 52 columns, 33 exact: the seven bf_*
          quota, transfer and per-capita columns (MDS, D-2026-10-06
          series), population, the ten crop_* and cattle_* (PAM/PPM), the
          15 elec_* and election_year other than the winner id (TSE).
          Nineteen drift, each under a closed version entry:
            census (D-2026-10-07-b, D-2026-10-07-c): pea 10,334 cells,
              502 municipalities, max 26,600 persons (the 2010 anchor);
              informal 10,532 cells, max 0.20 on the 0-100 scale;
              bf_transfers_pea_brl_2024 8,016 cells, max R$ 16 (pea
              denominator).
            ibge_pib (D-2026-10-10-b): gdp_brl and five derived, 9,301 to
              9,434 cells, 502 municipalities, gdp_brl within R$ 500
              (integer API unit), gdp_brl_2024 within R$ 1,740, the logs
              within 6.2e-5.
            prices (D-2026-10-10-a): price_index_crop, _lag1 within
              1.1e-3 (0.19 percent); price_index_cattle within 0.27 (0.15
              percent); the four z-scores within 1.8e-2; 8,672 to 10,521
              cells, 483 to 501 municipalities.
            tse (D-2026-10-10-c): elec_winner_id and _gov redefined on
              sequencial_candidato, 10,526 and 10,024 cells, not a
              numeric drift.
            ibge_areas (D-2026-10-10-i): munic_area_km2 of Santa Cruz do
              Arari restored, 21 cells (reference NA).
          Derived, 12 columns, 11 exact: pressure_defor/agri/score,
          at_risk_broad/narrow, ppcdam_main/robust/ever_full,
          defor_norm_ref0003/0007, log_forest_exposure. avail_ok differs
          on the 21 cells of Santa Cruz do Arari, triage class (c): the
          reference carries FALSE with no derivation from raw
          (D-2026-10-12-d).
        The verdict. No recovered-raw column differs, so no triage of
        class (a) or (b) arises; the one class (c) discrepancy has its
        notebook evidence and entry; every drifting source has its
        version entry. The gate passes. Known defects carried into phase
        2 under KNOWN DEFECTS: tasks 1.3 (prices, script 06), 1.4 (quota,
        02-social-bf), 1.5 (electoral timing, 03-controls-elections), 1.6
        (post-2000 municipalities), 1.8 (NA versus zero, area vintage),
        1.9 (units) and 2.2 (storage audit flags).
        The working reference. The phase 1 panel
        _data/final/panel_amz_2000_2020_final (.csv sha256 16a40cae…76a1,
        10,840,692 bytes; .rds sha256 0f427112…0dde, 7,050,897 bytes) is
        frozen by the author in _data/reference/ as
        panel_amz_2000_2020_final_phase1.csv/.rds (provenance rows added);
        phase 2 fixes compare against it, never against dataset_final_v6,
        which stays as the pre-rebuild oracle.
Evidence: scratch gate report (compare_outputs by column class) in this
          entry and the gate commit; _audit/*.qmd for each source
Affects: none (acceptance record)
Supersedes: none

## D-2026-10-15-a  Atlas Brasil vintages joined by geocode: shares rescaled to 0-1, income kept at its 2010 base and also in 2024 reais, urban share from the census counts

Status: closed (descriptive sprint, day 3)
Scope: 09-descriptive-data.R (ATLAS_PCT_TO_SHARE, ATLAS_INCOME_BASE); _data/interim/atlas_indicators_1991_2010; the *_c2000 and *_c2010 columns of _data/final/panel_amz_2000_2020_descriptive
Context: the sprint plan (rebuild/06-descriptive-sprint.md section 5)
         re-acquires Atlas Brasil as censo_total_1991_2010.xlsx with the
         seven-digit code, retiring the legacy name match and alias table
         of 07-data-analysis-EDA-poverty.R (_audit/atlas.qmd Q2, Q3).
Reason: PIND, PMPOB and PPOB are served on 0-100 and the naming
        convention admits shares on 0-1 only, so the three are divided
        by 100 (poverty_extreme_share, poverty_share,
        vulnerability_share); RDPC is in reais of 1 August 2010 (Siglas)
        and is stored as income_pc_brl_2010 and, multiplied by the
        annual 2010 IPCA factor of deflator_ipca_2024.csv, as
        income_pc_brl_2024, an approximation of the monthly anchor by the
        annual factor that the codebook records; urban_share is
        pesourb / pesotot, under the administrative census definition
        of urban whose limitation task 4.9 addresses; atlas_population
        is pesotot, kept beside, never in place of, the panel's census
        anchors. The panel carries the 2000 and 2010 vintages as
        time-invariant columns suffixed _c2000 and _c2010 (naming
        convention section 3.6); 1991 stays in the interim file. Names
        follow the panel's current (pre-rename) style and migrate with
        scripts 09-11 at phase 2R.
Evidence: _audit/atlas.qmd Q4-Q6; script assertions (502 sample
          municipalities present in all three vintages, shares on 0-1)
Affects: new columns only; no panel value
Supersedes: none

## D-2026-10-15-b  Value added by activity from SIDRA 5938: land-based dependence is the agropecuária share, Santarém undivided before 2013

Status: closed (descriptive sprint, day 3)
Scope: 09-descriptive-data.R (VA_VARIABLES, BRL_PER_MIL); _data/interim/ibge_pib_value_added_2002_2023; the va_* columns of the descriptive panel
Context: task 4.1 asks for the weight of agriculture, livestock, mining
         and logging in the local economy; the 5938 extracts of
         2026-10-09 carry value added by four activities (variables 513,
         517, 6575, 525) and the total (498), integer mil reais.
Reason: the only activity split IBGE publishes at municipal level puts
        agriculture, livestock, forestry (logging included) and fishing
        under agropecuária and mining inside industry, so the
        land-based measure is va_agro_share = va_agro / va_total, with
        the mining caveat stated in the chapter; the four shares and the
        nominal and 2024-reais levels are stored for all 5,570
        municipalities 2002-2023. Unlike script 03 (D-2026-10-10-e), the
        Santarém series before 2013 is not split with Mojuí dos Campos:
        shares are invariant to a proportional split and Mojuí is outside
        the sample, so Mojuí is NA before 2013 and Santarém covers both.
        Negative values served by IBGE (one agro cell, 42 industry cells,
        none in the sample) are kept as served, as for GDP
        (D-2026-10-10-b).
Evidence: _audit/ibge_pib.qmd Q1 holds for the value-added files (same
          symbols, 5,570 series); script assertions (components sum to
          the total within rounding)
Affects: new columns only
Supersedes: none

## D-2026-10-15-c  SICONFI expenditure by function: stage empenhadas, functions from 2002, published total from 2004, absent function in a reporting year is zero, gaps never imputed, state series 2013-2024

Status: closed (descriptive sprint, day 3)
Scope: 09-descriptive-data.R (EXP_STAGE, EXP_FUNCTION_YEARS, EXP_BY_NAME_YEARS, EXP_FUNCTIONS, EXP_TOTAL_NAMES, EXP_UF_YEARS); _data/interim/siconfi_expenditure_munic_2000_2024, siconfi_expenditure_uf_2013_2024; the exp_* columns of the descriptive panel
Context: task 4.5 (supervisor request of 2026-10-13); extracts of
         2026-10-15 from basedosdados.br_me_siconfi, SQL in
         _scripts/sql/2026-10-15-siconfi-extraction.sql.
Reason: (1) Despesas Empenhadas is the only stage before 2013 and runs
        through the FINBRA-to-SICONFI break, so it is the stage for every
        year (liquidadas and pagas exist from 2013 only). (2) 2000-2001
        use the pre-Portaria-42 classification (Saúde e Saneamento,
        Educação e Cultura, Assistência e Previdência), which does not
        map onto health, education, social assistance, social security
        and environmental management, so the function columns start in
        2002 and 2000-2001 keep the total only. (3) Function rows are
        read by the Base dos Dados id 3.FF.000 from 2004 and by name in
        2002-2003, when no id exists; the total is the published total
        row from 2004 (Despesas por Função to 2012; the exceto
        intraorçamentárias total under its three spellings from 2013;
        the duplicate Total Geral da Despesa por Função of 2010-2012 is
        not read) and the sum of the 28 function rows in 2002-2003, the
        function rows summing to the published total within one percent
        in every later year. (4) A function absent from a
        municipality-year that has a total is zero, because the SICONFI
        layout lists non-zero functions only, while a municipality-year
        without a total is a reporting gap kept as NA: 388 to 500 of the
        502 sample municipalities report in a given year and nothing is
        imputed. (5) exp_social is health plus education plus social
        assistance. (6) Values are deflated with the annual IPCA factor
        (D-2026-10-05-c) and divided by the interpolated census
        population of script 03 (2002-2022; NA outside), the state
        population being the sum of its municipalities. (7) The Base dos
        Dados state table begins in 2013, so the state series is
        2013-2024; the FINBRA state files 2000-2012 are an open follow-up
        under task 4.5. Column names follow the panel's current style
        (exp_<function>_brl, _brl_2024, _pc_brl_2024) and migrate at
        phase 2R.
Evidence: _audit/siconfi.qmd Q1-Q5, Q7
Affects: new columns only
Supersedes: none

## D-2026-10-15-d  SICONFI municipal revenue: items read at the parent level of each account plan, FINBRA names to 2012 and Tesouro codes from 2013, gross of deductions, absent item in a reporting year is zero

Status: closed (descriptive sprint, day 3)
Scope: 09-descriptive-data.R (REV_STAGE, REV_RULES, REV_TRANSFER_ITEMS); _data/interim/siconfi_revenue_munic_2000_2024; the rev_* columns of the descriptive panel
Context: task 4.5 asks for the federal transfers that fund social
         expenditure (FPM and the conditioned SUS, FUNDEB, FNDE and
         FNAS transfers, in the classification of Alcaraz 2024); the
         account names change at 2002, 2004, 2013, 2018, 2019 and 2022,
         the Base dos Dados id is filled for eight percent of revenue
         rows, and the same display name recurs within a
         municipality-year under current and capital transfers.
Reason: the stage is Receitas Brutas Realizadas (gross, before the
        FUNDEB and other deductions, which are separate stages). Up to
        2012 the FINBRA names are unique within a municipality-year and
        are read directly; from 2013 the Tesouro account code (column
        portaria) is unique within a municipality-year and the rules
        name, for each item and period, the parent-level code of the
        current-revenue branch, so that children never add to a parent
        (the parents equal the sum of their children wherever both
        exist, _audit/siconfi.qmd Q6). The rule table is REV_RULES in
        the script: total (Rec Orçamentária, Total Receitas, TOTAL DAS
        RECEITAS), current, current transfers, Union transfers, tax;
        FPM (Cota FPM; 1.7.2.1.01.02-04; the three 1.7.1.8.01 quotas of
        2018-2021, when no parent row exists; 1.7.1.1.51.0.0); SUS from
        the Union (SUS União; 1.7.2.1.33; 1.7.1.8.03 plus the 2019-2021
        investment block 1.7.1.8.04; 1.7.1.3.00.0.0); FUNDEF/FUNDEB
        including the Union complement (Fundef União and Estado; Transf
        Multigov FUNDEF/FUNDEB and Comp; 1.7.2.4.01 and .02; 1.7.5.8.01
        plus 1.7.1.8.09; 1.7.5.1 plus 1.7.1.5); FNDE including
        salário-educação (FNDE and Cota Salário Educação; 1.7.2.1.35 and
        .01.30; 1.7.1.8.05; 1.7.1.4); FNAS (FNAS; 1.7.2.1.34;
        1.7.1.8.04 in 2018 and 1.7.1.8.12 in 2019-2021; 1.7.1.6). An
        item absent from a municipality-year that has a total row is
        zero; FNDE and FNAS have no line in the 2000-2001 layout and are
        NA there. The published totals are net of the FUNDEF/FUNDEB
        deductions taken from 2002 while the transfer lines are gross,
        so 120 to 342 municipalities a year in 2002-2012 (and 24 in
        2024) carry current transfers above the total; the script
        therefore asserts FPM against current transfers. The 2018 transition layout reports the SUS
        and FNAS parents for fewer municipalities (zero shares of 15
        percent that year), shown in the audit and not corrected. The
        author checked the table against Alcaraz (2024, Quadro 2) on
        2026-10-15: SUS, FUNDEB with the other FNDE lines and FNAS are
        its conditioned transfers (TC), FPM is FPM, and its total (RT)
        is current revenue, carried here as rev_current; its own revenue
        (AP, current revenue except transfers) is added as rev_own, the
        difference between the current total and current transfers,
        negative in 30 municipality-years (28 in 2024) for the same
        net-versus-gross reason and left as is. Its devolutive (ICMS,
        IPVA, ITR, royalties) and other transfers are not extracted.
Evidence: _audit/siconfi.qmd Q1, Q6, Q7
Affects: new columns only
Supersedes: none

## D-2026-10-16-a  Descriptive groups: listing status, baseline terciles on the 2004-07 window, and urban classes at 0.5 and 0.75 of the 2010 urban share

Status: closed (descriptive sprint, day 4)
Scope: 10-descriptive-figures.R (BASELINE_YEARS, END_YEARS, DECADE_START, DECADE_END, MIN_BASELINE_REPORTS, URBAN_BREAKS, tercile()); displays D3-D6 and the chapter's grouped tables; script 11 reuses the groups
Context: the sprint's grouped trajectories, structure cross-tab and
         long differences need municipal groups that are fixed before
         the enforcement period and do not depend on the outcome.
Reason: listing status is ever-listed (any ppcdam_list == 1 in
        2000-2020, 61 municipalities) against never listed (441).
        Terciles are cut on the full sample at the 1/3 and 2/3
        quantiles of three baseline characteristics: municipal social
        spending per capita (health, education and social assistance,
        2024 reais) averaged over 2004-2007, the pre-listing window of
        the moderator; the 2000 census poverty rate (Atlas Brasil,
        poverty_share_c2000); and the agropecuária share of value
        added averaged over 2004-2007. The spending tercile requires
        at least two reported SICONFI years in the window, which
        leaves 471 municipalities; the other two cover all 502. The
        long-difference end window is 2017-2020 under the same
        two-year rule (470 municipalities for the spending change);
        the poverty change uses 2000-2002 and 2010-2012 clearing
        windows around the two censuses. Urban classes are cut on
        the 2010 census urban share at 0.5 (rural majority) and 0.75
        (urban), the two round thresholds that split the sample into
        215, 208 and 79 municipalities; the census definition is
        administrative and the robustness classification is task 4.9.
Evidence: output/tables/tab-structure-crosstab.csv, tab-summary-by-group.csv; fig-trajectories-*.png
Affects: analysis-layer groups only; no panel column
Supersedes: none

## D-2026-10-16-b  SICONFI filing screen in the analysis layer: a municipality-year whose per-capita total is more than ten times or less than a tenth of the municipality's own median is a reporting gap

Status: closed (descriptive sprint, day 4)
Scope: _scripts/utils/screen_siconfi.R (siconfi_flags, siconfi_screen, SICONFI_SCREEN_FACTOR = 10); applied by 10-descriptive-figures.R to the exp_* columns of _data/interim/siconfi_expenditure_munic_2000_2024 and to the exp_* and rev_* columns of the descriptive panel; script 11 applies the same screen. The interim files of script 09 and the raw extracts are unchanged.
Context: the day-4 displays showed a 2011 spike in every municipal
         spending series. Manoel Urbano (AC, 1200344) reports R$ 1.45
         billion of expenditure in 2011 against R$ 13.7 million in 2010,
         a per-capita total of R$ 365,474 at 2024 prices, 127 times its
         own median, on the revenue side as well (R$ 342,650 per
         capita): a filing in the wrong unit. Scanning the 808
         municipalities for the same pattern found a second kind of
         error, totals filed at a few reais or less per capita (ratios
         to the own median below 1e-7), concentrated in Maranhão in
         2013.
Reason: an order-of-magnitude screen on the municipality's own
        series, symmetric at a factor of ten, catches both kinds and
        nothing else: the 99.9th percentile of the ratio across all
        808 municipalities is 3.0. Flags are computed on the
        expenditure total per capita and, separately, on current
        revenue per capita; a flagged municipality-year has every
        column of that side set to NA, the same treatment as a year
        the municipality did not file (D-2026-10-15-c), and is never
        corrected. Counts, asserted in script 10: 16 expenditure and 26
        revenue municipality-years among the 808 municipalities, 16
        distinct municipality-years in the sample (11 expenditure, 11
        revenue, 6 on both sides), out of 8,628 reported sample cells.
        The screen removed the 2011 spike from the state panel (AC),
        the tercile panels and the sample means, and changed the
        within/between decomposition of every spending variable
        (social spending per capita from 9 to 40 percent between),
        which is why it is a logged decision and not a display choice.
        Promotion to the pipeline (task 4.8) carries the screen into
        script 02 as an assertion with a listed exception table.
Evidence: _scripts/utils/screen_siconfi.R; the flag list printed by script 10 (message at run); _audit/siconfi.qmd Q8 to be added at promotion
Affects: 16 sample municipality-years on the exp_*/rev_* columns of the descriptive panel, analysis layer only
Supersedes: none

## D-2026-10-17-a  Correlational exercise: outcome years 2004-2019, regressors lagged one year within municipality, log(1 + x) for per-capita spending, transfers in hundreds of reais and employment in ten-point units, one common complete-case sample

Status: closed (descriptive sprint, day 5)
Scope: 11-correlations.R (CORR_YEARS, CORR_LAG, TRANSFERS_SCALE, EMP_SCALE, BF_END_YEARS, reg_main, ld_main); output/tables/tab-corr-twfe-main.csv, -full.csv, -robustness.csv, tab-corr-long-difference.csv; manuscript section sec-ch4-correlations
Context: the sprint plan (rebuild/06-descriptive-sprint.md, section 7)
         fixes the estimator (fixest two-way fixed effects, municipality
         and year, clustered by municipality), the outcome (log of one
         plus cleared km2), the regressor list and the window
         (2004-2019, Bolsa Família ending in 2019, D-2026-10-05-b), but
         not the lag mechanics, the treatment of zero spending, the
         scaling or the sample rule.
Reason: the outcome years are 2004-2019 and every regressor except
        listing status is the previous year's value, built as the
        positional lag within municipality on the balanced 2000-2020
        panel (asserted). Listing status is contemporaneous because it
        is the enforcement regime of the outcome year, not a social
        condition. The price indices enter through the pipeline's own
        one-year lags (price_index_*_lag1_z, D-2026-10-11-d) and are not
        lagged again. A function absent from a reporting year is a zero
        (D-2026-10-15-c), 63 health, 45 education and 86 social
        assistance municipality-years in 2003-2019, so spending per
        capita enters as log(1 + x), never log(x). BF transfers per
        capita enter in hundreds of 2024 reais and the formal employment
        rate in units of ten points, so that the coefficients read at
        the scale the variables move on. The main table is estimated on
        one common complete-case sample, so that the separate and the
        joint specifications are read on the same municipality-years:
        6,679 municipality-years and 499 municipalities, outcome years
        2005-2019. The 2004 outcome drops because Bolsa Família has no
        2003 value; Santa Cruz do Arari (PA), Ipiranga do Norte and
        Itanhangá (MT) drop because they carry no price index in any
        year; the rest of the 851 missing cells out of 7,530 are
        SICONFI reporting gaps (after the filing screen,
        D-2026-10-16-b, applied as in script 10) and a few GDP or
        employment cells. The robustness outcomes (forest rate,
        normalized outcome) use their own complete cases, which turn
        out to be the same 6,679 cells. The long difference uses the
        windows of D-2026-10-16-a (2004-07 to 2017-20, two reported
        SICONFI years in each window), the end window 2017-2019 for the
        Bolsa Família variables, window means, log(1 + x) for the
        clearing and spending changes, heteroskedasticity-robust
        standard errors (HC1), and one common complete-case cross
        section of 469 municipalities (the 470 of the spending change
        less Santa Cruz do Arari, which has no price change). Nothing is
        weighted: the average municipality is the unit, as in script 10.
        The exercise is a conditional-correlation description and not a
        candidate identification design, which is why the script lives
        in _scripts/ and not in _scripts/models/.
Evidence: output/tables/tab-corr-twfe-full.csv (years, counts and every coefficient per specification); the message printed by script 11 at run
Affects: analysis-layer estimation samples only; no panel column
Supersedes: none

## D-2026-10-21-a  Mato Grosso estimates of the LAI quota workbook realigned to their municipalities

Status: closed (phase 2, step 2.1, branch phase2-2.1-quota-realign, 2026-10-21, commit 69bf5a6; comparator report in the commit message)
Scope: 02-social-bf.R (realignment block, BF_QUOTA_REALIGN_*); bf_quota, bf_families_quota_ratio, bf_transfers_quota_brl_2024
Context: task 1.4 flagged 14 Mato Grosso municipalities with 2006-07 quota
         ratios above 3 (D-2026-10-05-n, verify upstream first). Verified
         against the re-acquired source (_audit/bf.qmd Q4b): the LAI
         workbook's Mato Grosso block lists Santa Cruz do Xingu and União do
         Sul ahead of Acorizal while its four "estimativa de famílias
         pobres" columns run in strict alphabetical order over all 141
         rows, so every estimate from Acorizal to the slot of Santa Cruz do
         Xingu sits two rows above its municipality, the next 21 one row
         above, the last five in place. Cuiabá's row carries 376 poor
         families for 2006-2008 and Conquista D'Oeste's 16,882; the
         workbook's own coverage column reports the same misalignment
         (Cuiabá at 5,393 percent in 2006). In the other 26 states the
         stored estimate and the December families correlate at 0.97 or
         above; in Mato Grosso at 0.12 as stored and 0.994 realigned, and
         the zero "até 2005" estimates land on Ipiranga do Norte and
         Itanhangá, created after the 2000 census (task 1.6).
Reason: the misalignment is structural and exact (shift counts 113, 21, 5),
        and the realigned block has the ratio distribution of the other
        states (median absolute log ratio 0.14, 95th percentile 1.3). The
        correction is applied in the script to the four period columns of
        the Mato Grosso rows, the raw file untouched, with the row
        structure asserted before and the families-quota correlation after.
        The three Mato Grosso do Sul and one Paraná municipalities above 3
        are consistent with their states and stay as stored. No external
        quota series is needed; the defect is wider than the 14 cases
        (136 municipalities move) and not confined to 2006-2007.
Evidence: _audit/bf.qmd Q4b
Affects: bf_quota, bf_families_quota_ratio, bf_transfers_quota_brl_2024 for
         136 Mato Grosso municipalities, 2004-2019, both reference months;
         downstream the baseline coverage ratio and the median split of
         D-2026-10-05-i (recheck, issue #19)
Supersedes: D-2026-10-05-n

## D-2026-10-22-a  Bolsa Família series extended to 2024 across two file layouts, 2022 empty

Status: closed (phase 2, step 2.2, branch phase2-2.2-bf-endpoint, 2026-10-22; comparator report in the commit message)
Scope: 02-social-bf.R (BF_YEARS, BF_FILE_YEARS, BF_NEW_LAYOUT_YEARS, read_bf_new); interim bf_panel_2004_2024; 04-panel-assemble.R input name; all bf_* columns 2020-2024
Context: D-2026-10-05-q opened the endpoint; the re-acquired MI Social files
         cover 2020-2021 in the 2004-2019 layout (12 months, 5,571 codes)
         and 2023-2026 in a new layout (codigo_ibge, anomes_s, suffixed
         value columns, an average-benefit column; 5,570 codes until
         October 2025, Boa Esperança do Norte (MT, installed 2025) joining
         in November; 2023 starts in March, the first month of the
         re-created programme). No file exists for 2022 and the service
         returns no values for it (provenance notes, _audit/bf.qmd Q1).
Reason: the panel's target endpoint is 2024, the deflator base and the last
        year the IPCA lookup covers; 2025-2026 stay unused. The interim grid
        is completed over 2004-2024 for both reference months so the regime
        flag of D-2026-10-22-b is defined in every year: 2022 rows carry NA
        in every value column, as do February 2023 and the 2023-2024 rows of
        the municipality absent from the new layout. The average-benefit
        column of the new layout is not carried (it is the ratio of the two
        stored columns, rounded by the provider). The 2023-2024 values are
        the re-created programme's (families on the 502 biome municipalities
        rise 41 percent from June 2021 to June 2023 and the nominal transfer
        per family 3.5 times); they enter as reported and are identified by
        the regime flag, comparability being an analysis decision.
Evidence: _audit/bf.qmd Q1, Q6
Affects: bf_* columns 2020-2024 (2020 in the current panel); interim file
         renamed bf_panel_2004_2024
Supersedes: D-2026-10-05-b (closed by this entry together with D-2026-10-05-q)

## D-2026-10-22-b  Auxílio Emergencial substitution months stored as reported and flagged by bf_regime

Status: closed (phase 2, step 2.2, branch phase2-2.2-bf-endpoint, 2026-10-22; comparator report in the commit message)
Scope: 02-social-bf.R (bf_regime, BF_REGIME_*); 04-panel-assemble.R (column carried into the panel); bf_transfers_* 2020-2021
Context: on the 502 biome municipalities the transfer per beneficiary
         family in the monthly files falls from R$ 212 (nominal) in
         January-March 2020 to R$ 6-7 in April-August 2020 and R$ 35-41
         in September-December, returns to R$ 206-210 in January-March
         2021, halves to R$ 99-106 in April-October 2021 and is empty from
         November 2021 (Auxílio Brasil). Beneficiary families are stable
         throughout (2.11-2.24 million). The LAI workbook's annual totals
         on the same municipalities show no fall (R$ 5.58 bn in 2020
         against 5.54 bn in 2019; 4.60 bn for January-October 2021): the
         monthly service reports what the programme disbursed after
         families paid the Auxílio Emergencial were taken off the Bolsa
         Família payroll, the workbook the entitlement. June, the
         reference month, lies inside the substitution window in both
         years; February, the alternative month, is a regular month in
         both.
Reason: the stored values are what the source reports and are not recoded;
        a character column bf_regime names the programme regime of each
        stored month (pbf: regular Bolsa Família; pbf_emergency_aid: April-
        December 2020 and April-October 2021; auxilio_brasil: November 2021
        to February 2023; pbf_2023: March 2023 on) so that analysis scripts
        exclude or model the substitution months and the 2023 programme
        explicitly instead of hardcoding 2019 as the endpoint. The column
        is source information, constant across municipalities within a
        month, and sample-independent, so it is stored in the pipeline
        (GUIDANCE.md, governing rule for derived variables). The reference
        month is unchanged (D-2026-10-05-a); the interim file keeps
        February, which is undistorted in 2020 and 2021, should the author
        reopen that choice. Coverage variables (bf_families_n,
        bf_families_quota_ratio) are usable in 2020-2021 as stored; the
        transfer variables are not.
Evidence: _audit/bf.qmd Q6
Affects: new column bf_regime (interim and panel; NA before 2004 in the
         panel); the June 2020 and June 2021 values of
         bf_transfers_total_brl_2024, bf_transfers_avg_brl_2024,
         bf_transfers_pea_brl_2024, bf_transfers_quota_brl_2024 and, in
         script 04, bf_transfers_pc_brl_2024 are disbursement artifacts
Supersedes: none

## D-2026-10-22-c  Municipal quota carried to 2021 by the workbook's last period, NA from 2022

Status: closed (phase 2, step 2.2, branch phase2-2.2-bf-endpoint, 2026-10-22; comparator report in the commit message)
Scope: 02-social-bf.R (BF_QUOTA_PERIOD_YEARS); bf_quota, bf_families_quota_ratio, bf_transfers_quota_brl_2024, 2020-2024
Context: the LAI workbook's fourth estimate column is headed "de 2012 até o
         momento" and the workbook runs to October 2021 (D-2026-10-07-g);
         no later estimate of poor families exists in the re-acquired
         sources, and the re-created programme of 2023 uses a different
         eligibility rule.
Reason: the 2012 estimate applies to 2020-2021 by the source's own
        statement; from 2022 the quota and the two ratios over it are NA
        rather than an extrapolation.
Evidence: _audit/bf.qmd Q4, Q6
Affects: bf_quota, bf_families_quota_ratio, bf_transfers_quota_brl_2024 in
         2020-2021 (values) and 2022-2024 (NA)
Supersedes: none
