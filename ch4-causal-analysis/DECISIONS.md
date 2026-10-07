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

Status: closed (pre-rebuild; script 02 at tag pre-reorg; refactored in phase 1, 02-social-bf.R); reopened by D-2026-10-05-q
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

Status: closed (pre-rebuild; scripts 05, 06 at tag pre-reorg)
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

Status: closed (pre-rebuild; script 05 at tag pre-reorg); the statement that the parent's series are not adjusted holds for PIA, population and deforestation only, see D-2026-10-07-d
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

Status: open (Task 1.4; phase 2 step 2.1)
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

Status: open (Task 1.2; phase 2 step 2.2)
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

Status: closed (phase 1, script 02, 2026-10-07: D-2026-10-07-b builds the 2010 anchor from Tabela 616 PEA 15+; drift quantified in the script 02 commit message)
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
Evidence: _audit/census.qmd, Q1; compare_outputs report in the phase 1
          script 02 commit message
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
Evidence: _audit/census.qmd, Q2; compare_outputs report in the phase 1
          script 02 commit message
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
Evidence: _audit/census.qmd, Q3; compare_outputs report in the phase 1
          script 02 commit message (informal: 10,532 cells differ, max
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
Evidence: _audit/bf.qmd, Q4; compare_outputs report in the phase 1 script
          02 commit message
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
Evidence: _audit/bf.qmd, Q5; compare_outputs report in the phase 1 script
          02 commit message
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
