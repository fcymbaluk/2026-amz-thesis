# Descriptive sprint: Chapter 4 for the supervisor, 13 to 18 October 2026

Approved by the author on 2026-10-13 (decision memo
`docs/decisions/2026-10-13-descriptive-sprint-before-phase-2.md`). The
sprint runs on the branch `ch4-descriptive-sprint`, ahead of phase 2, and
touches only the manuscript, the analysis layer (`_scripts/07-` onward,
`_scripts/utils/`, `_audit/siconfi.qmd`), the raw layer (new sources
placed by the author with provenance rows) and the guidance records.
Pipeline scripts 01-06 and their outputs do not change, so the branch
cannot conflict with the phase 2 work that resumes after it.

## 1. The ask

The supervisor asked (2026-10-13) for the chapter by 18 October with an
improved descriptive opening: more storytelling with descriptive
statistics, graphs and tables; the overlap between the deforestation
trajectory and the social and income trajectory 2004-2020; social
expenditure by function (health, education, social assistance) and the
federal transfers that fund it, comparing municipalities and states. The
author adds the simplest correlational exercise between deforestation and
social policy. Task board items 4.1-4.3 are the descriptive stage already
foreseen; 4.5-4.7 were added for this sprint.

## 2. Git flow

1. `stage5-atlas-data` (author-driven, short): Atlas Brasil files placed
   in `_data/raw/atlas/` with provenance rows, merged to `main` before the
   sprint joins them. Closes the open stage 5 item. If the files are not
   in hand by 2026-10-15 the sprint proceeds with the fallbacks of
   section 5 and the branch waits.
2. `ch4-descriptive-sprint`: one branch for the five days, one commit per
   day and concern (setup and text; reproduction; data; descriptives;
   correlations and text; ship). Invariant: nothing under scripts 01-06
   or `_data/interim`/`_data/final` files they write changes. New data
   decisions get D-IDs in `DECISIONS.md` (append-only).
3. Delivery on 2026-10-18: render docx, author copies it to
   `docs/submitted/`, tag `ch4-descriptive-2026-10-18`, merge the sprint
   into `main`, delete the branch, resume phase 2 at step 2.1. Follow-up
   registered as task 4.8: promote the SICONFI and Atlas builds into the
   pipeline as script 02 sources after step 2.9. Done 2026-10-18: `main`
   merged into the branch first (Chapter 1 and 2 guidance work), docx
   rendered from the repository root with no unresolved references;
   carried to the author: the two CHECK comments in the qmd (sample 499
   versus 500; script 08 cohorts 2008-2012 versus the text's 2008/09)
   and the nine citation keys still absent from `references.bib`
   (plain author-year in the text, TODO-cite comments).

## 3. Chapter reproduction

The August 2026 text (former Chapter 3, now `docs/submitted/`) goes into
`manuscript/ch4-causal-analysis.qmd` with `@fig-`/`@tbl-`/`@sec-` cross
references and `@key` citations. Figures are embedded from
`manuscript/figures` and `manuscript/tables`, symlinks to
`ch4-causal-analysis/output/figures` and `output/tables` (Quarto copies
the resolved files into the book; a path outside the project is not
copied). The figure PNGs are committed as the deliverable (author
decision 2026-10-13).

Scripts 07 and 08 get only what makes them run on the phase 1 panel: the
input path (`_data/final/panel_amz_2000_2020_final.csv`), removal of
`rm(list = ls())` and `install.packages("did")`, and the `ggsave` lines
made live under the `fig-<slug>.png` convention. They keep their
monolithic shape and native pipe; the restyle is a logged deferral.

| Chapter figure (August numbering) | Script block | Output file |
|---|---|---|
| Figure 1 policy timeline | 07 A1 | `fig-policy-timeline.png` |
| Figure 2 variable availability | 07 block 0 | `fig-variable-availability.png` |
| Figure 3 small multiples, natural units | 07 A2 | `fig-series-natural-units.png` |
| Figure 4 indexed series by group | 07d (fig09) | `fig-series-indexed-groups.png` |
| Figure 5 Bolsa Família components | 07b (fig05) | `fig-bf-components.png` |
| Figure 6 wage brackets | 07b (fig03) | `fig-wage-brackets.png` |
| Figure 7 cohort and exit paths | 07 C1 (fig15) | `fig-cohort-exit-paths.png` |
| Figure 9 coverage map | 07 (fig22, geobr) | `fig-bf-coverage-map.png` |
| Figure 10 moderator credibility | 07 B2 (fig11) | `fig-moderator-credibility.png` |
| Figure 11 event study by stratum | 08 | `fig-event-study-strata.png` |
| Table 2 stratified ATT | 08 | `tab-att-strata.csv` |

Known text corrections at reproduction: the broader sample is 500, not
499 (avail_ok, D-2026-10-12-i); Table 2 lost its decimals in Word; the
figure numbering skipped 8 and repeated 11; "Chapter 3" self-references
become "this chapter". The two footnotes on the price-index and electoral
defects stay, since both hold on the phase 1 panel (tasks 1.3, 1.5). The
stratified ATTs are re-run and the text numbers updated if they move
(GDP and informality are drift columns in the doubly robust step).

Citation keys missing from `references.bib` (author adds in Zotero;
text carries plain author-year and a `TODO-cite` comment until then):
binswanger1991, burgess2012, mahar1988, schneider1995, sills2015,
carneiro2015, carneiro2022, para2023 (Governo do Pará, Programa
Municípios Verdes), alcaraz2024.

## 4. New scripts

| Script | Does | Writes |
|---|---|---|
| `_scripts/09-descriptive-data.R` | reads the raw SICONFI extracts, Atlas and the SIDRA 5938 value-added series; deflates to 2024 BRL (`aux/deflator_ipca_2024.csv`), per capita; joins onto the panel by geocode and year | `_data/interim/siconfi_expenditure_munic_2000_2024.rds`, `siconfi_expenditure_uf_2000_2024.rds`, `siconfi_revenue_munic_2000_2024.rds`, `atlas_indicators_1991_2010.rds`, `ibge_pib_value_added_2002_2021.rds`, `_data/final/panel_amz_2000_2020_descriptive.rds` |
| `_scripts/10-descriptive-figures.R` | the descriptive displays D1-D6 (done 2026-10-16) | `output/figures/fig-comovement-natural-units.png`, `fig-comovement-indexed.png`, `fig-municipal-revenue-sources.png`, `fig-states-vs-municipalities.png`, `fig-trajectories-listing.png`, `fig-trajectories-terciles.png`, `fig-structure-distributions.png`, `fig-structure-maps.png`, `fig-long-differences.png`; `output/tables/tab-municipal-share-social-spending.csv`, `tab-structure-crosstab.csv`, `tab-long-differences.csv`, `tab-summary-by-period.csv` (and `-long`), `tab-summary-by-group.csv`, `tab-within-between.csv` |
| `_scripts/11-correlations.R` | the correlational regressions | `output/tables/tab-corr-*.csv` |
| `_scripts/utils/theme_ch4.R` | task 4.4: shared theme and palettes (done 2026-10-16) | sourced by 10, 11 (07 at its restyle) |
| `_scripts/utils/screen_siconfi.R` | D-2026-10-16-b: order-of-magnitude screen for SICONFI filings in the wrong unit, applied before every display | sourced by 10, 11 |
| `_audit/siconfi.qmd` | coverage by year, the 2013 FINBRA-to-SICONFI break, function totals against subfunctions | rendered HTML |

Script 11 stays out of `_scripts/models/` because it is not a candidate
identification design; it is a conditional-correlation exercise and says
so in its header. The `siconfi` source token is added to the interim
vocabulary in `docs/repo-conventions.md`.

## 5. New data

In priority order.

1. **SICONFI/FINBRA via BigQuery** (2026-10-15):
   `basedosdados.br_me_siconfi.municipio_despesas_funcao` and
   `uf_despesas_funcao`, 2000-2024, nine Legal Amazon states, stage
   `estagio_bd = "Despesas Empenhadas"` (the only stage before 2013 and
   continuous across the break), function-level rows only (`conta` in
   Saúde, Educação, Assistência Social, Previdência Social, Gestão
   Ambiental, Despesas por Função; subfunction rows are children and
   would double count); `municipio_receitas_orcamentarias` for FPM and
   the conditioned transfers (SUS, FUNDEB and other FNDE, FNAS) in the
   classification of Alcaraz (2024, Quadro 2). Each query dry-run and
   reported, executed under the 100 GB cap, SQL logged in
   `_scripts/sql/`, extracts placed by the author in `_data/raw/siconfi/`
   with provenance rows. Probes of 2026-10-13 are in
   `_scripts/sql/2026-10-13-siconfi-probes.sql`. Reporting gaps are real
   (19 to 21 of Acre's 22 municipalities per year) and are shown, never
   imputed.
2. **Atlas Brasil** 1991/2000/2010 (author, `stage5-atlas-data`):
   poverty, extreme poverty, income per capita, Gini, urban population
   share, IDHM, joined by the seven-digit geocode (the legacy script
   matched on names with an alias table; that route is retired).
   Done 2026-10-13: `_data/raw/atlas/censo_total_1991_2010.xlsx`, merged to
   `main`, audited in `_audit/atlas.qmd`. Follow-ups on the rural-urban
   split (Census 2022, IBGE 2017 typology, MapBiomas urbanized area) are
   tasks 4.9 and 4.10.
3. **Value added by activity** (already in `_data/raw/ibge/sidra/`,
   SIDRA 5938 extracts): task 4.1 needs no download.

Out of scope this week, listed for later: IBAMA fines, BPC and rural
pensions, Bolsa Verde, MUNIC, IGD-M.

Fallbacks. Without Atlas, the income trajectory uses GDP per capita and
the census anchors in the panel, and rurality comes from the census
population tables on SIDRA. Without SICONFI by the evening of 2026-10-15,
the chapter ships with displays D1, D3, D4 and D6 on existing data and
expenditure is deferred to the follow-up.

## 6. Descriptive program (new opening of the EDA section)

- **D1 Co-movement 2004-2020**, sample aggregates: deforestation against
  BF coverage, transfers per capita, formal employment, GDP per capita
  and social expenditure per capita by function; small multiples in
  natural units plus an indexed overlay (2004 = 100). Extends the
  August Figures 3 and 4.
- **D2 Municipalities versus states**: for each of the nine states,
  state-government social spending per capita, the municipal aggregate
  per capita and state deforestation on one panel; the municipal share
  of subnational social spending by state.
- **D3 Grouped trajectories** (task 4.3): ever-listed versus never,
  baseline social-spending terciles, baseline poverty terciles,
  land-based dependence terciles; indexed deforestation and social series
  by group.
- **D4 Structure** (tasks 4.1, 4.2): land-based dependence from value
  added, rural/urban split, their cross-tab; maps if geobr can fetch
  shapes from the sandbox.
- **D5 Long differences**: change in social spending per capita 2004-07
  to 2017-20 against change in deforestation; Atlas poverty change
  2000-2010 against deforestation change.
- **D6 Tables**: summary statistics by period and group; the
  within/between decomposition extended to the expenditure variables.

## 7. The correlation exercise (task 4.7)

Two-way fixed-effects regressions with `fixest::feols`, municipality and
year effects, standard errors clustered by municipality, 2004-2019 (BF
ends in 2019). Outcome log(1 + cleared km²); the forest rate and the
normalized outcome as robustness. Regressors lagged one year: BF coverage
ratio, BF transfers per capita, log social expenditure per capita by
function, formal employment rate; controls log GDP per capita, lagged
price indices, cattle density, listing status. A long-difference cross
section complements it. Reported as conditional correlations, with the
caveat that BF coverage is mostly between-municipality variation, so
within estimates lean on little.

## 8. Schedule

| Day | Deliverable | Commit |
|---|---|---|
| Tue 13 | branch, `.gitignore` and `literature/`, this plan, decision memo, task board 4.5-4.8, chapter text in the qmd | setup and text |
| Wed 14 | scripts 07/08 on the phase 1 panel; the eleven outputs regenerated; text numbers re-checked; first HTML and docx render | reproduction |
| Thu 15 | SICONFI pulls, provenance, script 09, `_audit/siconfi.qmd`; Atlas joined (in hand since 2026-10-13); Census 2022 urban/rural from SIDRA if the day allows (task 4.9) | data |
| Fri 16 | script 10, displays D1-D6; new opening subsections drafted under `docs/writing-rules.md` | descriptives |
| Sat 17 | script 11, correlation table and paragraph; full render; cross-reference and bibliography check | correlations and text |
| Sun 18 | buffer; final docx; tag, merge proposal, frontmatter and decision updates | ship |

## 9. Files added by the author outside git

- `ch4-causal-analysis/literature/` (gitignored, `.gitkeep` tracked):
  PDFs read for the chapter, starting with Alcaraz (2024). Each one also
  enters Zotero so the chapter cites it by key.
- `docs/submitted/2026-08-ch3-causal-inference.docx`: the version sent to
  the supervisor in August 2026 (the author moves it; the folder is
  write-denied for Claude).
