# Stage 5 re-acquisition plan: inputs for scripts 03 and 04

Drafted 2026-10-08 (Claude Code session) as a working document in the
gitignored `scratch/`; promoted to `rebuild/` on 2026-10-10 at the close of
stage 5 (author decision), because sections 5-9 hold the findings the script
03 audit starts from. Sections 1-4 are the plan as executed; 5-9 the session
records. The throwaway download, check and provenance scripts named below
live in `scratch/staging/<provider>/` and are not versioned. Sources read: `_annex/parked/u-appendix-data-description.qmd`,
`_scripts/03-data-cleaning-controls.R`, `_scripts/04-data-panel.R`,
`_data/aux/provenance.csv`, `docs/bigquery.md`, `docs/execution-path.md`.

## 1. What scripts 03 and 04 need

Script 04 reads only the outputs of 01-03, so every missing input belongs to
script 03. Already in the raw layer: the IPCA series (`raw/ibge/
ipca_202509SerieHist.xlsx`, deflator in `aux/deflator_ipca_2024.csv`) and the
census population workbooks (`raw/ibge/Ibge_pop_1970-80-91-00-10.xlsx`,
`raw/ibge/Ibge_pop_2022 (Censo - Sidra).xlsx`). Missing:

| # | Source | Exact object | Years | Legacy path in script 03 | Panel columns fed |
|---|---|---|---|---|---|
| 1 | IBGE PIB dos Municípios | SIDRA Tabela 5938, PIB a preços correntes (mil R$), all municipalities | 2002 to latest release (2021/2022) | `_data/raw_ibge_pib/pib_munic.csv` (skip = 3, n_max = 5570, NA = "...", "-") | `population` (trimmed to GDP years), `gdp_brl`, `gdp_per_capita`, `gdp_per_capita_log`, `gdp_brl_2024`, `gdp_per_capita_2024`, `gdp_per_capita_2024_log` |
| 2 | TSE via Base dos Dados | Mayoral results, first round, ordinary elections. Columns used: `ano`, `turno`, `tipo_eleicao`, `cargo`, `sigla_uf`, `id_municipio`, `id_candidato_bd`, `numero_candidato`, `sigla_partido`, `numero_partido`, `votos` | 2000, 2004, ..., 2020, plus 2024 | `_data/raw-tse-results-2.csv` | `elec_enc`, `elec_mov`, `elec_winner_share`, `elec_n_candidates`, `elec_winner_party`, `elec_winner_id`, `elec_competitive`, `elec_uncontested`, `election_year`, and the `_gov` lags |
| 3 | IBGE PAM | SIDRA Tabela 5457, "Área plantada ou destinada à colheita" (ha), four crops: algodão herbáceo, arroz, milho, soja | 1988 to 2024 (legacy: 771 municipalities x 37 years x 4 crops) | `_data/raw-ibge-PAM.xlsx` (sheet Tabela, two-row header) | `crop_{soy,corn,rice,cotton}_area_planted_km2`, `crop_*_munic_area_share`, crop base-year weights (avg 2000-2002) |
| 4 | IBGE PPM | SIDRA Tabela 3939, efetivo dos rebanhos, bovino (cabeças) | 1974 to 2024 | `_data/raw-ibge-PPM.xlsx` (sheet Tabela, single header row) | `cattle_heads`, `cattle_per_km2`, cattle base-year weight (avg 2000-2002) |
| 5 | IBGE Áreas territoriais | Workbook with sheet `AR_BR_MUN_2025`, columns `geocode`, `Area km2` | 2025 release | `_data/raw-ibge-area-munics.xlsx` | `munic_area_km2`; denominator of every crop share and the cattle density |
| 6 | World Bank Pink Sheet | `CMO-Historical-Data-Annual.xlsx`, sheet "Annual Prices (Real)" (2010 USD, MUV-deflated), skip = 6, units row dropped; series Soybeans, Maize, Rice Thai 5%, Cotton A Index, Beef | 2000 to 2024 | `_data/raw-prices-world bank-pink sheet.xlsx` | `price_index_crop`, `price_index_cattle`, `price_index_crop_lag1`, the `_z` variants (script 06) |

Not in scope: Atlas Brasil (read by `07-data-analysis-EDA-poverty.R` and needed
for descriptive tasks 4.2 and 4.3, not by 03 or 04). Keep it on the stage 5
list.

Spot-check values from the reference panel (geocode 1100015, Alta Floresta
D'Oeste, RO) for the first look at each download:

| Column | Year | Reference value |
|---|---|---|
| `munic_area_km2` | all | 7067.127 |
| `cattle_heads` | 2000 | 191685 |
| `gdp_brl` | 2002 | 111291000 (SIDRA "mil reais" 111291 x 1000) |
| `elec_n_candidates` / `elec_enc` | 2000 | 4 / 2.956464 |
| `elec_n_candidates` / `elec_enc` | 2004 | 2 / 1.995740 |
| `crop_soy_area_planted_km2` | 2015 | 3.5 |
| `price_index_cattle` | 2000 | 33.769 (defective normalization, task 1.3, reproduced in phase 1) |

## 2. Route per source

### 2.1 IBGE tables 1, 3, 4: SIDRA agregados API v3

Same route as the census anchors on 2026-10-06 (`scratch/staging/ibge_sidra/
download_log.csv` has the URL pattern:
`https://servicodados.ibge.gov.br/api/v3/agregados/<table>/periodos/<p>/variaveis/<v>?localidades=N6[all]&classificacao=<c>`).
One JSON per table and cut, read by `_scripts/utils/read_sidra.R`, placed in
`_data/raw/ibge/sidra/` under the `sidra_<table>_<content>_<years>_N6.json`
name.

Rules:

- `N6[all]` and the full year span. Phase 2 extends the panel to the nine
  Legal Amazon states (task 1.1) and to 2024 (task 1.2), so download
  everything and filter in the script.
- Confirm every variable and classification id against
  `https://servicodados.ibge.gov.br/api/v3/agregados/<table>/metadados`
  before the pull. Expected ids, from memory, to be verified:
  - 5938: variable 37 (PIB a preços correntes). Also pull, for task 4.1:
    valor adicionado bruto agropecuária, indústria, serviços (exclusive
    administração), administração pública, impostos, and VA total.
  - 5457: variable 8331 (área plantada ou destinada à colheita),
    classification 782 (produto das lavouras), categories for algodão
    herbáceo (em caroço), arroz (em casca), milho (em grão), soja (em grão).
  - 3939: variable 105 (efetivo dos rebanhos), classification 79 (tipo de
    rebanho), category bovino.
- The API refuses very large responses; chunk by year block (e.g. PAM
  1988-1999, 2000-2011, 2012-2024, one file per crop if needed) and log each
  chunk.
- Optional, recommended: SIDRA Tabela 6579 (estimativas anuais de população,
  2001-2024) in the same session, for fix 1.6 (post-2000 municipalities)
  and the `pop_source` flag.

### 2.2 Areas, table 5: direct download

IBGE geociências, "Áreas dos municípios":
https://www.ibge.gov.br/geociencias/organizacao-do-territorio/estrutura-territorial/15761-areas-dos-municipios.html
Fetch the 2025 release workbook (the script reads sheet `AR_BR_MUN_2025` by
name). Keep the provider's filename; place in `_data/raw/ibge/`. The IBGE
area revises every year; a different release changes every share, so the
release used is the provenance fact to record.

### 2.3 TSE, table 2: Base dos Dados on BigQuery

The legacy file carried `id_candidato_bd`, which only Base dos Dados has, so
the matching public source is
`basedosdados.br_tse_eleicoes.resultados_candidato_municipio_zona`
(zona level; the script sums to candidate x municipality x year, so zona
versus seção does not matter for the result).

Under `docs/bigquery.md`, without exception:

- author runs `gcloud auth login` and application-default login first;
- `bq show --schema` to confirm column names;
- select only the eleven columns above; WHERE on `ano IN (2000, 2004, 2008,
  2012, 2016, 2020, 2024)`, `cargo = 'prefeito'`, `sigla_uf IN ('AC','AM',
  'AP','MA','MT','PA','RO','RR','TO')`;
- `--dry_run` first, report bytes, then execute with
  `--maximum_bytes_billed=100000000000`;
- SQL logged to `_scripts/sql/2026-MM-DD-tse-mayoral-results.sql`;
- result saved as CSV under `_data/raw/tse/` (gitignored if large; covered
  by the Drive mirror).

Including 2024 now avoids a second pull when fix 1.5 (electoral timing)
and the 2024 extension land. Phase 1 reproduces the off-by-one on purpose.

Fallback without BigQuery: TSE open-data portal
(`votacao_candidato_munzona_<ano>.zip`), which lacks `id_candidato_bd`;
the script would then key candidates on `numero_candidato` x
`sigla_partido`. Only if the BigQuery route fails.

### 2.4 Pink Sheet, table 6: direct download

World Bank Commodity Markets page
(https://www.worldbank.org/en/research/commodity-markets), file
`CMO-Historical-Data-Annual.xlsx`. Keep the provider's filename; place in
`_data/raw/worldbank/`. The sheet header carries the release date; record
it. The real series is re-deflated at every MUV revision, so the
2000-normalized values will drift from the reference: expected, quantified
at the phase 1 gate, logged as a version decision, not fixed.

## 3. Workflow (copies the BF and census downloads of 2026-10-06)

1. Branch `stage5-controls-data` off `main`.
2. Claude writes a throwaway download script in
   `scratch/staging/<provider>/` (curl; records file, url, downloaded_utc,
   http_code, size_bytes, sha256 in `download_log.csv`) plus a spot-check
   script against the reference values in section 1. Nothing the pipeline
   depends on is produced here.
3. Author unlocks the raw layer, moves the files, relocks (Claude hands
   over the exact commands, run with the `!` prefix):
   `chflags -R nouchg ch4-causal-analysis/_data/raw` ... `mv` ...
   `chflags -R uchg ch4-causal-analysis/_data/raw`.
4. Claude appends one provenance row per file to `_data/aux/provenance.csv`,
   class `re-acquired`, with the download URL, timestamp, sha256 and the
   release markers found inside the file.
5. Author refreshes the Drive mirror:
   `rclone sync ch4-causal-analysis/_data gdrive:2026-AMZ/data-mirror --exclude .DS_Store`.
6. Tick the stage 5 boxes in `docs/execution-path.md`; update
   `current_stage`; propose the commit.

Order of sessions:

- Session A (no auth): IBGE API pulls (5938, 5457, 3939, optionally 6579)
  and the areas workbook. Four of the five missing sources.
- Session B: TSE on BigQuery (needs the author's gcloud login).
- Session C (minutes): Pink Sheet, provenance, mirror, commit.

Script 03's phase 1 refactor can start once A and B are in.

## 4. Decisions for the author (settled 2026-10-09)

1. TSE source: Base dos Dados table, with the TSE portal files as fallback.
   The author confirmed `id_candidato_bd` is not important for the
   analysis, so the fallback is fully viable (candidates keyed on
   `numero_candidato` x `sigla_partido`).
2. PIB route: SIDRA API JSON (consistent with the census anchors). The FTP
   release workbook (`ftp.ibge.gov.br/Pib_Municipios/<year>/base/`) is not
   used.
3. Front-load SIDRA 6579 annual population estimates for fix 1.6 in
   session A.
4. Pull value added by activity from 5938 now for task 4.1.

Section 5 (population NA in 2000-2001) acknowledged by the author; carry
into the script 03 audit as written.

## 5. Finding to carry into the script 03 audit

In the reference panel `population` is NA for all 502 municipalities in
2000 and 2001 (and for 9 municipalities, the post-2000 MT creations of task
1.6, through 2009). The stored series was trimmed to the GDP years rather
than anchored on the 2000 census point. Record in `_audit/ibge_pib.qmd`
(or the population notebook) and open a decision entry when script 03 is
refactored; do not fix in phase 1.

## 6. Session A record (2026-10-09, branch `stage5-controls-data`)

Done: 154 SIDRA JSON files (3939 bovino 1974-2025, 20 files; 5457 four crops
1988-2025, 58 files; 5938 seven variables 2002-2023, 68 files; 6579
population estimates 2001-2026, 8 files) and the 2025 areas workbook, in
`scratch/staging/ibge_sidra/` and `scratch/staging/ibge_areas/`, logged in
each folder's `download_log.csv`. Driver: `download_controls_sidra.sh`;
check: `check_controls_vs_reference.R`; provenance: `append_provenance.R`
(run after the move).

Verified ids (metadata endpoint): 5457 var 8331, class 782 = 40099 algodão
herbáceo, 40102 arroz, 40122 milho, 40124 soja; 3939 var 105, class 79[2670]
bovino; 5938 vars 37, 498, 513, 517, 6575, 525, 543; 6579 var 9324.

API behaviour: ~7 s per year of N6[all] data, truncation past ~40 s (HTTP
200 with cut JSON), instant 500 on hyphen ranges and on long requests. Three-
year pipe-separated chunks with validation and single-year fallback worked;
10 chunks fell back, none failed. `curl -g` is required (square brackets).

Results vs the reference panel (502 x 2000-2020): cattle_heads exact
(10,542 cells); four crop areas exact (10,521 each); gdp_brl within R$ 1,000
on 9,521 of 9,532 cells (API serves integer mil reais, legacy export had
decimals; apisidra also integer), 11 Santarém cells = legacy Mojuí split;
areas 501/501 exact.

Findings for the script 03 audit:
- `read_sidra.R` fails on tables with no classification (5938, 6579):
  `result$classificacoes[[1]]` out of bounds. Fix at the script 03 session
  (guard on `length(result$classificacoes) > 0`; local variant in the check
  script).
- Santa Cruz do Arari (1506401) has NA `munic_area_km2` in the reference
  for all years though the workbook has 1076.652 km2: legacy join loss.
- The areas workbook carries IBGE's columns (`CD_MUN`, `AR_MUN_2025`), not
  the legacy hand-renamed `geocode`/`Area km2`, plus two trailing rows
  without a code.
- Series counts differ by table (5564 PAM, 5568 PPM, 5570 PIB, 5571 pop):
  identify the extra/missing localities.
- GDP rounding drift: log as a version decision (like the Pink Sheet).

Pending for the author: unlock, move, relock (commands handed over);
`Rscript scratch/staging/ibge_sidra/append_provenance.R`; Drive mirror sync;
commit.

## 7. Session B record (2026-10-09)

Done: `basedosdados.br_tse_eleicoes.resultados_candidato_municipio_zona`
(schema via `bq show --schema`; `id_candidato_bd` no longer exists, the
candidate key is `sequencial_candidato`), cargo prefeito, nine states,
elections 2000-2024, all turnos and tipos de eleição, summed over zona to
candidate x municipality x year x turno, 16 columns, 17,374 rows. Dry run
675,978,042 bytes; executed with the 100 GB cap; SQL logged in
`_scripts/sql/2026-10-09-tse-mayoral-results.sql`; file in
`scratch/staging/tse/` with `download_log.csv`; provenance appender
`append_provenance_tse.R` (run after the move).

Checks: 1100015 reproduces (4 / 2.956464 in 2000, 2 / 1.995740 in 2004);
over the panel, elec_n_candidates and elec_enc (turno 1, ordinária)
reproduce on 2,990 of 3,007 municipality-elections as served and 2,998 when
votes are summed by numero_candidato.

Findings for the script 03 audit:
- 36 duplicate rows in 2000 and 2004 (same election, municipality and
  ballot number, two sequencial_candidato, identical votes): a Base dos
  Dados artefact. The legacy pipeline double-counted them (its ENC matches
  summed votes). Phase 1 reproduces; phase 2 opens a decision to dedupe.
- 9 residual cells where neither treatment reproduces: the legacy
  aggregation key (id_candidato_bd) is lost; examine at the audit.
- Two 2024 MT rows have an empty id_municipio (TSE code 73709 without an
  IBGE match).
- 802-808 municipalities per election: the pull already covers the Legal
  Amazon extension (task 1.1) and 2024 (tasks 1.2, 1.5).

Pending for the author: `mkdir _data/raw/tse`, move, relock;
`Rscript scratch/staging/tse/append_provenance_tse.R`; mirror; commit.

Addendum 2026-10-10: the author placed the Dahis, de las Heras and Saavedra
(2026) replication file (`_data/raw/dahis-2026/bq_resultados_candidato_
municipio_v2024.csv`, Base dos Dados export of 2024-07-03, all Brazil,
2000-2020, ordinária only, with `id_candidato_bd`). On the Legal Amazon,
ordinária, 2000-2020 it is identical to the 2026 pull (14,891 rows, no vote
differs). Kept under the new provenance class `external cross-check`; not
read by the pipeline. `id_candidato_bd` does not resolve the 2000-2004
duplicates (one id per pair) nor the 9 residual 2004 cells (the reference
has one candidate more than either file: rows since removed upstream).

## 8. Session C (scheduled 2026-10-11)

Branch `stage5-controls-data` stays open (two commits: session A 0ad1507,
session B). Tasks: Pink Sheet `CMO-Historical-Data-Annual.xlsx` (section
2.4) to `_data/raw/worldbank/` with a provenance row and the release date
from the sheet header; `price_index_cattle` 2000 spot-check against 33.769
(defective normalisation, expected to drift); tick the stage 5 Pink Sheet
box; author locks raw (`chflags -R uchg`) and refreshes the Drive mirror;
commit, then merge to `main`, push, delete the branch. Atlas Brasil stays on
the stage 5 list for the descriptive tasks (not needed by scripts 03-04).

## 9. Session C record (2026-10-10, one day early)

Done: `CMO-Historical-Data-Annual.xlsx` downloaded from
`https://thedocs.worldbank.org/en/doc/74e8be41ceb20fa0da750cda2f6b9e4e-0050012026/related/CMO-Historical-Data-Annual.xlsx`
(link resolved from the Commodity Markets page) to `scratch/staging/worldbank/`,
3,175,465 bytes, sha256 d07bcb67...3a8b, logged in `download_log.csv`.
Driver: `download_pink_sheet.sh`; check: `check_pink_sheet_vs_reference.R`;
provenance: `append_provenance_worldbank.R` (run after the move).

Release markers (sheet header): "Updated on September 02, 2026"; annual
prices 1960 to present, real 2010 US dollars; 66 year rows (1960-2025);
sheets Cover, Annual Prices (Nominal), Annual Indices (Nominal), Annual
Prices (Real), Annual Indices (Real), Description, Definitions, Index
Weights. The legacy layout holds (names row 7, units row 8, data from row
9; skip = 6 and slice(-1) still work); all five series present under the
legacy names; no NA on 2000-2024.

Check vs the reference: `price_index_cattle` = weight_i x beef_t, so the
ratio to the 2000 value recovers the beef index (spread across the 496
municipalities with a non-zero weight = 0). New beef series normalised to
2000 matches it within 0.15% on every year 2000-2020 (mean +0.03%): MUV
re-deflation drift, as predicted in 2.4. 1100015 `price_index_cattle` 2000
= 33.76907 confirmed (equals the cattle weight; task 1.3). Crop index not
inverted (needs the municipal weights); checked at the script 03 session.

Repo edits this session: `docs/execution-path.md` (current_stage, Pink
Sheet box ticked), `ch4-causal-analysis/GUIDANCE.md` frontmatter.

Closed the same day: file placed and locked by the author, provenance row
appended (226 rows), "one provenance row per download" ticked. Decisions
opened at the close of stage 5: D-2026-10-10-a (Pink Sheet release drift),
D-2026-10-10-b (GDP integer thousands), D-2026-10-10-c (TSE candidate key).
Atlas Brasil stays on the stage 5 list (script 07 only). Mirror refresh,
commit, merge to main and branch deletion follow in the same session.
