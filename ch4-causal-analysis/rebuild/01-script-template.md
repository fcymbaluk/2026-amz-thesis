# Pipeline script template

Applies to `_scripts/01` to `_scripts/06`. A pipeline script contains only code that changes the output if deleted, plus the comments needed to understand why a non-obvious choice was made. Everything else lives in the audit notebook, the annex, or `DECISIONS.md`.

## Skeleton

Fixed block order. No numbered subsections, no prose narration of steps.

```
header block        purpose, inputs, outputs, decisions, known defects, run order
libraries
source utils        only the shared functions this script uses
constants           every tunable choice, named, with its decision ID
read
assert: raw state   expected properties of the inputs
transform
assert: output      expected properties of the output
write
```

## Rules

1. One environment per script. No `rm(list = ls())` anywhere. No re-reading raw files in the middle of a script.
2. Nothing interactive: no `view()`, `glimpse()`, `head()`, `tail()`, `print(n = ...)`, `problems()`, plots, or `install.packages()`. Package installation is handled by `renv`.
3. Every choice that could have been made differently is a named constant at the top of the script, with the decision ID as a comment.
4. Comments in the body answer "why", never "what". The code already says what it does. A body comment cites a decision ID.
5. Every fact currently reported in prose ("no NAs", "503 unique geocodes", "no duplicates") is a `stopifnot()` call. Assertions document the expected state of the data and fail loudly when an upstream change breaks it.
6. Inputs come from `_data/raw/` or `_data/interim/`. Outputs go to `_data/interim/` (scripts 01 to 05) or `_data/final/` (script 06). Raw files are never modified.
7. Outputs are written as `.rds` (typed) and `.csv` (portable). Column types are set explicitly before writing so the CSV round-trips without coercion. File names follow `docs/repo-conventions.md`: interim outputs source-first (`bf_panel.rds`), the final panel content-first (`panel_amz_2000_2024`).
8. Source descriptions (what MapBiomas is, how the PPCDAm list works) do not belong in the script. The header points to the annex section.
9. A function used by more than one script moves to `_scripts/utils/` and is sourced right after the libraries. Utils files contain function definitions only: no side effects, no reads or writes at source time. `compare_outputs.R` lives in `utils/` but is the acceptance comparator, never sourced by a pipeline script.
10. Whether a derived variable is built here (stored in the panel) or in an analysis script is decided by the governing rule for derived variables in `GUIDANCE.md` (Methodological standards). Sample-dependent quantities never enter the pipeline.
11. Lines stay within 80 characters, the tidyverse limit that `.lintr` at the repository root enforces (`line_length_linter(80)`); wrap comments and long calls rather than exceed it. Added 2026-10-06; applies to `utils/` and the analysis scripts as well.

## Constants

Constants follow a fixed pattern, distinct from the data-column grammar in `naming-convention.md`:

1. SCREAMING_SNAKE_CASE, reserved for constants; data columns are snake_case and the two never mix.
2. A source or domain token prefixes source-specific constants (`BF_`, `RAIS_`, `TSE_`), matching the audit-notebook vocabulary, not the panel's domain prefixes.
3. Recurring roles use recurring markers: `N_*_EXPECTED` for assertion counts, `*_YEARS` for windows (stored as start:end), `*_REF_*` for reference choices, `*_BASE` for base years.
4. Every constant that encodes a decision carries its decision ID as a comment (rule 3 above); the codebook `construction` column names the constant where one governs the rule, so annex, codebook and script cross-reference mechanically.
5. When a variable name embeds a constant's value (`defor_norm_ref0007`, `_brl_2024`, `_c2006_07`), the name is generated from the constant with `paste0()` or an assertion ties the two, so the name cannot drift from the parameter.
6. A constant is promoted to a panel variable when its value stops being uniform across rows (sample membership becoming `biome_amazon` and `legal_amazon` flags in task 1.1; value provenance becoming `pop_source` in task 1.6). The promoted column inherits the constant's decision ID in its codebook row and is subject to the governing rule for derived variables like any other.

## Header block

The header is plain field lines, no decoration rules. This header is the only sanctioned boilerplate in a pipeline script: nothing beyond its fields is added, and body comments answer "why", never "what", with no narration of steps. The `# ---- Section ----` markers in the body stay, because RStudio builds its outline from them.

```r
# 02-social-bolsa-familia.R
# PURPOSE   Build the municipality-year Bolsa Família panel (coverage,
#           transfers, quota ratio) for 2004-2019.
#
# INPUTS    _data/raw/mds_bolsa_familia/bf-YYYY.txt   MDS Dados Abertos,
#                                                       release 2026-03
#           _data/raw/mds_quota/quota_municipal.xlsx   MDS, accessed 2026-03-10
#           _data/interim/population_pia.rds           02-social-population.R
#           _data/aux/deflator_ipca_2024.csv
#
# OUTPUTS   _data/interim/bf_panel.rds
#           _data/interim/bf_panel.csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex §4.1; evidence: _audit/bf.qmd;
#            history: DECISIONS.md)
#   D-2025-11-03-a  Reference month = June; February kept as robustness.
#   D-2025-11-03-b  Panel truncated at 2019 (COVID emergency-transfer break).
#   D-2026-01-14    Monetary values deflated to 2024 BRL (IPCA, December).
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.4  bf_quota carries source errors for 14 MT municipalities.
#             Reproduced on purpose to preserve equivalence with reference.
# RUN ORDER 02-social-population.R must run first (PIA denominator).
```

## Body example

```r
library(tidyverse)  # pipe operator: magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
BF_REF_MONTH     <- 6L          # D-2025-11-03-a
BF_ALT_MONTH     <- 2L          # D-2025-11-03-a
BF_YEARS         <- 2004:2019   # D-2025-11-03-b
DEFLATOR_BASE    <- 2024L       # D-2026-01-14
N_MUNIC_EXPECTED <- 503L        # IBGE predominant-biome list; update at Task 1.1

# ---- Read -------------------------------------------------------------------
bf_files <- list.files("_data/raw/mds_bolsa_familia",
                       pattern = "^bf-\\d{4}\\.txt$", full.names = TRUE)

read_bf <- function(file) {
  read_csv(
    file,
    col_types = cols(
      ibge   = col_double(),
      anomes = col_double(),
      qtd_familias_beneficiarias_bolsa_familia = col_double(),
      valor_repassado_bolsa_familia            = col_double()
    )
  ) %>%
    mutate(source_file = basename(file))
}

bf_raw <- map_dfr(bf_files, read_bf)

# ---- Assert: raw state ------------------------------------------------------
# Expected state documented in _audit/bf.qmd, question 1.
stopifnot(
  !anyDuplicated(paste(bf_raw$ibge, bf_raw$anomes)),
  all(bf_raw$valor_repassado_bolsa_familia >= 0, na.rm = TRUE),
  all(bf_raw$qtd_familias_beneficiarias_bolsa_familia >= 0, na.rm = TRUE)
)

# ---- Transform --------------------------------------------------------------
bf_panel <- bf_raw %>%
  mutate(
    year  = as.integer(anomes %/% 100),
    month = as.integer(anomes %% 100)
  ) %>%
  filter(year %in% BF_YEARS, month %in% c(BF_REF_MONTH, BF_ALT_MONTH)) %>%
  # ... coverage, transfers, deflation, quota ratio ...
  identity()

# ---- Assert: output state ---------------------------------------------------
stopifnot(
  n_distinct(bf_panel$geocode) == N_MUNIC_EXPECTED,
  nrow(bf_panel) == N_MUNIC_EXPECTED * length(BF_YEARS),
  all(bf_panel$bf_families_quota_ratio >= 0, na.rm = TRUE),
  is.numeric(bf_panel$bf_transfers_total_brl_2024)
)

# ---- Write ------------------------------------------------------------------
write_rds(bf_panel, "_data/interim/bf_panel.rds")
write_csv(bf_panel, "_data/interim/bf_panel.csv")
```

## What leaves the current scripts, and where it goes

| Currently in scripts 01 to 03 | Destination |
|---|---|
| "Understand the data" prose (source, provider, definitions) | Annex, per-variable section |
| `glimpse`, `head`, `view`, `print(n = 100)` | Deleted |
| Outlier boxplots and histograms | Audit notebook |
| Cross-validation against the MapBiomas platform (Acrelândia, Paragominas) | Audit notebook, one question |
| Reference-month decision table (stability, centrality, completeness) | Audit notebook, one question; result becomes `BF_REF_MONTH` |
| "I find no NAs / no duplicates / 503 geocodes" | `stopifnot()` |
| Duplicate-name municipalities list (Araguanã, Pau D'Arco, ...) | Audit notebook; assertion that geocode is the key |
| Mojuí dos Campos handling | Stays in code (it changes output), justified by a decision ID |
| Same-name municipality note, "I repeated this check for 1985, 1986, 2023" | Audit notebook |
| PIA vs PEA denominator argument | Chapter methods section; short version in annex |
