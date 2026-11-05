# 02-social-rais-wages.R
# PURPOSE   Build the municipality-year formal wage distribution from the
#           RAIS bond-level snapshot (task 3.A and task 3.B steps 2 and 4
#           of rebuild/upstream-reform-rais.md): bond counts, quantiles and
#           mean of the nominal and deflated average wage, the minimum wage
#           relative to the municipal median and the shares of bonds at or
#           below multiples of the year's minimum wage, for the whole
#           formal sector and for the private sector, 1999-2024, nine
#           Legal Amazon states.
#
# INPUTS    _data/raw/rais/rais_vinculos_v2/*.parquet   Own BigQuery
#                                                extraction from Base dos
#                                                Dados RAIS (Query 1 v2,
#                                                2026-11-04, D-2026-11-04-a)
#           _data/aux/minimum_wage_dec_1999_2024.csv   Nominal minimum wage
#                                                in force on 31 December
#           _data/aux/deflator_ipca_2024.csv           IPCA deflator, 2024 = 1
#
# OUTPUTS   _data/interim/rais_wage_distribution_1999_2024.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, labor-market section, to be written;
#            evidence: _audit/rais.qmd Q6-Q9; history: DECISIONS.md)
#   D-2026-11-04-a  The bond-level snapshot is the source for every RAIS
#                   aggregate; 2023-2024 follow the current Base dos Dados
#                   version.
#   D-2026-11-05-a  Public sector = natureza jurídica 1xxx (CONCLA
#                   Administração Pública); an empty code is private; the
#                   CNAE 75/84 alternative is counted beside it.
#   D-2026-11-05-b  Wage statistics on the positive average wage of the
#                   Query 1 filter; the minimum wage in force on
#                   31 December (brief decision 0.3); mw_to_median nominal
#                   on both sides; threshold shares at 1.0, 1.2 and 1.5
#                   times the contemporaneous minimum wage.
#   D-2026-11-05-c  Reliability floor of 50 bonds (brief decision 0.4,
#                   author 2026-11-05) on the count of the sector the
#                   statistic describes: the statistics are NA below it,
#                   the counts are kept.
# RUN ORDER Independent of the other 02 scripts. Feeds 04-panel-assemble.R
#           once the labor-market dataset is defined (RAIS track).

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(DBI)

source("_scripts/utils/rais_snapshot.R")

# ---- Constants --------------------------------------------------------------
RAIS_YEARS              <- 1999:2024
RAIS_WAGE_QUANTILES     <- c(p10 = 0.10, p25 = 0.25, median = 0.50,
                             p75 = 0.75)                 # D-2026-11-05-b
RAIS_MW_MULTIPLES       <- c(1.0, 1.2, 1.5)               # D-2026-11-05-b
RAIS_FLOOR_N_BONDS      <- 50L                            # D-2026-11-05-c
DEFLATOR_BASE           <- 2024L                          # D-2026-11-05-b
N_RAIS_SHARDS           <- 252L                           # D-2026-11-04-a
N_RAIS_BONDS_EXPECTED   <- 90156865L                      # D-2026-11-04-a
N_RAIS_MUNIC_EXPECTED   <- 809L   # 808 of the nine states plus Boa
                                  # Esperança do Norte (D-2026-11-04-a)
N_RAIS_MUNIC_YEARS_EXPECTED <- 20870L

# ---- Read -------------------------------------------------------------------
mw_dec <- read_csv(
  "_data/aux/minimum_wage_dec_1999_2024.csv",
  col_types = cols(year = col_integer(), mw_nominal_dec = col_double(),
                   legal_instrument = col_character())
)
deflator <- read_csv(
  "_data/aux/deflator_ipca_2024.csv",
  col_types = cols(year = col_integer(), ipca_index = col_double(),
                   ipca_deflator = col_double())
)

con <- rais_snapshot_connect(n_shards = N_RAIS_SHARDS)

snapshot_state <- dbGetQuery(con, "
  SELECT CAST(ano AS INTEGER) AS year, COUNT(*) AS n,
         MIN(ipca_deflator) AS deflator_min, MAX(ipca_deflator) AS deflator_max,
         MIN(valor_remuneracao_media) AS wage_min,
         COUNT(DISTINCT id_municipio) AS n_munic
  FROM rais GROUP BY ano ORDER BY ano
")

# ---- Assert: raw state ------------------------------------------------------
# _audit/rais.qmd Q6 (snapshot and deflator), Q8 (minimum wage).
mw_years <- mw_dec %>%
  left_join(deflator, by = "year") %>%
  mutate(mw_brl_2024 = mw_nominal_dec * ipca_deflator)
stopifnot(
  identical(mw_dec$year, RAIS_YEARS),
  all(mw_dec$mw_nominal_dec > 0),
  all(diff(mw_dec$mw_nominal_dec) > 0),
  !anyNA(mw_years$ipca_deflator),
  deflator$ipca_deflator[deflator$year == DEFLATOR_BASE] == 1,
  identical(snapshot_state$year, RAIS_YEARS),
  sum(snapshot_state$n) == N_RAIS_BONDS_EXPECTED,
  all(snapshot_state$wage_min > 0),
  # The snapshot's deflator is the aux lookup, year by year.
  all(abs(snapshot_state$deflator_min -
            deflator$ipca_deflator[match(snapshot_state$year,
                                         deflator$year)]) < 1e-9),
  all(snapshot_state$deflator_min == snapshot_state$deflator_max),
  all(snapshot_state$n_munic >= 762L & snapshot_state$n_munic <= 809L)
)

# ---- Transform --------------------------------------------------------------
duckdb::duckdb_register(con, "mw", mw_years %>% select(year, mw_nominal_dec))

# Query 3 of the brief as DuckDB SQL, parameterised by the bond filter so
# the whole-sector and private-sector passes share one text.
quantile_sql <- function(expr, suffix) {
  paste(sprintf("quantile_cont(%s, %s) AS wage_%s_%s",
                expr, RAIS_WAGE_QUANTILES, names(RAIS_WAGE_QUANTILES),
                suffix),
        collapse = ",\n    ")
}
share_name <- function(m) sprintf("share_le_mw%03d", round(m * 100))
share_sql <- paste(
  sprintf("AVG(CASE WHEN r.valor_remuneracao_media <= %s * mw.mw_nominal_dec
                    THEN 1.0 ELSE 0.0 END) AS %s",
          RAIS_MW_MULTIPLES, share_name(RAIS_MW_MULTIPLES)),
  collapse = ",\n    ")
query3 <- function(where) sprintf("
  SELECT CAST(r.ano AS INTEGER) AS year, r.id_municipio AS geocode,
    COUNT(*) AS n_bonds,
    SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS n_bonds_public,
    SUM(CASE WHEN %s THEN 1 ELSE 0 END) AS n_bonds_public_cnae,
    %s,
    AVG(r.valor_remuneracao_media) AS wage_mean_nom,
    %s,
    AVG(r.valor_remuneracao_media_2024) AS wage_mean_2024,
    %s
  FROM rais AS r
  LEFT JOIN mw ON mw.year = r.ano
  WHERE %s
  GROUP BY r.ano, r.id_municipio",
  rais_public_sql(), rais_public_cnae_sql(),
  quantile_sql("r.valor_remuneracao_media", "nom"),
  quantile_sql("r.valor_remuneracao_media_2024", "2024"),
  share_sql, where)

wages_all  <- dbGetQuery(con, query3("TRUE")) %>% as_tibble()
wages_priv <- dbGetQuery(con, query3(sprintf("NOT %s", rais_public_sql()))) %>%
  as_tibble() %>%
  select(-n_bonds_public, -n_bonds_public_cnae) %>%
  rename_with(~ paste0(.x, "_priv"), -c(year, geocode))

dbDisconnect(con, shutdown = TRUE)

stat_cols <- function(suffix) {
  c(paste0("wage_", c(names(RAIS_WAGE_QUANTILES), "mean"), "_nom"),
    paste0("wage_", c(names(RAIS_WAGE_QUANTILES), "mean"), "_2024"),
    share_name(RAIS_MW_MULTIPLES), "mw_to_median") %>%
    paste0(suffix)
}

rais_wages <- wages_all %>%
  left_join(wages_priv, by = c("year", "geocode")) %>%
  left_join(mw_years %>% select(year, mw_nominal_dec), by = "year") %>%
  mutate(
    # A municipality-year with no private bond is observed with zero private
    # bonds; its private statistics are undefined (D-2026-11-05-b).
    n_bonds_priv      = coalesce(n_bonds_priv, 0L),
    mw_to_median      = mw_nominal_dec / wage_median_nom,     # D-2026-11-05-b
    mw_to_median_priv = mw_nominal_dec / wage_median_nom_priv,
    across(c(year, n_bonds, n_bonds_public, n_bonds_public_cnae,
             n_bonds_priv), as.integer),
    # Below the floor the statistics rest on too few bonds to be read
    # (D-2026-11-05-c; evidence _audit/rais.qmd Q7); the counts stay.
    across(all_of(stat_cols("")),
           ~ if_else(n_bonds >= RAIS_FLOOR_N_BONDS, as.double(.x), NA_real_)),
    across(all_of(stat_cols("_priv")),
           ~ if_else(n_bonds_priv >= RAIS_FLOOR_N_BONDS, as.double(.x),
                     NA_real_))
  ) %>%
  select(geocode, year, n_bonds, n_bonds_public, n_bonds_public_cnae,
         n_bonds_priv, all_of(stat_cols("")), all_of(stat_cols("_priv"))) %>%
  arrange(geocode, year)

# ---- Assert: output state ---------------------------------------------------
# _audit/rais.qmd Q7-Q9.
all_stats   <- rais_wages %>% select(all_of(stat_cols("")))
priv_stats  <- rais_wages %>% select(all_of(stat_cols("_priv")))
below_floor <- rais_wages$n_bonds < RAIS_FLOOR_N_BONDS
below_priv  <- rais_wages$n_bonds_priv < RAIS_FLOOR_N_BONDS
shares      <- rais_wages %>% select(starts_with("share_le_mw"))
stopifnot(
  nrow(rais_wages) == N_RAIS_MUNIC_YEARS_EXPECTED,
  !anyDuplicated(paste(rais_wages$geocode, rais_wages$year)),
  n_distinct(rais_wages$geocode) == N_RAIS_MUNIC_EXPECTED,
  all(nchar(rais_wages$geocode) == 7),
  setequal(rais_wages$year, RAIS_YEARS),
  sum(rais_wages$n_bonds) == N_RAIS_BONDS_EXPECTED,
  all(rais_wages$n_bonds > 0L),
  all(rais_wages$n_bonds_public + rais_wages$n_bonds_priv ==
        rais_wages$n_bonds),
  all(rais_wages$n_bonds_public_cnae <= rais_wages$n_bonds),
  all(map_lgl(all_stats, ~ identical(is.na(.x), below_floor))),
  all(map_lgl(priv_stats, ~ identical(is.na(.x), below_priv))),
  sum(below_floor) < 0.06 * nrow(rais_wages),
  sum(below_priv) < 0.25 * nrow(rais_wages),
  with(rais_wages, all(wage_p10_nom <= wage_p25_nom &
                         wage_p25_nom <= wage_median_nom &
                         wage_median_nom <= wage_p75_nom, na.rm = TRUE)),
  with(rais_wages, all(wage_p10_nom > 0 & wage_mean_nom > 0, na.rm = TRUE)),
  with(rais_wages, all(share_le_mw100 <= share_le_mw120 &
                         share_le_mw120 <= share_le_mw150, na.rm = TRUE)),
  all(shares >= 0 & shares <= 1, na.rm = TRUE),
  all(rais_wages$mw_to_median > 0, na.rm = TRUE),
  is.character(rais_wages$geocode),
  is.integer(rais_wages$year),
  is.integer(rais_wages$n_bonds),
  is.integer(rais_wages$n_bonds_priv),
  is.double(rais_wages$wage_median_nom),
  is.double(rais_wages$share_le_mw100_priv)
)

# ---- Write ------------------------------------------------------------------
write_rds(rais_wages, "_data/interim/rais_wage_distribution_1999_2024.rds")
write_csv(rais_wages, "_data/interim/rais_wage_distribution_1999_2024.csv")
