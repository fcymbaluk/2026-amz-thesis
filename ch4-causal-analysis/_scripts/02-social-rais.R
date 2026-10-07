# 02-social-rais.R
# PURPOSE   Build the municipality-year formal-employment panel from the RAIS
#           aggregate (workers by wage bracket and agricultural occupation):
#           counts by wage group, employment rates over the working-age
#           population (PIA) and wage-group shares, 1999-2024, nine Legal
#           Amazon states.
#
# INPUTS    _data/raw/rais/export_table_v4.csv   Own BigQuery extraction from
#                                                Base dos Dados RAIS (Query 2;
#                                                table export_table_v4 of
#                                                2025-11-25, exported
#                                                2026-10-06)
#           _data/interim/census_pia_1999_2024.rds   02-social-census.R
#
# OUTPUTS   _data/interim/rais_employment_1999_2024.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd
#            §2.1 and §2.3 until the annex is written; evidence:
#            _audit/rais.qmd; history: DECISIONS.md)
#   D-2026-10-05-d  Wage codes 1-6 at the 2024 minimum wage, grouped as low
#                   (1), mid (2-4) and high (5-6).
#   D-2026-10-05-e  PIA as the employment denominator.
#   D-2026-10-07-i  PEA-normalised and agricultural-by-wage rates of the
#                   lost script not rebuilt; none reached the panel.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.8  workers_agri_{low,mid,high} stay NA where the municipality-year
#             has no worker at all in that wage group, while the all-worker
#             counts read 0 (the lost script's replace_na() named the wrong
#             columns); shares with a zero denominator are NA, with no count
#             column to tell "no agricultural employment" from "missing".
#             Reproduced on purpose.
# RUN ORDER 02-social-census.R must run first (PIA denominator). Feeds
#           04-data-panel.R.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
RAIS_YEARS       <- 1999:2024
RAIS_WAGE_CODES  <- 1:6                                       # D-2026-10-05-d
RAIS_WAGE_GROUPS <- list(low = 1L, mid = 2:4, high = 5:6)     # D-2026-10-05-d
N_RAIS_ROWS_EXPECTED  <- 174062L
N_RAIS_MUNIC_EXPECTED <- 808L   # nine Legal Amazon states; task 1.1 keeps it

# ---- Read -------------------------------------------------------------------
rais_raw <- read_csv(
  "_data/raw/rais/export_table_v4.csv",
  col_types = cols(
    year                 = col_integer(),
    geocode              = col_character(),
    workers_agricultural = col_integer(),
    wage_code            = col_integer(),
    workers_total        = col_integer()
  ),
  progress = FALSE
)

census_pia <- read_rds("_data/interim/census_pia_1999_2024.rds")

# ---- Assert: raw state ------------------------------------------------------
# _audit/rais.qmd Q1.
stopifnot(
  identical(names(rais_raw), c("year", "geocode", "workers_agricultural",
                               "wage_code", "workers_total")),
  nrow(rais_raw) == N_RAIS_ROWS_EXPECTED,
  !anyNA(rais_raw),
  setequal(rais_raw$year, RAIS_YEARS),
  all(nchar(rais_raw$geocode) == 7),
  n_distinct(rais_raw$geocode) == N_RAIS_MUNIC_EXPECTED,
  all(rais_raw$wage_code %in% RAIS_WAGE_CODES),
  all(rais_raw$workers_agricultural %in% 0:1),
  all(rais_raw$workers_total > 0),
  !anyDuplicated(paste(rais_raw$year, rais_raw$geocode,
                       rais_raw$workers_agricultural, rais_raw$wage_code)),
  all(rais_raw$geocode %in% census_pia$geocode)
)

# ---- Transform --------------------------------------------------------------
wage_group_of <- function(code) {
  case_when(
    code %in% RAIS_WAGE_GROUPS$low  ~ "low",
    code %in% RAIS_WAGE_GROUPS$mid  ~ "mid",
    code %in% RAIS_WAGE_GROUPS$high ~ "high"
  )
}

rais_by_code <- rais_raw %>%
  group_by(year, geocode, wage_code) %>%
  summarise(
    workers_agric = sum(workers_total[workers_agricultural == 1L]),
    workers_total = sum(workers_total),
    .groups = "drop"
  ) %>%
  mutate(wage_group = wage_group_of(wage_code))

workers_wide <- rais_by_code %>%
  group_by(year, geocode, wage_group) %>%
  summarise(workers = sum(workers_total), .groups = "drop") %>%
  pivot_wider(names_from = wage_group, values_from = workers,
              names_prefix = "workers_", values_fill = 0L)

# No values_fill here: the lost script left these cells NA (task 1.8).
workers_agri_wide <- rais_by_code %>%
  group_by(year, geocode, wage_group) %>%
  summarise(workers = sum(workers_agric), .groups = "drop") %>%
  pivot_wider(names_from = wage_group, values_from = workers,
              names_prefix = "workers_agri_")

rais_employment <- rais_by_code %>%
  group_by(year, geocode) %>%
  summarise(workers_total = sum(workers_total),
            workers_agric = sum(workers_agric), .groups = "drop") %>%
  left_join(workers_wide, by = c("year", "geocode")) %>%
  left_join(workers_agri_wide, by = c("year", "geocode")) %>%
  left_join(census_pia, by = c("year", "geocode")) %>%
  mutate(
    emp_pia_rate_total = workers_total / pia,                 # D-2026-10-05-e
    emp_pia_rate_agric = workers_agric / pia,
    emp_pia_rate_low   = workers_low / pia,
    emp_pia_rate_mid   = workers_mid / pia,
    emp_pia_rate_high  = workers_high / pia,
    emp_share_low_workers  = workers_low / workers_total,
    emp_share_mid_workers  = workers_mid / workers_total,
    emp_share_high_workers = workers_high / workers_total,
    # 0 / 0 is stored as NA, not NaN (task 1.8).
    emp_share_agri_low_workers  = if_else(workers_agric > 0L,
                                          workers_agri_low / workers_agric,
                                          NA_real_),
    emp_share_agri_mid_workers  = if_else(workers_agric > 0L,
                                          workers_agri_mid / workers_agric,
                                          NA_real_),
    emp_share_agri_high_workers = if_else(workers_agric > 0L,
                                          workers_agri_high / workers_agric,
                                          NA_real_),
    emp_share_agri_workers = workers_agric / workers_total
  ) %>%
  select(geocode, year, workers_total, workers_agric,
         workers_low, workers_mid, workers_high,
         workers_agri_low, workers_agri_mid, workers_agri_high, pia,
         starts_with("emp_pia_rate_"), starts_with("emp_share_")) %>%
  arrange(geocode, year)

# ---- Assert: output state ---------------------------------------------------
# _audit/rais.qmd Q2, Q3.
share_cols <- rais_employment %>% select(starts_with("emp_share_"))
stopifnot(
  !anyDuplicated(paste(rais_employment$geocode, rais_employment$year)),
  n_distinct(rais_employment$geocode) == N_RAIS_MUNIC_EXPECTED,
  all(rais_employment$workers_total > 0L),
  all(rais_employment$workers_low + rais_employment$workers_mid +
        rais_employment$workers_high == rais_employment$workers_total),
  all(rais_employment$workers_agric <= rais_employment$workers_total),
  all(share_cols >= 0 & share_cols <= 1, na.rm = TRUE),
  all(abs(rais_employment$emp_share_low_workers +
            rais_employment$emp_share_mid_workers +
            rais_employment$emp_share_high_workers - 1) < 1e-12),
  identical(is.na(rais_employment$emp_pia_rate_total),
            is.na(rais_employment$pia)),
  is.character(rais_employment$geocode),
  is.integer(rais_employment$year),
  is.integer(rais_employment$workers_total),
  is.integer(rais_employment$workers_agri_low),
  is.double(rais_employment$pia),
  is.double(rais_employment$emp_pia_rate_total),
  is.double(rais_employment$emp_share_agri_low_workers)
)

# ---- Write ------------------------------------------------------------------
write_rds(rais_employment, "_data/interim/rais_employment_1999_2024.rds")
write_csv(rais_employment, "_data/interim/rais_employment_1999_2024.csv")
