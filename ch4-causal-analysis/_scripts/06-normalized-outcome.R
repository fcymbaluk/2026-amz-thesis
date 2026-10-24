# 06-normalized-outcome.R
# PURPOSE   Build the outcome columns on the sample panel: the annual
#           deforestation increment centred and scaled on each municipality's
#           own pre-treatment reference (two windows), and the log of lagged
#           forest area as the exposure offset for the Poisson specifications.
#           Writes the phase 1 final panel.
#
# INPUTS    _data/interim/panel_amz_2000_2020_sample.rds  05-sample-definition.R
#
# OUTPUTS   _data/final/panel_amz_2000_2020_final.rds / .csv
#                           502 municipalities x 2000-2020, the 82 sample-panel
#                           columns plus defor_norm_ref0003, defor_norm_ref0007
#                           and log_forest_exposure
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence:
#            _audit/panel-outcome.qmd; history: DECISIONS.md)
#   D-2026-10-05-h  Outcome defor_norm_ref0007; defor_norm_ref0003 as the
#                   short-window alternative.
#   D-2026-10-12-f  Within-municipality normalisation on a pre-treatment
#                   reference (2000-2007 main, 2000-2003 short); a degenerate
#                   reference sd gives NA; flagged for the storage audit
#                   (task 2.2).
#   D-2026-10-12-g  Exposure offset = log of the previous year's forest area;
#                   NA in the first panel year.
#   D-2026-10-12-h  Task 1.10 at script 06: typed final output; the
#                   diagnostics leave the pipeline; helper columns never
#                   stored.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.8  Nova Ubiratã's forest area refers to the pre-split territory
#             (D-2026-10-11-c); its exposure offset follows the mapped area.
# RUN ORDER After 05-sample-definition.R. Last pipeline script; the analysis
#           scripts (07+) read the final panel.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
DEFOR_REF_SHORT_YEARS <- 2000:2003   # D-2026-10-12-f
DEFOR_REF_MAIN_YEARS  <- 2000:2007   # D-2026-10-12-f, D-2026-10-05-h
DEFOR_SD_FLOOR        <- 1e-9        # D-2026-10-12-f
PANEL_YEARS           <- 2000:2020   # D-2026-10-11-a
N_MUNIC_EXPECTED      <- 502L        # D-2026-10-05-l; task 1.1
N_SAMPLE_COLS_EXPECTED <- 77L        # script 05 output; 82 + bf_regime (D-2026-10-22-b) - 8 elec_*_gov (D-2026-10-22-e) + 2 muni_* (D-2026-10-23-f)
# panel-outcome.qmd Q2: Afuá and Santa Cruz do Arari (short window), Santa
# Cruz do Arari (main window); none in the at-risk sample.
N_DEGENERATE_SHORT_EXPECTED <- 2L
N_DEGENERATE_MAIN_EXPECTED  <- 1L

# The column names embed the reference windows (template rule 5).
ref_tag <- function(years) {
  sprintf("%02d%02d", min(years) %% 100L, max(years) %% 100L)
}
DEFOR_NORM_SHORT <- paste0("defor_norm_ref", ref_tag(DEFOR_REF_SHORT_YEARS))
DEFOR_NORM_MAIN  <- paste0("defor_norm_ref", ref_tag(DEFOR_REF_MAIN_YEARS))
stopifnot(DEFOR_NORM_SHORT == "defor_norm_ref0003",
          DEFOR_NORM_MAIN  == "defor_norm_ref0007")
OUTCOME_COLS <- c(DEFOR_NORM_SHORT, DEFOR_NORM_MAIN, "log_forest_exposure")

# ---- Read -------------------------------------------------------------------
panel_in <- read_rds("_data/interim/panel_amz_2000_2020_sample.rds")

# ---- Assert: raw state ------------------------------------------------------
# panel-outcome.qmd Q1: the sample panel is complete and typed, the two
# series the outcome needs have no gap, and forest area is positive so the
# log offset is defined wherever the lag exists.
stopifnot(
  ncol(panel_in) == N_SAMPLE_COLS_EXPECTED,
  n_distinct(panel_in$geocode) == N_MUNIC_EXPECTED,
  nrow(panel_in) == N_MUNIC_EXPECTED * length(PANEL_YEARS),
  !anyDuplicated(paste(panel_in$geocode, panel_in$year)),
  is.character(panel_in$geocode), is.integer(panel_in$year),
  setequal(panel_in$year, PANEL_YEARS),
  all(DEFOR_REF_MAIN_YEARS %in% PANEL_YEARS),
  all(DEFOR_REF_SHORT_YEARS %in% DEFOR_REF_MAIN_YEARS),
  is.double(panel_in$deforestation_area_km2),
  !anyNA(panel_in$deforestation_area_km2),
  all(panel_in$deforestation_area_km2 >= 0),
  is.double(panel_in$forest_area_km2),
  !anyNA(panel_in$forest_area_km2),
  all(panel_in$forest_area_km2 > 0),
  is.logical(panel_in$at_risk_broad),
  !any(OUTCOME_COLS %in% names(panel_in))
)

# ---- Transform --------------------------------------------------------------
# Each municipality is centred and scaled on its own pre-treatment years so
# the outcome scale carries no policy effect; under municipality fixed
# effects only the sd matters, the subtracted mean being absorbed
# (D-2026-10-12-f). A municipality with no reference variation cannot be
# normalised and gets NA, never Inf.
ref_stats <- function(d, years) {
  d %>%
    filter(year %in% years) %>%
    group_by(geocode) %>%
    summarise(ref_mean = mean(deforestation_area_km2),
              ref_sd   = sd(deforestation_area_km2),
              .groups = "drop")
}

norm_on <- function(d, years, name) {
  d %>%
    left_join(ref_stats(d, years), by = "geocode") %>%
    mutate(!!name := if_else(ref_sd > DEFOR_SD_FLOOR,
                             (deforestation_area_km2 - ref_mean) / ref_sd,
                             NA_real_)) %>%
    select(-ref_mean, -ref_sd)
}

panel <- panel_in %>%
  arrange(geocode, year) %>%
  norm_on(DEFOR_REF_SHORT_YEARS, DEFOR_NORM_SHORT) %>%
  norm_on(DEFOR_REF_MAIN_YEARS, DEFOR_NORM_MAIN) %>%
  # Exposure is the forest standing at the start of the year, the stock at
  # risk of being cleared (D-2026-10-12-g).
  group_by(geocode) %>%
  mutate(log_forest_exposure = log(lag(forest_area_km2, 1L))) %>%
  ungroup()

# ---- Assert: output state ---------------------------------------------------
# panel-outcome.qmd Q2-Q4.
first_year <- panel$year == min(PANEL_YEARS)
na_munic <- function(x) n_distinct(panel$geocode[is.na(x)])
stopifnot(
  nrow(panel) == N_MUNIC_EXPECTED * length(PANEL_YEARS),
  !anyDuplicated(paste(panel$geocode, panel$year)),
  identical(names(panel), c(names(panel_in), OUTCOME_COLS)),
  all(map_lgl(panel[OUTCOME_COLS], is.double)),
  all(map_lgl(panel[OUTCOME_COLS], ~ all(is.finite(.x) | is.na(.x)))),
  # A degenerate reference blanks the whole municipality, and no at-risk
  # municipality is degenerate.
  na_munic(panel[[DEFOR_NORM_SHORT]]) == N_DEGENERATE_SHORT_EXPECTED,
  na_munic(panel[[DEFOR_NORM_MAIN]])  == N_DEGENERATE_MAIN_EXPECTED,
  sum(is.na(panel[[DEFOR_NORM_SHORT]])) ==
    N_DEGENERATE_SHORT_EXPECTED * length(PANEL_YEARS),
  sum(is.na(panel[[DEFOR_NORM_MAIN]])) ==
    N_DEGENERATE_MAIN_EXPECTED * length(PANEL_YEARS),
  !any(is.na(panel[[DEFOR_NORM_SHORT]]) & panel$at_risk_broad),
  !any(is.na(panel[[DEFOR_NORM_MAIN]]) & panel$at_risk_broad),
  # The offset is missing in the first year only.
  all(is.na(panel$log_forest_exposure[first_year])),
  !anyNA(panel$log_forest_exposure[!first_year])
)

# ---- Write ------------------------------------------------------------------
write_rds(panel, "_data/final/panel_amz_2000_2020_final.rds")
write_csv(panel, "_data/final/panel_amz_2000_2020_final.csv")
