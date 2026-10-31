# 06-normalized-outcome.R
# PURPOSE   Build the outcome columns on the sample panel: the annual
#           deforestation increment centred and scaled on each municipality's
#           own pre-treatment reference (2000-2007), and the log of lagged
#           forest area as the exposure offset for the Poisson specifications.
#           Then rename the panel through the crosswalk, guard the naming
#           convention and the codebook, and write the final panel.
#
# INPUTS    _data/interim/panel_amz_2000_2020_sample.rds  05-sample-definition.R
#           _data/aux/crosswalk.csv                        phase 2R rename
#           _data/aux/codebook.csv                         one row per column
#
# OUTPUTS   _data/final/panel_amz_2000_2020_final.rds / .csv
#                           807 municipalities x 2000-2020 (the nine Legal
#                           Amazon states from phase 2, step 2.9), 76
#                           columns under the names of
#                           rebuild/naming-convention.md: the sample-panel
#                           columns plus env_defor_area_z_bl0007 and
#                           env_forest_exposure_log
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence:
#            _audit/panel-outcome.qmd; history: DECISIONS.md)
#   D-2026-10-05-h  Outcome defor_norm_ref0007 (env_defor_area_z_bl0007).
#   D-2026-10-12-f  Within-municipality normalisation on a pre-treatment
#                   reference; a degenerate reference sd gives NA. From
#                   phase 2R only the main window (2000-2007) is stored;
#                   the short window (2000-2003) and any other are built
#                   in the analysis scripts with norm_within()
#                   (D-2026-10-30-e).
#   D-2026-10-12-g  Exposure offset = log of the previous year's forest area;
#                   NA in the first panel year.
#   D-2026-10-12-h  Task 1.10 at script 06: typed final output; the
#                   diagnostics leave the pipeline; helper columns never
#                   stored.
#   D-2026-10-30-a  Phase 2R rename: the final panel takes the names of
#                   _data/aux/crosswalk.csv as the last transform; the
#                   convention guard and the codebook row-and-type check
#                   run before the write.
#   D-2026-10-30-e  Short-window outcome left the pipeline.
# RUN ORDER After 05-sample-definition.R. Last pipeline script; the analysis
#           scripts (07+) read the final panel.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
source("_scripts/utils/crosswalk.R")
source("_scripts/utils/derived_variables.R")

# ---- Constants --------------------------------------------------------------
DEFOR_REF_MAIN_YEARS  <- 2000:2007   # D-2026-10-12-f, D-2026-10-05-h
DEFOR_SD_FLOOR        <- 1e-9        # D-2026-10-12-f
PANEL_YEARS           <- 2000:2020   # D-2026-10-11-a
N_MUNIC_EXPECTED      <- 807L        # D-2026-10-29-a (808 of the nine states), D-2026-10-05-l (Mojuí dropped)
# Script 05 output: 70 panel columns from script 04 (73 at step 2.9, minus
# the four z-scores plus the cattle lag, D-2026-10-30-b) plus the three
# gates and the main treatment flag (D-2026-10-30-c, -d).
N_SAMPLE_COLS_EXPECTED <- 74L
N_FINAL_COLS_EXPECTED  <- 76L
CODEBOOK_PATH <- "_data/aux/codebook.csv"
CODEBOOK_DATASET <- "panel"          # rows of the final panel
# panel-outcome.qmd Q2: Santa Cruz do Arari (main window); none in the
# at-risk sample.
N_DEGENERATE_MAIN_EXPECTED  <- 1L

# The column name embeds the reference window (template rule 5); the
# crosswalk carries it to env_defor_area_z_bl0007.
ref_tag <- function(years) {
  sprintf("%02d%02d", min(years) %% 100L, max(years) %% 100L)
}
DEFOR_NORM_MAIN  <- paste0("defor_norm_ref", ref_tag(DEFOR_REF_MAIN_YEARS))
stopifnot(DEFOR_NORM_MAIN == "defor_norm_ref0007")
OUTCOME_COLS <- c(DEFOR_NORM_MAIN, "log_forest_exposure")

# ---- Read -------------------------------------------------------------------
panel_in <- read_rds("_data/interim/panel_amz_2000_2020_sample.rds")
xwalk    <- read_crosswalk()                                  # D-2026-10-30-a
codebook <- read_csv(CODEBOOK_PATH, col_types = cols(.default = "c"),
                     show_col_types = FALSE, progress = FALSE)

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
  is.double(panel_in$deforestation_area_km2),
  !anyNA(panel_in$deforestation_area_km2),
  all(panel_in$deforestation_area_km2 >= 0),
  is.double(panel_in$forest_area_km2),
  !anyNA(panel_in$forest_area_km2),
  all(panel_in$forest_area_km2 > 0),
  is.logical(panel_in$at_risk_broad),
  !any(OUTCOME_COLS %in% names(panel_in)),
  all(names(panel_in) %in% xwalk$old_name),
  all(c("variable", "dataset", "type") %in% names(codebook))
)

# ---- Transform --------------------------------------------------------------
# Each municipality is centred and scaled on its own pre-treatment years so
# the outcome scale carries no policy effect; under municipality fixed
# effects only the sd matters, the subtracted mean being absorbed
# (D-2026-10-12-f). A municipality with no reference variation cannot be
# normalised and gets NA, never Inf (norm_within() in
# utils/derived_variables.R, shared with the analysis scripts for any other
# window, D-2026-10-30-e).
panel <- panel_in %>%
  arrange(geocode, year) %>%
  norm_within("deforestation_area_km2", DEFOR_REF_MAIN_YEARS,
              DEFOR_NORM_MAIN, DEFOR_SD_FLOOR) %>%
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
  na_munic(panel[[DEFOR_NORM_MAIN]])  == N_DEGENERATE_MAIN_EXPECTED,
  sum(is.na(panel[[DEFOR_NORM_MAIN]])) ==
    N_DEGENERATE_MAIN_EXPECTED * length(PANEL_YEARS),
  !any(is.na(panel[[DEFOR_NORM_MAIN]]) & panel$at_risk_broad),
  # The offset is missing in the first year only.
  all(is.na(panel$log_forest_exposure[first_year])),
  !anyNA(panel$log_forest_exposure[!first_year])
)

# ---- Rename (D-2026-10-30-a) -------------------------------------------------
# The last transform of the pipeline: the crosswalk is the single source of
# the names, and the guard fails on any column outside the grammar.
panel <- rename_through_crosswalk(panel, xwalk)

# ---- Assert: names and codebook ---------------------------------------------
# The panel carries exactly the crosswalk's panel columns, in the pipeline's
# order; the codebook has one row per column with the type the data holds
# (rebuild/04-codebook-and-decisions.md, section 1).
codebook_panel <- codebook %>% filter(dataset == CODEBOOK_DATASET)
panel_types <- map_chr(panel, typeof)
stopifnot(
  ncol(panel) == N_FINAL_COLS_EXPECTED,
  identical(names(panel), xwalk$new_name[xwalk$panel]),
  assert_naming_convention(names(panel)),
  setequal(names(panel), codebook_panel$variable),
  !anyDuplicated(codebook_panel$variable),
  identical(unname(panel_types[codebook_panel$variable]),
            codebook_panel$type)
)

# ---- Write ------------------------------------------------------------------
write_rds(panel, "_data/final/panel_amz_2000_2020_final.rds")
write_csv(panel, "_data/final/panel_amz_2000_2020_final.csv")
