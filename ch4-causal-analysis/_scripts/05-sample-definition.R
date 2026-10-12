# 05-sample-definition.R
# PURPOSE   Define the estimation samples on pre-treatment characteristics:
#           drop Mojuí dos Campos, summarise each municipality's 2000-2003
#           baseline, flag the forest-availability floor and the broad and
#           narrow at-risk frontier gates, build the window-consistent
#           PPCDAm treatment flags and the continuous pressure scores, and
#           carry the flags and scores onto the panel.
#
# INPUTS    _data/interim/panel_amz_2000_2020.rds   04-panel-assemble.R
#
# OUTPUTS   _data/interim/panel_amz_2000_2020_sample.rds / .csv
#                           panel, 502 municipalities x 2000-2020, with the
#                           three sample flags, three pressure scores and
#                           three treatment flags appended
#           _data/interim/panel_amz_baseline.rds / .csv
#                           one row per municipality: baseline summaries,
#                           flags and scores (the task 1.7 anchor store)
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence:
#            _audit/panel-sample.qmd; history: DECISIONS.md)
#   D-2026-10-05-l  Mojuí dos Campos dropped (no series before 2013).
#   D-2026-10-05-h  Availability floor and the broad and narrow at-risk
#                   gates on 2000-2003 baseline clearing area.
#   D-2026-10-12-a  Baseline summaries: 2000-2003 means, 2000 forest share;
#                   NA where the municipality has no baseline value.
#   D-2026-10-12-b  Treatment flags defined on the analysis window
#                   (2004-2010 main, 2004-2012 robust, full panel ever).
#   D-2026-10-12-c  Pressure scores z-scored across the 502 municipalities;
#                   flagged for the storage audit (task 2.2).
#   D-2026-10-12-d  Santa Cruz do Arari enters the availability floor with
#                   its restored area (reference: FALSE).
#   D-2026-10-12-e  Task 1.10 at script 05: typed output; the diagnostics
#                   leave the pipeline, the municipality lookup moves to
#                   _data/interim/.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.6  Ipiranga do Norte and Itanhangá (MT) have no 2000-2003 PAM or
#             PPM rows, so their pressure_agri and pressure_score are NA
#             (D-2026-10-10-g); Santa Cruz do Arari has no crop series
#             (D-2026-10-10-i).
#   Task 1.8  Nova Ubiratã's forest share and cattle density are computed
#             on the post-split 2025 area (D-2026-10-11-c).
#   Task 2.2  The cross-sectional standardisation of the pressure scores is
#             sample-dependent (D-2026-10-12-c).
# RUN ORDER After 04-panel-assemble.R. Script 06 reads the panel output.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
SAMPLE_BASE_YEARS         <- 2000:2003   # D-2026-10-12-a
SAMPLE_FOREST_SHARE_FLOOR <- 0.10        # D-2026-10-05-h
SAMPLE_DEFOR_CUT_BROAD    <- 1           # km2 per year, D-2026-10-05-h
SAMPLE_DEFOR_CUT_NARROW   <- 5           # km2 per year, D-2026-10-05-h
PPCDAM_WINDOW_START       <- 2004L       # D-2026-10-12-b
PPCDAM_WINDOW_END_MAIN    <- 2010L       # D-2026-10-12-b
PPCDAM_WINDOW_END_ROBUST  <- 2012L       # D-2026-10-12-b
PANEL_YEARS               <- 2000:2020   # D-2026-10-11-a
MOJUI_GEOCODE             <- "1504752"   # D-2026-10-05-l
N_MUNIC_IN_EXPECTED       <- 503L        # script 04 output; task 1.1
N_MUNIC_EXPECTED          <- 502L        # D-2026-10-05-l; task 1.1
N_PANEL_COLS_EXPECTED     <- 73L
# panel-sample.qmd Q3-Q4: cohorts 2008-2009 in the main window, 2011-2012
# added in the robust window; every treated municipality is at risk.
N_TREATED_MAIN_EXPECTED   <- 43L
N_TREATED_ROBUST_EXPECTED <- 51L
N_TREATED_EVER_EXPECTED   <- 61L
# panel-sample.qmd Q2, Q4: 500 above the floor (D-2026-10-12-d), 468 and
# 408 in the broad and narrow gates.
N_AVAIL_EXPECTED          <- 500L
N_BROAD_EXPECTED          <- 468L
N_NARROW_EXPECTED         <- 408L
# panel-sample.qmd Q2: Ipiranga do Norte, Itanhangá, Santa Cruz do Arari.
N_AGRI_NA_EXPECTED        <- 3L
Z_TOL                     <- 1e-6

SAMPLE_COLS <- c("pressure_defor", "pressure_agri", "pressure_score",
                 "avail_ok", "at_risk_broad", "at_risk_narrow",
                 "ppcdam_main", "ppcdam_robust", "ppcdam_ever_full")
SAMPLE_LGL_COLS <- c("avail_ok", "at_risk_broad", "at_risk_narrow")
SAMPLE_INT_COLS <- c("ppcdam_main", "ppcdam_robust",
                     "ppcdam_ever_full")                      # D-2026-10-12-e
MUNI_COLS <- c("geocode", "municipality", "state_abbreviation",
               "defor_area_b", "defor_rate_b", "cattle_dens_b",
               "crop_share_b", "forest_share_2000", "forest_km2_2000",
               "log_defor_area_b", "log_cattle_dens_b", "log_crop_share_b",
               SAMPLE_COLS)

# ---- Read -------------------------------------------------------------------
panel_in <- read_rds("_data/interim/panel_amz_2000_2020.rds")

# ---- Assert: raw state ------------------------------------------------------
# panel-sample.qmd Q1: the script 04 panel is complete and typed, and
# the baseline inputs exist for every municipality in the window.
stopifnot(
  ncol(panel_in) == N_PANEL_COLS_EXPECTED,
  n_distinct(panel_in$geocode) == N_MUNIC_IN_EXPECTED,
  nrow(panel_in) == N_MUNIC_IN_EXPECTED * length(PANEL_YEARS),
  !anyDuplicated(paste(panel_in$geocode, panel_in$year)),
  is.character(panel_in$geocode), is.integer(panel_in$year),
  is.integer(panel_in$ppcdam_list), !anyNA(panel_in$ppcdam_list),
  MOJUI_GEOCODE %in% panel_in$geocode,
  all(SAMPLE_BASE_YEARS %in% panel_in$year),
  !anyNA(panel_in$deforestation_area_km2),
  !anyNA(panel_in$forest_area_km2),
  !anyNA(panel_in$munic_area_km2),
  !any(SAMPLE_COLS %in% names(panel_in))
)

# ---- Transform --------------------------------------------------------------
panel <- panel_in %>% filter(geocode != MOJUI_GEOCODE)   # D-2026-10-05-l

# A municipality with no baseline value in a source gets NA, not NaN
# (D-2026-10-12-a).
mean_base <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
z_cross <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

baseline <- panel %>%
  filter(year %in% SAMPLE_BASE_YEARS) %>%
  mutate(crop_share_all = crop_soy_munic_area_share +
           crop_corn_munic_area_share + crop_rice_munic_area_share +
           crop_cotton_munic_area_share) %>%
  group_by(geocode, municipality, state_abbreviation) %>%
  summarise(
    defor_area_b  = mean_base(deforestation_area_km2),
    defor_rate_b  = mean_base(deforestation_forest_rate),
    cattle_dens_b = mean_base(cattle_per_km2),
    crop_share_b  = mean_base(crop_share_all),
    .groups = "drop"
  )

forest_first <- panel %>%
  filter(year == min(SAMPLE_BASE_YEARS)) %>%
  transmute(geocode,
            forest_share_2000 = forest_area_km2 / munic_area_km2,
            forest_km2_2000   = forest_area_km2)
stopifnot(min(SAMPLE_BASE_YEARS) == 2000L)   # the column name embeds it

# Treated means listed at any point within the window, so a municipality
# first listed after the window is a not-yet-treated control in that
# design (D-2026-10-12-b).
listed_within <- function(d, y_start, y_end, name) {
  d %>%
    filter(year >= y_start, year <= y_end) %>%
    group_by(geocode) %>%
    summarise(!!name := as.integer(max(ppcdam_list) == 1L),
              .groups = "drop")
}

muni <- baseline %>%
  left_join(forest_first, by = "geocode") %>%
  left_join(listed_within(panel, PPCDAM_WINDOW_START,
                          PPCDAM_WINDOW_END_MAIN, "ppcdam_main"),
            by = "geocode") %>%
  left_join(listed_within(panel, PPCDAM_WINDOW_START,
                          PPCDAM_WINDOW_END_ROBUST, "ppcdam_robust"),
            by = "geocode") %>%
  left_join(listed_within(panel, min(PANEL_YEARS), max(PANEL_YEARS),
                          "ppcdam_ever_full"),
            by = "geocode") %>%
  # Right-skewed baselines are logged before the cross-sectional z-scores;
  # pressure_agri is a half-half mix of two z-scores and is not
  # re-standardised (D-2026-10-12-c).
  mutate(
    log_defor_area_b  = log1p(defor_area_b),
    log_cattle_dens_b = log1p(cattle_dens_b),
    log_crop_share_b  = log1p(crop_share_b),
    pressure_defor    = z_cross(log_defor_area_b),
    pressure_agri     = 0.5 * z_cross(log_cattle_dens_b) +
      0.5 * z_cross(log_crop_share_b),
    pressure_score    = z_cross(pressure_defor + pressure_agri),
    # Absolute baseline clearing, not the rate: listing is assigned on
    # area, so the gate preserves overlap with treatment (D-2026-10-05-h).
    avail_ok          = forest_share_2000 >= SAMPLE_FOREST_SHARE_FLOOR,
    at_risk_broad     = avail_ok & defor_area_b >= SAMPLE_DEFOR_CUT_BROAD,
    at_risk_narrow    = avail_ok & defor_area_b >= SAMPLE_DEFOR_CUT_NARROW
  ) %>%
  select(all_of(MUNI_COLS)) %>%
  arrange(geocode)

panel <- panel %>%
  left_join(select(muni, geocode, all_of(SAMPLE_COLS)), by = "geocode") %>%
  arrange(geocode, year)

# ---- Assert: output state ---------------------------------------------------
# panel-sample.qmd Q2-Q7.
stopifnot(
  nrow(muni) == N_MUNIC_EXPECTED,
  !anyDuplicated(muni$geocode),
  identical(names(muni), MUNI_COLS),
  nrow(panel) == N_MUNIC_EXPECTED * length(PANEL_YEARS),
  !anyDuplicated(paste(panel$geocode, panel$year)),
  identical(names(panel), c(names(panel_in), SAMPLE_COLS)),
  !MOJUI_GEOCODE %in% panel$geocode,
  !anyNA(muni$defor_area_b), !anyNA(muni$forest_share_2000),
  sum(is.na(muni$pressure_agri)) == N_AGRI_NA_EXPECTED,
  all(map_lgl(muni[SAMPLE_LGL_COLS], ~ is.logical(.x) && !anyNA(.x))),
  all(map_lgl(muni[SAMPLE_INT_COLS], ~ is.integer(.x) && !anyNA(.x))),
  all(unlist(muni[SAMPLE_INT_COLS]) %in% 0:1),
  sum(muni$avail_ok)       == N_AVAIL_EXPECTED,
  sum(muni$at_risk_broad)  == N_BROAD_EXPECTED,
  sum(muni$at_risk_narrow) == N_NARROW_EXPECTED,
  all(muni$at_risk_narrow <= muni$at_risk_broad),
  all(muni$at_risk_broad  <= muni$avail_ok),
  sum(muni$ppcdam_main)      == N_TREATED_MAIN_EXPECTED,
  sum(muni$ppcdam_robust)    == N_TREATED_ROBUST_EXPECTED,
  sum(muni$ppcdam_ever_full) == N_TREATED_EVER_EXPECTED,
  all(muni$ppcdam_main <= muni$ppcdam_robust),
  all(muni$ppcdam_robust <= muni$ppcdam_ever_full),
  # The gates remove controls only: every treated municipality survives.
  all(muni$at_risk_narrow[muni$ppcdam_robust == 1L]),
  all(abs(map_dbl(muni[c("pressure_defor", "pressure_score")],
                  mean, na.rm = TRUE)) < Z_TOL),
  all(abs(map_dbl(muni[c("pressure_defor", "pressure_score")],
                  sd, na.rm = TRUE) - 1) < Z_TOL),
  # Every appended column is constant within municipality.
  all(map_lgl(panel[SAMPLE_COLS],
              ~ n_distinct(paste(panel$geocode, .x)) == N_MUNIC_EXPECTED))
)

# ---- Write ------------------------------------------------------------------
write_rds(panel, "_data/interim/panel_amz_2000_2020_sample.rds")
write_csv(panel, "_data/interim/panel_amz_2000_2020_sample.csv")
write_rds(muni, "_data/interim/panel_amz_baseline.rds")
write_csv(muni, "_data/interim/panel_amz_baseline.csv")
