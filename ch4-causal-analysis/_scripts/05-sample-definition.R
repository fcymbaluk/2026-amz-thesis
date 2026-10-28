# 05-sample-definition.R
# PURPOSE   Define the estimation samples on pre-treatment characteristics:
#           drop Mojuí dos Campos, summarise each municipality's 2000-2003
#           baseline, flag the forest-availability floor and the broad and
#           narrow at-risk frontier gates, build the window-consistent
#           PPCDAm treatment flags and the continuous pressure scores, and
#           carry the flags and scores onto the panel.
#
# INPUTS    _data/interim/panel_amz_2000_2020.rds   04-panel-assemble.R
#           _data/interim/census_anchors_2000_2022.rds 02-social-census.R
#           _data/interim/ibge_pop_2000_2022.rds       03-controls-gdp.R
#
# OUTPUTS   _data/interim/panel_amz_2000_2020_sample.rds / .csv
#                           panel, 502 municipalities x 2000-2020, with the
#                           three sample flags, three pressure scores and
#                           three treatment flags appended
#           _data/interim/panel_amz_baseline.rds / .csv
#                           one row per municipality: baseline summaries,
#                           flags and scores, and the census anchors of
#                           PIA, PEA, informality and population as read
#                           (`_c2000`, `_c2010`, `_c2022`)
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
#   D-2026-10-26-a  Census anchors in the municipality store (phase 2,
#                   step 2.6): the values as read at the census years,
#                   never the interpolated series, are the baseline values
#                   a pre-treatment covariate takes.
#
#   D-2026-10-28-d  Installation rule (phase 2, step 2.7): Ipiranga do
#                   Norte and Itanhangá (MT, installed 2005) are NA in
#                   the 2000-2003 window, so their pressure_agri and
#                   pressure_score stay NA, never imputed from the
#                   parents; Santa Cruz do Arari has no crop series
#                   (D-2026-10-10-i).
#   D-2026-10-28-g  forest_share_2000 and the cattle term of pressure_agri
#                   divide by the 2022 municipal area (phase 2, step 2.7),
#                   the MapBiomas mesh; Nova Ubiratã pre-split.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
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
CENSUS_YEARS              <- c(2000L, 2010L, 2022L)   # D-2026-10-26-a
ANCHOR_VARS               <- c("pia", "pea", "informal", "population")
POP_CENSUS_SOURCE         <- "census"    # pop_source of 03-controls-gdp.R
# panel-sample.qmd Q9: the nine Mato Grosso municipalities installed after
# the 2000 census have no 2000 anchor (D-2026-10-23-d).
N_ANCHOR_NA_2000_EXPECTED <- 9L
N_MUNIC_IN_EXPECTED       <- 503L        # script 04 output; task 1.1
N_MUNIC_EXPECTED          <- 502L        # D-2026-10-05-l; task 1.1
N_PANEL_COLS_EXPECTED     <- 71L   # 73 + bf_regime (D-2026-10-22-b) - 8 elec_*_gov (D-2026-10-22-e) + 2 muni_* (D-2026-10-23-f) + 3 counts (D-2026-10-28-a)
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
ANCHOR_COLS <- paste0(rep(ANCHOR_VARS, each = length(CENSUS_YEARS)), "_c",
                      rep(CENSUS_YEARS, times = length(ANCHOR_VARS)))
MUNI_COLS <- c("geocode", "municipality", "state_abbreviation",
               "defor_area_b", "defor_rate_b", "cattle_dens_b",
               "crop_share_b", "forest_share_2000", "forest_km2_2000",
               "log_defor_area_b", "log_cattle_dens_b", "log_crop_share_b",
               SAMPLE_COLS, ANCHOR_COLS)

# ---- Read -------------------------------------------------------------------
panel_in       <- read_rds("_data/interim/panel_amz_2000_2020.rds")
census_anchors <- read_rds("_data/interim/census_anchors_2000_2022.rds")
ibge_pop       <- read_rds("_data/interim/ibge_pop_2000_2022.rds")

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
  !any(SAMPLE_COLS %in% names(panel_in)),
  !any(ANCHOR_COLS %in% names(panel_in)),
  # panel-sample.qmd Q9: one anchor row per municipality and census year,
  # the population anchors marked in the population file.
  setequal(census_anchors$year, CENSUS_YEARS),
  !anyDuplicated(paste(census_anchors$geocode, census_anchors$year)),
  all(panel_in$geocode %in% census_anchors$geocode),
  all(panel_in$geocode %in%
        ibge_pop$geocode[ibge_pop$pop_source %in% POP_CENSUS_SOURCE &
                           ibge_pop$year == max(CENSUS_YEARS)])
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

# The census anchors as read, wide by vintage (D-2026-10-26-a). The annual
# series of the panel blend the next census into every intercensal year,
# so a baseline covariate measured on them carries post-treatment
# information; the store holds the clean values.
anchors <- census_anchors %>%
  left_join(ibge_pop %>%
              filter(pop_source %in% POP_CENSUS_SOURCE) %>%
              select(geocode, year, population),
            by = c("geocode", "year")) %>%
  filter(geocode %in% panel$geocode) %>%
  pivot_wider(id_cols = geocode, names_from = year,
              values_from = all_of(ANCHOR_VARS),
              names_glue = "{.value}_c{year}") %>%
  select(geocode, all_of(ANCHOR_COLS))

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
  left_join(anchors, by = "geocode") %>%
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
# panel-sample.qmd Q2-Q7, Q9. The anchors agree with the panel's values at
# the census years inside the window, NA included.
same_or_na <- function(a, b) {
  all((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-9))
}
anchor_vs_panel <- panel %>%
  filter(year %in% CENSUS_YEARS) %>%
  select(geocode, year, pea, informal, population) %>%
  left_join(anchors, by = "geocode") %>%
  mutate(pea_c = if_else(year == CENSUS_YEARS[1], pea_c2000, pea_c2010),
         informal_c = if_else(year == CENSUS_YEARS[1], informal_c2000,
                              informal_c2010),
         population_c = if_else(year == CENSUS_YEARS[1], population_c2000,
                                population_c2010))
anchor_2000_cols <- ANCHOR_COLS[str_detect(ANCHOR_COLS, "_c2000$")]
anchor_later_cols <- setdiff(ANCHOR_COLS, anchor_2000_cols)
stopifnot(
  setequal(panel$year[panel$year %in% CENSUS_YEARS], CENSUS_YEARS[1:2]),
  same_or_na(anchor_vs_panel$pea, anchor_vs_panel$pea_c),
  same_or_na(anchor_vs_panel$informal, anchor_vs_panel$informal_c),
  same_or_na(anchor_vs_panel$population, anchor_vs_panel$population_c),
  all(map_int(muni[anchor_2000_cols], ~ sum(is.na(.x))) ==
        N_ANCHOR_NA_2000_EXPECTED),
  n_distinct(map(muni[anchor_2000_cols], is.na)) == 1L,
  !anyNA(muni[anchor_later_cols]),
  all(map_lgl(muni[ANCHOR_COLS], is.double)),
  all(unlist(muni[ANCHOR_COLS]) >= 0, na.rm = TRUE),
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
