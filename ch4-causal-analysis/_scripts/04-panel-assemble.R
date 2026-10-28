# 04-panel-assemble.R
# PURPOSE   Assemble the municipality-year panel of the Amazon-biome sample
#           (503 municipalities, 2000-2020) from the interim outputs of
#           scripts 01-03, and add the two derived blocks the analysis
#           scripts inherit: pooled z-scores of the price indices and
#           per-capita Bolsa Família transfers.
#
# INPUTS    _data/interim/mapbiomas_deforestation_amz_1987_2023.rds
#                                                     01-env-deforestation.R
#           _data/interim/ppcdam_list_2004_2024.rds   01-env-ppcdam.R
#           _data/interim/bf_panel_2004_2024.rds      02-social-bf.R
#           _data/interim/rais_employment_1999_2024.rds
#                                                     02-social-rais.R
#           _data/interim/census_pea_informal_2000_2022.rds
#                                                     02-social-census.R
#           _data/interim/census_pia_1999_2024.rds    02-social-census.R
#           _data/interim/ibge_pop_2000_2022.rds      03-controls-gdp.R
#           _data/interim/ibge_pib_gdp_2002_2022.rds  03-controls-gdp.R
#           _data/interim/tse_elections_2000_2024.rds 03-controls-elections.R
#           _data/interim/pam_ppm_munic_area_2022.rds 03-controls-agro.R
#           _data/interim/pam_ppm_crop_areas_1988_2025.rds
#                                                     03-controls-agro.R
#           _data/interim/pam_ppm_cattle_1974_2025.rds
#                                                     03-controls-agro.R
#           _data/interim/prices_index_2000_2020.rds  03-controls-prices.R
#           _data/aux/munic_post2000.csv              lookup, D-2026-10-23-f
#
# OUTPUTS   _data/interim/panel_amz_2000_2020.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/panel.qmd;
#            history: DECISIONS.md)
#   D-2026-10-11-a  Left joins onto the MapBiomas biome sample, 2000-2020;
#                   Mojuí dos Campos kept (dropped in script 05).
#   D-2026-10-11-b  ppcdam_list 0 where the list does not cover the
#                   municipality-year; listed municipalities outside the
#                   biome sample fall out of the join.
#   D-2026-10-11-c  Forest and deforestation areas converted from hectares
#                   to km2 at the join.
#   D-2026-10-11-d  Price indices z-scored on the pooled panel; level and
#                   lag share one centre and spread; the cattle lag is
#                   built from the z-score; flagged for the storage audit
#                   (task 2.2). The indices arrive on the exposure scale
#                   and stay there (D-2026-10-23-a).
#   D-2026-10-11-f  bf_transfers_pc_brl_2024 = transfers / population, NA
#                   where population is missing or zero.
#   D-2026-10-11-g  Columns not carried into the panel at the join.
#   D-2026-10-11-h  Task 1.10: typed output; the xlsx copy retired.
#   D-2026-10-22-b  bf_regime carried into the panel (phase 2, step 2.2);
#                   NA before 2004, when the programme did not exist.
#   D-2026-10-22-e  The elec_*_gov lag retired (phase 2, step 2.3): the
#                   elec_* columns arrive on the governing years from
#                   script 03 (D-2026-10-22-d); elec_governing_year stays
#                   in the interim file.
#   D-2026-10-23-e  population joined from its own 2000-2022 series
#                   (phase 2, step 2.5), so 2000-2001 and the post-2000
#                   municipalities' pre-2010 years are filled.
#   D-2026-10-23-f  muni_new_post2000 and muni_parent_split from the
#                   lookup (phase 2, step 2.5); parents flagged, never
#                   adjusted.
#   D-2026-10-28-a  emp_workers_n and emp_agri_workers_n carried from the
#                   RAIS file and pia joined from the census file (phase 2,
#                   step 2.7): the denominators of the emp_* block, so no
#                   agricultural employment can be told from not observed.
#   D-2026-10-28-b  The 64 municipality-years absent from the RAIS export
#                   stay NA in every emp_* column (not observed).
#   D-2026-10-28-d  Installation rule asserted against the lookup: NA
#                   before the installation year in crops, cattle,
#                   employment, population and GDP, observed from it.
#   D-2026-10-28-g  munic_area_km2 is IBGE's 2022 release (phase 2, step
#                   2.7), the mesh of MapBiomas Collection 9: no forest
#                   area exceeds the municipal area.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.9  Areas arrive in hectares from script 01 and are converted
#             here; the conversion moves upstream with the unit pattern.
# RUN ORDER After every 01, 02 and 03 script. Script 05 reads the output.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
PANEL_YEARS   <- 2000:2020                                    # D-2026-10-11-a
BF_REF_MONTH  <- 6L                                           # D-2026-10-05-a
HA_PER_KM2    <- 100                                          # D-2026-10-11-c
ELEC_VARS     <- c("elec_enc", "elec_mov", "elec_winner_share",
                   "elec_n_candidates", "elec_winner_party",
                   "elec_winner_id", "elec_competitive",
                   "elec_uncontested")                        # D-2026-10-22-d
PPCDAM_FIRST_LISTING_YEAR <- 2008L                            # D-2026-10-11-b
# Mojuí dos Campos included, dropped in script 05 (D-2026-10-05-l); update
# at task 1.1.
N_MUNIC_EXPECTED <- 503L
# PPCDAm municipalities outside the biome sample: Grajaú (MA) and three
# Mato Grosso municipalities of the Cerrado (panel.qmd Q2).
N_PPCDAM_OUTSIDE_EXPECTED <- 4L                               # D-2026-10-11-b
# ---- Fix (D-2026-10-28-g; phase 2, step 2.7) ---------------------------------
# The 2022 area is on the MapBiomas mesh, so no forest area exceeds the
# municipal area (was 4: Nova Ubiratã 2000-2003 against the post-split 2025
# area, panel.qmd Q3).
N_FOREST_OVER_AREA_EXPECTED <- 0L
# ---- end fix -----------------------------------------------------------------
Z_TOL <- 1e-6
# Municipality-years after 2000 without a governing election in the data
# (D-2026-10-22-d): Mojuí dos Campos 2001-2012 (installed 2013), and
# 2001-2004 for Ipiranga do Norte, Itanhangá (installed 2005), Altamira do
# Maranhão and Brejo de Areia (no 2000 election in the file; panel.qmd Q5).
N_ELEC_NA_POST2000_EXPECTED <- 28L
POST2000_LOOKUP <- "_data/aux/munic_post2000.csv"             # D-2026-10-23-f
# panel.qmd Q8: the nine Mato Grosso municipalities and Mojuí dos Campos;
# ten of their twelve parents are in the biome sample.
N_POST2000_EXPECTED <- 10L
N_PARENT_EXPECTED   <- 10L
# Municipality-years absent from the RAIS export (rais.qmd Q2; D-2026-10-28-b):
# Mojuí dos Campos 2000-2012, the nine Mato Grosso municipalities before
# installation (17) and 34 reporting gaps of 2000-2005.
N_EMP_NA_EXPECTED <- 64L
# pia is NA before 2010 for the nine municipalities without a 2000 census
# (D-2026-10-23-d) and in every year for Mojuí dos Campos, absent from the
# 2000 and 2010 censuses (panel.qmd Q9).
N_PIA_NA_EXPECTED <- 111L
# Columns observed from the installation year of a lookup municipality and
# NA before it (panel.qmd Q9; D-2026-10-28-d); gdp_brl within its years.
INSTALL_RULE_COLS  <- c("crop_soy_area_planted_km2", "cattle_heads",
                        "emp_workers_n", "population", "gdp_brl")
GDP_FIRST_YEAR     <- 2002L

PANEL_COLS <- c(
  "geocode", "municipality", "state", "state_abbreviation", "year",
  "ppcdam_list", "forest_area_km2", "deforestation_area_km2",
  "deforestation_forest_rate",
  "bf_families_n", "bf_quota", "bf_transfers_total_brl_2024",
  "bf_transfers_avg_brl_2024", "bf_families_quota_ratio",
  "bf_transfers_pea_brl_2024", "bf_transfers_quota_brl_2024",
  "emp_pia_rate_total", "emp_pia_rate_agric", "emp_pia_rate_low",
  "emp_pia_rate_mid", "emp_pia_rate_high", "emp_share_low_workers",
  "emp_share_mid_workers", "emp_share_high_workers",
  "emp_share_agri_low_workers", "emp_share_agri_mid_workers",
  "emp_share_agri_high_workers", "emp_share_agri_workers",
  "pea", "informal", "population",
  "gdp_brl", "gdp_per_capita", "gdp_per_capita_log", "gdp_brl_2024",
  "gdp_per_capita_2024", "gdp_per_capita_2024_log",
  ELEC_VARS, "election_year",
  "munic_area_km2",
  "crop_soy_area_planted_km2", "crop_corn_area_planted_km2",
  "crop_rice_area_planted_km2", "crop_cotton_area_planted_km2",
  "crop_soy_munic_area_share", "crop_corn_munic_area_share",
  "crop_rice_munic_area_share", "crop_cotton_munic_area_share",
  "cattle_heads", "cattle_per_km2",
  "price_index_crop", "price_index_cattle", "price_index_crop_lag1",
  "price_index_crop_z", "price_index_crop_lag1_z", "price_index_cattle_z",
  "price_index_cattle_lag1_z",
  # Phase 2, step 2.3 (D-2026-10-22-e): the eight elec_*_gov columns that
  # stood here are retired.
  "bf_transfers_pc_brl_2024",
  # Phase 2, step 2.2 (D-2026-10-22-b): programme regime of the reference
  # month, appended after the phase 1 columns so their order is unchanged.
  "bf_regime",
  # Phase 2, step 2.5 (D-2026-10-23-f): installed after the 2000 census;
  # ceded territory to such a municipality.
  "muni_new_post2000", "muni_parent_split",
  # Phase 2, step 2.7 (D-2026-10-28-a): the denominators of the emp_*
  # block, formal bonds, agricultural formal bonds and the interpolated
  # working-age population.
  "emp_workers_n", "emp_agri_workers_n", "pia"
)
PANEL_INT_COLS <- c("year", "ppcdam_list", "elec_n_candidates",
                    "elec_competitive", "elec_uncontested",
                    "election_year",                    # D-2026-10-11-h
                    "muni_new_post2000", "muni_parent_split",
                    "emp_workers_n", "emp_agri_workers_n")  # D-2026-10-28-a
PANEL_CHR_COLS <- c("geocode", "municipality", "state", "state_abbreviation",
                    "elec_winner_party", "elec_winner_id", "bf_regime")

# ---- Read -------------------------------------------------------------------
interim <- function(name) read_rds(file.path("_data/interim", name))

mapbiomas  <- interim("mapbiomas_deforestation_amz_1987_2023.rds")
ppcdam     <- interim("ppcdam_list_2004_2024.rds")
bf         <- interim("bf_panel_2004_2024.rds")
rais       <- interim("rais_employment_1999_2024.rds")
census     <- interim("census_pea_informal_2000_2022.rds")
census_pia <- interim("census_pia_1999_2024.rds")
pop        <- interim("ibge_pop_2000_2022.rds")
gdp        <- interim("ibge_pib_gdp_2002_2022.rds")
tse        <- interim("tse_elections_2000_2024.rds")
munic_area <- interim("pam_ppm_munic_area_2022.rds")
crops      <- interim("pam_ppm_crop_areas_1988_2025.rds")
cattle     <- interim("pam_ppm_cattle_1974_2025.rds")
prices     <- interim("prices_index_2000_2020.rds")
post2000   <- read_csv(POST2000_LOOKUP,
                       col_types = cols(.default = col_character(),
                                        install_year = col_integer()))
parents    <- unique(unlist(str_split(post2000$parent_geocodes, ";")))

# ---- Assert: raw state ------------------------------------------------------
# panel.qmd Q1: every input is keyed on geocode (and year), the biome sample
# is complete over PANEL_YEARS, and the PPCDAm indicator is 0/1 where known.
keyed <- list(mapbiomas, ppcdam, rais, census, census_pia, pop, gdp, tse,
              crops, cattle, prices)
sample_geocodes <- sort(unique(mapbiomas$geocode))
stopifnot(
  all(map_lgl(keyed, ~ !anyDuplicated(paste(.x$geocode, .x$year)))),
  all(map_lgl(c(keyed, list(bf)),
              ~ is.character(.x$geocode) && is.integer(.x$year))),
  # The BF file stacks the reference and the alternative month.
  !anyDuplicated(paste(bf$geocode, bf$year, bf$bf_ref_month)),
  BF_REF_MONTH %in% bf$bf_ref_month,
  !anyDuplicated(munic_area$geocode),
  length(sample_geocodes) == N_MUNIC_EXPECTED,
  all(PANEL_YEARS %in% mapbiomas$year),
  nrow(filter(mapbiomas, year %in% PANEL_YEARS)) ==
    N_MUNIC_EXPECTED * length(PANEL_YEARS),
  all(sample_geocodes %in% munic_area$geocode),
  all(ppcdam$ppcdam_list %in% c(0L, 1L, NA)),
  length(setdiff(unique(ppcdam$geocode), sample_geocodes)) ==
    N_PPCDAM_OUTSIDE_EXPECTED,
  all(ELEC_VARS %in% names(tse)),
  all(c("population", "pop_source") %in% names(pop)),
  !"population" %in% names(gdp),
  all(post2000$geocode %in% sample_geocodes)
)

# ---- Transform --------------------------------------------------------------
# Hectares to km2 so every area in the panel shares one unit
# (D-2026-10-11-c). deforestation_rate is a ratio and is renamed only.
deforestation <- mapbiomas %>%
  filter(year %in% PANEL_YEARS) %>%
  transmute(
    geocode, municipality, state, state_abbreviation, year,
    forest_area_km2           = area_forest / HA_PER_KM2,
    deforestation_area_km2    = area_deforestation / HA_PER_KM2,
    deforestation_forest_rate = deforestation_rate
  )

# Only the columns the legacy join carried forward (D-2026-10-11-g); June
# is the reference month (D-2026-10-05-a).
bf_cols   <- bf %>%
  filter(bf_ref_month == BF_REF_MONTH) %>%
  select(-bf_ref_month)
# The counts take their panel names here (D-2026-10-28-a); the interim
# file keeps the names its assertions use. pia comes from the census file,
# not from the RAIS rows, so it is not lost in the RAIS gaps.
rais_cols <- rais %>%
  select(geocode, year, starts_with("emp_"),
         emp_workers_n = workers_total, emp_agri_workers_n = workers_agric)
pia_cols  <- census_pia %>% select(geocode, year, pia)
tse_cols  <- tse %>% select(-elec_total_votes, -elec_governing_year)  # D-2026-10-11-g, -22-e

panel <- deforestation %>%
  left_join(ppcdam %>% select(geocode, year, ppcdam_list),
            by = c("geocode", "year")) %>%
  left_join(bf_cols,    by = c("geocode", "year")) %>%
  left_join(rais_cols,  by = c("geocode", "year")) %>%
  left_join(census,     by = c("geocode", "year")) %>%
  left_join(pia_cols,   by = c("geocode", "year")) %>%
  left_join(pop %>% select(geocode, year, population),
            by = c("geocode", "year")) %>%
  left_join(gdp,        by = c("geocode", "year")) %>%
  left_join(tse_cols,   by = c("geocode", "year")) %>%
  left_join(munic_area, by = "geocode") %>%
  left_join(crops,      by = c("geocode", "year")) %>%
  left_join(cattle,     by = c("geocode", "year")) %>%
  left_join(prices,     by = c("geocode", "year")) %>%
  # The list file covers listed municipalities from 2004; every other
  # municipality-year is not listed (D-2026-10-11-b).
  mutate(ppcdam_list = replace_na(ppcdam_list, 0L)) %>%
  arrange(geocode, year)

# Pooled standardisation over all municipality-years; the crop lag uses the
# level's centre and spread so level and lag read on one metric
# (D-2026-10-11-d). Flagged for the storage audit (task 2.2).
crop_m   <- mean(panel$price_index_crop,   na.rm = TRUE)
crop_s   <- sd(panel$price_index_crop,     na.rm = TRUE)
cattle_m <- mean(panel$price_index_cattle, na.rm = TRUE)
cattle_s <- sd(panel$price_index_cattle,   na.rm = TRUE)

panel <- panel %>%
  mutate(
    price_index_crop_z      = (price_index_crop      - crop_m)   / crop_s,
    price_index_crop_lag1_z = (price_index_crop_lag1 - crop_m)   / crop_s,
    price_index_cattle_z    = (price_index_cattle    - cattle_m) / cattle_s
  ) %>%
  group_by(geocode) %>%
  mutate(
    price_index_cattle_lag1_z = lag(price_index_cattle_z, n = 1L)
  ) %>%
  ungroup() %>%
  mutate(
    bf_transfers_pc_brl_2024 = if_else(
      !is.na(population) & population > 0,
      bf_transfers_total_brl_2024 / population,
      NA_real_
    ),                                                        # D-2026-10-11-f
    # Time-invariant flags from the lookup (D-2026-10-23-f).
    muni_new_post2000 = as.integer(geocode %in% post2000$geocode),
    muni_parent_split = as.integer(geocode %in% parents)
  ) %>%
  select(all_of(PANEL_COLS)) %>%
  mutate(across(all_of(PANEL_INT_COLS), as.integer))

# ---- Assert: output state ---------------------------------------------------
# panel.qmd Q1-Q5, Q9.
first_year <- panel$year == min(PANEL_YEARS)
z_cols <- c("price_index_crop_z", "price_index_cattle_z")
emp_na <- is.na(panel$emp_workers_n)
agri_share_cols <- c("emp_share_agri_low_workers", "emp_share_agri_mid_workers",
                     "emp_share_agri_high_workers")
install_rule <- panel %>%
  inner_join(post2000 %>% select(geocode, install_year), by = "geocode") %>%
  select(geocode, year, install_year, all_of(INSTALL_RULE_COLS))
pre_install  <- install_rule %>% filter(year < install_year)
# GDP starts in 2002: the years before are not a gap of the rule.
post_install <- install_rule %>%
  filter(year >= install_year) %>%
  mutate(gdp_brl = if_else(year < GDP_FIRST_YEAR, 0, gdp_brl))
stopifnot(
  # D-2026-10-28-b, -a: the employment NA set is the absent municipality-
  # years, shared by every count, rate and share; the agricultural shares
  # are NA there and where no agricultural bond exists.
  sum(emp_na) == N_EMP_NA_EXPECTED,
  identical(emp_na, is.na(panel$emp_agri_workers_n)),
  identical(emp_na, is.na(panel$emp_share_agri_workers)),
  identical(emp_na, is.na(panel$emp_share_low_workers)),
  all(map_lgl(panel[agri_share_cols],
              ~ identical(is.na(.x), emp_na | panel$emp_agri_workers_n == 0L))),
  all(is.na(panel$emp_pia_rate_total) == (emp_na | is.na(panel$pia))),
  sum(is.na(panel$pia)) == N_PIA_NA_EXPECTED,
  identical(is.na(panel$pia), is.na(panel$pea)),
  # D-2026-10-28-d: NA before installation, observed from it.
  all(is.na(pre_install[INSTALL_RULE_COLS])),
  !anyNA(post_install[INSTALL_RULE_COLS]),
  all(emp_na[panel$geocode %in% post2000$geocode &
               panel$year < post2000$install_year[match(panel$geocode,
                                                        post2000$geocode)]]),
  nrow(panel) == N_MUNIC_EXPECTED * length(PANEL_YEARS),
  !anyDuplicated(paste(panel$geocode, panel$year)),
  identical(names(panel), PANEL_COLS),
  !anyNA(panel$ppcdam_list),
  all(panel$ppcdam_list[panel$year < PPCDAM_FIRST_LISTING_YEAR] == 0L),
  sum(panel$forest_area_km2 > panel$munic_area_km2, na.rm = TRUE) ==
    N_FOREST_OVER_AREA_EXPECTED,
  all(abs(map_dbl(panel[z_cols], mean, na.rm = TRUE)) < Z_TOL),
  all(abs(map_dbl(panel[z_cols], sd, na.rm = TRUE) - 1) < Z_TOL),
  all(is.na(panel$price_index_cattle_lag1_z[first_year])),
  # Governing-year storage (D-2026-10-22-d): 2000 NA; after 2000 the only
  # NA municipality-years are those without a governing election in the
  # data (panel.qmd Q5).
  all(is.na(panel$elec_enc[first_year])),
  sum(is.na(panel$elec_enc[!first_year])) == N_ELEC_NA_POST2000_EXPECTED,
  all(is.na(panel$elec_mov) ==
        (is.na(panel$elec_enc) | panel$elec_uncontested == 1L)),
  all(is.na(panel$elec_competitive) == is.na(panel$elec_mov)),
  all(is.na(panel$bf_transfers_pc_brl_2024) ==
        (is.na(panel$bf_transfers_total_brl_2024) |
           is.na(panel$population) | panel$population <= 0)),
  identical(is.na(panel$bf_regime), panel$year < min(bf$year)),
  # Every sample municipality without a 2000 census population is in the
  # lookup, and only those (panel.qmd Q8).
  setequal(panel$geocode[first_year & is.na(panel$population)],
           post2000$geocode),
  sum(panel$muni_new_post2000[first_year]) == N_POST2000_EXPECTED,
  sum(panel$muni_parent_split[first_year]) == N_PARENT_EXPECTED,
  all(map_lgl(panel[PANEL_INT_COLS], is.integer)),
  all(map_lgl(panel[PANEL_CHR_COLS], is.character)),
  all(map_lgl(panel[setdiff(PANEL_COLS, c(PANEL_INT_COLS, PANEL_CHR_COLS))],
              is.double))
)

# ---- Write ------------------------------------------------------------------
write_rds(panel, "_data/interim/panel_amz_2000_2020.rds")
write_csv(panel, "_data/interim/panel_amz_2000_2020.csv")
