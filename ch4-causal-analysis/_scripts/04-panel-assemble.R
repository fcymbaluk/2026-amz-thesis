# 04-panel-assemble.R
# PURPOSE   Assemble the municipality-year panel of the nine Legal Amazon
#           states (808 municipalities, 2000-2020) from the interim outputs of
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
#   D-2026-10-11-a  Left joins onto the MapBiomas sample, 2000-2020; Mojuí
#                   dos Campos kept (dropped in script 05). The sample is
#                   the nine-state subset from phase 2, step 2.9
#                   (D-2026-10-29-a).
#   D-2026-10-11-b  ppcdam_list 0 where the list does not cover the
#                   municipality-year; from step 2.9 every listed
#                   municipality is in the sample.
#   D-2026-10-11-c  Superseded by D-2026-10-28-j (phase 2, step 2.8): the
#                   areas arrive in km2 and the rate on 0-1 from script 01
#                   and are carried as they are.
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
#   D-2026-10-29-a  biome_amazon and legal_amazon carried from script 01
#                   (phase 2, step 2.9): constant 0/1 flags, 503 and 772
#                   of the 808 municipalities.
#   D-2026-10-29-b  The six Cerrado municipalities of Mato Grosso installed
#                   in 2001 join the lookup, so the post-2000 and parent
#                   flags and the installation rule cover the whole sample.
#   D-2026-10-29-c  The pooled z-scores keep the biome municipalities as
#                   their reference population: centre and spread over the
#                   biome_amazon == 1 municipality-years, applied to every
#                   row, so no value of the audited sample moves.
#
# RUN ORDER After every 01, 02 and 03 script. Script 05 reads the output.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
PANEL_YEARS   <- 2000:2020                                    # D-2026-10-11-a
BF_REF_MONTH  <- 6L                                           # D-2026-10-05-a
ELEC_VARS     <- c("elec_enc", "elec_mov", "elec_winner_share",
                   "elec_n_candidates", "elec_winner_party",
                   "elec_winner_id", "elec_competitive",
                   "elec_uncontested")                        # D-2026-10-22-d
# Unit pattern (phase 2, step 2.8; D-2026-10-28-h, -i): every share and
# rate in the panel lies on 0-1, no percent storage. The bonds-per-PIA
# rates are bounded in practice only: formal bonds are counted at the
# establishment, the working-age population at the residence, so a
# municipality can exceed 1 (phase 2, step 2.9, D-2026-10-29-d; three
# cells of two municipalities outside the biome, panel.qmd Q11).
PIA_RATE_COLS <- c("emp_pia_rate_total", "emp_pia_rate_agric",
                   "emp_pia_rate_low", "emp_pia_rate_mid",
                   "emp_pia_rate_high")
N_PIA_RATE_OVER_1_EXPECTED <- 3L
SHARE_RATE_COLS <- c("deforestation_forest_rate", "informal",
                     "emp_share_low_workers",
                     "emp_share_mid_workers", "emp_share_high_workers",
                     "emp_share_agri_low_workers",
                     "emp_share_agri_mid_workers",
                     "emp_share_agri_high_workers", "emp_share_agri_workers",
                     "elec_mov", "elec_winner_share",
                     "crop_soy_munic_area_share", "crop_corn_munic_area_share",
                     "crop_rice_munic_area_share",
                     "crop_cotton_munic_area_share")
PPCDAM_FIRST_LISTING_YEAR <- 2008L                            # D-2026-10-11-b
# ---- Fix (D-2026-10-29-a, -b, -c; phase 2, step 2.9) -------------------------
# The nine-state sample of script 01, Mojuí dos Campos included and dropped
# in script 05 (D-2026-10-05-l); 503 biome and 772 Legal Amazon
# municipalities among the 808.
N_MUNIC_EXPECTED       <- 808L
N_MUNIC_BIOME_EXPECTED <- 503L
N_MUNIC_LEGAL_EXPECTED <- 772L
FLAG_COLS <- c("biome_amazon", "legal_amazon")
# Every PPCDAm municipality is in the nine states (panel.qmd Q2, Q11).
N_PPCDAM_OUTSIDE_EXPECTED <- 0L                               # D-2026-10-11-b
# Reference population of the pooled z-scores (D-2026-10-29-c).
Z_REF_FLAG <- "biome_amazon"
# ---- end fix -----------------------------------------------------------------
# ---- Fix (D-2026-10-28-g; phase 2, step 2.7) ---------------------------------
# The 2022 area is on the MapBiomas mesh, so no forest area exceeds the
# municipal area (was 4: Nova Ubiratã 2000-2003 against the post-split 2025
# area, panel.qmd Q3).
N_FOREST_OVER_AREA_EXPECTED <- 0L
# ---- end fix -----------------------------------------------------------------
Z_TOL <- 1e-6
# Municipality-years after 2000 without a governing election in the data
# (D-2026-10-22-d): Mojuí dos Campos 2001-2012 (installed 2013), 2001-2004
# for Ipiranga do Norte, Itanhangá (installed 2005), Altamira do Maranhão,
# Brejo de Areia and Humberto de Campos (no 2000 election in the file),
# 2009-2012 for Paranatinga and 2017-2020 for Sambaíba (panel.qmd Q5, Q11).
N_ELEC_NA_POST2000_EXPECTED <- 40L
POST2000_LOOKUP <- "_data/aux/munic_post2000.csv"             # D-2026-10-23-f
# panel.qmd Q8, Q11: the fifteen Mato Grosso municipalities installed after
# the 2000 census and Mojuí dos Campos; their parents in the nine states
# (D-2026-10-29-b).
N_POST2000_EXPECTED <- 16L
N_PARENT_EXPECTED   <- 17L
# Municipality-years absent from the RAIS export (rais.qmd Q2; D-2026-10-28-b):
# Mojuí dos Campos 2000-2012, the fifteen Mato Grosso municipalities before
# installation (23) and the reporting gaps of small municipalities
# (panel.qmd Q11).
N_EMP_NA_EXPECTED <- 99L
# pia is NA before 2010 for the fifteen municipalities without a 2000
# census (D-2026-10-23-d) and in every year for Mojuí dos Campos, absent
# from the 2000 and 2010 censuses (panel.qmd Q9).
N_PIA_NA_EXPECTED <- 171L
# Columns observed from the installation year of a lookup municipality and
# NA before it (panel.qmd Q9; D-2026-10-28-d); gdp_brl within its years.
INSTALL_RULE_COLS  <- c("crop_soy_area_planted_km2", "cattle_heads",
                        "emp_workers_n", "population", "gdp_brl")
GDP_FIRST_YEAR     <- 2002L
# RAIS reporting gaps after installation, NA under D-2026-10-28-b: Serra
# Nova Dourada (MT, installed 2001) has no bond in 2001-2002 (panel.qmd
# Q11; phase 2, step 2.9).
N_EMP_GAP_POST_INSTALL_EXPECTED <- 2L

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
  "emp_workers_n", "emp_agri_workers_n", "pia",
  # Phase 2, step 2.9 (D-2026-10-29-a): the Amazon biome and the Legal
  # Amazon as constant flags.
  FLAG_COLS
)
PANEL_INT_COLS <- c("year", "ppcdam_list", "elec_n_candidates",
                    "elec_competitive", "elec_uncontested",
                    "election_year",                    # D-2026-10-11-h
                    "muni_new_post2000", "muni_parent_split",
                    "emp_workers_n", "emp_agri_workers_n",  # D-2026-10-28-a
                    FLAG_COLS)                               # D-2026-10-29-a
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
# panel.qmd Q1, Q11: every input is keyed on geocode (and year), the sample
# is complete over PANEL_YEARS, carries the two flags, and the PPCDAm
# indicator is 0/1 where known.
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
  all(FLAG_COLS %in% names(mapbiomas)),
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
# The areas arrive in km2 and the rate on 0-1 from script 01
# (D-2026-10-28-j, -h; phase 2, step 2.8).
deforestation <- mapbiomas %>%
  filter(year %in% PANEL_YEARS) %>%
  select(geocode, municipality, state, state_abbreviation, year,
         forest_area_km2, deforestation_area_km2, deforestation_forest_rate,
         all_of(FLAG_COLS))

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

# Pooled standardisation; the crop lag uses the level's centre and spread
# so level and lag read on one metric (D-2026-10-11-d). Flagged for the
# storage audit (task 2.2).
# ---- Fix (D-2026-10-29-c; phase 2, step 2.9) ---------------------------------
# The reference population stays the biome municipality-years, the pool of
# the audited sample; the centre and spread apply to every row.
z_ref    <- panel[[Z_REF_FLAG]] == 1L
crop_m   <- mean(panel$price_index_crop[z_ref],   na.rm = TRUE)
crop_s   <- sd(panel$price_index_crop[z_ref],     na.rm = TRUE)
cattle_m <- mean(panel$price_index_cattle[z_ref], na.rm = TRUE)
cattle_s <- sd(panel$price_index_cattle[z_ref],   na.rm = TRUE)
# ---- end fix -----------------------------------------------------------------

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
# panel.qmd Q1-Q5, Q9, Q11.
first_year <- panel$year == min(PANEL_YEARS)
z_cols <- c("price_index_crop_z", "price_index_cattle_z")
z_ref  <- panel[[Z_REF_FLAG]] == 1L
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
  # D-2026-10-28-d: NA before installation, observed from it; the RAIS
  # column admits the documented reporting gaps (D-2026-10-28-b).
  all(is.na(pre_install[INSTALL_RULE_COLS])),
  !anyNA(post_install[setdiff(INSTALL_RULE_COLS, "emp_workers_n")]),
  sum(is.na(post_install$emp_workers_n)) == N_EMP_GAP_POST_INSTALL_EXPECTED,
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
  # Unit pattern (phase 2, step 2.8): every share and rate on 0-1; the
  # bonds-per-PIA rates non-negative, above 1 in the known cells only
  # (D-2026-10-29-d).
  all(map_lgl(panel[SHARE_RATE_COLS],
              ~ all(.x >= 0 & .x <= 1, na.rm = TRUE))),
  all(map_lgl(panel[PIA_RATE_COLS], ~ all(.x >= 0, na.rm = TRUE))),
  sum(panel$emp_pia_rate_total > 1, na.rm = TRUE) ==
    N_PIA_RATE_OVER_1_EXPECTED,
  all(map_lgl(panel[setdiff(PIA_RATE_COLS, "emp_pia_rate_total")],
              ~ all(.x <= panel$emp_pia_rate_total, na.rm = TRUE))),
  # D-2026-10-29-c: mean 0 and sd 1 over the reference population.
  all(abs(map_dbl(panel[z_ref, z_cols], mean, na.rm = TRUE)) < Z_TOL),
  all(abs(map_dbl(panel[z_ref, z_cols], sd, na.rm = TRUE) - 1) < Z_TOL),
  # D-2026-10-29-a: the flags are constant within municipality, the biome
  # inside the Legal Amazon, with the expected counts.
  all(map_lgl(panel[FLAG_COLS],
              ~ n_distinct(paste(panel$geocode, .x)) == N_MUNIC_EXPECTED)),
  all(panel$biome_amazon <= panel$legal_amazon),
  sum(panel$biome_amazon[first_year]) == N_MUNIC_BIOME_EXPECTED,
  sum(panel$legal_amazon[first_year]) == N_MUNIC_LEGAL_EXPECTED,
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
