rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(readxl)
library(writexl)
library(tidyverse)
library(magrittr)


outcome_bf <- readRDS(file.path("_data", "outcome_bf.rds"))
outcome_deforestation <- readRDS(file.path("_data", "outcome_deforestation.rds"))
outcome_employment <- readRDS(file.path("_data", "outcome_employment.rds"))
outcome_informality <- readRDS(file.path("_data", "outcome_population_informality.rds"))
outcome_pea <- readRDS(file.path("_data", "outcome_population_pea.rds"))
outcome_ppcdam <- readRDS(file.path("_data", "outcome_ppcdam_long.rds"))
outcome_gdp <- readRDS(file.path("_data", "outcome_gdp.rds"))
outcome_electoral <- readRDS(file.path("_data", "outcome_electoral_competitiveness.rds"))

glimpse (outcome_bf) #keep all
glimpse (outcome_deforestation) # do not keep "biome", "deforestation_mean"and "deforestation_sd" 
glimpse (outcome_employment) # only keep "year", "geocode", "emp_rate_total", "emp_rate_agric", "emp_rate_low", "emp_rate_mid", "emp_rate_high", "share_low_workers", "share_mid_workers", "share_high_workers", "share_agri_low_workers", "share_agri_mid_workers", "share_agri_high_workers", "share_agri_workers"
# not that I modified the names on the employment dataset to include PEA for robustness checks. The script must be revised in face of this changes. 
glimpse (outcome_informality) # keep all
glimpse (outcome_pea) # keep all
glimpse (outcome_ppcdam) # do not keep "municipality"
glimpse (outcome_gdp) # keep all
glimpse (outcome_electoral) # keep all

# 1. Standardize and select variables

bf_final <- outcome_bf %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  ) %>%
  select(-pea)

deforestation_final <- outcome_deforestation %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year),
    # convert forest cover & deforestation from hectares to km² (1 ha = 0.01 km²)
    # so the whole panel is in km². deforestation_rate (a ratio) and
    # deforestation_z (standardized) are unit-free and left untouched.
    area_forest_km2        = area_forest / 100,
    area_deforestation_km2 = area_deforestation / 100
  ) %>%
  select(
    geocode,
    municipality,
    state,
    state_abbreviation,
    year,
  #  area_forest,
    area_forest_km2,
  #  area_deforestation,
    area_deforestation_km2,
    deforestation_rate,
    deforestation_z
  )

deforestation_final

employment_final <- outcome_employment %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  ) %>%
  select(
    geocode,
    year,
    emp_rate_total,
    emp_rate_agric,
    emp_rate_low,
    emp_rate_mid,
    emp_rate_high,
    share_low_workers,
    share_mid_workers,
    share_high_workers,
    share_agri_low_workers,
    share_agri_mid_workers,
    share_agri_high_workers,
    share_agri_workers
  )

informality_final <- outcome_informality %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  )

pea_final <- outcome_pea %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  )

ppcdam_final <- outcome_ppcdam %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  ) %>%
  select(
    geocode,
    year,
    ppcdam_list
  )

gdp_final <- outcome_gdp %>% 
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  )

electoral_final <- outcome_electoral %>%
  mutate(
    geocode = as.character(geocode),
    year = as.integer(year)
  )
electoral_final

# 1b. Crop & cattle data (price-index inputs)

# crop base-year weights (time-invariant): long by crop -> wide
crop_weights_wide <- read_csv(
  file.path("_data", "outcome_crop_weights_base_municarea.csv"),
  col_types = cols(geocode = col_character())
) %>%
  select(geocode, crop, crop_weight) %>%
  pivot_wider(names_from = crop, values_from = crop_weight, names_prefix = "crop_w_")

# crop area shares (time-varying): long by crop+year -> wide
crop_ts_raw <- read_csv(
  file.path("_data", "outcome_agriculture_production_shares.csv"),
  col_types = cols(geocode = col_character())
)

crop_ts_wide <- crop_ts_raw %>%
  select(geocode, year, crop, area_planted_km2, area_crop_share) %>%
  pivot_wider(
    names_from  = crop,
    values_from = c(area_planted_km2, area_crop_share),
    names_glue  = "crop_{crop}_{.value}"
  )

# municipal area (time-invariant), taken once from the crop time series
munic_area <- crop_ts_raw %>%
  distinct(geocode, area_munic_km2)

# cattle base-year weight (time-invariant)
cattle_weights_final <- read_csv(
  file.path("_data", "outcome_cattle_weights_base_municarea.csv"),
  col_types = cols(geocode = col_character())
) %>%
  select(geocode, cattle_weight = weight)

# cattle density (time-varying)
cattle_ts_final <- read_csv(
  file.path("_data", "outcome_cattle_density.csv"),
  col_types = cols(geocode = col_character())
) %>%
  select(geocode, year, cattle_heads = heads, cattle_per_km2)


# 2. Merge all datasets

final_dataset <- deforestation_final %>%
  left_join(bf_final, by = c("geocode", "year")) %>%
  left_join(employment_final, by = c("geocode", "year")) %>%
  left_join(informality_final, by = c("geocode", "year")) %>%
  left_join(pea_final, by = c("geocode", "year")) %>%
  left_join(ppcdam_final, by = c("geocode", "year")) %>%
  left_join(gdp_final, by = c("geocode", "year")) %>% 
  left_join(electoral_final, by = c("geocode", "year")) %>%
  arrange(geocode, year)

final_dataset

colnames(final_dataset)

table(substr(electoral_final$geocode, 1, 2))      # expect mostly/only "31"
table(substr(deforestation_final$geocode, 1, 2))  # your panel: 11–17, 21, 51
length(intersect(electoral_final$geocode, deforestation_final$geocode))  # ~0

# 3. Reorder columns

final_dataset <- final_dataset %>%
  select(
    geocode, municipality, state, state_abbreviation, year, ppcdam_list,
    area_forest_km2, area_deforestation_km2, deforestation_rate, deforestation_z,
    bf_families_n, bf_transfers_total_brl_2024, bf_transfers_avg_brl_2024,
    bf_quota, bf_families_quota_ratio, bf_transfers_pea_brl_2024,
    bf_transfers_quota_brl_2024, 
    emp_rate_total, emp_rate_agric, emp_rate_low, emp_rate_mid, emp_rate_high,
    share_low_workers, share_mid_workers, share_high_workers,
    share_agri_low_workers, share_agri_mid_workers, share_agri_high_workers,
    share_agri_workers, pea, informal, population, gdp_brl, gdp_per_capita, log_gdp_per_capita,
    gdp_brl_2024, gdp_per_capita_2024, log_gdp_per_capita_2024,
    enc,  mov,  winner_share, n_candidates, total_votes, winner_party, winner_id, competitive, uncontested, election_year
    ) %>% 
  filter(year >= 2000, year <= 2020) 



final_dataset
glimpse(final_dataset)
head(final_dataset)
tail(final_dataset)
colnames(final_dataset)

# 3b. Merge crop & cattle data (appended after the reorder)
# Weights & municipal area are time-invariant -> join on geocode only
# (broadcast across years). The time series join on geocode + year.

final_dataset <- final_dataset %>%
  left_join(munic_area,           by = "geocode") %>%
  left_join(crop_weights_wide,    by = "geocode") %>%
  left_join(cattle_weights_final, by = "geocode") %>%
  left_join(crop_ts_wide,         by = c("geocode", "year")) %>%
  left_join(cattle_ts_final,      by = c("geocode", "year")) %>%
  arrange(geocode, year)

# coverage check: panel municipalities with no crop/cattle match (NA)
final_dataset %>%
  summarise(
    n_rows            = n(),
    missing_crop_w    = sum(is.na(crop_w_soy)),
    missing_crop_ts   = sum(is.na(crop_soy_area_crop_share)),
    missing_cattle_w  = sum(is.na(cattle_weight)),
    missing_cattle_ts = sum(is.na(cattle_per_km2))
  )



# 3c. Check for duplicates and missing values

range(final_dataset$year, na.rm = TRUE)

final_dataset %>%
  count(geocode, year) %>%
  filter(n > 1)

summary(final_dataset)

final_dataset %>%
  summarise(
    n_rows = n(),
    missing_bf = sum(is.na(bf_families_n)),
    missing_employment = sum(is.na(emp_rate_total)),
    missing_informal = sum(is.na(informal)),
    missing_pea = sum(is.na(pea)),
    missing_ppcdam = sum(is.na(ppcdam_list))
  )




final_dataset <- final_dataset %>%
  mutate(
    ppcdam_list = as.integer(ppcdam_list)
  )

final_dataset <- final_dataset %>%
  mutate(
    ppcdam_list = if_else(is.na(ppcdam_list), 0L, ppcdam_list)
  )

final_dataset

# 4. Save the final dataset

write_csv(
  final_dataset, 
  file = ("_data/final_dataset.csv")
)

library(writexl)

write_xlsx(
  final_dataset, 
  "_data/final_dataset.xlsx")


write_rds(
  final_dataset, 
  file = ("_data/final_dataset.rds")
)




final_panel <- read_rds(file.path("_data", "final_dataset.rds"))

head(final_panel)


# To be added above later

# assembled price index (time-varying): crop and cattle indices + lag
# built in 03-data-cleaning-controls.R and saved to outcome_price_index_panel.csv
price_index_final <- read_csv(
  file.path("_data", "outcome_price_index_panel.csv"),
  col_types = cols(geocode = col_character())
) %>%
  select(geocode, year,
         price_index_crop, price_index_cattle, price_index_crop_lag1)


price_index_final



final_dataset <- final_panel %>% 
  left_join(price_index_final,    by = c("geocode", "year")) %>%
  arrange(geocode, year)


glimpse(final_dataset)

# coverage check: panel municipalities with no crop/cattle match (NA)
final_dataset %>%
  summarise(
    n_rows            = n(),
    missing_crop_w    = sum(is.na(crop_w_soy)),
    missing_crop_ts   = sum(is.na(crop_soy_area_crop_share)),
    missing_cattle_w  = sum(is.na(cattle_weight)),
    missing_cattle_ts = sum(is.na(cattle_per_km2)),
    missing_price_idx = sum(is.na(price_index_crop))
  )

write_csv(
  final_dataset, 
  file = ("_data/final_dataset_2.csv")
)


colnames(final_dataset)

final_dataset <- final_dataset %>%
  select(
    geocode, municipality, state, state_abbreviation, year, ppcdam_list,
    area_forest_km2, area_deforestation_km2, deforestation_rate, deforestation_z, 
    bf_families_n, bf_quota, bf_transfers_total_brl_2024, bf_transfers_avg_brl_2024,
    bf_families_quota_ratio, bf_transfers_pea_brl_2024, bf_transfers_quota_brl_2024, 
    emp_rate_total, emp_rate_agric, emp_rate_low, emp_rate_mid, emp_rate_high,
    share_low_workers, share_mid_workers, share_high_workers, share_agri_low_workers, 
    share_agri_mid_workers, share_agri_high_workers, share_agri_workers,
    pea, informal, population, gdp_brl, gdp_per_capita, log_gdp_per_capita,
    gdp_brl_2024, gdp_per_capita_2024, log_gdp_per_capita_2024,
    enc,  mov,  winner_share, n_candidates, winner_party, winner_id, competitive, uncontested, election_year,
    area_munic_km2, crop_soy_area_planted_km2, crop_corn_area_planted_km2, crop_rice_area_planted_km2, crop_cotton_area_planted_km2,
    crop_soy_area_crop_share, crop_corn_area_crop_share, crop_rice_area_crop_share, crop_cotton_area_crop_share, cattle_heads, 
    cattle_per_km2, price_index_crop, price_index_cattle, price_index_crop_lag1)

final_dataset <- final_dataset %>%
  rename(
    forest_area_km2 = area_forest_km2,
    deforestation_area_km2 = area_deforestation_km2,
    deforestation_forest_rate = deforestation_rate,
    emp_rate_pia_total = emp_rate_total,
    emp_rate_pia_agric = emp_rate_agric,
    emp_rate_pia_low = emp_rate_low,
    emp_rate_pia_mid = emp_rate_mid,
    emp_rate_pia_high = emp_rate_high,
    emp_share_low_workers = share_low_workers,
    emp_share_mid_workers = share_mid_workers,
    emp_share_high_workers = share_high_workers,
    emp_share_agri_low_workers = share_agri_low_workers,
    emp_share_agri_mid_workers = share_agri_mid_workers,
    emp_share_agri_high_workers = share_agri_high_workers,
    emp_share_agri_workers = share_agri_workers,
    gdp_per_capita_log = log_gdp_per_capita,
    gdp_per_capita_2024_log = log_gdp_per_capita_2024,
    elec_enc = enc,
    elec_mov = mov,
    elec_winner_share = winner_share,
    elec_n_candidates = n_candidates,
    elec_winner_party = winner_party,
    elec_winner_id = winner_id,
    elec_competitive = competitive,
    elec_uncontested = uncontested,
    election_year = election_year,
    munic_area_km2 = area_munic_km2,
    crop_soy_munic_area_share = crop_soy_area_crop_share,
    crop_corn_munic_area_share = crop_corn_area_crop_share,
    crop_rice_munic_area_share = crop_rice_area_crop_share,
    crop_cotton_munic_area_share = crop_cotton_area_crop_share
  )  

    
final_dataset <- final_dataset %>%
  rename(
    emp_pia_rate_total = emp_rate_pia_total,
    emp_pia_rate_agric = emp_rate_pia_agric,
    emp_pia_rate_low = emp_rate_pia_low,
    emp_pia_rate_mid = emp_rate_pia_mid,
    emp_pia_rate_high = emp_rate_pia_high
  )


write_csv(
  final_dataset, 
  file = ("_data/final_dataset_3.csv")
)


# ---- Drop Mojui dos Campos (PA, geocode 1504752) ----------------------------
# Created in 2013 from a split of Santarem. It has no formal-employment records
# and no municipal-area figure for any year, and only partial coverage of the
# transfer, electoral, and agricultural series. The missing area also propagates
# into the crop price index and the crop area shares, so the unit carries no
# usable values on several covariates. Removed here, before standardization, so
# the price-index z-scores are estimated on the retained sample.

df <- df %>% filter(geocode != 1504752)

# Sanity check: 502 municipalities, 10,542 municipality-years.
stopifnot(n_distinct(df$geocode) == 502, nrow(df) == 502 * 21)



# 05-transformations.R
# Post-merge transformations applied to final_dataset_3.csv
#
#   (a) standardize the crop and cattle price indices
#   (b) lag the electoral block by one year (align with the governing mayor)
#   (c) build per-capita Bolsa Familia transfers
#
# Input : _data/final_dataset_3.csv
# Output: _data/final_dataset_4.csv  (+ .rds)
#
# All original columns are kept. New columns use clear suffixes:
#   _z   = standardized (z-score)
#   _gov = aligned to the mayor governing in that calendar year
#   _pc  = per capita

library(dplyr)
library(readr)


# ---- 0. Read and order ------------------------------------------------------
# geocode as character so it is never treated as a number; year as integer so
# arrange() and lag() order the panel correctly. Any lag operation below
# assumes the data is sorted within municipality by year.

df <- read_csv(
  file.path("_data", "final_dataset_3.csv"),
  col_types = cols(geocode = col_character(), year = col_integer())
)

df <- df %>% arrange(geocode, year)


# ---- (a) Standardize the price indices --------------------------------------
# The crop index (land-share weighted) and the cattle index (head-density
# weighted) sit on scales about three orders of magnitude apart, so their raw
# coefficients are not comparable and the cattle index dominates any pooled
# variance. z-scoring puts both in standard-deviation units, so a coefficient
# reads as "effect of a one SD change in the index".
#
# Standardization is pooled over the whole panel (all municipality-years), with
# na.rm so structural gaps (such as the 2000 crop lag) do not distort the mean
# and sd. The contemporaneous crop level and its one-year lag are scaled with
# the SAME center and spread, so level and lag stay on one common metric.
#
# Note: this is a linear rescaling (subtract a constant, divide by a constant).
# Under municipality and year fixed effects it does not change model fit, only
# the units in which the coefficients are read.

crop_m   <- mean(df$price_index_crop,   na.rm = TRUE)
crop_s   <- sd(df$price_index_crop,     na.rm = TRUE)
cattle_m <- mean(df$price_index_cattle, na.rm = TRUE)
cattle_s <- sd(df$price_index_cattle,   na.rm = TRUE)

df <- df %>%
  mutate(
    price_index_crop_z      = (price_index_crop      - crop_m)   / crop_s,
    price_index_crop_lag1_z = (price_index_crop_lag1 - crop_m)   / crop_s,
    price_index_cattle_z    = (price_index_cattle    - cattle_m) / cattle_s
  )

# The indices enter the analysis lagged one year. A cattle lag is not in the
# source file, so build it here from the standardized cattle series, within
# municipality. (The crop lag already exists and is standardized above.)
df <- df %>%
  group_by(geocode) %>%
  mutate(price_index_cattle_lag1_z = lag(price_index_cattle_z, n = 1)) %>%
  ungroup()

glimpse(df)

# ---- (b) Lag the electoral block by one year --------------------------------
# Mayors elected in October of year t take office on 1 January of t+1 and govern
# t+1 to t+4. In the source file the competitiveness values are carried forward
# from each election across the election year itself plus the three following
# years, so in an election year the value reflects the newly elected mayor and
# not the one actually governing. Shifting the block down by one year fixes the
# alignment: each calendar year then carries the election result of the mayor in
# office during that year. Year 2000 becomes NA, since the 1996 election is
# outside the data.
#
# New columns get the suffix _gov. Originals are kept. election_year (the 0/1
# marker of actual election years) is left unlagged, since it flags when an
# election happened, not who was governing.

elec_vars <- c(
  "elec_enc", "elec_mov", "elec_winner_share", "elec_n_candidates",
  "elec_winner_party", "elec_winner_id", "elec_competitive", "elec_uncontested"
)

df <- df %>%
  group_by(geocode) %>%
  mutate(across(all_of(elec_vars), ~ lag(.x, n = 1), .names = "{.col}_gov")) %>%
  ungroup()

glimpse(df)

# ---- (c) Per-capita Bolsa Familia transfers ---------------------------------
# The methodology sets the primary transfer-intensity denominator as total
# population (or the quota), not PEA, but the file only carries the PEA- and
# quota-normalized versions. This builds the per-capita version: total transfers
# in 2024 BRL per resident. Population is guarded against missing or zero values.
# Where population or transfers are missing (for example 2000-2001, or any year
# before the 2004 program start) the result is NA by construction.

df <- df %>%
  mutate(
    bf_transfers_pc_brl_2024 = if_else(
      !is.na(population) & population > 0,
      bf_transfers_total_brl_2024 / population,
      NA_real_
    )
  )


# ---- Quick checks -----------------------------------------------------------
# Standardized indices should have mean ~0 and sd ~1.
cat("z-index means (should be ~0):\n")
print(round(sapply(
  df[c("price_index_crop_z", "price_index_cattle_z")],
  mean, na.rm = TRUE
), 4))
cat("z-index sds (should be ~1):\n")
print(round(sapply(
  df[c("price_index_crop_z", "price_index_cattle_z")],
  sd, na.rm = TRUE
), 4))

# Electoral lag: year 2000 must be all NA in the _gov columns; later years not.
cat("\nelec_winner_party_gov NA count by year (2000 should equal n. municipalities):\n")
print(
  df %>%
    group_by(year) %>%
    summarise(na_gov = sum(is.na(elec_winner_party_gov)), .groups = "drop") %>%
    filter(year %in% c(2000, 2001, 2005))
)

# Per-capita BF: non-missing only where both transfers and population exist.
cat("\nbf_transfers_pc_brl_2024 summary:\n")
print(summary(df$bf_transfers_pc_brl_2024))


# ---- Save -------------------------------------------------------------------
write_csv(df, file.path("_data", "final_dataset_4.csv"))
saveRDS(df,   file.path("_data", "final_dataset_4.rds"))
