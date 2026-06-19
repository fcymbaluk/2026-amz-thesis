rm(list = ls())

library(dplyr)
library(stringr)
library(tidyr)
library(readr)

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


