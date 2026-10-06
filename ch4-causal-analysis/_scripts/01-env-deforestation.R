# 01-env-deforestation.R
# PURPOSE   Build the municipality-year MapBiomas forest-area and deforestation
#           panel (hectares) for the Amazon-biome municipalities, 1987-2023,
#           with the biome attributes from the IBGE predominant-biome list.
#
# INPUTS    _data/raw/mapbiomas/mapbiomas_munic.xlsx   MapBiomas, Deforestation and
#                                                      Secondary Vegetation, Collection 9
#                                                      (Aug 2024), sheet DEF_SECVEG
#           _data/raw/ibge/ibge_bioma.csv              IBGE, Bioma predominante por
#                                                      município (Malha Municipal 2022)
#
# OUTPUTS   _data/interim/mapbiomas_deforestation_amz_1987_2023.rds
#           _data/interim/mapbiomas_deforestation_amz_1987_2023.csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd §1.1-1.2
#            until the annex is written; evidence: _audit/mapbiomas.qmd and
#            _audit/ibge_biome.qmd; history: DECISIONS.md)
#   D-2026-10-06-a  Year window 1987-2023: suppression is all zero in 1985-86.
#   D-2026-10-06-b  Forest = primary + secondary vegetation; deforestation =
#                   primary + secondary suppression; all level-1 classes summed.
#   D-2026-10-06-c  Sample: nine Legal Amazon states, then IBGE predominant biome
#                   Amazônia (503 municipalities, Mojuí dos Campos included).
#   D-2026-10-06-d  deforestation_rate = deforestation / contemporaneous forest x 100.
#   D-2026-10-06-e  Municipality mean, sd and z-score over the full window stored.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.9  deforestation_rate is stored on 0-100; rescaled to 0-1 in phase 2.
#   Task 1.1  The biome filter becomes biome_amazon / legal_amazon flags in
#             phase 2 step 2.9; the N_MUNIC_* constants update then.
# RUN ORDER First script; no pipeline inputs. Feeds 04-data-panel.R.

library(tidyverse)  # pipe operator: magrittr %>% throughout, never the native pipe
library(readxl)

# ---- Constants --------------------------------------------------------------
MAPBIOMAS_SHEET              <- "DEF_SECVEG"
MAPBIOMAS_YEARS_SOURCE       <- 1985:2023
MAPBIOMAS_YEARS              <- 1987:2023                                    # D-2026-10-06-a
MAPBIOMAS_FOREST_TRANSITIONS <- c("Veg. Primária", "Veg. Secundária")         # D-2026-10-06-b
MAPBIOMAS_DEFOR_TRANSITIONS  <- c("Supressão Veg. Primária",
                                  "Supressão Veg. Secundária")               # D-2026-10-06-b
AMZ_STATES                   <- c("Acre", "Amapá", "Amazonas", "Maranhão",
                                  "Mato Grosso", "Pará", "Rondônia", "Roraima",
                                  "Tocantins")                               # D-2026-10-06-c
BIOME_KEEP                   <- "Amazônia"                                    # D-2026-10-06-c
RATE_SCALE                   <- 100                                           # D-2026-10-06-d; task 1.9
N_MUNIC_BR_EXPECTED          <- 5571L
N_MUNIC_STATES_EXPECTED      <- 808L
N_MUNIC_BIOME_EXPECTED       <- 503L  # Mojuí dos Campos included, dropped downstream (D-2026-10-05-l); update at task 1.1
N_BIOME_FOOTER_ROWS          <- 7L

# ---- Read -------------------------------------------------------------------
mapbiomas_raw <- read_xlsx("_data/raw/mapbiomas/mapbiomas_munic.xlsx",
                           sheet = MAPBIOMAS_SHEET)

biome_raw <- read_delim("_data/raw/ibge/ibge_bioma.csv", delim = ";",
                        col_types = cols(.default = col_character()),
                        locale = locale(encoding = "UTF-8"),
                        show_col_types = FALSE, progress = FALSE)

# The data file ends with a metadata footer; only 7-digit municipal codes
# identify municipalities (ibge_biome.qmd Q1).
biome_munic <- biome_raw %>%
  filter(str_detect(Geocódigo, "^[0-9]{7}$"))

# ---- Assert: raw state ------------------------------------------------------
year_cols_source <- as.character(MAPBIOMAS_YEARS_SOURCE)
year_cols_dropped <- as.character(setdiff(MAPBIOMAS_YEARS_SOURCE, MAPBIOMAS_YEARS))

# _audit/mapbiomas.qmd Q1.
stopifnot(
  all(c("state", "municipality", "geocode", "transition_name", year_cols_source)
      %in% names(mapbiomas_raw)),
  all(nchar(as.character(mapbiomas_raw$geocode)) == 7),
  !anyNA(mapbiomas_raw[, year_cols_source]),
  n_distinct(mapbiomas_raw$geocode) == N_MUNIC_BR_EXPECTED,
  all(c(MAPBIOMAS_FOREST_TRANSITIONS, MAPBIOMAS_DEFOR_TRANSITIONS)
      %in% mapbiomas_raw$transition_name),
  n_distinct(mapbiomas_raw$geocode[mapbiomas_raw$state %in% AMZ_STATES])
    == N_MUNIC_STATES_EXPECTED
)

# _audit/mapbiomas.qmd Q2 (D-2026-10-06-a).
suppression_dropped_years <- mapbiomas_raw %>%
  filter(transition_name %in% MAPBIOMAS_DEFOR_TRANSITIONS) %>%
  select(all_of(year_cols_dropped))
stopifnot(all(suppression_dropped_years == 0))

# _audit/ibge_biome.qmd Q1, Q2, Q4.
biome_amz <- biome_munic %>%
  filter(`Bioma predominante` == BIOME_KEEP)
munic_names <- mapbiomas_raw %>%
  filter(state %in% AMZ_STATES) %>%
  distinct(geocode = as.character(geocode), municipality) %>%
  inner_join(biome_amz, by = c("geocode" = "Geocódigo"))
stopifnot(
  nrow(biome_raw) - nrow(biome_munic) == N_BIOME_FOOTER_ROWS,
  !anyDuplicated(biome_munic$Geocódigo),
  nrow(biome_amz) == N_MUNIC_BIOME_EXPECTED,
  nrow(munic_names) == N_MUNIC_BIOME_EXPECTED,
  all(munic_names$municipality == munic_names$`Nome do município`)
)

# ---- Transform --------------------------------------------------------------
deforestation_states <- mapbiomas_raw %>%
  select(state, municipality, geocode, transition_name,
         all_of(as.character(MAPBIOMAS_YEARS))) %>%
  filter(
    state %in% AMZ_STATES,
    transition_name %in% c(MAPBIOMAS_FOREST_TRANSITIONS, MAPBIOMAS_DEFOR_TRANSITIONS)
  ) %>%
  mutate(
    geocode = as.character(geocode),
    vegetation_category = if_else(transition_name %in% MAPBIOMAS_FOREST_TRANSITIONS,
                                  "forest", "deforestation")
  ) %>%
  pivot_longer(
    cols = all_of(as.character(MAPBIOMAS_YEARS)),
    names_to = "year",
    values_to = "area_ha",
    names_transform = list(year = as.integer)
  ) %>%
  group_by(state, municipality, geocode, year, vegetation_category) %>%
  summarise(total_area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  # A municipality with no rows for a transition has zero area in it (mapbiomas.qmd Q7).
  pivot_wider(
    id_cols = c(state, municipality, geocode, year),
    names_from = vegetation_category,
    values_from = total_area_ha,
    values_fill = 0,
    names_prefix = "area_"
  ) %>%
  mutate(
    deforestation_rate = if_else(area_forest > 0,
                                 (area_deforestation / area_forest) * RATE_SCALE,
                                 NA_real_)
  ) %>%
  group_by(geocode) %>%
  mutate(
    deforestation_mean = mean(area_deforestation, na.rm = TRUE),
    deforestation_sd   = sd(area_deforestation, na.rm = TRUE),
    deforestation_z    = (area_deforestation - deforestation_mean) / deforestation_sd
  ) %>%
  ungroup()

# Biome restriction, one block governed by BIOME_KEEP (D-2026-10-06-c); becomes flags at task 1.1.
deforestation_amz <- deforestation_states %>%
  left_join(
    biome_munic %>%
      select(geocode = Geocódigo, state_abbreviation = `Sigla da UF`,
             biome = `Bioma predominante`),
    by = "geocode"
  ) %>%
  filter(biome == BIOME_KEEP) %>%
  select(geocode, municipality, state, state_abbreviation, biome, year,
         area_forest, area_deforestation, deforestation_rate, deforestation_z,
         deforestation_mean, deforestation_sd) %>%
  arrange(geocode, year)

# ---- Assert: output state ---------------------------------------------------
# _audit/mapbiomas.qmd Q8, Q9.
stopifnot(
  nrow(deforestation_amz) == N_MUNIC_BIOME_EXPECTED * length(MAPBIOMAS_YEARS),
  n_distinct(deforestation_amz$geocode) == N_MUNIC_BIOME_EXPECTED,
  !anyDuplicated(paste(deforestation_amz$geocode, deforestation_amz$year)),
  all(deforestation_amz$biome == BIOME_KEEP),
  !anyNA(deforestation_amz$area_forest),
  !anyNA(deforestation_amz$area_deforestation),
  all(deforestation_amz$area_forest >= 0),
  all(deforestation_amz$area_deforestation >= 0),
  identical(is.na(deforestation_amz$deforestation_rate), deforestation_amz$area_forest == 0),
  is.character(deforestation_amz$geocode),
  is.integer(deforestation_amz$year),
  is.double(deforestation_amz$area_forest),
  is.double(deforestation_amz$area_deforestation),
  is.double(deforestation_amz$deforestation_rate)
)

# ---- Write ------------------------------------------------------------------
write_rds(deforestation_amz, "_data/interim/mapbiomas_deforestation_amz_1987_2023.rds")
write_csv(deforestation_amz, "_data/interim/mapbiomas_deforestation_amz_1987_2023.csv")
