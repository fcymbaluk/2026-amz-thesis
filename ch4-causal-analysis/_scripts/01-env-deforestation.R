# 01-env-deforestation.R
# PURPOSE   Build the municipality-year MapBiomas forest-area and deforestation
#           panel (km2) for the 808 municipalities of the nine Legal Amazon
#           states, 1987-2023, with the predominant biome from the IBGE list
#           and two constant flags: biome_amazon (predominant biome
#           Amazônia, 503) and legal_amazon (IBGE's 2022 Legal Amazon list,
#           772).
#
# INPUTS    _data/raw/mapbiomas/mapbiomas_munic.xlsx
#             MapBiomas, Deforestation and Secondary Vegetation, Collection 9
#             (Aug 2024), sheet DEF_SECVEG
#           _data/raw/ibge/ibge_bioma.csv
#             IBGE, Bioma predominante por município (Malha Municipal 2022)
#           _data/raw/ibge/Municipios_da_Amazonia_Legal_2022.xlsx
#             IBGE, Municípios da Amazônia Legal 2022 (geoftp, July 2023)
#
# OUTPUTS   _data/interim/mapbiomas_deforestation_amz_1987_2023.rds
#           _data/interim/mapbiomas_deforestation_amz_1987_2023.csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd
#            §1.1-1.2 until the annex is written; evidence:
#            _audit/mapbiomas.qmd and _audit/ibge_biome.qmd; history:
#            DECISIONS.md)
#   D-2026-10-06-a  Year window 1987-2023: suppression is all zero in 1985-86.
#   D-2026-10-06-b  Forest = primary + secondary vegetation; deforestation =
#                   primary + secondary suppression; all level-1 classes summed.
#   D-2026-10-06-c  Superseded by D-2026-10-29-a (phase 2, step 2.9): the
#                   nine-state subset of 808 municipalities is the sample;
#                   the biome is a flag, no longer a filter.
#   D-2026-10-29-a  biome_amazon (IBGE predominant biome Amazônia, 503,
#                   Mojuí dos Campos included) and legal_amazon (IBGE's
#                   2022 Legal Amazon list, 772) stored as constant 0/1
#                   flags; the 36 municipalities outside the Legal Amazon
#                   are Maranhão's Cerrado east of the 44th meridian.
#   D-2026-10-06-d  deforestation_forest_rate = deforestation / contemporaneous
#                   forest; on 0-1 from phase 2, step 2.8 (D-2026-10-28-h),
#                   the denominator timing confirmed on mapbiomas.qmd Q9.
#   D-2026-10-06-e  Municipality mean, sd and z-score over the full window
#                   stored.
#   D-2026-10-28-j  Areas converted from hectares to km2 here, at
#                   construction, and the three columns carry their panel
#                   names (phase 2, step 2.8; supersedes D-2026-10-11-c).
#
# RUN ORDER First script; no pipeline inputs. Feeds 04-panel-assemble.R.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

# ---- Constants --------------------------------------------------------------
MAPBIOMAS_SHEET        <- "DEF_SECVEG"
MAPBIOMAS_YEARS_SOURCE <- 1985:2023
MAPBIOMAS_YEARS        <- 1987:2023                           # D-2026-10-06-a
MAPBIOMAS_FOREST_TRANSITIONS <- c("Veg. Primária",            # D-2026-10-06-b
                                  "Veg. Secundária")
MAPBIOMAS_DEFOR_TRANSITIONS  <- c("Supressão Veg. Primária",  # D-2026-10-06-b
                                  "Supressão Veg. Secundária")
AMZ_STATES <- c("Acre", "Amapá", "Amazonas", "Maranhão",      # D-2026-10-06-c
                "Mato Grosso", "Pará", "Rondônia", "Roraima",
                "Tocantins")
# ---- Fix (D-2026-10-29-a; phase 2, step 2.9) ---------------------------------
# The sample is the nine-state subset; the Amazon biome and the Legal
# Amazon are constant flags, never filters. BIOME_KEEP names the
# predominant biome that biome_amazon marks.
BIOME_KEEP             <- "Amazônia"                          # D-2026-10-06-c
LEGAL_AMAZON_FILE      <- "_data/raw/ibge/Municipios_da_Amazonia_Legal_2022.xlsx"
LEGAL_AMAZON_SHEET     <- "Planilha1"
LEGAL_AMAZON_COLS      <- c("CD_MUN", "SIGLA_UF", "NM_MUN")
N_MUNIC_LEGAL_EXPECTED <- 772L     # IBGE 2022 list, all within the nine states
# Outside the Legal Amazon: Maranhão east of the 44th meridian, all Cerrado.
N_MUNIC_NON_LEGAL_EXPECTED <- 36L
NON_LEGAL_STATE            <- "MA"
NON_LEGAL_BIOME            <- "Cerrado"
# One name differs between MapBiomas and the IBGE lists: Santo Antônio de
# Leverger (MT), spelt "do Leverger" by IBGE's 2022 release; the panel keeps
# the MapBiomas spelling, as for every municipality (mapbiomas.qmd Q10).
NAME_MISMATCH_GEOCODES     <- "5107800"
# ---- end fix -----------------------------------------------------------------
HA_PER_KM2             <- 100                    # D-2026-10-28-j
N_MUNIC_BR_EXPECTED     <- 5571L
N_MUNIC_STATES_EXPECTED <- 808L
# Mojuí dos Campos included, dropped in script 05 (D-2026-10-05-l).
N_MUNIC_BIOME_EXPECTED  <- 503L
N_BIOME_FOOTER_ROWS     <- 7L

# ---- Read -------------------------------------------------------------------
mapbiomas_raw <- read_xlsx("_data/raw/mapbiomas/mapbiomas_munic.xlsx",
                           sheet = MAPBIOMAS_SHEET)

biome_raw <- read_delim("_data/raw/ibge/ibge_bioma.csv", delim = ";",
                        col_types = cols(.default = col_character()),
                        locale = locale(encoding = "UTF-8"),
                        show_col_types = FALSE, progress = FALSE)

# IBGE's list of the Legal Amazon municipalities on the 2022 mesh
# (D-2026-10-29-a).
legal_raw <- read_xlsx(LEGAL_AMAZON_FILE, sheet = LEGAL_AMAZON_SHEET,
                       col_types = "text")

# The IBGE file ends with a metadata footer; only 7-digit geocodes are
# municipalities (ibge_biome.qmd Q1).
biome_munic <- biome_raw %>%
  filter(str_detect(Geocódigo, "^[0-9]{7}$"))

# ---- Assert: raw state ------------------------------------------------------
year_cols_source  <- as.character(MAPBIOMAS_YEARS_SOURCE)
year_cols_dropped <- as.character(setdiff(MAPBIOMAS_YEARS_SOURCE,
                                          MAPBIOMAS_YEARS))

# _audit/mapbiomas.qmd Q1.
geocodes_states <- mapbiomas_raw %>%
  filter(state %in% AMZ_STATES) %>%
  pull(geocode)
stopifnot(
  all(c("state", "municipality", "geocode", "transition_name",
        year_cols_source) %in% names(mapbiomas_raw)),
  all(nchar(as.character(mapbiomas_raw$geocode)) == 7),
  !anyNA(mapbiomas_raw[, year_cols_source]),
  n_distinct(mapbiomas_raw$geocode) == N_MUNIC_BR_EXPECTED,
  all(c(MAPBIOMAS_FOREST_TRANSITIONS, MAPBIOMAS_DEFOR_TRANSITIONS)
      %in% mapbiomas_raw$transition_name),
  n_distinct(geocodes_states) == N_MUNIC_STATES_EXPECTED
)

# _audit/mapbiomas.qmd Q2 (D-2026-10-06-a).
suppression_dropped_years <- mapbiomas_raw %>%
  filter(transition_name %in% MAPBIOMAS_DEFOR_TRANSITIONS) %>%
  select(all_of(year_cols_dropped))
stopifnot(all(suppression_dropped_years == 0))

# _audit/ibge_biome.qmd Q1, Q2, Q4; _audit/mapbiomas.qmd Q10 (the nine-state
# sample, the Legal Amazon list and the names of all 808 municipalities).
biome_amz <- biome_munic %>%
  filter(`Bioma predominante` == BIOME_KEEP)
munic_names <- mapbiomas_raw %>%
  filter(state %in% AMZ_STATES) %>%
  distinct(geocode = as.character(geocode), municipality) %>%
  inner_join(biome_munic, by = c("geocode" = "Geocódigo"))
legal_geocodes <- legal_raw$CD_MUN
non_legal <- biome_munic %>%
  filter(Geocódigo %in% geocodes_states, !Geocódigo %in% legal_geocodes)
stopifnot(
  nrow(biome_raw) - nrow(biome_munic) == N_BIOME_FOOTER_ROWS,
  !anyDuplicated(biome_munic$Geocódigo),
  nrow(biome_amz) == N_MUNIC_BIOME_EXPECTED,
  all(biome_amz$Geocódigo %in% geocodes_states),
  nrow(munic_names) == N_MUNIC_STATES_EXPECTED,
  setequal(munic_names$geocode[munic_names$municipality !=
                                 munic_names$`Nome do município`],
           NAME_MISMATCH_GEOCODES),
  # D-2026-10-29-a: the Legal Amazon list lies within the nine states and
  # contains every biome municipality; the rest is Maranhão's Cerrado.
  all(LEGAL_AMAZON_COLS %in% names(legal_raw)),
  nrow(legal_raw) == N_MUNIC_LEGAL_EXPECTED,
  !anyDuplicated(legal_geocodes),
  all(legal_geocodes %in% geocodes_states),
  all(biome_amz$Geocódigo %in% legal_geocodes),
  nrow(non_legal) == N_MUNIC_NON_LEGAL_EXPECTED,
  all(non_legal$`Sigla da UF` == NON_LEGAL_STATE),
  all(non_legal$`Bioma predominante` == NON_LEGAL_BIOME)
)

# ---- Transform --------------------------------------------------------------
deforestation_states <- mapbiomas_raw %>%
  select(state, municipality, geocode, transition_name,
         all_of(as.character(MAPBIOMAS_YEARS))) %>%
  filter(
    state %in% AMZ_STATES,
    transition_name %in% c(MAPBIOMAS_FOREST_TRANSITIONS,
                           MAPBIOMAS_DEFOR_TRANSITIONS)
  ) %>%
  mutate(
    geocode = as.character(geocode),
    vegetation_category = if_else(
      transition_name %in% MAPBIOMAS_FOREST_TRANSITIONS,
      "forest", "deforestation"
    )
  ) %>%
  pivot_longer(
    cols = all_of(as.character(MAPBIOMAS_YEARS)),
    names_to = "year",
    values_to = "area_ha",
    names_transform = list(year = as.integer)
  ) %>%
  group_by(state, municipality, geocode, year, vegetation_category) %>%
  summarise(total_area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
  # A municipality with no rows for a transition has zero area in it
  # (mapbiomas.qmd Q7).
  pivot_wider(
    id_cols = c(state, municipality, geocode, year),
    names_from = vegetation_category,
    values_from = total_area_ha,
    values_fill = 0,
    names_prefix = "area_"
  ) %>%
  # ---- Fix (D-2026-10-28-j, -h; phase 2, step 2.8) -------------------------
  # Hectares to km2 at construction, and the rate as a plain quotient on
  # 0-1: the share of the same year's forest stock cleared in the year
  # (D-2026-10-06-d; mapbiomas.qmd Q9 on the denominator timing).
  transmute(
    state, municipality, geocode, year,
    forest_area_km2           = area_forest / HA_PER_KM2,
    deforestation_area_km2    = area_deforestation / HA_PER_KM2,
    deforestation_forest_rate = if_else(
      forest_area_km2 > 0,
      deforestation_area_km2 / forest_area_km2,
      NA_real_
    )
  ) %>%
  group_by(geocode) %>%
  mutate(
    deforestation_mean = mean(deforestation_area_km2, na.rm = TRUE),
    deforestation_sd   = sd(deforestation_area_km2, na.rm = TRUE),
    deforestation_z    = (deforestation_area_km2 - deforestation_mean) /
      deforestation_sd
  ) %>%
  ungroup()

# ---- Fix (D-2026-10-29-a; phase 2, step 2.9) ---------------------------------
# The biome filter of D-2026-10-06-c becomes two constant flags: every
# municipality of the nine states stays, with its predominant biome.
deforestation_amz <- deforestation_states %>%
  left_join(
    biome_munic %>%
      select(geocode = Geocódigo, state_abbreviation = `Sigla da UF`,
             biome = `Bioma predominante`),
    by = "geocode"
  ) %>%
  mutate(biome_amazon = as.integer(biome == BIOME_KEEP),
         legal_amazon = as.integer(geocode %in% legal_geocodes)) %>%
  select(geocode, municipality, state, state_abbreviation, biome,
         biome_amazon, legal_amazon, year,
         forest_area_km2, deforestation_area_km2, deforestation_forest_rate,
         deforestation_z,
         deforestation_mean, deforestation_sd) %>%
  arrange(geocode, year)
# ---- end fix -----------------------------------------------------------------

# ---- Assert: output state ---------------------------------------------------
# _audit/mapbiomas.qmd Q8, Q9, Q10.
first_year <- deforestation_amz$year == min(MAPBIOMAS_YEARS)
stopifnot(
  nrow(deforestation_amz) == N_MUNIC_STATES_EXPECTED * length(MAPBIOMAS_YEARS),
  n_distinct(deforestation_amz$geocode) == N_MUNIC_STATES_EXPECTED,
  !anyDuplicated(paste(deforestation_amz$geocode, deforestation_amz$year)),
  # D-2026-10-29-a: the two flags are 0/1, constant within municipality,
  # the biome inside the Legal Amazon, with the expected counts.
  !anyNA(deforestation_amz$biome),
  is.integer(deforestation_amz$biome_amazon),
  is.integer(deforestation_amz$legal_amazon),
  all(deforestation_amz$biome_amazon %in% 0:1),
  all(deforestation_amz$legal_amazon %in% 0:1),
  all(deforestation_amz$biome_amazon <= deforestation_amz$legal_amazon),
  identical(deforestation_amz$biome_amazon == 1L,
            deforestation_amz$biome == BIOME_KEEP),
  sum(deforestation_amz$biome_amazon[first_year]) == N_MUNIC_BIOME_EXPECTED,
  sum(deforestation_amz$legal_amazon[first_year]) == N_MUNIC_LEGAL_EXPECTED,
  n_distinct(paste(deforestation_amz$geocode,
                   deforestation_amz$biome_amazon)) == N_MUNIC_STATES_EXPECTED,
  n_distinct(paste(deforestation_amz$geocode,
                   deforestation_amz$legal_amazon)) == N_MUNIC_STATES_EXPECTED,
  !anyNA(deforestation_amz$forest_area_km2),
  !anyNA(deforestation_amz$deforestation_area_km2),
  all(deforestation_amz$forest_area_km2 >= 0),
  all(deforestation_amz$deforestation_area_km2 >= 0),
  identical(is.na(deforestation_amz$deforestation_forest_rate),
            deforestation_amz$forest_area_km2 == 0),
  # D-2026-10-28-h: a share on 0-1 (mapbiomas.qmd Q9).
  all(deforestation_amz$deforestation_forest_rate >= 0 &
        deforestation_amz$deforestation_forest_rate <= 1, na.rm = TRUE),
  is.character(deforestation_amz$geocode),
  is.integer(deforestation_amz$year),
  is.double(deforestation_amz$forest_area_km2),
  is.double(deforestation_amz$deforestation_area_km2),
  is.double(deforestation_amz$deforestation_forest_rate)
)

# ---- Write ------------------------------------------------------------------
out_stem <- "_data/interim/mapbiomas_deforestation_amz_1987_2023"
write_rds(deforestation_amz, paste0(out_stem, ".rds"))
write_csv(deforestation_amz, paste0(out_stem, ".csv"))
