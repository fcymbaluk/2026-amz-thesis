# 03-controls-agro.R
# PURPOSE   Build the municipal agricultural exposure series: the 2025
#           municipal area; planted area of soy, corn, rice and cotton
#           (PAM) as km2 and as a share of the municipal area, 1988-2025;
#           cattle heads and density (PPM), 1974-2025; and the base-window
#           (2000-2002) crop and cattle weights of the price indices. All
#           municipalities.
#
# INPUTS    _data/raw/ibge/sidra/sidra_5457_areaplanted_{algodao,arroz,
#             milho,soja}_*_N6.json      IBGE PAM, SIDRA Tabela 5457,
#                                        variable 8331, API v3, 2026-10-09
#           _data/raw/ibge/sidra/sidra_3939_bovino_*_N6.json
#                                        IBGE PPM, SIDRA Tabela 3939,
#                                        variable 105, API v3, 2026-10-09
#           _data/raw/ibge/AR_BR_RG_UF_RGINT_RGI_MUN_2025.xls
#                                        IBGE Áreas Territoriais 2025,
#                                        sheet AR_BR_MUN_2025
#
# OUTPUTS   _data/interim/pam_ppm_munic_area_2025.rds / .csv
#           _data/interim/pam_ppm_crop_areas_1988_2025.rds / .csv
#           _data/interim/pam_ppm_cattle_1974_2025.rds / .csv
#           _data/interim/pam_ppm_weights_base_2000_2002.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/pam_ppm.qmd;
#            history: DECISIONS.md)
#   D-2026-10-10-f  Crop and cattle exposure weights = 2000-2002 means of
#                   the crop share and of the cattle density, over the
#                   2025 municipal area.
#   D-2026-10-10-g  SIDRA symbols in PAM and PPM: "-" is zero, every other
#                   symbol is NA.
#   D-2026-10-10-i  The municipal area is stored for every municipality
#                   from the areas workbook, not through the PAM join;
#                   Santa Cruz do Arari (1506401), absent from PAM, keeps
#                   its area and its cattle index.
#   D-2026-10-10-j  Task 1.10: typed outputs; all-NA base windows stored
#                   as NA, not NaN.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.8  A municipality absent from the survey in a year reads NA
#             ("...") and a zero reads 0 ("-"), as inherited; whether the
#             survey covered it is not checked here.
# RUN ORDER First of the 03 scripts; no pipeline inputs. Feeds
#           03-controls-prices.R (weights) and 04 (area).

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

source("_scripts/utils/read_sidra.R")

# ---- Constants --------------------------------------------------------------
PAM_YEARS          <- 1988:2025
PPM_YEARS          <- 1974:2025
WEIGHT_BASE_YEARS  <- 2000:2002                              # D-2026-10-10-f
SIDRA_DIR          <- "_data/raw/ibge/sidra"
SIDRA_ZERO_SYMBOLS <- "-"                                     # D-2026-10-10-g
# Tabela 5457, classification 782 (produto das lavouras temporárias e
# permanentes): category id per crop, in the panel's crop vocabulary.
PAM_CROP_IDS <- c(soy = "40124", corn = "40122", rice = "40102",
                  cotton = "40099")
PAM_FILE_TOKENS <- c(soy = "soja", corn = "milho", rice = "arroz",
                     cotton = "algodao")
PPM_CATTLE_ID   <- "2670"           # Tabela 3939, classification 79, bovino
HA_PER_KM2      <- 100
AREAS_SHEET     <- "AR_BR_MUN_2025"
AREAS_FILE      <- "_data/raw/ibge/AR_BR_RG_UF_RGINT_RGI_MUN_2025.xls"
N_MUNIC_PAM_EXPECTED   <- 5564L
N_MUNIC_PPM_EXPECTED   <- 5568L
# The 2025 grid: 5,571 municipalities (Boa Esperança do Norte, installed
# 2025, included) plus the Lagoa dos Patos and Lagoa Mirim operational areas.
N_LOCALITIES_AREAS_EXPECTED <- 5573L
N_MUNIC_AREAS_EXPECTED      <- 5571L
AREAS_NON_MUNICIPAL         <- c("4300001", "4300002")
SANTA_CRUZ_ARARI_GEOCODE <- "1506401"                         # D-2026-10-10-i

# ---- Read -------------------------------------------------------------------
sidra_files <- function(pattern) {
  list.files(SIDRA_DIR, pattern = pattern, full.names = TRUE)
}
pam_raw <- map_dfr(names(PAM_FILE_TOKENS), function(crop) {
  pattern <- paste0("^sidra_5457_areaplanted_", PAM_FILE_TOKENS[[crop]], "_")
  map_dfr(sidra_files(pattern), read_sidra_json) %>%
    mutate(crop = crop)
})
ppm_raw <- map_dfr(sidra_files("^sidra_3939_bovino_"), read_sidra_json)

areas_raw <- read_excel(AREAS_FILE, sheet = AREAS_SHEET, col_types = "text")

# ---- Assert: raw state ------------------------------------------------------
is_geocode <- function(x) !is.na(x) & str_detect(x, "^[0-9]{7}$")
sidra_symbol_ok <- function(x) {
  all(str_detect(x, "^[0-9]+$") | x %in% c("-", "..."))
}

# pam_ppm.qmd Q1, Q2: one series per municipality and crop over the full
# span, symbols "-" and "..." only, category ids as documented.
stopifnot(
  all(pam_raw$category_id == PAM_CROP_IDS[pam_raw$crop]),
  setequal(pam_raw$year, PAM_YEARS),
  n_distinct(pam_raw$geocode) == N_MUNIC_PAM_EXPECTED,
  !anyDuplicated(paste(pam_raw$geocode, pam_raw$year, pam_raw$crop)),
  nrow(pam_raw) == N_MUNIC_PAM_EXPECTED * length(PAM_YEARS) *
    length(PAM_CROP_IDS),
  all(is_geocode(pam_raw$geocode)),
  sidra_symbol_ok(pam_raw$value),
  all(ppm_raw$category_id == PPM_CATTLE_ID),
  setequal(ppm_raw$year, PPM_YEARS),
  n_distinct(ppm_raw$geocode) == N_MUNIC_PPM_EXPECTED,
  !anyDuplicated(paste(ppm_raw$geocode, ppm_raw$year)),
  nrow(ppm_raw) == N_MUNIC_PPM_EXPECTED * length(PPM_YEARS),
  all(is_geocode(ppm_raw$geocode)),
  sidra_symbol_ok(ppm_raw$value)
)

# pam_ppm.qmd Q3: IBGE's column names, one row per municipality plus two
# trailing note rows without a code.
areas <- areas_raw %>%
  filter(is_geocode(CD_MUN)) %>%
  transmute(geocode = CD_MUN, munic_area_km2 = as.numeric(AR_MUN_2025))
stopifnot(
  identical(names(areas_raw), c("CD_UF", "NM_UF", "NM_UF_SIGLA", "CD_MUN",
                                "NM_MUN", "AR_MUN_2025")),
  nrow(areas_raw) == N_LOCALITIES_AREAS_EXPECTED + 2L,
  nrow(areas) == N_LOCALITIES_AREAS_EXPECTED,
  !anyDuplicated(areas$geocode),
  !anyNA(areas$munic_area_km2),
  all(areas$munic_area_km2 > 0),
  all(unique(pam_raw$geocode) %in% areas$geocode),
  all(unique(ppm_raw$geocode) %in% areas$geocode),
  !SANTA_CRUZ_ARARI_GEOCODE %in% pam_raw$geocode,
  SANTA_CRUZ_ARARI_GEOCODE %in% ppm_raw$geocode,
  !is.na(areas$munic_area_km2[areas$geocode == SANTA_CRUZ_ARARI_GEOCODE])
)

# ---- Transform --------------------------------------------------------------
# Every municipality of the 2025 grid, whether or not PAM or PPM report it
# (D-2026-10-10-i).
munic_area <- areas %>%
  filter(!geocode %in% AREAS_NON_MUNICIPAL) %>%
  arrange(geocode)

# Shares use the 2025 area for every year, so a share above 1 is possible
# where successive crops are planted in one year (D-2026-10-10-f).
crop_long <- pam_raw %>%
  transmute(geocode, year, crop,
            area_planted_km2 = sidra_value(value, SIDRA_ZERO_SYMBOLS) /
              HA_PER_KM2) %>%
  left_join(areas, by = "geocode") %>%
  mutate(munic_area_share = area_planted_km2 / munic_area_km2)

crop_areas <- crop_long %>%
  pivot_wider(id_cols = c(geocode, year),
              names_from = crop,
              values_from = c(area_planted_km2, munic_area_share),
              names_glue = "crop_{crop}_{.value}") %>%
  select(geocode, year,
         all_of(paste0("crop_", names(PAM_CROP_IDS), "_area_planted_km2")),
         all_of(paste0("crop_", names(PAM_CROP_IDS), "_munic_area_share"))) %>%
  arrange(geocode, year)

cattle <- ppm_raw %>%
  transmute(geocode, year,
            cattle_heads = sidra_value(value, SIDRA_ZERO_SYMBOLS)) %>%
  left_join(areas, by = "geocode") %>%
  transmute(geocode, year, cattle_heads,
            cattle_per_km2 = cattle_heads / munic_area_km2) %>%
  arrange(geocode, year)

# Base-window means; a window with no observed value is NA
# (D-2026-10-10-f, D-2026-10-10-j).
mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}
crop_weights <- crop_long %>%
  filter(year %in% WEIGHT_BASE_YEARS) %>%
  group_by(geocode, crop) %>%
  summarise(w = mean_or_na(munic_area_share), .groups = "drop") %>%
  pivot_wider(names_from = crop, values_from = w, names_prefix = "crop_w_")
cattle_weights <- cattle %>%
  filter(year %in% WEIGHT_BASE_YEARS) %>%
  group_by(geocode) %>%
  summarise(cattle_weight = mean_or_na(cattle_per_km2), .groups = "drop")
weights <- crop_weights %>%
  full_join(cattle_weights, by = "geocode") %>%
  select(geocode, all_of(paste0("crop_w_", names(PAM_CROP_IDS))),
         cattle_weight) %>%
  arrange(geocode)

# ---- Assert: output state ---------------------------------------------------
# pam_ppm.qmd Q4, Q5.
share_cols <- paste0("crop_", names(PAM_CROP_IDS), "_munic_area_share")
stopifnot(
  nrow(munic_area) == N_MUNIC_AREAS_EXPECTED,
  all(unique(c(pam_raw$geocode, ppm_raw$geocode)) %in% munic_area$geocode),
  !anyDuplicated(munic_area$geocode),
  !anyNA(munic_area$munic_area_km2),
  nrow(crop_areas) == N_MUNIC_PAM_EXPECTED * length(PAM_YEARS),
  !anyDuplicated(paste(crop_areas$geocode, crop_areas$year)),
  all(crop_areas[share_cols] >= 0, na.rm = TRUE),
  nrow(cattle) == N_MUNIC_PPM_EXPECTED * length(PPM_YEARS),
  !anyDuplicated(paste(cattle$geocode, cattle$year)),
  all(cattle$cattle_heads >= 0, na.rm = TRUE),
  all(cattle$cattle_per_km2 >= 0, na.rm = TRUE),
  nrow(weights) == n_distinct(c(pam_raw$geocode, ppm_raw$geocode)),
  !anyDuplicated(weights$geocode),
  !any(is.nan(as.matrix(weights[-1]))),
  is.character(munic_area$geocode),
  is.double(munic_area$munic_area_km2),
  is.character(crop_areas$geocode),
  is.integer(crop_areas$year),
  all(map_lgl(crop_areas[-(1:2)], is.double)),
  is.character(cattle$geocode),
  is.integer(cattle$year),
  is.double(cattle$cattle_heads),
  is.double(cattle$cattle_per_km2),
  is.character(weights$geocode),
  all(map_lgl(weights[-1], is.double))
)

# ---- Write ------------------------------------------------------------------
write_rds(munic_area, "_data/interim/pam_ppm_munic_area_2025.rds")
write_csv(munic_area, "_data/interim/pam_ppm_munic_area_2025.csv")
write_rds(crop_areas, "_data/interim/pam_ppm_crop_areas_1988_2025.rds")
write_csv(crop_areas, "_data/interim/pam_ppm_crop_areas_1988_2025.csv")
write_rds(cattle, "_data/interim/pam_ppm_cattle_1974_2025.rds")
write_csv(cattle, "_data/interim/pam_ppm_cattle_1974_2025.csv")
write_rds(weights, "_data/interim/pam_ppm_weights_base_2000_2002.rds")
write_csv(weights, "_data/interim/pam_ppm_weights_base_2000_2002.csv")
