# 03-controls-agro.R
# PURPOSE   Build the municipal agricultural exposure series: the 2022
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
#           _data/raw/ibge/AR_BR_RG_UF_RGINT_MES_MIC_MUN_2022.xls
#                                        IBGE Áreas Territoriais 2022,
#                                        sheet AR_BR_MUN_2022
#
# OUTPUTS   _data/interim/pam_ppm_munic_area_2022.rds / .csv
#           _data/interim/pam_ppm_crop_areas_1988_2025.rds / .csv
#           _data/interim/pam_ppm_cattle_1974_2025.rds / .csv
#           _data/interim/pam_ppm_weights_base_2000_2002.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/pam_ppm.qmd;
#            history: DECISIONS.md)
#   D-2026-10-10-f  Crop and cattle exposure weights = 2000-2002 means of
#                   the crop share and of the cattle density, over the
#                   municipal area (vintage: D-2026-10-28-g).
#   D-2026-10-10-g  SIDRA symbols in PAM and PPM: "-" is zero, every other
#                   symbol is NA.
#   D-2026-10-10-i  The municipal area is stored for every municipality
#                   from the areas workbook, not through the PAM join;
#                   Santa Cruz do Arari (1506401), absent from PAM, keeps
#                   its area and its cattle index.
#   D-2026-10-10-j  Task 1.10: typed outputs; all-NA base windows stored
#                   as NA, not NaN.
#   D-2026-10-28-d  Task 1.8 (phase 2, step 2.7): the symbol rule
#                   confirmed; "..." is exactly the years before
#                   installation for the post-2000 municipalities and a
#                   survey gap elsewhere (pam_ppm.qmd Q6); the
#                   installation rule is asserted in script 04.
#   D-2026-10-28-g  Task 1.8 (phase 2, step 2.7): the municipal area is
#                   IBGE's 2022 release, the mesh MapBiomas Collection 9
#                   maps, in every year (was the 2025 release, post-split
#                   for Nova Ubiratã); Boa Esperança do Norte (MT,
#                   installed 2025, reported by PAM and PPM) has no 2022
#                   area, so its shares and density are NA and it stays
#                   in the interim files.
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
# ---- Fix (D-2026-10-28-g; phase 2, step 2.7) ---------------------------------
AREAS_SHEET     <- "AR_BR_MUN_2022"
AREAS_FILE      <- "_data/raw/ibge/AR_BR_RG_UF_RGINT_MES_MIC_MUN_2022.xls"
AREAS_COLUMN    <- "AR_MUN_2022"
# ---- end fix -----------------------------------------------------------------
N_MUNIC_PAM_EXPECTED   <- 5564L
N_MUNIC_PPM_EXPECTED   <- 5568L
# The 2022 grid: 5,570 municipalities plus the Lagoa dos Patos and Lagoa
# Mirim operational areas (D-2026-10-28-g). Boa Esperança do Norte (MT,
# installed 2025) is reported by PAM and PPM but has no 2022 area.
N_LOCALITIES_AREAS_EXPECTED <- 5572L
N_MUNIC_AREAS_EXPECTED      <- 5570L
N_AREAS_TRAILING_ROWS       <- 3L     # two blank rows and the IBGE note
AREAS_NON_MUNICIPAL         <- c("4300001", "4300002")
BOA_ESPERANCA_NORTE_GEOCODE <- "5101837"                      # D-2026-10-28-g
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

# pam_ppm.qmd Q3: IBGE's column names, one row per municipality plus three
# trailing rows without a code.
areas <- areas_raw %>%
  filter(is_geocode(CD_MUN)) %>%
  transmute(geocode = CD_MUN, munic_area_km2 = as.numeric(.data[[AREAS_COLUMN]]))
stopifnot(
  identical(names(areas_raw), c("ID", "CD_UF", "NM_UF", "NM_UF_SIGLA",
                                "CD_MUN", "NM_MUN", AREAS_COLUMN)),
  nrow(areas_raw) == N_LOCALITIES_AREAS_EXPECTED + N_AREAS_TRAILING_ROWS,
  nrow(areas) == N_LOCALITIES_AREAS_EXPECTED,
  !anyDuplicated(areas$geocode),
  !anyNA(areas$munic_area_km2),
  all(areas$munic_area_km2 > 0),
  # D-2026-10-28-g: the one PAM/PPM municipality without a 2022 area.
  identical(setdiff(unique(pam_raw$geocode), areas$geocode),
            BOA_ESPERANCA_NORTE_GEOCODE),
  identical(setdiff(unique(ppm_raw$geocode), areas$geocode),
            BOA_ESPERANCA_NORTE_GEOCODE),
  !BOA_ESPERANCA_NORTE_GEOCODE %in% areas$geocode,
  !SANTA_CRUZ_ARARI_GEOCODE %in% pam_raw$geocode,
  SANTA_CRUZ_ARARI_GEOCODE %in% ppm_raw$geocode,
  !is.na(areas$munic_area_km2[areas$geocode == SANTA_CRUZ_ARARI_GEOCODE])
)

# ---- Transform --------------------------------------------------------------
# Every municipality of the 2022 grid, whether or not PAM or PPM report it
# (D-2026-10-10-i, D-2026-10-28-g).
munic_area <- areas %>%
  filter(!geocode %in% AREAS_NON_MUNICIPAL) %>%
  arrange(geocode)

# Shares use the 2022 area for every year, so a share above 1 is possible
# where successive crops are planted in one year (D-2026-10-10-f,
# D-2026-10-28-g); Boa Esperança do Norte has no area, so NA shares.
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
  identical(setdiff(unique(c(pam_raw$geocode, ppm_raw$geocode)),
                    munic_area$geocode), BOA_ESPERANCA_NORTE_GEOCODE),
  # D-2026-10-28-g: NA shares and density only for the municipality without
  # an area; elsewhere the density is NA exactly where the herd is.
  all(is.na(crop_areas[crop_areas$geocode == BOA_ESPERANCA_NORTE_GEOCODE,
                       share_cols])),
  all(is.na(cattle$cattle_per_km2[cattle$geocode ==
                                    BOA_ESPERANCA_NORTE_GEOCODE])),
  identical(is.na(cattle$cattle_per_km2[cattle$geocode !=
                                          BOA_ESPERANCA_NORTE_GEOCODE]),
            is.na(cattle$cattle_heads[cattle$geocode !=
                                        BOA_ESPERANCA_NORTE_GEOCODE])),
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
write_rds(munic_area, "_data/interim/pam_ppm_munic_area_2022.rds")
write_csv(munic_area, "_data/interim/pam_ppm_munic_area_2022.csv")
write_rds(crop_areas, "_data/interim/pam_ppm_crop_areas_1988_2025.rds")
write_csv(crop_areas, "_data/interim/pam_ppm_crop_areas_1988_2025.csv")
write_rds(cattle, "_data/interim/pam_ppm_cattle_1974_2025.rds")
write_csv(cattle, "_data/interim/pam_ppm_cattle_1974_2025.csv")
write_rds(weights, "_data/interim/pam_ppm_weights_base_2000_2002.rds")
write_csv(weights, "_data/interim/pam_ppm_weights_base_2000_2002.csv")
