# 03-controls-prices.R
# PURPOSE   Build the municipality-year agricultural price indices for
#           2000-2020: the World Bank real series of soy, corn, rice,
#           cotton and beef normalised to 2000, the crop index weighted by
#           the base-window crop shares, the cattle index weighted by the
#           base-window cattle density, and the crop index lag. All
#           municipalities with a base-window weight of either kind. Each
#           index is an exposure-weighted relative price: the world series
#           are normalised to 2000 = 1, the index is not, and its 2000
#           value equals the municipality's base-window exposure
#           (D-2026-10-23-a).
#
# INPUTS    _data/raw/worldbank/CMO-Historical-Data-Annual.xlsx
#             World Bank Commodity Markets Outlook (Pink Sheet), release of
#             2026-09-02, sheet Annual Prices (Real), 2010 US dollars
#           _data/interim/pam_ppm_weights_base_2000_2002.rds
#           _data/interim/pam_ppm_crop_areas_1988_2025.rds (PAM coverage)
#             03-controls-agro.R
#
# OUTPUTS   _data/interim/prices_real_normalized_2000_2020.rds / .csv
#           _data/interim/prices_index_2000_2020.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/prices.qmd;
#            history: DECISIONS.md)
#   D-2026-10-10-a  Pink Sheet release of September 2026; MUV re-deflation
#                   drift accepted.
#   D-2026-10-10-h  Price index = sum over crops of the base-window share
#                   times the real price relative to 2000; cattle index =
#                   base-window density times the relative beef price;
#                   one-year lag of the crop index.
#   D-2026-10-10-i  A municipality absent from PAM keeps its cattle index
#                   (the indices are joined in full, not through the crop
#                   index).
#   D-2026-10-10-j  Task 1.10: typed outputs.
#   D-2026-10-23-a  The indices stay on the exposure scale (no anchoring
#                   at 2000 = 1 within the municipality); the level is
#                   absorbed by municipality fixed effects (task 1.3
#                   resolved by reinterpretation).
#   D-2026-10-23-b  A municipality whose four crop weights are all missing
#                   has no crop index (NA, not 0).
# RUN ORDER After 03-controls-agro.R (weights).

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

# ---- Constants --------------------------------------------------------------
PRICES_YEARS  <- 2000:2020
PRICES_BASE   <- 2000L                                        # D-2026-10-10-h
PINK_FILE     <- "_data/raw/worldbank/CMO-Historical-Data-Annual.xlsx"
PINK_SHEET    <- "Annual Prices (Real)"
PINK_NAME_ROW <- 7L   # commodity names; units on row 8, data from row 9
PINK_DATA_ROW <- 9L
PINK_SERIES   <- c(soy = "Soybeans", corn = "Maize", rice = "Rice, Thai 5%",
                   cotton = "Cotton, A Index", beef = "Beef")
PINK_UNITS    <- c(soy = "($/mt)", corn = "($/mt)", rice = "($/mt)",
                   cotton = "($/kg)", beef = "($/kg)")
CROPS         <- c("soy", "corn", "rice", "cotton")
PINK_FIRST_YEAR <- 1960L

# ---- Read -------------------------------------------------------------------
pink_raw <- read_excel(PINK_FILE, sheet = PINK_SHEET, col_names = FALSE,
                       col_types = "text", .name_repair = "minimal")
weights <- read_rds("_data/interim/pam_ppm_weights_base_2000_2002.rds")
pam_geocodes <- unique(
  read_rds("_data/interim/pam_ppm_crop_areas_1988_2025.rds")$geocode
)

# ---- Assert: raw state ------------------------------------------------------
# prices.qmd Q1: legacy layout (names row 7, units row 8, data from row 9,
# year in the first column), the five series present with their units.
pink_names <- str_trim(unlist(pink_raw[PINK_NAME_ROW, ], use.names = FALSE))
pink_units <- str_trim(unlist(pink_raw[PINK_NAME_ROW + 1L, ],
                              use.names = FALSE))
series_col <- match(PINK_SERIES, pink_names)
stopifnot(
  !anyNA(series_col),
  all(pink_units[series_col] == PINK_UNITS),
  pink_raw[[1]][PINK_DATA_ROW] == as.character(PINK_FIRST_YEAR),
  all(c("crop_w_soy", "crop_w_corn", "crop_w_rice", "crop_w_cotton",
        "cattle_weight") %in% names(weights)),
  !anyDuplicated(weights$geocode),
  all(pam_geocodes %in% weights$geocode)
)

prices_real <- pink_raw[PINK_DATA_ROW:nrow(pink_raw), c(1L, series_col)] %>%
  set_names(c("year", names(PINK_SERIES))) %>%
  mutate(across(everything(), as.numeric), year = as.integer(year)) %>%
  filter(year %in% PRICES_YEARS) %>%
  arrange(year)
stopifnot(
  identical(prices_real$year, PRICES_YEARS),
  !anyNA(prices_real)
)

# ---- Transform --------------------------------------------------------------
prices_norm <- prices_real %>%
  mutate(across(all_of(names(PINK_SERIES)),
                ~ .x / .x[year == PRICES_BASE]))

crop_prices <- prices_norm %>%
  select(year, all_of(CROPS)) %>%
  pivot_longer(-year, names_to = "crop", values_to = "price_rel")

# A missing weight is dropped from the weighted sum (a crop without a PAM
# value in the window counts as absent), but a municipality whose four
# weights are all missing has no exposure measure and reads NA, not 0
# (D-2026-10-23-b); a municipality with no PAM series has no crop index
# either. The cattle index is NA where the density weight is NA.
crop_all_na_geocodes <- weights %>%
  filter(if_all(starts_with("crop_w_"), is.na)) %>%
  pull(geocode)
crop_index <- weights %>%
  select(geocode, starts_with("crop_w_")) %>%
  filter(if_any(starts_with("crop_w_"), ~ !is.na(.x)) |
           geocode %in% pam_geocodes) %>%
  pivot_longer(-geocode, names_to = "crop", names_prefix = "crop_w_",
               values_to = "w") %>%
  inner_join(crop_prices, by = "crop", relationship = "many-to-many") %>%
  group_by(geocode, year) %>%
  summarise(price_index_crop = if (all(is.na(w))) NA_real_
                               else sum(w * price_rel, na.rm = TRUE),
            .groups = "drop")

beef_rel <- prices_norm %>% select(year, beef)
cattle_index <- weights %>%
  select(geocode, cattle_weight) %>%
  cross_join(beef_rel) %>%
  transmute(geocode, year, price_index_cattle = cattle_weight * beef)

prices_index <- crop_index %>%
  full_join(cattle_index, by = c("geocode", "year")) %>%
  arrange(geocode, year) %>%
  group_by(geocode) %>%
  mutate(price_index_crop_lag1 = lag(price_index_crop)) %>%
  ungroup()

# ---- Assert: output state ---------------------------------------------------
# prices.qmd Q2, Q3.
stopifnot(
  nrow(prices_norm) == length(PRICES_YEARS),
  all(unlist(prices_norm[prices_norm$year == PRICES_BASE,
                         names(PINK_SERIES)]) == 1),
  all(prices_norm[names(PINK_SERIES)] > 0),
  nrow(prices_index) == nrow(weights) * length(PRICES_YEARS),
  !anyDuplicated(paste(prices_index$geocode, prices_index$year)),
  all(!(crop_all_na_geocodes %in% pam_geocodes) |
        crop_all_na_geocodes %in% crop_index$geocode),
  all(is.na(prices_index$price_index_crop) ==
        (prices_index$geocode %in% crop_all_na_geocodes)),    # D-2026-10-23-b
  all(prices_index$price_index_crop >= 0, na.rm = TRUE),
  all(prices_index$price_index_cattle >= 0, na.rm = TRUE),
  all(is.na(prices_index$price_index_crop_lag1) ==
        (prices_index$year == PRICES_YEARS[1] |
           is.na(prices_index$price_index_crop))),
  is.integer(prices_norm$year),
  all(map_lgl(prices_norm[-1], is.double)),
  is.character(prices_index$geocode),
  is.integer(prices_index$year),
  all(map_lgl(prices_index[-(1:2)], is.double))
)

# ---- Write ------------------------------------------------------------------
write_rds(prices_norm, "_data/interim/prices_real_normalized_2000_2020.rds")
write_csv(prices_norm, "_data/interim/prices_real_normalized_2000_2020.csv")
write_rds(prices_index, "_data/interim/prices_index_2000_2020.rds")
write_csv(prices_index, "_data/interim/prices_index_2000_2020.csv")
