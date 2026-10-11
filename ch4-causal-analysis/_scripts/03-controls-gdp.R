# 03-controls-gdp.R
# PURPOSE   Build the municipality-year GDP panel for 2002-2022: current
#           and 2024 reais, per capita and its log, with the resident
#           population interpolated between the 2000, 2010 and 2022
#           censuses as denominator. All municipalities.
#
# INPUTS    _data/raw/ibge/sidra/sidra_5938_pib_*_N6.json
#             IBGE PIB dos Municípios, SIDRA Tabela 5938, variable 37
#             (PIB a preços correntes, mil reais), API v3, 2026-10-09
#           _data/raw/ibge/Ibge_pop_1970-80-91-00-10.xlsx
#             IBGE censuses, resident population 1970-2010 (SIDRA export)
#           _data/raw/ibge/Ibge_pop_2022 (Censo - Sidra).xlsx
#             IBGE Censo 2022, SIDRA Tabela 4709, resident population
#           _data/aux/deflator_ipca_2024.csv
#
# OUTPUTS   _data/interim/ibge_pib_gdp_2002_2022.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/ibge_pib.qmd;
#            history: DECISIONS.md)
#   D-2026-10-05-c  Monetary values deflated to 2024 BRL with the IPCA.
#   D-2026-10-05-f  Linear interpolation between the census anchors.
#   D-2026-10-10-b  GDP read from the SIDRA API in integer thousands of
#                   reais.
#   D-2026-10-10-e  Santarém's GDP and population before 2013 split with
#                   Mojuí dos Campos by their 2022 shares.
#   D-2026-10-10-j  Task 1.10: typed outputs.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.6  Municipalities absent from the 2000 census have no population
#             before 2010, so gdp_per_capita_* is NA for 2002-2009.
#             Reproduced on purpose (D-2026-10-05-k).
#   Task 1.7  Only the interpolated population is stored, and only for the
#             GDP years (D-2026-10-10-k); the anchors are not.
# RUN ORDER Independent of the other 03 scripts; no pipeline inputs.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

source("_scripts/utils/read_sidra.R")
source("_scripts/utils/interpolate_census.R")

# ---- Constants --------------------------------------------------------------
# D-2026-10-10-k: the GDP series starts in 2002 and is capped at the last
# census.
GDP_YEARS      <- 2002:2022
CENSUS_YEARS   <- c(2000L, 2010L, 2022L)                      # D-2026-10-05-f
POP_YEARS      <- 2000:2022
DEFLATOR_BASE  <- 2024L                                       # D-2026-10-05-c
BRL_PER_MIL    <- 1000                                        # D-2026-10-10-b
SIDRA_DIR      <- "_data/raw/ibge/sidra"
POP_FILE_2000_2010 <- "_data/raw/ibge/Ibge_pop_1970-80-91-00-10.xlsx"
POP_FILE_2022      <- "_data/raw/ibge/Ibge_pop_2022 (Censo - Sidra).xlsx"
POP_SHEET          <- "Tabela"
MOJUI_GEOCODE      <- "1504752"                               # D-2026-10-10-e
SANTAREM_GEOCODE   <- "1506807"
MOJUI_SPLIT_YEAR   <- 2013L         # Mojuí installed; own series from here
N_MUNIC_PIB_EXPECTED      <- 5570L
N_MUNIC_POP_2010_EXPECTED <- 5566L  # 2010 grid plus Cococi (CE), extinct
N_MUNIC_POP_2022_EXPECTED <- 5570L
N_GDP_NEGATIVE_EXPECTED   <- 1L     # Guamaré (RN) 2012, as served by IBGE

# ---- Read -------------------------------------------------------------------
pib_raw <- map_dfr(list.files(SIDRA_DIR, pattern = "^sidra_5938_pib_",
                              full.names = TRUE),
                   read_sidra_json)
pop_2000_2010_raw <- read_excel(POP_FILE_2000_2010, sheet = POP_SHEET,
                                col_types = "text")
pop_2022_raw <- read_excel(POP_FILE_2022, sheet = POP_SHEET,
                           col_types = "text", col_names = FALSE,
                           .name_repair = "minimal")
deflator <- read_csv("_data/aux/deflator_ipca_2024.csv",
                     col_types = cols(year = col_integer(),
                                      ipca_index = col_double(),
                                      ipca_deflator = col_double())) %>%
  select(year, ipca_deflator)

# ---- Assert: raw state ------------------------------------------------------
is_geocode <- function(x) !is.na(x) & str_detect(x, "^[0-9]{7}$")

# ibge_pib.qmd Q1: one GDP series per municipality 2002-2023, the only
# symbol "..." (not available); one negative value, outside the sample.
stopifnot(
  all(is.na(pib_raw$category_id)),
  n_distinct(pib_raw$geocode) == N_MUNIC_PIB_EXPECTED,
  all(is_geocode(pib_raw$geocode)),
  !anyDuplicated(paste(pib_raw$geocode, pib_raw$year)),
  all(GDP_YEARS %in% pib_raw$year),
  all(str_detect(pib_raw$value, "^-?[0-9]+$") | pib_raw$value == "..."),
  sum(str_detect(pib_raw$value, "^-")) == N_GDP_NEGATIVE_EXPECTED,
  identical(names(pop_2000_2010_raw),
            c("munic_id", "munic", "1970", "1980", "1991", "2000", "2010")),
  all(is_geocode(pop_2000_2010_raw$munic_id)),
  !anyDuplicated(pop_2000_2010_raw$munic_id),
  nrow(pop_2000_2010_raw) == N_MUNIC_POP_2010_EXPECTED,
  all(deflator$year[deflator$ipca_deflator == 1] == DEFLATOR_BASE),
  all(GDP_YEARS %in% deflator$year)
)

# ibge_pib.qmd Q2: the 2022 workbook has four header rows, a Brasil row,
# one row per municipality and a source footer; "..." is the only symbol
# in the census columns.
pop_anchors <- bind_rows(
  pop_2000_2010_raw %>%
    select(geocode = munic_id, `2000`, `2010`) %>%
    pivot_longer(-geocode, names_to = "year", values_to = "value"),
  pop_2022_raw %>%
    set_names(c("level", "geocode", "municipality", "value")) %>%
    filter(is_geocode(geocode)) %>%
    transmute(geocode, year = "2022", value)
) %>%
  transmute(geocode, year = as.integer(year),
            population = sidra_value(value, zero_symbols = character(0)))
stopifnot(
  identical(unlist(pop_2022_raw[3, 2:3], use.names = FALSE),
            c("Cód.", "Brasil e Município")),
  pop_2022_raw[[4]][4] == "2022",
  sum(is_geocode(pop_anchors$geocode[pop_anchors$year == 2022L])) ==
    N_MUNIC_POP_2022_EXPECTED,
  setequal(pop_anchors$year, CENSUS_YEARS),
  !anyDuplicated(paste(pop_anchors$geocode, pop_anchors$year)),
  all(pop_anchors$population > 0, na.rm = TRUE),
  all(!is.na(pop_anchors$population) |
        pop_anchors$year < CENSUS_YEARS[3])
)

# ---- Transform --------------------------------------------------------------
population <- pop_anchors %>%
  group_by(geocode) %>%
  complete(year = POP_YEARS) %>%
  arrange(geocode, year) %>%
  mutate(population = interp_linear(population, year)) %>%
  ungroup()

gdp <- pib_raw %>%
  transmute(geocode, year,
            gdp_brl = sidra_value(value, zero_symbols = character(0)) *
              BRL_PER_MIL) %>%
  filter(year %in% GDP_YEARS) %>%
  left_join(deflator, by = "year") %>%
  mutate(gdp_brl_2024 = gdp_brl * ipca_deflator) %>%
  select(-ipca_deflator) %>%
  left_join(population, by = c("geocode", "year"))

# Mojuí dos Campos has no series before its installation; Santarém's
# values are split by the pair's 2022 shares, each variable by its own
# share, and the gap in Mojuí's population is then interpolated
# (D-2026-10-10-e).
pair_2022 <- gdp %>%
  filter(year == CENSUS_YEARS[3],
         geocode %in% c(MOJUI_GEOCODE, SANTAREM_GEOCODE))
mojui_share <- function(v) {
  v[pair_2022$geocode == MOJUI_GEOCODE] / sum(v)
}
share_pop      <- mojui_share(pair_2022$population)
share_gdp      <- mojui_share(pair_2022$gdp_brl)
share_gdp_2024 <- mojui_share(pair_2022$gdp_brl_2024)

santarem_pre <- gdp %>%
  filter(geocode == SANTAREM_GEOCODE, year < MOJUI_SPLIT_YEAR)
gdp_split <- gdp %>%
  filter(!(geocode %in% c(MOJUI_GEOCODE, SANTAREM_GEOCODE) &
             year < MOJUI_SPLIT_YEAR)) %>%
  bind_rows(
    santarem_pre %>%
      mutate(gdp_brl      = gdp_brl * (1 - share_gdp),
             gdp_brl_2024 = gdp_brl_2024 * (1 - share_gdp_2024),
             population   = population * (1 - share_pop)),
    santarem_pre %>%
      mutate(geocode      = MOJUI_GEOCODE,
             gdp_brl      = gdp_brl * share_gdp,
             gdp_brl_2024 = gdp_brl_2024 * share_gdp_2024,
             population   = population * share_pop)
  ) %>%
  arrange(geocode, year) %>%
  group_by(geocode) %>%
  mutate(population = interp_linear(population, year)) %>%
  ungroup()

ibge_pib_gdp <- gdp_split %>%
  mutate(
    gdp_per_capita = if_else(!is.na(population) & population > 0,
                             gdp_brl / population, NA_real_),
    gdp_per_capita_log = log(if_else(gdp_per_capita > 0, gdp_per_capita,
                                     NA_real_)),
    gdp_per_capita_2024 = if_else(!is.na(population) & population > 0,
                                  gdp_brl_2024 / population, NA_real_),
    gdp_per_capita_2024_log = log(if_else(gdp_per_capita_2024 > 0,
                                          gdp_per_capita_2024, NA_real_))
  ) %>%
  select(geocode, year, population, gdp_brl, gdp_per_capita,
         gdp_per_capita_log, gdp_brl_2024, gdp_per_capita_2024,
         gdp_per_capita_2024_log)

# ---- Assert: output state ---------------------------------------------------
# ibge_pib.qmd Q3, Q4. Population is missing only before the first census
# that counts the municipality (task 1.6), Mojuí excepted; the split pair
# sums to Santarém's original values.
first_anchor <- pop_anchors %>%
  filter(!is.na(population)) %>%
  group_by(geocode) %>%
  summarise(first_census = min(year), .groups = "drop")
pop_missing_ok <- ibge_pib_gdp %>%
  left_join(first_anchor, by = "geocode") %>%
  mutate(ok = !is.na(population) |
           (year < first_census & geocode != MOJUI_GEOCODE)) %>%
  pull(ok)
pair_sum <- ibge_pib_gdp %>%
  filter(geocode %in% c(MOJUI_GEOCODE, SANTAREM_GEOCODE),
         year < MOJUI_SPLIT_YEAR) %>%
  group_by(year) %>%
  summarise(gdp_brl = sum(gdp_brl), population = sum(population),
            .groups = "drop") %>%
  inner_join(gdp %>% filter(geocode == SANTAREM_GEOCODE),
             by = "year", suffix = c("_pair", "_orig"))
stopifnot(
  nrow(ibge_pib_gdp) == N_MUNIC_PIB_EXPECTED * length(GDP_YEARS),
  !anyDuplicated(paste(ibge_pib_gdp$geocode, ibge_pib_gdp$year)),
  sum(ibge_pib_gdp$gdp_brl < 0, na.rm = TRUE) == N_GDP_NEGATIVE_EXPECTED,
  all(ibge_pib_gdp$population > 0, na.rm = TRUE),
  all(pop_missing_ok),
  all(abs(pair_sum$gdp_brl_pair - pair_sum$gdp_brl_orig) < 1e-6),
  all(abs(pair_sum$population_pair - pair_sum$population_orig) < 1e-6),
  all(is.na(ibge_pib_gdp$gdp_per_capita) ==
        (is.na(ibge_pib_gdp$gdp_brl) | is.na(ibge_pib_gdp$population))),
  is.character(ibge_pib_gdp$geocode),
  is.integer(ibge_pib_gdp$year),
  all(map_lgl(ibge_pib_gdp[-(1:2)], is.double))
)

# ---- Write ------------------------------------------------------------------
write_rds(ibge_pib_gdp, "_data/interim/ibge_pib_gdp_2002_2022.rds")
write_csv(ibge_pib_gdp, "_data/interim/ibge_pib_gdp_2002_2022.csv")
