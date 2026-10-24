# 03-controls-gdp.R
# PURPOSE   Build the resident population series for 2000-2022, all
#           municipalities (census anchors interpolated; IBGE's official
#           annual estimates for municipalities installed after the 2000
#           census, from installation to 2009), and the municipality-year
#           GDP panel for 2002-2022: current and 2024 reais, per capita and
#           its log, on that population.
#
# INPUTS    _data/raw/ibge/sidra/sidra_5938_pib_*_N6.json
#             IBGE PIB dos Municípios, SIDRA Tabela 5938, variable 37
#             (PIB a preços correntes, mil reais), API v3, 2026-10-09
#           _data/raw/ibge/Ibge_pop_1970-80-91-00-10.xlsx
#             IBGE censuses, resident population 1970-2010 (SIDRA export)
#           _data/raw/ibge/Ibge_pop_2022 (Censo - Sidra).xlsx
#             IBGE Censo 2022, SIDRA Tabela 4709, resident population
#           _data/raw/ibge/sidra/sidra_6579_pop_estimate_*_N6.json
#             IBGE annual population estimates, SIDRA Tabela 6579,
#             2001-2026 less the count and census years, API v3,
#             2026-10-09
#           _data/aux/munic_post2000.csv
#             Municipalities installed after the 2000 census: installation
#             year and parents (IBGE municipal register, 2026-10-23)
#           _data/aux/deflator_ipca_2024.csv
#
# OUTPUTS   _data/interim/ibge_pop_2000_2022.rds / .csv
#           _data/interim/ibge_pib_gdp_2002_2022.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/ibge_pib.qmd;
#            history: DECISIONS.md)
#   D-2026-10-05-c  Monetary values deflated to 2024 BRL with the IPCA.
#   D-2026-10-05-f  Linear interpolation between the census anchors.
#   D-2026-10-10-b  GDP read from the SIDRA API in integer thousands of
#                   reais.
#   D-2026-10-10-j  Task 1.10: typed outputs.
#   D-2026-10-23-e  Population stored 2000-2022 as its own series with a
#                   pop_source column (phase 2, step 2.5); a municipality
#                   absent from the 2000 census is NA before installation
#                   and carries the official estimate from installation to
#                   2009, the 2007 count year interpolated between the
#                   adjacent estimates.
#   D-2026-10-23-f  The Santarém and Mojuí dos Campos split of
#                   D-2026-10-10-e retired (phase 2, step 2.5): Santarém
#                   as published, Mojuí NA before 2013.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.7  Only the interpolated population is stored; the anchors are
#             not.
# RUN ORDER Independent of the other 03 scripts; no pipeline inputs.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

source("_scripts/utils/read_sidra.R")
source("_scripts/utils/interpolate_census.R")

# ---- Constants --------------------------------------------------------------
# D-2026-10-10-k (closed at step 2.5): the GDP series starts in 2002 and is
# capped at the last census; the population series runs 2000-2022.
GDP_YEARS      <- 2002:2022
CENSUS_YEARS   <- c(2000L, 2010L, 2022L)                      # D-2026-10-05-f
POP_YEARS      <- 2000:2022                                   # D-2026-10-23-e
DEFLATOR_BASE  <- 2024L                                       # D-2026-10-05-c
BRL_PER_MIL    <- 1000                                        # D-2026-10-10-b
SIDRA_DIR      <- "_data/raw/ibge/sidra"
POP_FILE_2000_2010 <- "_data/raw/ibge/Ibge_pop_1970-80-91-00-10.xlsx"
POP_FILE_2022      <- "_data/raw/ibge/Ibge_pop_2022 (Censo - Sidra).xlsx"
POP_SHEET          <- "Tabela"
POST2000_LOOKUP    <- "_data/aux/munic_post2000.csv"          # D-2026-10-23-f
# Tabela 6579 serves no value for the 2007 population count and the census
# years (ibge_pib.qmd Q4).
EST_YEARS_SERVED   <- c(2001:2006, 2008:2009, 2011:2021, 2024:2026)
POP_COUNT_YEAR     <- 2007L         # population count, interpolated
POP_SOURCES        <- c("census", "census_interp", "ibge_estimate",
                        "estimate_interp")                    # D-2026-10-23-e
N_MUNIC_PIB_EXPECTED      <- 5570L
N_MUNIC_POP_2010_EXPECTED <- 5566L  # 2010 grid plus Cococi (CE), extinct
N_MUNIC_POP_2022_EXPECTED <- 5570L
N_GDP_NEGATIVE_EXPECTED   <- 1L     # Guamaré (RN) 2012, as served by IBGE
# ibge_pib.qmd Q7: the nine Mato Grosso municipalities of the lookup carry
# an interpolated 2007; Mojuí dos Campos (installed 2013) has no gap.
N_EST_INTERP_EXPECTED     <- 9L

# ---- Read -------------------------------------------------------------------
pib_raw <- map_dfr(list.files(SIDRA_DIR, pattern = "^sidra_5938_pib_",
                              full.names = TRUE),
                   read_sidra_json)
est_raw <- map_dfr(list.files(SIDRA_DIR, pattern = "^sidra_6579_pop_estimate_",
                              full.names = TRUE),
                   read_sidra_json)
pop_2000_2010_raw <- read_excel(POP_FILE_2000_2010, sheet = POP_SHEET,
                                col_types = "text")
pop_2022_raw <- read_excel(POP_FILE_2022, sheet = POP_SHEET,
                           col_types = "text", col_names = FALSE,
                           .name_repair = "minimal")
post2000 <- read_csv(POST2000_LOOKUP,
                     col_types = cols(.default = col_character(),
                                      install_year = col_integer()))
deflator <- read_csv("_data/aux/deflator_ipca_2024.csv",
                     col_types = cols(year = col_integer(),
                                      ipca_index = col_double(),
                                      ipca_deflator = col_double())) %>%
  select(year, ipca_deflator)

# ---- Assert: raw state ------------------------------------------------------
is_geocode <- function(x) !is.na(x) & str_detect(x, "^[0-9]{7}$")

# ibge_pib.qmd Q1: one GDP series per municipality 2002-2023, the only
# symbol "..." (not available); one negative value, outside the sample.
# Q4: the estimates are served without a classification for the years in
# EST_YEARS_SERVED, "..." where a municipality did not yet exist.
stopifnot(
  all(is.na(pib_raw$category_id)),
  n_distinct(pib_raw$geocode) == N_MUNIC_PIB_EXPECTED,
  all(is_geocode(pib_raw$geocode)),
  !anyDuplicated(paste(pib_raw$geocode, pib_raw$year)),
  all(GDP_YEARS %in% pib_raw$year),
  all(str_detect(pib_raw$value, "^-?[0-9]+$") | pib_raw$value == "..."),
  sum(str_detect(pib_raw$value, "^-")) == N_GDP_NEGATIVE_EXPECTED,
  all(is.na(est_raw$category_id)),
  all(is_geocode(est_raw$geocode)),
  !anyDuplicated(paste(est_raw$geocode, est_raw$year)),
  setequal(est_raw$year, EST_YEARS_SERVED),
  all(str_detect(est_raw$value, "^[0-9]+$") | est_raw$value == "..."),
  identical(names(pop_2000_2010_raw),
            c("munic_id", "munic", "1970", "1980", "1991", "2000", "2010")),
  all(is_geocode(pop_2000_2010_raw$munic_id)),
  !anyDuplicated(pop_2000_2010_raw$munic_id),
  nrow(pop_2000_2010_raw) == N_MUNIC_POP_2010_EXPECTED,
  all(is_geocode(post2000$geocode)),
  !anyDuplicated(post2000$geocode),
  all(post2000$install_year > CENSUS_YEARS[1]),
  all(deflator$year[deflator$ipca_deflator == 1] == DEFLATOR_BASE),
  all(GDP_YEARS %in% deflator$year)
)

# ibge_pib.qmd Q2: the 2022 workbook has four header rows, a Brasil row,
# one row per municipality and a source footer; "..." is the only symbol
# in the census columns. Every lookup municipality has no 2000 anchor.
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
        pop_anchors$year < CENSUS_YEARS[3]),
  all(post2000$geocode %in% pop_anchors$geocode),
  all(is.na(pop_anchors$population[pop_anchors$year == CENSUS_YEARS[1] &
                                     pop_anchors$geocode %in%
                                       post2000$geocode]))
)

# ---- Transform --------------------------------------------------------------
# Census anchors interpolated (D-2026-10-05-f); NA before the first census
# that counts the municipality. Cococi (CE), extinct, has no anchor and
# leaves the series (ibge_pib.qmd Q2).
population <- pop_anchors %>%
  group_by(geocode) %>%
  filter(any(!is.na(population))) %>%
  complete(year = POP_YEARS) %>%
  arrange(geocode, year) %>%
  mutate(first_census = min(year[!is.na(population)]),
         pop_source   = case_when(!is.na(population) ~ "census",
                                  year > first_census ~ "census_interp",
                                  TRUE ~ NA_character_),
         population   = interp_linear(population, year)) %>%
  ungroup()

# Official estimates from the installation year to the year before the
# first census that counts the municipality; the 2007 count year, not
# served, is interpolated between the adjacent estimates (D-2026-10-23-e).
# Estimates IBGE published before installation (the 2004 value of the two
# municipalities installed in 2005) are not used.
estimates <- est_raw %>%
  transmute(geocode, year,
            estimate = sidra_value(value, zero_symbols = character(0)))

population <- population %>%
  left_join(post2000 %>% select(geocode, install_year), by = "geocode") %>%
  left_join(estimates, by = c("geocode", "year")) %>%
  mutate(use_est    = !is.na(install_year) & year >= install_year &
           year < first_census,
         population = if_else(use_est, estimate, population),
         pop_source = if_else(use_est, "ibge_estimate", pop_source)) %>%
  group_by(geocode) %>%
  mutate(est_gap    = use_est & is.na(population),
         population = if_else(est_gap, interp_linear(population, year),
                              population),
         pop_source = if_else(est_gap, "estimate_interp", pop_source)) %>%
  ungroup()

ibge_pop <- population %>%
  select(geocode, year, population, pop_source)

gdp <- pib_raw %>%
  transmute(geocode, year,
            gdp_brl = sidra_value(value, zero_symbols = character(0)) *
              BRL_PER_MIL) %>%
  filter(year %in% GDP_YEARS) %>%
  left_join(deflator, by = "year") %>%
  mutate(gdp_brl_2024 = gdp_brl * ipca_deflator) %>%
  select(-ipca_deflator) %>%
  left_join(ibge_pop %>% select(geocode, year, population),
            by = c("geocode", "year"))

ibge_pib_gdp <- gdp %>%
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
  select(geocode, year, gdp_brl, gdp_per_capita, gdp_per_capita_log,
         gdp_brl_2024, gdp_per_capita_2024, gdp_per_capita_2024_log)

# ---- Assert: output state ---------------------------------------------------
# ibge_pib.qmd Q4, Q6, Q7. Population is complete for every municipality
# counted in 2000; a lookup municipality is NA before its installation
# year and complete from it; every other municipality absent from a census
# is NA before the first census that counts it. The GDP of a lookup
# municipality is likewise NA before installation.
first_anchor <- pop_anchors %>%
  filter(!is.na(population)) %>%
  group_by(geocode) %>%
  summarise(first_census = min(year), .groups = "drop")
pop_rule <- ibge_pop %>%
  left_join(first_anchor, by = "geocode") %>%
  left_join(post2000 %>% select(geocode, install_year), by = "geocode") %>%
  mutate(expect_na = if_else(is.na(install_year), year < first_census,
                             year < install_year))
gdp_rule <- ibge_pib_gdp %>%
  inner_join(post2000 %>% select(geocode, install_year), by = "geocode")
stopifnot(
  nrow(ibge_pop) == N_MUNIC_POP_2022_EXPECTED * length(POP_YEARS),
  !anyDuplicated(paste(ibge_pop$geocode, ibge_pop$year)),
  all(ibge_pop$population > 0, na.rm = TRUE),
  all(is.na(pop_rule$population) == pop_rule$expect_na),
  all(is.na(ibge_pop$pop_source) == is.na(ibge_pop$population)),
  all(ibge_pop$pop_source %in% POP_SOURCES | is.na(ibge_pop$pop_source)),
  all(ibge_pop$pop_source[ibge_pop$year %in% CENSUS_YEARS &
                            !is.na(ibge_pop$population)] == "census"),
  sum(ibge_pop$pop_source == "estimate_interp", na.rm = TRUE) ==
    N_EST_INTERP_EXPECTED,
  all(ibge_pop$year[which(ibge_pop$pop_source == "estimate_interp")] ==
        POP_COUNT_YEAR),
  nrow(ibge_pib_gdp) == N_MUNIC_PIB_EXPECTED * length(GDP_YEARS),
  !anyDuplicated(paste(ibge_pib_gdp$geocode, ibge_pib_gdp$year)),
  sum(ibge_pib_gdp$gdp_brl < 0, na.rm = TRUE) == N_GDP_NEGATIVE_EXPECTED,
  all(is.na(gdp_rule$gdp_brl[gdp_rule$year < gdp_rule$install_year])),
  !anyNA(gdp_rule$gdp_brl[gdp_rule$year >= gdp_rule$install_year]),
  all(is.na(ibge_pib_gdp$gdp_per_capita) ==
        (is.na(ibge_pib_gdp$gdp_brl) |
           is.na(gdp$population) | gdp$population <= 0)),
  is.character(ibge_pop$geocode),
  is.integer(ibge_pop$year),
  is.double(ibge_pop$population),
  is.character(ibge_pop$pop_source),
  is.character(ibge_pib_gdp$geocode),
  is.integer(ibge_pib_gdp$year),
  all(map_lgl(ibge_pib_gdp[-(1:2)], is.double))
)

# ---- Write ------------------------------------------------------------------
write_rds(ibge_pop, "_data/interim/ibge_pop_2000_2022.rds")
write_csv(ibge_pop, "_data/interim/ibge_pop_2000_2022.csv")
write_rds(ibge_pib_gdp, "_data/interim/ibge_pib_gdp_2002_2022.rds")
write_csv(ibge_pib_gdp, "_data/interim/ibge_pib_gdp_2002_2022.csv")
