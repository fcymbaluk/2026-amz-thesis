# 02-social-census.R
# PURPOSE   Build the annual municipal series that the censuses anchor: the
#           working-age population (PIA, 14+) for 1999-2024, and the
#           economically active population (PEA) and the informality share
#           of the occupied population for 2000-2022, all municipalities.
#
# INPUTS    _data/raw/ibge/data-censo2000-pia.xlsx   IBGE Censo 2000, Dados
#                                                     do Universo, population
#                                                     by age group
#           _data/raw/ibge/data-censo2010-pia.xlsx   IBGE SIDRA Tabela 1378
#           _data/raw/ibge/data-censo2022-pia.xlsx   IBGE SIDRA Tabela 9514
#           _data/raw/ibge/sidra/sidra_616_pea10_2000_2010_N6.json
#           _data/raw/ibge/sidra/sidra_616_pea10to14_2000_2010_N6.json
#           _data/raw/ibge/sidra/sidra_6580_labourforce14_2022_N6.json
#           _data/raw/ibge/sidra/sidra_2031_position_{2000,2010}_c*.json
#           _data/raw/ibge/sidra/sidra_10261_position_2022_N6_part*.json
#             IBGE SIDRA agregados API v3, queried 2026-10-06
#
# OUTPUTS   _data/interim/census_pia_1999_2024.rds / .csv
#           _data/interim/census_pea_informal_2000_2022.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd
#            §2.2 until the annex is written; evidence: _audit/census.qmd;
#            history: DECISIONS.md)
#   D-2026-10-05-f  Linear interpolation between the 2000, 2010 and 2022
#                   anchors (PIA, PEA, informality).
#   D-2026-10-07-a  PIA = resident population aged 14 and over, summed from
#                   the census age groups.
#   D-2026-10-07-b  PEA anchors: Tabela 616 (15+) for 2000 and 2010,
#                   Tabela 6580 labour force (14+) for 2022.
#   D-2026-10-07-c  Informality = share of the occupied population outside
#                   the formal categories (Tabelas 2031 and 10261), unrounded.
#   D-2026-10-07-d  Mojuí dos Campos 2000 and 2010 PEA allocated from
#                   Santarém by the 2022 share; informality copied.
#   D-2026-10-07-e  SIDRA "..." read as zero in the 2000 anchors.
#   D-2026-10-07-f  PIA extrapolated to 1999 and 2023-2024 with the slope
#                   of the adjacent census decade.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.6  Municipalities absent from the 2000 census carry pea = 0 and
#             informal = 0 in 2000 (missing coded as zero) and a
#             back-extrapolated PIA for 2000-2009; Mojuí dos Campos has
#             no PIA before 2022. Reproduced on purpose.
#   Task 1.7  Only the interpolated series are stored; the anchors are not.
#   Task 1.9  informal is stored on 0-100; rescaled to 0-1 in phase 2.
# RUN ORDER First of the 02 scripts; no pipeline inputs. Feeds
#           02-social-rais.R (pia) and 02-social-bf.R (pea).

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

source("_scripts/utils/read_sidra.R")
source("_scripts/utils/interpolate_census.R")

# ---- Constants --------------------------------------------------------------
CENSUS_YEARS   <- c(2000L, 2010L, 2022L)                      # D-2026-10-05-f
PIA_YEARS      <- 1999:2024                                   # D-2026-10-07-f
PEA_YEARS      <- 2000:2022
PIA_AGE_GROUPS_2000 <- c("14 anos", "15 a 19 anos",           # D-2026-10-07-a
                         paste(seq(20, 95, 5), "a", seq(24, 99, 5), "anos"),
                         "100 anos ou mais")
PIA_AGE_GROUPS_2010 <- c("14 anos", "15 a 17 anos", "18 ou 19 anos",
                         paste(seq(20, 55, 5), "a", seq(24, 59, 5), "anos"),
                         "60 a 69 anos", "70 anos ou mais")
PIA_AGE_GROUPS_2022 <- PIA_AGE_GROUPS_2000
PIA_HEADER_ROW_2010 <- 6L
PIA_HEADER_ROW_2022 <- 6L
SIDRA_DIR           <- "_data/raw/ibge/sidra"
SIDRA_ZERO_SYMBOLS  <- c("-", "...")       # D-2026-10-07-e; task 1.6
# Tabela 2031 category ids (2000, 2010): formal = employees with a signed
# card, military and statutory public servants, employers.
INFORMAL_TOTAL_ID_2031  <- "0"                                # D-2026-10-07-c
INFORMAL_FORMAL_IDS_2031 <- c("96167", "96168", "96170")
# Tabela 10261 category ids (2022): formal = private, domestic, public
# non-statutory and state-company employees with a signed card, military,
# statutory servants, employers with CNPJ, self-employed with CNPJ.
INFORMAL_TOTAL_ID_10261  <- "96165"
INFORMAL_FORMAL_IDS_10261 <- c("31722", "79367", "79369", "79370", "79372",
                               "79375", "45934", "45936")
INFORMAL_SCALE   <- 100                                        # task 1.9
MOJUI_GEOCODE    <- "1504752"                                 # D-2026-10-07-d
SANTAREM_GEOCODE <- "1506807"
N_MUNIC_2000_EXPECTED <- 5507L
N_MUNIC_2010_EXPECTED <- 5565L
N_MUNIC_2022_EXPECTED <- 5570L
N_SIDRA_2031_FILES    <- 9L   # per census year: total plus eight positions
N_SIDRA_10261_FILES   <- 6L

# ---- Read -------------------------------------------------------------------
pia_2000_raw <- read_excel("_data/raw/ibge/data-censo2000-pia.xlsx",
                           sheet = "Tabela", col_types = "text")
pia_2010_raw <- read_excel("_data/raw/ibge/data-censo2010-pia.xlsx",
                           sheet = "Tabela", col_types = "text",
                           col_names = FALSE, .name_repair = "minimal")
pia_2022_raw <- read_excel("_data/raw/ibge/data-censo2022-pia.xlsx",
                           sheet = "Tabela", col_types = "text",
                           col_names = FALSE, .name_repair = "minimal")

sidra_file <- function(pattern) {
  list.files(SIDRA_DIR, pattern = pattern, full.names = TRUE)
}
pea10_raw    <- read_sidra_json(sidra_file("^sidra_616_pea10_2000_2010"))
pea10to14_raw <- read_sidra_json(sidra_file("^sidra_616_pea10to14"))
lf14_raw     <- read_sidra_json(sidra_file("^sidra_6580_labourforce14"))
pos_2000_raw <- map_dfr(sidra_file("^sidra_2031_position_2000"),
                        read_sidra_json)
pos_2010_raw <- map_dfr(sidra_file("^sidra_2031_position_2010"),
                        read_sidra_json)
pos_2022_raw <- map_dfr(sidra_file("^sidra_10261_position_2022"),
                        read_sidra_json)

# ---- Assert: raw state ------------------------------------------------------
is_geocode <- function(x) !is.na(x) & str_detect(x, "^[0-9]{7}$")

# Workbooks: one block of age-group rows (2000, 2010) or columns (2022) per
# municipality, under a "Total" that is the population of all ages; "-" is
# the SIDRA zero (census.qmd Q1).
pia_2000_long <- pia_2000_raw %>%
  fill(Estado, `Cód.`, `Brasil e Município`) %>%
  filter(is_geocode(`Cód.`), `Grupo de idade` != "Total") %>%
  transmute(geocode = `Cód.`, age_group = `Grupo de idade`,
            value = sidra_value(Populacao, zero_symbols = "-"))

pia_2010_long <- pia_2010_raw %>%
  set_names(c("geocode", "municipality", "age_group", "condition",
              "value")) %>%
  fill(geocode, municipality) %>%
  filter(is_geocode(geocode), age_group != "Total") %>%
  transmute(geocode, age_group, condition,
            value = sidra_value(value, zero_symbols = "-"))

age_header_2022 <- unlist(pia_2022_raw[PIA_HEADER_ROW_2022, ])
pia_2022_long <- pia_2022_raw %>%
  set_names(c("geocode", "municipality", "declaration",
              age_header_2022[-(1:3)])) %>%
  filter(is_geocode(geocode)) %>%
  pivot_longer(-c(geocode, municipality, declaration),
               names_to = "age_group", values_to = "value_chr") %>%
  filter(age_group != "Total") %>%
  transmute(geocode, age_group, declaration,
            value = sidra_value(value_chr, zero_symbols = "-"))

stopifnot(
  identical(names(pia_2000_raw),
            c("Estado", "Cód.", "Brasil e Município", "Grupo de idade",
              "Populacao")),
  setequal(pia_2000_long$age_group, PIA_AGE_GROUPS_2000),
  n_distinct(pia_2000_long$geocode) == N_MUNIC_2000_EXPECTED,
  !anyNA(pia_2000_long$value),
  setequal(pia_2010_long$age_group, PIA_AGE_GROUPS_2010),
  all(pia_2010_long$condition == "Total"),
  n_distinct(pia_2010_long$geocode) == N_MUNIC_2010_EXPECTED,
  !anyNA(pia_2010_long$value),
  setequal(pia_2022_long$age_group, PIA_AGE_GROUPS_2022),
  all(pia_2022_long$declaration == "Total"),
  n_distinct(pia_2022_long$geocode) == N_MUNIC_2022_EXPECTED,
  !anyNA(pia_2022_long$value),
  !anyDuplicated(paste(pia_2000_long$geocode, pia_2000_long$age_group)),
  !anyDuplicated(paste(pia_2010_long$geocode, pia_2010_long$age_group)),
  !anyDuplicated(paste(pia_2022_long$geocode, pia_2022_long$age_group))
)

# SIDRA extracts: one series per municipality of the census grid, values
# digits or the symbols "-" (zero) and "..." (not available)
# (census.qmd Q2, Q3, Q5).
sidra_symbol_ok <- function(x) {
  all(str_detect(x, "^[0-9]+$") | x %in% c("-", "..."))
}
stopifnot(
  all(pea10_raw$categories == "Economicamente ativa | Total | Total | Total"),
  all(pea10to14_raw$categories ==
        "Economicamente ativa | Total | Total | 10 a 14 anos"),
  all(lf14_raw$categories == "Força de trabalho | Total | Total"),
  setequal(pea10_raw$year, c(2000L, 2010L)),
  setequal(pea10to14_raw$year, c(2000L, 2010L)),
  all(lf14_raw$year == 2022L),
  n_distinct(pea10_raw$geocode) == N_MUNIC_2010_EXPECTED,
  n_distinct(pea10to14_raw$geocode) == N_MUNIC_2010_EXPECTED,
  n_distinct(lf14_raw$geocode) == N_MUNIC_2022_EXPECTED,
  n_distinct(pos_2000_raw$category_id) == N_SIDRA_2031_FILES,
  n_distinct(pos_2010_raw$category_id) == N_SIDRA_2031_FILES,
  all(c(INFORMAL_TOTAL_ID_2031, INFORMAL_FORMAL_IDS_2031)
      %in% pos_2000_raw$category_id),
  all(c(INFORMAL_TOTAL_ID_2031, INFORMAL_FORMAL_IDS_2031)
      %in% pos_2010_raw$category_id),
  all(c(INFORMAL_TOTAL_ID_10261, INFORMAL_FORMAL_IDS_10261)
      %in% pos_2022_raw$category_id),
  all(pos_2000_raw$year == 2000L),
  all(pos_2010_raw$year == 2010L),
  all(pos_2022_raw$year == 2022L),
  all(pos_2022_raw$categories == paste(pos_2022_raw$category,
                                       "| Total | Total")),
  sidra_symbol_ok(c(pea10_raw$value, pea10to14_raw$value, lf14_raw$value,
                    pos_2000_raw$value, pos_2010_raw$value,
                    pos_2022_raw$value))
)

# ---- Transform --------------------------------------------------------------
# PIA anchors and the 1999-2024 series (D-2026-10-07-a, D-2026-10-07-f).
pia_anchors <- bind_rows(
  pia_2000_long %>% mutate(year = CENSUS_YEARS[1]),
  pia_2010_long %>% mutate(year = CENSUS_YEARS[2]),
  pia_2022_long %>% mutate(year = CENSUS_YEARS[3])
) %>%
  group_by(geocode, year) %>%
  summarise(pia = sum(value), .groups = "drop")

census_pia <- pia_anchors %>%
  group_by(geocode) %>%
  complete(year = PIA_YEARS) %>%
  arrange(geocode, year) %>%
  mutate(pia = interp_linear(pia, year),
         pia = extrapolate_linear(pia, year)) %>%
  ungroup() %>%
  select(geocode, year, pia)

# PEA anchors (D-2026-10-07-b, D-2026-10-07-e).
pea_anchors <- pea10_raw %>%
  select(geocode, year, pea10 = value) %>%
  inner_join(pea10to14_raw %>% select(geocode, year, pea10to14 = value),
             by = c("geocode", "year")) %>%
  transmute(geocode, year,
            pea = sidra_value(pea10, SIDRA_ZERO_SYMBOLS) -
              sidra_value(pea10to14, SIDRA_ZERO_SYMBOLS)) %>%
  bind_rows(lf14_raw %>%
              transmute(geocode, year,
                        pea = sidra_value(value, SIDRA_ZERO_SYMBOLS)))

# Informality anchors (D-2026-10-07-c, D-2026-10-07-e). A municipality with
# no occupied population in the extract reads 0, as inherited (task 1.6).
informal_anchor <- function(positions, total_id, formal_ids) {
  positions %>%
    mutate(value = sidra_value(value, SIDRA_ZERO_SYMBOLS)) %>%
    group_by(geocode, year) %>%
    summarise(
      occupied = sum(value[category_id == total_id]),
      formal   = sum(value[category_id %in% formal_ids]),
      .groups  = "drop"
    ) %>%
    transmute(geocode, year,
              informal = if_else(occupied > 0,
                                 INFORMAL_SCALE * (occupied - formal) /
                                   occupied,
                                 0))
}
informal_anchors <- bind_rows(
  informal_anchor(pos_2000_raw, INFORMAL_TOTAL_ID_2031,
                  INFORMAL_FORMAL_IDS_2031),
  informal_anchor(pos_2010_raw, INFORMAL_TOTAL_ID_2031,
                  INFORMAL_FORMAL_IDS_2031),
  informal_anchor(pos_2022_raw, INFORMAL_TOTAL_ID_10261,
                  INFORMAL_FORMAL_IDS_10261)
)

# Mojuí dos Campos was emancipated from Santarém in 2013 and has no 2000 or
# 2010 census row; its share of the 2022 labour force splits Santarém's
# earlier PEA, and Santarém's informality stands in for its own
# (D-2026-10-07-d).
pea_2022_pair <- pea_anchors %>%
  filter(year == CENSUS_YEARS[3],
         geocode %in% c(MOJUI_GEOCODE, SANTAREM_GEOCODE))
mojui_share <- pea_2022_pair$pea[pea_2022_pair$geocode == MOJUI_GEOCODE] /
  sum(pea_2022_pair$pea)
pea_santarem_pre <- pea_anchors %>%
  filter(geocode == SANTAREM_GEOCODE, year < CENSUS_YEARS[3])
pea_anchors_adj <- pea_anchors %>%
  filter(!(geocode == SANTAREM_GEOCODE & year < CENSUS_YEARS[3])) %>%
  bind_rows(
    pea_santarem_pre %>% mutate(pea = pea * (1 - mojui_share)),
    pea_santarem_pre %>% mutate(geocode = MOJUI_GEOCODE,
                                pea = pea * mojui_share)
  )
informal_anchors_adj <- informal_anchors %>%
  bind_rows(
    informal_anchors %>%
      filter(geocode == SANTAREM_GEOCODE, year < CENSUS_YEARS[3]) %>%
      mutate(geocode = MOJUI_GEOCODE)
  )

census_pea_informal <- pea_anchors_adj %>%
  full_join(informal_anchors_adj, by = c("geocode", "year")) %>%
  group_by(geocode) %>%
  complete(year = PEA_YEARS) %>%
  arrange(geocode, year) %>%
  mutate(pea      = interp_linear(pea, year),
         informal = interp_linear(informal, year)) %>%
  ungroup() %>%
  select(geocode, year, pea, informal)

# ---- Assert: output state ---------------------------------------------------
# census.qmd Q4, Q5, Q6.
n_munic_pia <- n_distinct(pia_anchors$geocode)
n_munic_pea <- n_distinct(pea_anchors_adj$geocode)
stopifnot(
  n_munic_pia == N_MUNIC_2022_EXPECTED,
  nrow(census_pia) == n_munic_pia * length(PIA_YEARS),
  !anyDuplicated(paste(census_pia$geocode, census_pia$year)),
  all(census_pia$pia >= 0, na.rm = TRUE),
  !anyNA(census_pia$pia[census_pia$geocode %in% pia_2010_long$geocode]),
  all(is.na(census_pia$pia[!census_pia$geocode %in% pia_2010_long$geocode &
                             census_pia$year < CENSUS_YEARS[3]])),
  n_munic_pea == N_MUNIC_2022_EXPECTED,
  nrow(census_pea_informal) == n_munic_pea * length(PEA_YEARS),
  !anyDuplicated(paste(census_pea_informal$geocode,
                       census_pea_informal$year)),
  all(census_pea_informal$pea >= 0, na.rm = TRUE),
  all(census_pea_informal$informal >= 0 &
        census_pea_informal$informal <= INFORMAL_SCALE, na.rm = TRUE),
  !anyNA(census_pea_informal %>%
           filter(geocode %in% pea10_raw$geocode) %>%
           select(pea, informal)),
  is.character(census_pia$geocode),
  is.integer(census_pia$year),
  is.double(census_pia$pia),
  is.character(census_pea_informal$geocode),
  is.integer(census_pea_informal$year),
  is.double(census_pea_informal$pea),
  is.double(census_pea_informal$informal)
)

# ---- Write ------------------------------------------------------------------
write_rds(census_pia, "_data/interim/census_pia_1999_2024.rds")
write_csv(census_pia, "_data/interim/census_pia_1999_2024.csv")
write_rds(census_pea_informal,
          "_data/interim/census_pea_informal_2000_2022.rds")
write_csv(census_pea_informal,
          "_data/interim/census_pea_informal_2000_2022.csv")
