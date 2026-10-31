# 09-descriptive-data.R
# Purpose   Descriptive sprint (rebuild/06-descriptive-sprint.md, section 4):
#           read the sprint's new sources (Atlas Brasil census vintages,
#           IBGE value added by activity, SICONFI expenditure and revenue),
#           put them on the panel's units (seven-digit geocode, 2024 reais,
#           per capita) and join them onto the phase 1 panel. Analysis
#           layer: pipeline 01-06 untouched; promoted under task 4.8.
# Inputs    _data/final/panel_amz_2000_2020_final.rds        script 06
#           _data/interim/ibge_pop_2000_2022.rds               script 03 (since phase 2, step 2.5)
#           _data/raw/atlas/censo_total_1991_2010.xlsx         Atlas Brasil
#           _data/raw/ibge/sidra/sidra_5938_va_*_N6.json       SIDRA 5938
#           _data/raw/siconfi/siconfi_*_amz_*.csv              Base dos Dados,
#                                                              2026-10-15
#           _data/aux/deflator_ipca_2024.csv
# Outputs   _data/interim/atlas_indicators_1991_2010.rds / .csv
#           _data/interim/ibge_pib_value_added_2002_2023.rds / .csv
#           _data/interim/siconfi_expenditure_munic_2000_2024.rds / .csv
#           _data/interim/siconfi_expenditure_uf_2013_2024.rds / .csv
#           _data/interim/siconfi_revenue_munic_2000_2024.rds / .csv
#           _data/final/panel_amz_2000_2020_descriptive.rds / .csv
# Status    2026-10-15, sprint day 3. Decisions D-2026-10-15-a onward in
#           DECISIONS.md; evidence in _audit/atlas.qmd, ibge_pib.qmd,
#           siconfi.qmd.
#           Phase 2R, session 2 (2026-10-30): the panel's columns carry the
#           crosswalk names (D-2026-10-30-a); the script selects the
#           chapter's sample, the 502 biome municipalities, right after the
#           read (rebuild/tasks.md 1.12, D-2026-10-30-g), and scripts 10-11
#           inherit it. The sprint's own columns (va_*, the Atlas anchors,
#           exp_*, rev_*, and state_abbreviation / population in the SICONFI
#           files) keep their sprint names until task 4.8. Outputs not
#           regenerated before stage 10.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

source("_scripts/utils/read_sidra.R")

# ---- Constants --------------------------------------------------------------
DEFLATOR_BASE     <- 2024L                                  # D-2026-10-05-c
ATLAS_FILE        <- "_data/raw/atlas/censo_total_1991_2010.xlsx"
ATLAS_SHEET       <- "MUN 91-00-10"
ATLAS_YEARS       <- c(1991L, 2000L, 2010L)
ATLAS_INCOME_BASE <- 2010L   # reais of 1 Aug 2010 (Siglas); D-2026-10-15-a
ATLAS_PCT_TO_SHARE <- 100                                   # D-2026-10-15-a
N_MUNIC_ATLAS_EXPECTED <- 5565L
SIDRA_DIR         <- "_data/raw/ibge/sidra"
VA_VARIABLES      <- c(va_total = "va_total", va_agro = "va_agro",
                       va_industry = "va_industria",
                       va_services = "va_servicos", va_public = "va_adm")
VA_YEARS          <- 2002:2023
BRL_PER_MIL       <- 1000                                   # D-2026-10-10-b
N_MUNIC_VA_EXPECTED <- 5570L
N_MUNIC_SAMPLE_EXPECTED <- 502L
# The chapter's sample (D-2026-10-30-g); geo_legal_amazon_dum is the
# robustness sample.
SAMPLE_REGION_COL <- "geo_biome_amazon_dum"
PANEL_YEARS       <- 2000:2020
SICONFI_DIR       <- "_data/raw/siconfi"
SICONFI_FILES     <- c(
  exp_munic_a = "siconfi_municipio_despesas_funcao_amz_2000_2003.csv",
  exp_munic_b = "siconfi_municipio_despesas_funcao_amz_2004_2024.csv",
  exp_uf      = "siconfi_uf_despesas_funcao_amz_2000_2024.csv",
  rev_munic   = "siconfi_municipio_receitas_orcamentarias_amz_2000_2024.csv"
)
SICONFI_UFS       <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
EXP_STAGE         <- "Despesas Empenhadas"                   # D-2026-10-15-c
REV_STAGE         <- "Receitas Brutas Realizadas"            # D-2026-10-15-d
EXP_MUNIC_YEARS   <- 2000:2024
EXP_FUNCTION_YEARS <- 2002:2024   # D-2026-10-15-c: 2000-01 old classification
EXP_UF_YEARS      <- 2013:2024    # the Base dos Dados state table starts 2013
EXP_BY_NAME_YEARS <- 2002:2003    # function rows carry no id before 2004
# Portaria 42/1999 functions, by the Base dos Dados id (3.FF.000) and name.
EXP_FUNCTIONS <- tibble(
  item = c("health", "education", "social_assistance", "social_security",
           "environment"),
  id   = c("3.10.000", "3.12.000", "3.08.000", "3.09.000", "3.18.000"),
  name = c("Saúde", "Educação", "Assistência Social", "Previdência Social",
           "Gestão Ambiental")
)
EXP_SOCIAL_ITEMS  <- c("health", "education", "social_assistance")
# One total row per municipality-year from 2004 (2010-12 repeat it under
# "Total Geral da Despesa por Função", which is not read); 2002-03 have no
# total row and the functions are summed (D-2026-10-15-c).
EXP_TOTAL_NAMES   <- c("Despesas por Função",
                       "Despesas (Exceto Intra-Orçamentárias)",
                       "Despesas (Exceto Intraorçamentárias)",
                       "Despesas Exceto Intraorçamentárias")
# Revenue items by period: FINBRA account names up to 2012, Tesouro account
# codes (column portaria) from 2013, parent level only, current revenues;
# federal fund-to-fund transfers gross of FUNDEB deductions (D-2026-10-15-d).
# Checked against Alcaraz (2024, Quadro 2): SUS, FUNDEB with other FNDE and
# FNAS are its conditioned transfers (TC), FPM is FPM, and own revenue (AP,
# current revenue except transfers) is derived below as rev_own_brl.
REV_TOTAL_2019    <- "TOTAL DAS RECEITAS (III) = (I + II)"
REV_RULES <- tribble(
  ~item,               ~from, ~to,   ~by,    ~key,
  "total",              2000L, 2012L, "name", "Rec Orçamentária",
  "total",              2013L, 2018L, "name", "Total Receitas",
  "total",              2019L, 2024L, "name", REV_TOTAL_2019,
  "current",            2000L, 2012L, "name", "Rec Correntes",
  "current",            2013L, 2017L, "code", "1.0.0.0.00.00.00",
  "current",            2018L, 2024L, "code", "1.0.0.0.00.0.0",
  "transfers_current",  2000L, 2012L, "name", "Rec Transf Correntes",
  "transfers_current",  2013L, 2017L, "code", "1.7.0.0.00.00.00",
  "transfers_current",  2018L, 2024L, "code", "1.7.0.0.00.0.0",
  "union_transfers",    2000L, 2012L, "name", "Transf Intergov da União",
  "union_transfers",    2013L, 2017L, "code", "1.7.2.1.00.00.00",
  "union_transfers",    2018L, 2024L, "code", "1.7.1.0.00.0.0",
  "tax",                2000L, 2012L, "name", "Rec Tributária",
  "tax",                2013L, 2017L, "code", "1.1.0.0.00.00.00",
  "tax",                2018L, 2024L, "code", "1.1.0.0.00.0.0",
  "fpm",                2000L, 2012L, "name", "Cota FPM",
  "fpm",                2013L, 2017L, "code", "1.7.2.1.01.02.00",
  "fpm",                2013L, 2017L, "code", "1.7.2.1.01.03.00",
  "fpm",                2013L, 2017L, "code", "1.7.2.1.01.04.00",
  "fpm",                2018L, 2021L, "code", "1.7.1.8.01.2.0",
  "fpm",                2018L, 2021L, "code", "1.7.1.8.01.3.0",
  "fpm",                2018L, 2021L, "code", "1.7.1.8.01.4.0",
  "fpm",                2022L, 2024L, "code", "1.7.1.1.51.0.0",
  "sus",                2000L, 2012L, "name", "SUS União",
  "sus",                2013L, 2017L, "code", "1.7.2.1.33.00.00",
  "sus",                2018L, 2021L, "code", "1.7.1.8.03.0.0",
  "sus",                2019L, 2021L, "code", "1.7.1.8.04.0.0",
  "sus",                2022L, 2024L, "code", "1.7.1.3.00.0.0",
  "fundeb",             2000L, 2001L, "name", "Fundef União",
  "fundeb",             2000L, 2001L, "name", "Fundef Estado",
  "fundeb",             2002L, 2006L, "name", "Transf Multigov FUNDEF",
  "fundeb",             2002L, 2006L, "name", "Transf Multigov FUNDEF Comp",
  "fundeb",             2007L, 2012L, "name", "Transf Multigov FUNDEB",
  "fundeb",             2007L, 2012L, "name", "Transf Multigov FUNDEB Comp",
  "fundeb",             2013L, 2017L, "code", "1.7.2.4.01.00.00",
  "fundeb",             2013L, 2017L, "code", "1.7.2.4.02.00.00",
  "fundeb",             2018L, 2021L, "code", "1.7.5.8.01.0.0",
  "fundeb",             2019L, 2021L, "code", "1.7.1.8.09.0.0",
  "fundeb",             2022L, 2024L, "code", "1.7.5.1.00.0.0",
  "fundeb",             2022L, 2024L, "code", "1.7.1.5.00.0.0",
  "fnde",               2002L, 2003L, "name", "FNDE",
  "fnde",               2002L, 2003L, "name", "Cota Salário Educação",
  "fnde",               2004L, 2012L, "name", "FNDE",
  "fnde",               2013L, 2017L, "code", "1.7.2.1.35.00.00",
  "fnde",               2013L, 2017L, "code", "1.7.2.1.01.30.00",
  "fnde",               2018L, 2021L, "code", "1.7.1.8.05.0.0",
  "fnde",               2022L, 2024L, "code", "1.7.1.4.00.0.0",
  "fnas",               2002L, 2012L, "name", "FNAS",
  "fnas",               2013L, 2017L, "code", "1.7.2.1.34.00.00",
  "fnas",               2018L, 2018L, "code", "1.7.1.8.04.0.0",
  "fnas",               2019L, 2021L, "code", "1.7.1.8.12.0.0",
  "fnas",               2022L, 2024L, "code", "1.7.1.6.00.0.0"
)
REV_ITEMS         <- unique(REV_RULES$item)
REV_TRANSFER_ITEMS <- c("fpm", "sus", "fundeb", "fnde", "fnas")
N_MUNIC_SICONFI_EXPECTED <- 808L
N_UF_EXPECTED     <- 9L

# ---- Read -------------------------------------------------------------------
panel <- read_rds("_data/final/panel_amz_2000_2020_final.rds") %>%
  filter(.data[[SAMPLE_REGION_COL]] == 1L)   # the sample; count asserted below
# Population from its own series since phase 2, step 2.5 (D-2026-10-23-e);
# the GDP file no longer carries it. Rerun due at stage 10.
population_all <- read_rds("_data/interim/ibge_pop_2000_2022.rds") %>%
  select(geocode, year, population)
deflator <- read_csv("_data/aux/deflator_ipca_2024.csv",
                     col_types = cols(year = col_integer(),
                                      ipca_index = col_double(),
                                      ipca_deflator = col_double())) %>%
  select(year, ipca_deflator)
atlas_raw <- read_excel(ATLAS_FILE, sheet = ATLAS_SHEET)
va_raw <- imap_dfr(VA_VARIABLES, function(token, name) {
  map_dfr(list.files(SIDRA_DIR,
                     pattern = paste0("^sidra_5938_", token, "_"),
                     full.names = TRUE),
          read_sidra_json) %>%
    mutate(variable = name)
})
read_siconfi <- function(key) {
  read_csv(file.path(SICONFI_DIR, SICONFI_FILES[[key]]),
           col_types = cols(.default = col_character(),
                            ano = col_integer(), valor = col_double()))
}
exp_munic_raw <- bind_rows(read_siconfi("exp_munic_a"),
                           read_siconfi("exp_munic_b"))
exp_uf_raw    <- read_siconfi("exp_uf")
rev_munic_raw <- read_siconfi("rev_munic")

# ---- Assert: raw state ------------------------------------------------------
is_geocode <- function(x) !is.na(x) & str_detect(x, "^[0-9]{7}$")

# atlas.qmd Q1, Q2, Q4, Q5: 5,565 municipalities in each vintage, no
# duplicate key, shares on 0-100 and nested, urban plus rural equals total.
stopifnot(
  all(c("Codmun7", "ANO", "PIND", "PMPOB", "PPOB", "GINI", "RDPC", "IDHM",
        "pesourb", "pesoRUR", "pesotot") %in% names(atlas_raw)),
  nrow(atlas_raw) == N_MUNIC_ATLAS_EXPECTED * length(ATLAS_YEARS),
  setequal(atlas_raw$ANO, ATLAS_YEARS),
  !anyDuplicated(paste(atlas_raw$Codmun7, atlas_raw$ANO)),
  all(atlas_raw$PIND <= atlas_raw$PMPOB & atlas_raw$PMPOB <= atlas_raw$PPOB),
  all(atlas_raw$PPOB <= 100),
  all(abs(atlas_raw$pesourb + atlas_raw$pesoRUR - atlas_raw$pesotot) <= 1),
  all(atlas_raw$pesotot > 0)
)
# ibge_pib.qmd Q1 holds for the value-added variables as for GDP: one series
# per municipality 2002-2023, "..." the only symbol, a few negatives served
# by IBGE (one in agro, 42 in industry).
stopifnot(
  all(is.na(va_raw$category_id)),
  all(is_geocode(va_raw$geocode)),
  !anyDuplicated(paste(va_raw$variable, va_raw$geocode, va_raw$year)),
  all(map_int(split(va_raw$geocode, va_raw$variable), n_distinct) ==
        N_MUNIC_VA_EXPECTED),
  all(VA_YEARS %in% va_raw$year),
  all(str_detect(va_raw$value, "^-?[0-9]+$") | va_raw$value == "..."),
  all(deflator$year[deflator$ipca_deflator == 1] == DEFLATOR_BASE),
  all(c(ATLAS_INCOME_BASE, VA_YEARS) %in% deflator$year),
  n_distinct(panel$geocode) == N_MUNIC_SAMPLE_EXPECTED,
  setequal(panel$year, PANEL_YEARS)
)
# siconfi.qmd Q1-Q3: 808 municipalities of the nine states, one stage,
# one total row per reporting municipality-year from 2004, function rows
# unique by id; the state table starts in 2013; revenue rows unique by
# account code from 2013 and by name before.
exp_totals <- exp_munic_raw %>% filter(conta %in% EXP_TOTAL_NAMES)
rev_keyed <- rev_munic_raw %>%
  mutate(key = if_else(ano >= 2013L, portaria, conta))
rev_total_names <- REV_RULES$key[REV_RULES$item == "total"]
stopifnot(
  all(exp_munic_raw$estagio_bd == EXP_STAGE),
  all(exp_munic_raw$sigla_uf %in% SICONFI_UFS),
  n_distinct(exp_munic_raw$id_municipio) == N_MUNIC_SICONFI_EXPECTED,
  all(is_geocode(exp_munic_raw$id_municipio)),
  setequal(exp_munic_raw$ano, EXP_MUNIC_YEARS),
  !anyDuplicated(paste(exp_totals$id_municipio, exp_totals$ano)),
  all(exp_totals$ano >= 2004L),
  !anyDuplicated(exp_munic_raw %>%
                   filter(id_conta_bd %in% EXP_FUNCTIONS$id) %>%
                   select(id_municipio, ano, id_conta_bd)),
  !anyDuplicated(exp_munic_raw %>%
                   filter(ano %in% EXP_BY_NAME_YEARS) %>%
                   select(id_municipio, ano, conta)),
  all(!is.na(exp_munic_raw$valor)),
  all(exp_uf_raw$sigla_uf %in% SICONFI_UFS),
  n_distinct(exp_uf_raw$sigla_uf) == N_UF_EXPECTED,
  setequal(exp_uf_raw$ano, EXP_UF_YEARS),
  all(rev_munic_raw$estagio_bd == REV_STAGE),
  n_distinct(rev_munic_raw$id_municipio) == N_MUNIC_SICONFI_EXPECTED,
  setequal(rev_munic_raw$ano, EXP_MUNIC_YEARS),
  all(!is.na(rev_keyed$key) | rev_keyed$ano >= 2013L),
  !anyDuplicated(rev_keyed %>%
                   filter(!is.na(key)) %>%
                   select(id_municipio, ano, key)),
  !anyDuplicated(rev_keyed %>%
                   filter(is.na(key), conta %in% rev_total_names) %>%
                   select(id_municipio, ano, conta)),
  all(!is.na(rev_munic_raw$valor))
)

# ---- Transform: Atlas -------------------------------------------------------
# D-2026-10-15-a: shares to 0-1; income per capita kept at its 2010 base and
# also expressed in 2024 reais with the annual IPCA factor of 2010.
income_factor <- deflator$ipca_deflator[deflator$year == ATLAS_INCOME_BASE]
atlas <- atlas_raw %>%
  transmute(
    geocode = as.character(Codmun7),
    census_year = as.integer(ANO),
    poverty_extreme_share = PIND / ATLAS_PCT_TO_SHARE,
    poverty_share = PMPOB / ATLAS_PCT_TO_SHARE,
    vulnerability_share = PPOB / ATLAS_PCT_TO_SHARE,
    gini = GINI,
    income_pc_brl_2010 = RDPC,
    income_pc_brl_2024 = RDPC * income_factor,
    idhm = IDHM,
    urban_share = pesourb / pesotot,
    atlas_population = pesotot
  ) %>%
  arrange(geocode, census_year)

# ---- Transform: value added -------------------------------------------------
# D-2026-10-15-b: land-based dependence is the agropecuária share of value
# added (agriculture, livestock, forestry and fishing; mining is inside
# industry and not separable at municipal level). Santarém's series before
# 2013 covers Mojuí dos Campos, which has none (no split, unlike script 03).
value_added <- va_raw %>%
  transmute(geocode, year, variable,
            value = sidra_value(value, zero_symbols = character(0)) *
              BRL_PER_MIL) %>%
  filter(year %in% VA_YEARS) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  left_join(deflator, by = "year") %>%
  mutate(
    across(all_of(names(VA_VARIABLES)), ~ .x * ipca_deflator,
           .names = "{.col}_brl_2024"),
    va_agro_share = va_agro / va_total,
    va_industry_share = va_industry / va_total,
    va_services_share = va_services / va_total,
    va_public_share = va_public / va_total
  ) %>%
  rename_with(~ paste0(.x, "_brl"), all_of(names(VA_VARIABLES))) %>%
  select(-ipca_deflator) %>%
  arrange(geocode, year)

# ---- Transform: SICONFI expenditure by function ------------------------------
# D-2026-10-15-c. Function rows by id from 2004 and by name in 2002-03; the
# total is the published total row from 2004 and the sum of the function
# rows in 2002-03. A function absent from a reporting municipality-year is
# a zero (SICONFI lists only non-zero functions), and a municipality-year
# without a total row is a reporting gap, kept as NA, never imputed.
exp_function_rows <- function(raw, keys) {
  raw %>%
    filter(year %in% EXP_FUNCTION_YEARS) %>%
    mutate(item = case_when(
      year %in% EXP_BY_NAME_YEARS ~
        EXP_FUNCTIONS$item[match(conta, EXP_FUNCTIONS$name)],
      TRUE ~ EXP_FUNCTIONS$item[match(id_conta_bd, EXP_FUNCTIONS$id)]
    )) %>%
    filter(!is.na(item)) %>%
    group_by(across(all_of(c(keys, "item")))) %>%
    summarise(brl = sum(valor), .groups = "drop")
}
exp_total_rows <- function(raw, keys) {
  published <- raw %>%
    filter(conta %in% EXP_TOTAL_NAMES) %>%
    select(all_of(keys), brl = valor)
  summed <- raw %>%
    filter(year %in% EXP_BY_NAME_YEARS) %>%
    group_by(across(all_of(keys))) %>%
    summarise(brl = sum(valor), .groups = "drop")
  bind_rows(published, summed) %>%
    mutate(item = "total")
}
exp_wide <- function(raw, keys) {
  totals <- exp_total_rows(raw, keys)
  bind_rows(totals, exp_function_rows(raw, keys)) %>%
    pivot_wider(names_from = item, values_from = brl,
                names_glue = "exp_{item}_brl") %>%
    semi_join(totals, by = keys) %>%
    mutate(across(all_of(paste0("exp_", EXP_FUNCTIONS$item, "_brl")),
                  ~ if_else(is.na(.x) & year %in% EXP_FUNCTION_YEARS,
                            0, .x)),
           exp_social_brl = exp_health_brl + exp_education_brl +
             exp_social_assistance_brl) %>%
    left_join(deflator, by = "year") %>%
    mutate(across(ends_with("_brl"), ~ .x * ipca_deflator,
                  .names = "{.col}_2024")) %>%
    select(-ipca_deflator)
}
per_capita <- function(df, cols) {
  df %>%
    mutate(across(all_of(cols),
                  ~ if_else(!is.na(population) & population > 0,
                            .x / population, NA_real_),
                  .names = "{.col}_pc")) %>%
    rename_with(~ str_replace(.x, "_brl_2024_pc$", "_pc_brl_2024"),
                ends_with("_brl_2024_pc"))
}
exp_cols_2024 <- paste0("exp_", c("total", EXP_FUNCTIONS$item, "social"),
                        "_brl_2024")

siconfi_expenditure_munic <- exp_munic_raw %>%
  rename(geocode = id_municipio, state_abbreviation = sigla_uf,
         year = ano) %>%
  exp_wide(keys = c("geocode", "state_abbreviation", "year")) %>%
  left_join(population_all, by = c("geocode", "year")) %>%
  per_capita(exp_cols_2024) %>%
  arrange(geocode, year)

population_uf <- population_all %>%
  inner_join(exp_munic_raw %>% distinct(geocode = id_municipio,
                                        state_abbreviation = sigla_uf),
             by = "geocode") %>%
  group_by(state_abbreviation, year) %>%
  summarise(population = sum(population), .groups = "drop")

siconfi_expenditure_uf <- exp_uf_raw %>%
  filter(estagio_bd == EXP_STAGE) %>%
  rename(state_abbreviation = sigla_uf, year = ano) %>%
  exp_wide(keys = c("state_abbreviation", "year")) %>%
  left_join(population_uf, by = c("state_abbreviation", "year")) %>%
  per_capita(exp_cols_2024) %>%
  arrange(state_abbreviation, year)

# ---- Transform: SICONFI municipal revenue -----------------------------------
# D-2026-10-15-d. Items are read at the parent level of each period's
# account plan, so children never add to their parent; an item absent from
# a municipality-year that reports a total is a zero, the rest is NA.
rev_rules_long <- REV_RULES %>%
  mutate(year = map2(from, to, seq)) %>%
  select(-from, -to) %>%
  unnest(year) %>%
  mutate(year = as.integer(year))
rev_items <- rev_munic_raw %>%
  rename(geocode = id_municipio, state_abbreviation = sigla_uf,
         year = ano) %>%
  mutate(by = if_else(year >= 2013L & !is.na(portaria), "code", "name"),
         key = if_else(by == "code", portaria, conta)) %>%
  inner_join(rev_rules_long, by = c("year", "by", "key"),
             relationship = "many-to-many") %>%
  group_by(geocode, state_abbreviation, year, item) %>%
  summarise(brl = sum(valor), .groups = "drop")
rev_cols_2024 <- paste0("rev_", c(REV_ITEMS, "own"), "_brl_2024")

siconfi_revenue_munic <- rev_items %>%
  pivot_wider(names_from = item, values_from = brl,
              names_glue = "rev_{item}_brl") %>%
  filter(!is.na(rev_total_brl)) %>%
  mutate(across(all_of(paste0("rev_", REV_ITEMS, "_brl")),
                ~ if_else(is.na(.x), 0, .x))) %>%
  # No FNDE or FNAS line exists in the 2000-01 FINBRA layout.
  mutate(across(c(rev_fnde_brl, rev_fnas_brl),
                ~ if_else(year < 2002L, NA_real_, .x))) %>%
  # Own revenue (Alcaraz 2024, AP): current revenue except current
  # transfers; negative in a few cells where the published current total
  # is net of deductions and the transfer lines are gross (shown, not fixed).
  mutate(rev_own_brl = rev_current_brl - rev_transfers_current_brl) %>%
  left_join(deflator, by = "year") %>%
  mutate(across(ends_with("_brl"), ~ .x * ipca_deflator,
                .names = "{.col}_2024")) %>%
  select(-ipca_deflator) %>%
  left_join(population_all, by = c("geocode", "year")) %>%
  per_capita(rev_cols_2024) %>%
  arrange(geocode, year)

# ---- Transform: descriptive panel ------------------------------------------
atlas_wide <- atlas %>%
  filter(census_year %in% c(2000L, 2010L)) %>%
  pivot_wider(
    id_cols = geocode,
    names_from = census_year,
    values_from = -c(geocode, census_year),
    names_glue = "{.value}_c{census_year}"
  )

siconfi_munic_cols <- siconfi_expenditure_munic %>%
  select(geocode, year, starts_with("exp_")) %>%
  full_join(siconfi_revenue_munic %>%
              select(geocode, year, starts_with("rev_")),
            by = c("geocode", "year"))

panel_descriptive <- panel %>%
  left_join(value_added, by = c("geocode", "year")) %>%
  left_join(atlas_wide, by = "geocode") %>%
  left_join(siconfi_munic_cols, by = c("geocode", "year")) %>%
  arrange(geocode, year)

# ---- Assert: output state ---------------------------------------------------
# atlas.qmd Q2: every sample municipality in the three vintages.
va_sum_ok <- value_added %>%
  filter(!is.na(va_total_brl)) %>%
  mutate(ok = abs(va_agro_brl + va_industry_brl + va_services_brl +
                    va_public_brl - va_total_brl) <= 4 * BRL_PER_MIL) %>%
  pull(ok)
stopifnot(
  nrow(atlas) == N_MUNIC_ATLAS_EXPECTED * length(ATLAS_YEARS),
  all(panel$geocode %in% atlas$geocode),
  all(atlas$poverty_share >= 0 & atlas$poverty_share <= 1),
  all(atlas$urban_share >= 0 & atlas$urban_share <= 1),
  nrow(value_added) == N_MUNIC_VA_EXPECTED * length(VA_YEARS),
  !anyDuplicated(paste(value_added$geocode, value_added$year)),
  all(va_sum_ok),
  nrow(panel_descriptive) == nrow(panel),
  !anyDuplicated(paste(panel_descriptive$geocode, panel_descriptive$year)),
  identical(panel_descriptive$geocode, panel$geocode),
  sum(is.na(panel_descriptive$poverty_share_c2010)) == 0,
  is.character(atlas$geocode), is.integer(atlas$census_year),
  is.character(value_added$geocode), is.integer(value_added$year),
  all(map_lgl(atlas[-(1:2)], is.double)),
  all(map_lgl(value_added[-(1:2)], is.double))
)
# siconfi.qmd Q4-Q6: function sums never exceed the total by more than
# rounding from 2004; the nine states' series are complete 2013-2024; FPM
# is non-negative and never above the current transfers it belongs to
# (current revenue is net of FUNDEB deductions, transfers are gross).
exp_sum_ok <- siconfi_expenditure_munic %>%
  filter(year >= 2004L) %>%
  mutate(fun = exp_health_brl + exp_education_brl +
           exp_social_assistance_brl + exp_social_security_brl +
           exp_environment_brl,
         ok = fun <= exp_total_brl * 1.001 + 1) %>%
  pull(ok)
stopifnot(
  !anyDuplicated(paste(siconfi_expenditure_munic$geocode,
                       siconfi_expenditure_munic$year)),
  all(!is.na(siconfi_expenditure_munic$exp_total_brl)),
  all(exp_sum_ok),
  all(is.na(siconfi_expenditure_munic$exp_health_brl) ==
        (siconfi_expenditure_munic$year < min(EXP_FUNCTION_YEARS))),
  nrow(siconfi_expenditure_uf) == N_UF_EXPECTED * length(EXP_UF_YEARS),
  all(!is.na(siconfi_expenditure_uf$exp_health_brl)),
  !anyDuplicated(paste(siconfi_revenue_munic$geocode,
                       siconfi_revenue_munic$year)),
  all(siconfi_revenue_munic$rev_fpm_brl >= 0),
  all(siconfi_revenue_munic$rev_fpm_brl <=
        siconfi_revenue_munic$rev_transfers_current_brl * 1.001 + 1),
  all(is.na(siconfi_revenue_munic$rev_fnas_brl) ==
        (siconfi_revenue_munic$year < 2002L)),
  is.character(siconfi_expenditure_munic$geocode),
  is.integer(siconfi_expenditure_munic$year),
  is.integer(siconfi_expenditure_uf$year),
  is.integer(siconfi_revenue_munic$year),
  all(map_lgl(select(siconfi_expenditure_munic, -geocode,
                     -state_abbreviation, -year), is.double)),
  all(map_lgl(select(siconfi_revenue_munic, -geocode,
                     -state_abbreviation, -year), is.double))
)

# ---- Write ------------------------------------------------------------------
write_rds(atlas, "_data/interim/atlas_indicators_1991_2010.rds")
write_csv(atlas, "_data/interim/atlas_indicators_1991_2010.csv")
write_rds(value_added, "_data/interim/ibge_pib_value_added_2002_2023.rds")
write_csv(value_added, "_data/interim/ibge_pib_value_added_2002_2023.csv")
write_rds(siconfi_expenditure_munic,
          "_data/interim/siconfi_expenditure_munic_2000_2024.rds")
write_csv(siconfi_expenditure_munic,
          "_data/interim/siconfi_expenditure_munic_2000_2024.csv")
write_rds(siconfi_expenditure_uf,
          "_data/interim/siconfi_expenditure_uf_2013_2024.rds")
write_csv(siconfi_expenditure_uf,
          "_data/interim/siconfi_expenditure_uf_2013_2024.csv")
write_rds(siconfi_revenue_munic,
          "_data/interim/siconfi_revenue_munic_2000_2024.rds")
write_csv(siconfi_revenue_munic,
          "_data/interim/siconfi_revenue_munic_2000_2024.csv")
write_rds(panel_descriptive,
          "_data/final/panel_amz_2000_2020_descriptive.rds")
write_csv(panel_descriptive,
          "_data/final/panel_amz_2000_2020_descriptive.csv")
