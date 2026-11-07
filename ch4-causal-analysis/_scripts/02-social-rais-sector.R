# 02-social-rais-sector.R
# PURPOSE   Build the sectoral composition of formal employment from the
#           RAIS bond-level snapshot (task 3.C of
#           rebuild/upstream-reform-rais.md): bond counts and shares by
#           municipality, year and CNAE section, whole sector and private
#           sector, under three classifications (the harmonized groups
#           across the 2006 break, the native CNAE 1.0 sections, the
#           native CNAE 2.0 sections), 1999-2024, nine Legal Amazon states;
#           and the national reference series (all Brazil, Brazil without
#           the nine states, the nine states) from the state-level counts
#           extracted on BigQuery.
#
# INPUTS    _data/raw/rais/rais_vinculos_v2/*.parquet   Own BigQuery
#                                                extraction from Base dos
#                                                Dados RAIS (Query 1 v2,
#                                                2026-11-04, D-2026-11-04-a)
#           _data/raw/rais/rais_sector_national.csv    Own BigQuery
#                                                extraction, all Brazil by
#                                                year, state, CNAE 1.0 and
#                                                2.0 class and public flag
#                                                (2026-11-06)
#           _data/raw/cnae/cnae_1.csv                  Base dos Dados
#           _data/raw/cnae/cnae_2.csv                  directories
#                                                br_bd_diretorios_brasil
#
# OUTPUTS   _data/interim/rais_sector_1999_2024.rds / .csv
#           _data/interim/rais_sector_national_1999_2024.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, labor-market section, to be written;
#            evidence: _audit/rais.qmd Q6, Q10-Q11; history: DECISIONS.md)
#   D-2026-11-04-a  The bond-level snapshot is the source for every RAIS
#                   aggregate; 2023-2024 follow the current Base dos Dados
#                   version.
#   D-2026-11-05-a  Public sector = natureza jurídica 1xxx; an empty code
#                   is private.
#   D-2026-11-05-c  Reliability floor of 50 bonds on the count of the
#                   sector the statistic describes: the municipal shares
#                   are NA below it, the counts are kept.
#   D-2026-11-06-a  Sections by division through the Base dos Dados
#                   directories; thirteen harmonized groups (twelve exact
#                   section matches and one residual), each bond classified
#                   by its CNAE 2.0 code where valid, else by its CNAE 1.0
#                   code; placeholder and null codes to "unk".
#   D-2026-11-06-b  Long layout, one row per unit, year, classification
#                   and section, the grid complete with zeros; the
#                   national series by state from one BigQuery pass with
#                   the Query 1 filter, three scopes, no floor.
# RUN ORDER Independent of the other 02 scripts. Feeds the labor-market
#           dataset of the RAIS track (memo of 2026-11-05).

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(DBI)

source("_scripts/utils/rais_snapshot.R")

# ---- Constants --------------------------------------------------------------
RAIS_YEARS                  <- 1999:2024
CNAE2_FROM                  <- 2006L                       # D-2026-11-06-a
RAIS_FLOOR_N_BONDS          <- 50L                         # D-2026-11-05-c
LEGAL_AMAZON_STATES         <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO",
                                 "RR", "TO")
CLASSIFICATIONS             <- c("harmonized", "cnae1", "cnae2")
SCOPES                      <- c("brazil", "brazil_excl_la", "la_states")
CNAE_DIR_FILES              <- c(`1` = "_data/raw/cnae/cnae_1.csv",
                                 `2` = "_data/raw/cnae/cnae_2.csv")
NATIONAL_FILE               <- "_data/raw/rais/rais_sector_national.csv"
N_RAIS_SHARDS               <- 252L                        # D-2026-11-04-a
N_RAIS_BONDS_EXPECTED       <- 90156865L                   # D-2026-11-04-a
N_RAIS_MUNIC_EXPECTED       <- 809L
N_RAIS_MUNIC_YEARS_EXPECTED <- 20870L
N_CNAE1_CLASSES             <- 581L
N_CNAE1_SECTIONS            <- 17L
N_CNAE2_SUBCLASSES          <- 1356L
N_CNAE2_SECTIONS            <- 21L
N_NATIONAL_ROWS_EXPECTED    <- 433077L                     # D-2026-11-06-b
N_NATIONAL_BONDS_EXPECTED   <- 1058183357
N_NATIONAL_STATES_EXPECTED  <- 28L   # 27 units plus IGNORADO
# Bonds whose code falls in no section (_audit/rais.qmd Q10): the ten null
# CNAE 1.0 codes of 1999; the CNAE 1.0 placeholders 00999 (2022) and 9999
# (2023-2024); the CNAE 2.0 placeholder 00000 (2006-2008).
N_UNK_HARMONIZED_EXPECTED   <- 10
N_UNK_CNAE1_EXPECTED        <- 900421
N_UNK_CNAE2_EXPECTED        <- 137715

# ---- Read -------------------------------------------------------------------
dir1 <- read_csv(CNAE_DIR_FILES[["1"]], col_types = cols(.default = col_character()))
dir2 <- read_csv(CNAE_DIR_FILES[["2"]], col_types = cols(.default = col_character()))
national_raw <- read_csv(
  NATIONAL_FILE,
  col_types = cols(ano = col_integer(), sigla_uf = col_character(),
                   cnae_1 = col_character(), cnae_2 = col_character(),
                   public_sector = col_logical(), n_bonds = col_integer())
)

con <- rais_snapshot_connect(n_shards = N_RAIS_SHARDS)
snapshot_state <- dbGetQuery(con, "
  SELECT CAST(ano AS INTEGER) AS year, COUNT(*) AS n,
         COUNT(DISTINCT id_municipio) AS n_munic
  FROM rais GROUP BY ano ORDER BY ano
") %>% as_tibble()

# ---- Assert: raw state ------------------------------------------------------
# _audit/rais.qmd Q6 (snapshot), Q10 (directories, national table).
lookup <- rais_section_lookup(dir1, dir2)
stopifnot(
  n_distinct(dir1$cnae_1) == N_CNAE1_CLASSES,
  n_distinct(dir1$secao) == N_CNAE1_SECTIONS,
  n_distinct(dir2$subclasse) == N_CNAE2_SUBCLASSES,
  n_distinct(dir2$secao) == N_CNAE2_SECTIONS,
  # A division belongs to one section in each version.
  !anyDuplicated(paste(lookup$version, lookup$division)),
  !anyNA(lookup$group),
  setequal(lookup$group, RAIS_SECTION_GROUPS$group),
  identical(snapshot_state$year, RAIS_YEARS),
  sum(snapshot_state$n) == N_RAIS_BONDS_EXPECTED,
  nrow(national_raw) == N_NATIONAL_ROWS_EXPECTED,
  sum(national_raw$n_bonds) == N_NATIONAL_BONDS_EXPECTED,
  setequal(national_raw$ano, RAIS_YEARS),
  n_distinct(national_raw$sigla_uf) == N_NATIONAL_STATES_EXPECTED,
  all(LEGAL_AMAZON_STATES %in% national_raw$sigla_uf),
  !anyNA(national_raw$n_bonds), all(national_raw$n_bonds > 0),
  !anyNA(national_raw$public_sector),
  # Nine-state totals of the national pass equal the snapshot, year by year.
  identical(
    national_raw %>% filter(sigla_uf %in% LEGAL_AMAZON_STATES) %>%
      group_by(year = ano) %>% summarise(n = sum(n_bonds), .groups = "drop") %>%
      arrange(year) %>% pull(n),
    as.integer(snapshot_state$n)
  )
)

# ---- Transform --------------------------------------------------------------
duckdb::duckdb_register(con, "lk", lookup %>% rename(grp = group))
duckdb::duckdb_register(con, "national", national_raw)

# One classification text for both sources: a bond-level view (unit, year,
# public flag, weight) is joined to the lookup by division in each version
# and counted under the three classifications (D-2026-11-06-a/b). The
# harmonized group takes the CNAE 2.0 code where it maps, else the
# CNAE 1.0 code.
sector_sql <- function(source) sprintf("
  WITH b AS (
    SELECT s.unit, s.year, s.public, s.w,
      l1.section AS sec1, l1.grp AS grp1, l2.section AS sec2, l2.grp AS grp2
    FROM (%s) AS s
    LEFT JOIN lk AS l1 ON l1.version = 1 AND l1.division = %s
    LEFT JOIN lk AS l2 ON l2.version = 2 AND l2.division = %s
  ),
  long AS (
    SELECT 'harmonized' AS classification, unit, year,
           COALESCE(grp2, grp1, '%4$s') AS section, public, w FROM b
    UNION ALL
    SELECT 'cnae1', unit, year, COALESCE(sec1, '%4$s'), public, w FROM b
    UNION ALL
    SELECT 'cnae2', unit, year, COALESCE(sec2, '%4$s'), public, w FROM b
    WHERE year >= %5$d
  )
  SELECT classification, unit, year, section,
    SUM(w) AS n_bonds,
    SUM(CASE WHEN public THEN w ELSE 0 END) AS n_bonds_public
  FROM long
  GROUP BY 1, 2, 3, 4",
  source, rais_division_sql("s.cnae_1"), rais_division_sql("s.cnae_2"),
  RAIS_SECTION_UNKNOWN, CNAE2_FROM)

snapshot_source <- sprintf("
  SELECT id_municipio AS unit, CAST(ano AS INTEGER) AS year, cnae_1, cnae_2,
         %s AS public, 1 AS w
  FROM rais", rais_public_sql())
national_source <- "
  SELECT sigla_uf AS unit, ano AS year, cnae_1, cnae_2,
         public_sector AS public, n_bonds AS w
  FROM national"

munic_counts <- dbGetQuery(con, sector_sql(snapshot_source)) %>% as_tibble()
state_counts <- dbGetQuery(con, sector_sql(national_source)) %>% as_tibble()
dbDisconnect(con, shutdown = TRUE)

# The categories of each classification: the sections of the directory (or
# the harmonized groups) plus the unknown code, so that a zero is a zero.
categories <- bind_rows(
  tibble(classification = "harmonized", section = RAIS_SECTION_GROUPS$group),
  lookup %>% filter(version == 1L) %>% distinct(section) %>%
    mutate(classification = "cnae1"),
  lookup %>% filter(version == 2L) %>% distinct(section) %>%
    mutate(classification = "cnae2")
) %>%
  bind_rows(tibble(classification = CLASSIFICATIONS,
                   section = RAIS_SECTION_UNKNOWN)) %>%
  select(classification, section)

# Complete the grid over the unit-years observed in each classification
# block and compute counts and shares; the floor applies to the municipal
# shares only (D-2026-11-05-c), on the count of the sector described.
finish_sector <- function(counts, unit_col, floor = NA_integer_) {
  unit_years <- counts %>% distinct(classification, unit, year)
  counts %>%
    right_join(
      unit_years %>% inner_join(categories, by = "classification",
                                relationship = "many-to-many"),
      by = c("classification", "unit", "year", "section")
    ) %>%
    mutate(across(c(n_bonds, n_bonds_public), ~ coalesce(as.numeric(.x), 0)),
           n_bonds_priv = n_bonds - n_bonds_public) %>%
    group_by(classification, unit, year) %>%
    mutate(total = sum(n_bonds), total_priv = sum(n_bonds_priv)) %>%
    ungroup() %>%
    mutate(
      share      = if_else(is.na(floor) | total >= floor,
                           n_bonds / total, NA_real_),
      share_priv = if_else(is.na(floor) | total_priv >= floor,
                           n_bonds_priv / total_priv, NA_real_),
      across(c(year, n_bonds, n_bonds_public, n_bonds_priv), as.integer),
      classification = factor(classification, levels = CLASSIFICATIONS)
    ) %>%
    arrange(unit, year, classification, section) %>%
    mutate(classification = as.character(classification)) %>%
    select(!!unit_col := unit, year, classification, section, n_bonds,
           n_bonds_public, n_bonds_priv, share, share_priv)
}

rais_sector <- finish_sector(munic_counts, "geocode", floor = RAIS_FLOOR_N_BONDS)

# The three scopes of the reference series, from the state counts.
scope_counts <- function(keep, scope) state_counts %>%
  filter(keep) %>%
  group_by(classification, year, section) %>%
  summarise(n_bonds = sum(n_bonds), n_bonds_public = sum(n_bonds_public),
            .groups = "drop") %>%
  mutate(unit = scope)
in_la <- state_counts$unit %in% LEGAL_AMAZON_STATES
rais_sector_national <- bind_rows(
  scope_counts(rep(TRUE, nrow(state_counts)), "brazil"),
  scope_counts(!in_la, "brazil_excl_la"),
  scope_counts(in_la, "la_states")
) %>%
  finish_sector("scope")

# ---- Assert: output state ---------------------------------------------------
# _audit/rais.qmd Q10-Q11.
n_cat <- categories %>% count(classification)
munic_years <- rais_sector %>% distinct(classification, geocode, year) %>%
  count(classification)
by_year_section <- function(d) d %>%
  group_by(year, classification, section) %>%
  summarise(n_bonds = sum(n_bonds), n_bonds_public = sum(n_bonds_public),
            .groups = "drop")
la_check <- rais_sector_national %>% filter(scope == "la_states") %>%
  by_year_section() %>%
  full_join(by_year_section(rais_sector), by = c("year", "classification", "section"),
            suffix = c("_nat", "_mun"))
scope_check <- rais_sector_national %>%
  select(scope, year, classification, section, n_bonds) %>%
  pivot_wider(names_from = scope, values_from = n_bonds)
share_sums <- function(d, unit_col) d %>%
  group_by(across(all_of(c(unit_col, "year", "classification")))) %>%
  summarise(s = sum(share), s_priv = sum(share_priv),
            total = sum(n_bonds), total_priv = sum(n_bonds_priv),
            .groups = "drop")
munic_sums <- share_sums(rais_sector, "geocode")
nat_sums   <- share_sums(rais_sector_national, "scope")
unk <- rais_sector %>% filter(section == RAIS_SECTION_UNKNOWN) %>%
  group_by(classification) %>% summarise(n = sum(n_bonds), .groups = "drop")
stopifnot(
  # Grid: every unit-year of a block carries every category once.
  nrow(rais_sector) == sum(munic_years$n * n_cat$n[match(munic_years$classification,
                                                          n_cat$classification)]),
  !anyDuplicated(paste(rais_sector$geocode, rais_sector$year,
                       rais_sector$classification, rais_sector$section)),
  n_distinct(rais_sector$geocode) == N_RAIS_MUNIC_EXPECTED,
  all(nchar(rais_sector$geocode) == 7),
  munic_years$n[munic_years$classification == "harmonized"] == N_RAIS_MUNIC_YEARS_EXPECTED,
  munic_years$n[munic_years$classification == "cnae1"] == N_RAIS_MUNIC_YEARS_EXPECTED,
  munic_years$n[munic_years$classification == "cnae2"] ==
    sum(snapshot_state$n_munic[snapshot_state$year >= CNAE2_FROM]),
  all(rais_sector$year[rais_sector$classification == "cnae2"] >= CNAE2_FROM),
  setequal(rais_sector$year, RAIS_YEARS),
  # Counts: the blocks sum to the snapshot; public plus private is the count.
  all(map_dbl(CLASSIFICATIONS[1:2],
              ~ sum(rais_sector$n_bonds[rais_sector$classification == .x])) ==
        N_RAIS_BONDS_EXPECTED),
  sum(rais_sector$n_bonds[rais_sector$classification == "cnae2"]) ==
    sum(snapshot_state$n[snapshot_state$year >= CNAE2_FROM]),
  all(rais_sector$n_bonds >= 0L & rais_sector$n_bonds_public >= 0L &
        rais_sector$n_bonds_priv >= 0L),
  all(rais_sector$n_bonds_public + rais_sector$n_bonds_priv == rais_sector$n_bonds),
  unk$n[unk$classification == "harmonized"] == N_UNK_HARMONIZED_EXPECTED,
  unk$n[unk$classification == "cnae1"] == N_UNK_CNAE1_EXPECTED,
  unk$n[unk$classification == "cnae2"] == N_UNK_CNAE2_EXPECTED,
  # Shares: in [0, 1], summing to one within a unit-year above the floor,
  # NA exactly below it (D-2026-11-05-c).
  all(rais_sector$share >= 0 & rais_sector$share <= 1, na.rm = TRUE),
  all(rais_sector$share_priv >= 0 & rais_sector$share_priv <= 1, na.rm = TRUE),
  all(abs(munic_sums$s - 1) < 1e-9, na.rm = TRUE),
  all(abs(munic_sums$s_priv - 1) < 1e-9, na.rm = TRUE),
  identical(is.na(munic_sums$s), munic_sums$total < RAIS_FLOOR_N_BONDS),
  identical(is.na(munic_sums$s_priv), munic_sums$total_priv < RAIS_FLOOR_N_BONDS),
  # National: three scopes, the nine states equal the municipal file cell by
  # cell, Brazil is the sum of its two parts, no floor.
  setequal(rais_sector_national$scope, SCOPES),
  nrow(rais_sector_national) == 3 * sum(n_cat$n * c(length(RAIS_YEARS), length(RAIS_YEARS),
                                                    sum(RAIS_YEARS >= CNAE2_FROM))[
                                          match(n_cat$classification, CLASSIFICATIONS)]),
  !anyNA(la_check$n_bonds_nat), !anyNA(la_check$n_bonds_mun),
  all(la_check$n_bonds_nat == la_check$n_bonds_mun),
  all(la_check$n_bonds_public_nat == la_check$n_bonds_public_mun),
  all(scope_check$brazil == scope_check$brazil_excl_la + scope_check$la_states),
  sum(rais_sector_national$n_bonds[rais_sector_national$scope == "brazil" &
                                     rais_sector_national$classification == "harmonized"]) ==
    N_NATIONAL_BONDS_EXPECTED,
  !anyNA(rais_sector_national$share), !anyNA(rais_sector_national$share_priv),
  all(abs(nat_sums$s - 1) < 1e-9), all(abs(nat_sums$s_priv - 1) < 1e-9),
  # Types.
  is.character(rais_sector$geocode), is.integer(rais_sector$year),
  is.character(rais_sector$classification), is.character(rais_sector$section),
  is.integer(rais_sector$n_bonds), is.integer(rais_sector$n_bonds_priv),
  is.double(rais_sector$share), is.double(rais_sector$share_priv),
  is.character(rais_sector_national$scope), is.integer(rais_sector_national$n_bonds)
)

# ---- Write ------------------------------------------------------------------
write_rds(rais_sector, "_data/interim/rais_sector_1999_2024.rds")
write_csv(rais_sector, "_data/interim/rais_sector_1999_2024.csv")
write_rds(rais_sector_national, "_data/interim/rais_sector_national_1999_2024.rds")
write_csv(rais_sector_national, "_data/interim/rais_sector_national_1999_2024.csv")
