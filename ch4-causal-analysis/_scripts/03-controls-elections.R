# 03-controls-elections.R
# PURPOSE   Build the municipality-year electoral competitiveness panel
#           (effective number of candidates, margin of victory, winner's
#           share, party and id, competitive and uncontested flags) from
#           first-round mayoral results, stored on the governing years of
#           each election over 2000-2024. All municipalities of the nine
#           Legal Amazon states.
#
# INPUTS    _data/raw/tse/tse_mayoral_results_2000_2024.csv
#             TSE via Base dos Dados (BigQuery,
#             br_tse_eleicoes.resultados_candidato_municipio_zona),
#             queried 2026-10-09; SQL in
#             _scripts/sql/2026-10-09-tse-mayoral-results.sql
#
# OUTPUTS   _data/interim/tse_elections_2000_2024.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: annex, pending; evidence: _audit/tse.qmd;
#            history: DECISIONS.md)
#   D-2026-10-10-c  Candidates keyed on ballot number within municipality,
#                   year and round; the winner's sequencial_candidato is
#                   elec_winner_id.
#   D-2026-10-10-d  First round, ordinary elections; ENC = 1 / sum of
#                   squared vote shares; margin = first minus second share;
#                   competitive if the margin is below 10 points; a single
#                   candidate is uncontested (margin NA, ENC 1).
#   D-2026-10-10-j  Task 1.10: typed outputs.
#   D-2026-10-22-d  Task 1.5 (phase 2, step 2.3): the mayor elected in
#                   October of t governs t+1 to t+4; each year is joined
#                   to its governing election, no carry-forward, so an
#                   uncontested race keeps its NA margin and flag over the
#                   four years; 2000 NA (1996 outside the data).
#   D-2026-10-22-f  Series 2000-2024: the 2020 election governs 2021-2024;
#                   the 2024 election governs 2025-2028 and enters only
#                   election_year.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   D-2026-10-10-c  36 ballot-number duplicates of 2000 and 2004 are
#             summed, so their votes count twice, as inherited. Dedupe
#             is a phase 2 decision.
# RUN ORDER Independent of the other 03 scripts; no pipeline inputs.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
TSE_FILE           <- "_data/raw/tse/tse_mayoral_results_2000_2024.csv"
ELEC_YEARS         <- 2000:2024                               # D-2026-10-22-f
TSE_ELECTION_YEARS <- seq(2000L, 2024L, by = 4L)             # D-2026-10-10-d
ELEC_CYCLE         <- 4L
ELEC_GOVERNING_LAG <- 1L   # elected in October of t, governs from t + 1 (D-2026-10-22-d)
# The election governing year y: the last election year at or before y - 1.
governing_election <- function(year) {
  as.integer(floor((year - ELEC_GOVERNING_LAG) / ELEC_CYCLE) * ELEC_CYCLE)
}
TSE_OFFICE         <- "prefeito"
TSE_ROUND          <- 1L                                      # D-2026-10-10-d
TSE_ELECTION_TYPE  <- "eleicao ordinaria"                     # D-2026-10-10-d
ELEC_COMPETITIVE_MOV <- 0.10                                  # D-2026-10-10-d
TSE_STATES <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
TSE_COLUMNS <- c("ano", "turno", "id_eleicao", "tipo_eleicao", "data_eleicao",
                 "sigla_uf", "id_municipio", "id_municipio_tse", "cargo",
                 "sequencial_candidato", "numero_candidato", "sigla_partido",
                 "numero_partido", "resultado", "votos", "n_zonas")
# Municipalities with a first-round ordinary election in 2000-2024, nine
# states (tse.qmd Q7: 805 with a code in 2024, plus two rows with an
# empty code; Macapá's postponed 2020 election is not typed ordinary).
N_MUNIC_TSE_EXPECTED <- 808L

# ---- Read -------------------------------------------------------------------
tse_raw <- read_csv(TSE_FILE, col_types = cols(.default = col_character()),
                    na = character())

# ---- Assert: raw state ------------------------------------------------------
# tse.qmd Q1: schema, one office, elections 2000-2024, votes as digits;
# the only empty municipality codes are two 2024 rows.
is_geocode <- function(x) str_detect(x, "^[0-9]{7}$")
stopifnot(
  identical(names(tse_raw), TSE_COLUMNS),
  all(tse_raw$cargo == TSE_OFFICE),
  setequal(tse_raw$ano, as.character(seq(2000L, 2024L, by = 4L))),
  all(tse_raw$turno %in% c("1", "2")),
  all(tse_raw$sigla_uf %in% TSE_STATES),
  all(str_detect(tse_raw$votos, "^[0-9]+$")),
  all(str_detect(tse_raw$sequencial_candidato, "^[0-9]+$")),
  all(str_detect(tse_raw$numero_candidato, "^[0-9]+$")),
  all(tse_raw$sigla_partido != ""),
  all(is_geocode(tse_raw$id_municipio) | tse_raw$ano == "2024")
)

# ---- Transform --------------------------------------------------------------
mayors <- tse_raw %>%
  filter(turno == as.character(TSE_ROUND),
         tipo_eleicao == TSE_ELECTION_TYPE) %>%
  transmute(geocode = id_municipio, year = as.integer(ano),
            sequencial_candidato, numero_candidato, sigla_partido,
            numero_partido, votes = as.integer(votos)) %>%
  filter(year %in% TSE_ELECTION_YEARS,
         year %in% governing_election(ELEC_YEARS))      # D-2026-10-22-f
stopifnot(all(is_geocode(mayors$geocode)))

# One row per candidate: votes summed over zones and over the ballot-number
# duplicates of 2000 and 2004 (D-2026-10-10-c); the smallest
# sequencial_candidato stands for the candidate.
candidates <- mayors %>%
  group_by(geocode, year, numero_candidato, sigla_partido, numero_partido) %>%
  summarise(candidate_votes = sum(votes),
            candidate_id = min(sequencial_candidato),
            .groups = "drop") %>%
  group_by(geocode, year) %>%
  mutate(total_votes = sum(candidate_votes),
         vote_share = candidate_votes / total_votes) %>%
  ungroup()

# Competitiveness per municipality-election (D-2026-10-10-d). The
# competitive flag is computed before the uncontested margin is set to NA,
# so an uncontested race has competitive = NA, as inherited.
elections <- candidates %>%
  arrange(geocode, year, desc(vote_share)) %>%
  group_by(geocode, year) %>%
  summarise(
    elec_enc          = 1 / sum(vote_share^2),
    elec_mov          = vote_share[1] - vote_share[2],
    elec_winner_share = vote_share[1],
    elec_n_candidates = n(),
    elec_total_votes  = first(total_votes),
    elec_winner_party = sigla_partido[1],
    elec_winner_id    = candidate_id[1],
    .groups = "drop"
  ) %>%
  mutate(
    elec_competitive = as.integer(elec_mov < ELEC_COMPETITIVE_MOV),
    elec_uncontested = as.integer(elec_n_candidates == 1L),
    elec_mov         = if_else(elec_uncontested == 1L, NA_real_, elec_mov),
    elec_enc         = if_else(elec_uncontested == 1L, 1, elec_enc)
  )

# ---- Governing-year storage (D-2026-10-22-d; phase 2, step 2.3) -----------
# Each year is joined to the election that governs it: the October election
# of t covers t + 1 to t + 4. No carry-forward, so the NA margin and flag of
# an uncontested race persist over its four years; 2000 is NA because the
# 1996 election is outside the data. election_year flags the election
# itself, including 2024, whose results govern beyond ELEC_YEARS.
tse_elections <- elections %>%
  distinct(geocode) %>%
  cross_join(tibble(year = ELEC_YEARS)) %>%
  mutate(elec_governing_year = governing_election(year)) %>%
  left_join(elections %>% rename(elec_governing_year = year),
            by = c("geocode", "elec_governing_year")) %>%
  mutate(elec_governing_year = if_else(is.na(elec_enc), NA_integer_,
                                       elec_governing_year),
         election_year = as.integer(year %in% TSE_ELECTION_YEARS)) %>%
  arrange(geocode, year)
# ---- end governing-year storage -------------------------------------------

# ---- Assert: output state ---------------------------------------------------
# tse.qmd Q2, Q3, Q4, Q7. Definitions hold on the election rows; every
# governing year equals its election row and the uncontested NA persists.
governing_check <- tse_elections %>%
  filter(!is.na(elec_governing_year)) %>%
  select(geocode, year, elec_governing_year, elec_enc, elec_mov,
         elec_winner_id, elec_competitive, elec_uncontested) %>%
  inner_join(elections %>%
               select(geocode, elec_governing_year = year,
                      enc0 = elec_enc, mov0 = elec_mov, id0 = elec_winner_id,
                      comp0 = elec_competitive, unc0 = elec_uncontested),
             by = c("geocode", "elec_governing_year"))
stopifnot(
  all((governing_check$year - governing_check$elec_governing_year) %in%
        seq_len(ELEC_CYCLE)),
  all(governing_check$elec_enc == governing_check$enc0),
  identical(governing_check$elec_mov, governing_check$mov0),
  identical(governing_check$elec_winner_id, governing_check$id0),
  identical(governing_check$elec_competitive, governing_check$comp0),
  all(is.na(tse_elections$elec_mov) ==
        (is.na(tse_elections$elec_enc) | tse_elections$elec_uncontested == 1L)),
  all(is.na(tse_elections$elec_enc[tse_elections$year == min(ELEC_YEARS)])),
  all(tse_elections$elec_governing_year[tse_elections$year > min(ELEC_YEARS)] %in%
        TSE_ELECTION_YEARS | is.na(tse_elections$elec_enc[tse_elections$year > min(ELEC_YEARS)])),
  is.integer(tse_elections$elec_governing_year)
)
stopifnot(
  !anyDuplicated(paste(elections$geocode, elections$year)),
  all(elections$elec_enc >= 1),
  all(elections$elec_mov >= 0 & elections$elec_mov <= 1, na.rm = TRUE),
  all(elections$elec_winner_share > 0 & elections$elec_winner_share <= 1),
  all(elections$elec_n_candidates >= 1L),
  all(is.na(elections$elec_mov) == (elections$elec_uncontested == 1L)),
  all(is.na(elections$elec_competitive) == (elections$elec_uncontested == 1L)),
  all(elections$elec_enc[elections$elec_uncontested == 1L] == 1),
  n_distinct(tse_elections$geocode) == N_MUNIC_TSE_EXPECTED,
  nrow(tse_elections) == N_MUNIC_TSE_EXPECTED * length(ELEC_YEARS),
  !anyDuplicated(paste(tse_elections$geocode, tse_elections$year)),
  all(is.na(tse_elections$elec_enc) == is.na(tse_elections$elec_winner_id)),
  all(is.na(tse_elections$elec_enc) == is.na(tse_elections$elec_n_candidates)),
  all(tse_elections$election_year %in% c(0L, 1L)),
  sum(tse_elections$election_year) ==
    N_MUNIC_TSE_EXPECTED * length(TSE_ELECTION_YEARS),
  is.character(tse_elections$geocode),
  is.integer(tse_elections$year),
  is.double(tse_elections$elec_enc),
  is.double(tse_elections$elec_mov),
  is.double(tse_elections$elec_winner_share),
  is.integer(tse_elections$elec_n_candidates),
  is.integer(tse_elections$elec_total_votes),
  is.character(tse_elections$elec_winner_party),
  is.character(tse_elections$elec_winner_id),
  is.integer(tse_elections$elec_competitive),
  is.integer(tse_elections$elec_uncontested),
  is.integer(tse_elections$election_year)
)

# ---- Write ------------------------------------------------------------------
write_rds(tse_elections, "_data/interim/tse_elections_2000_2024.rds")
write_csv(tse_elections, "_data/interim/tse_elections_2000_2024.csv")
