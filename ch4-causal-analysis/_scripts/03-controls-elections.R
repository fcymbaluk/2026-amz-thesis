# 03-controls-elections.R
# PURPOSE   Build the municipality-year electoral competitiveness panel
#           (effective number of candidates, margin of victory, winner's
#           share, party and id, competitive and uncontested flags) from
#           first-round mayoral results, carried forward over 2000-2020.
#           All municipalities of the nine Legal Amazon states.
#
# INPUTS    _data/raw/tse/tse_mayoral_results_2000_2024.csv
#             TSE via Base dos Dados (BigQuery,
#             br_tse_eleicoes.resultados_candidato_municipio_zona),
#             queried 2026-10-09; SQL in
#             _scripts/sql/2026-10-09-tse-mayoral-results.sql
#
# OUTPUTS   _data/interim/tse_elections_2000_2020.rds / .csv
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
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.5  Values change in the election year (2004 -> 2004-2007), not
#             in the governing years; and the carry-forward fills the NA
#             margin and competitive flag of an uncontested race with the
#             previous election's values. Reproduced on purpose.
#   D-2026-10-10-c  36 ballot-number duplicates of 2000 and 2004 are
#             summed, so their votes count twice, as inherited. Dedupe
#             is a phase 2 decision.
# RUN ORDER Independent of the other 03 scripts; no pipeline inputs.

library(tidyverse)  # magrittr %>% throughout, never the native pipe

# ---- Constants --------------------------------------------------------------
TSE_FILE           <- "_data/raw/tse/tse_mayoral_results_2000_2024.csv"
ELEC_YEARS         <- 2000:2020
TSE_ELECTION_YEARS <- seq(2000L, 2020L, by = 4L)             # D-2026-10-10-d
TSE_OFFICE         <- "prefeito"
TSE_ROUND          <- 1L                                      # D-2026-10-10-d
TSE_ELECTION_TYPE  <- "eleicao ordinaria"                     # D-2026-10-10-d
ELEC_COMPETITIVE_MOV <- 0.10                                  # D-2026-10-10-d
TSE_STATES <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
TSE_COLUMNS <- c("ano", "turno", "id_eleicao", "tipo_eleicao", "data_eleicao",
                 "sigla_uf", "id_municipio", "id_municipio_tse", "cargo",
                 "sequencial_candidato", "numero_candidato", "sigla_partido",
                 "numero_partido", "resultado", "votos", "n_zonas")
# Municipalities with a first-round ordinary election in 2000-2020, nine
# states.
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
  filter(year %in% TSE_ELECTION_YEARS)
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

# Election values carried forward to the next election year; fill() also
# overwrites the NA margin of an uncontested race (task 1.5).
tse_elections <- elections %>%
  distinct(geocode) %>%
  cross_join(tibble(year = ELEC_YEARS)) %>%
  left_join(elections, by = c("geocode", "year")) %>%
  arrange(geocode, year) %>%
  group_by(geocode) %>%
  fill(elec_enc, elec_mov, elec_winner_share, elec_n_candidates,
       elec_total_votes, elec_winner_party, elec_winner_id,
       elec_competitive, elec_uncontested, .direction = "down") %>%
  ungroup() %>%
  mutate(election_year = as.integer(year %in% TSE_ELECTION_YEARS))

# ---- Assert: output state ---------------------------------------------------
# tse.qmd Q2, Q3, Q4. Definitions hold on the election rows; the stored
# panel then carries them forward.
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
write_rds(tse_elections, "_data/interim/tse_elections_2000_2020.rds")
write_csv(tse_elections, "_data/interim/tse_elections_2000_2020.csv")
