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
#   D-2026-10-28-e  Task 1.8 (phase 2, step 2.7): candidates with zero
#                   valid votes (candidacies refused after the ballot was
#                   printed) leave the candidate set before the shares;
#                   the ENC is unchanged, the candidate count and the
#                   uncontested flag follow the valid candidates.
#   D-2026-10-31-c  After task 1.11: the winner is the candidate with most
#                   valid votes; a first-place tie is broken by the TSE
#                   declaration (resultado "eleito"), never by ballot
#                   order. Two ties in 2000-2020 after the dedupe below,
#                   each with exactly one declared winner among the tied.
#   D-2026-10-31-e  After task 1.11: one candidacy per ballot number. The
#                   36 ballot-number pairs of 2000 and 2004 (two
#                   sequencial_candidato with identical votes) count
#                   once: in 2004 the registration not marked
#                   "renuncia/falecimento/cassacao" stands (a substitute
#                   who kept the number), in 2000 the two rows are
#                   identical and the smaller id stands. The winner stays
#                   the most voted in the first round, not the TSE
#                   declaration, so the block measures the contest, not
#                   the office holder (decided against, same entry).
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
TSE_ELECTED          <- "eleito"                              # D-2026-10-31-c
TSE_RENOUNCED        <- "renuncia/falecimento/cassacao"       # D-2026-10-31-e
TSE_STATES <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
TSE_COLUMNS <- c("ano", "turno", "id_eleicao", "tipo_eleicao", "data_eleicao",
                 "sigla_uf", "id_municipio", "id_municipio_tse", "cargo",
                 "sequencial_candidato", "numero_candidato", "sigla_partido",
                 "numero_partido", "resultado", "votos", "n_zonas")
# Municipalities with a first-round ordinary election in 2000-2024, nine
# states (tse.qmd Q7: 805 with a code in 2024, plus two rows with an
# empty code; Macapá's postponed 2020 election is not typed ordinary).
N_MUNIC_TSE_EXPECTED <- 808L
# Zero-vote candidates in the first round of the governing elections, 54
# of 2012 and 2 of 2020 (tse.qmd Q6, Q8; D-2026-10-28-e).
N_ZERO_VOTE_CANDIDATES_EXPECTED <- 56L
# Ballot-number pairs in the governing elections (tse.qmd Q2; D-2026-10-31-e):
# 20 in 2000 (identical rows) and 16 in 2004 (one registration renounced).
N_DUP_PAIRS_EXPECTED <- c(`2000` = 20L, `2004` = 16L)
# First-place ties of valid votes after the dedupe (tse.qmd Q6;
# D-2026-10-31-c): Rio dos Bois 2000 and Senador Alexandre Costa 2004. Each
# has exactly one tied candidate declared elected; at most one candidate
# per election is.
N_TIED_ELECTIONS_EXPECTED <- 2L

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
            numero_partido, resultado, votes = as.integer(votos)) %>%
  filter(year %in% TSE_ELECTION_YEARS,
         year %in% governing_election(ELEC_YEARS))      # D-2026-10-22-f
stopifnot(all(is_geocode(mayors$geocode)))

# One row per registration (sequencial_candidato): votes summed over the
# rows of the extract. A registration is declared elected when any of its
# rows carries the TSE result "eleito" (D-2026-10-31-c; the result varies
# across rows for a few candidates) and renounced when every row reads
# "renuncia/falecimento/cassacao".
registrations <- mayors %>%
  group_by(geocode, year, numero_candidato, sequencial_candidato,
           sigla_partido, numero_partido) %>%
  summarise(candidate_votes = sum(votes),
            votes_signature = paste(sort(votes), collapse = ";"),
            elected   = any(resultado == TSE_ELECTED),
            renounced = all(resultado == TSE_RENOUNCED),
            .groups = "drop")

# ---- Fix (D-2026-10-31-e; phase 2, after task 1.11) --------------------------
# One candidacy per ballot number. The 36 pairs of 2000 and 2004 carry the
# same votes under two sequencial_candidato (identical row by row): in 2004
# one registration is renounced and the other ran under the same number,
# in 2000 the two rows are identical. The running registration stands
# (then the smaller id); its party and id name the candidate. Phase 1
# summed the pairs, doubling their votes (D-2026-10-10-c, KNOWN DEFECTS).
dup_pairs <- registrations %>%
  group_by(geocode, year, numero_candidato) %>%
  filter(n() > 1L) %>%
  summarise(n_rows      = n(),
            same_votes  = n_distinct(votes_signature) == 1L,
            same_party  = n_distinct(sigla_partido) == 1L,
            n_renounced = sum(renounced),
            .groups = "drop")
candidates_all <- registrations %>%
  group_by(geocode, year, numero_candidato) %>%
  arrange(renounced, sequencial_candidato, .by_group = TRUE) %>%
  slice(1L) %>%
  ungroup() %>%
  transmute(geocode, year, numero_candidato, sigla_partido, numero_partido,
            candidate_votes, candidate_id = sequencial_candidato, elected)
# ---- end fix -----------------------------------------------------------------

# ---- Fix (D-2026-10-28-e; phase 2, step 2.7) ---------------------------------
# A candidate with zero valid votes was not a competitor: dropped before the
# shares, so the candidate count and the uncontested flag see only valid
# candidates. The ENC is unaffected (a zero share adds nothing).
n_zero_vote <- sum(candidates_all$candidate_votes == 0L)
candidates <- candidates_all %>%
  filter(candidate_votes > 0L) %>%
# ---- end fix -----------------------------------------------------------------
  group_by(geocode, year) %>%
  mutate(total_votes = sum(candidate_votes),
         vote_share = candidate_votes / total_votes) %>%
  ungroup()

# ---- Fix (D-2026-10-31-c; phase 2, after task 1.11) --------------------------
# The winner is the candidate with most valid votes; a first-place tie is
# broken by the TSE declaration (elected), then by ballot number for a
# deterministic order. Phase 1 broke ties by ballot order alone, which
# stored the wrong winner wherever the declared winner had the higher
# ballot number (Senador Alexandre Costa 2004; Vilhena and Matinha 2004
# among the Q2 duplicates). Every tie has one declared winner (assertion).
# ---- end fix -----------------------------------------------------------------
# Competitiveness per municipality-election (D-2026-10-10-d). The
# competitive flag is computed before the uncontested margin is set to NA,
# so an uncontested race has competitive = NA, as inherited.
elections <- candidates %>%
  arrange(geocode, year, desc(vote_share), desc(elected),
          numero_candidato, sigla_partido, numero_partido) %>%
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
# ---- Assert: ballot-number pairs (D-2026-10-31-e) --------------------------
# tse.qmd Q2. Exactly the 36 known pairs, two rows each with identical
# votes; the 2000 pairs identical in party with no renounced row, the 2004
# pairs with exactly one renounced row; one candidacy per ballot number
# after the dedupe, and no registration lost elsewhere.
stopifnot(
  identical(table(factor(dup_pairs$year, levels = names(N_DUP_PAIRS_EXPECTED)))
            %>% as.integer(), unname(N_DUP_PAIRS_EXPECTED)),
  all(dup_pairs$n_rows == 2L),
  all(dup_pairs$same_votes),
  all(dup_pairs$same_party[dup_pairs$year == 2000L]),
  all(dup_pairs$n_renounced[dup_pairs$year == 2000L] == 0L),
  all(dup_pairs$n_renounced[dup_pairs$year == 2004L] == 1L),
  !anyDuplicated(paste(candidates_all$geocode, candidates_all$year,
                       candidates_all$numero_candidato)),
  nrow(candidates_all) == nrow(registrations) - nrow(dup_pairs)
)

# ---- Assert: ties (D-2026-10-31-c) -----------------------------------------
# tse.qmd Q6. At most one declared winner per election (none in the
# first round of a runoff municipality); every first-place tie holds exactly
# one declared winner among the tied candidates, and the stored winner is it.
ties <- candidates %>%
  group_by(geocode, year) %>%
  summarise(n_elected   = sum(elected),
            n_top       = sum(candidate_votes == max(candidate_votes)),
            top_elected = sum(elected & candidate_votes == max(candidate_votes)),
            .groups = "drop") %>%
  filter(n_top > 1L) %>%
  left_join(elections %>% select(geocode, year, elec_winner_id),
            by = c("geocode", "year")) %>%
  left_join(candidates %>% select(geocode, year, elec_winner_id = candidate_id,
                                  winner_elected = elected),
            by = c("geocode", "year", "elec_winner_id"))
stopifnot(
  all(tapply(candidates$elected, paste(candidates$geocode, candidates$year),
             sum) <= 1L),
  nrow(ties) == N_TIED_ELECTIONS_EXPECTED,
  all(ties$top_elected == 1L),
  all(ties$winner_elected)
)
stopifnot(
  n_zero_vote == N_ZERO_VOTE_CANDIDATES_EXPECTED,              # D-2026-10-28-e
  all(candidates$candidate_votes > 0L),
  n_distinct(paste(candidates$geocode, candidates$year)) ==
    n_distinct(paste(candidates_all$geocode, candidates_all$year)),
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
