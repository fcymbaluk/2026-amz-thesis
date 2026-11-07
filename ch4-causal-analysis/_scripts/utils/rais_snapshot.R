# rais_snapshot.R
# Shared helpers for the RAIS bond-level snapshot
# (_data/raw/rais/rais_vinculos_v2/, D-2026-11-04-a): the DuckDB connection
# over the 252 Parquet shards and the two public-sector predicates of the
# RAIS brief (rebuild/upstream-reform-rais.md, decision 0.1). Definitions
# only; sourced by 02-social-rais*.R and the audit notebook.

# The shard list, never the folder: README.md sits beside the shards.
rais_snapshot_files <- function(dir = "_data/raw/rais/rais_vinculos_v2") {
  list.files(dir, pattern = "\\.parquet$", full.names = TRUE)
}

# A DuckDB connection with the snapshot exposed as the view `rais`.
rais_snapshot_connect <- function(dir = "_data/raw/rais/rais_vinculos_v2",
                                  n_shards = 252L) {
  files <- rais_snapshot_files(dir)
  stopifnot(length(files) == n_shards)
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  DBI::dbExecute(con, sprintf(
    "CREATE VIEW rais AS SELECT * FROM read_parquet([%s])",
    paste(sprintf("'%s'", files), collapse = ", ")
  ))
  con
}

# Public sector, primary definition (D-2026-11-05-a): natureza jurídica in
# the CONCLA range 1xxx, Administração Pública. A null or empty code is not
# public: none of those rows carries a public-administration CNAE
# (_audit/rais.qmd Q6).
rais_public_sql <- function(prefix = "1") {
  # COALESCE keeps a null code out of the public set and inside its
  # complement, which a bare NOT would drop.
  sprintf("COALESCE(natureza_juridica LIKE '%s%%', FALSE)", prefix)
}

# Public sector, alternative definition (D-2026-11-05-a): CNAE division 75
# (CNAE 1.0, before 2006) or 84 (CNAE 2.0, from 2006), administração
# pública, defesa e seguridade social.
rais_public_cnae_sql <- function(div_cnae1 = "75", div_cnae2 = "84",
                                 cnae2_from = 2006L) {
  sprintf(
    "COALESCE(CASE WHEN ano < %d THEN LEFT(cnae_1, 2) = '%s'
                  ELSE LEFT(cnae_2, 2) = '%s' END, FALSE)",
    cnae2_from, div_cnae1, div_cnae2
  )
}

# ---- CNAE sections (task 3.C, D-2026-11-06-a) --------------------------------
# The sections come from the Base dos Dados directories
# (_data/raw/cnae/cnae_1.csv, cnae_2.csv), by division: every division of
# the snapshot is in its directory, which the class codes of 1999-2001
# (pre-2002 revision) are not (_audit/rais.qmd Q10). The placeholder codes
# (CNAE 1.0 00999 in 2022 and 9999 in 2023-2024, CNAE 2.0 00000 in
# 2006-2008) and the ten null codes of 1999 fall in no section and take
# the code RAIS_SECTION_UNKNOWN.
RAIS_SECTION_UNKNOWN <- "unk"

# The harmonized groups: twelve exact section matches across the 2006 break
# (0.96 or better in both directions on the bonds of 2006-2021 that carry
# both codes) and one residual pooling the sections whose boundaries moved
# (_audit/rais.qmd Q10).
RAIS_SECTION_GROUPS <- tibble::tribble(
  ~group, ~label,                                    ~cnae1, ~cnae2,
  "agr", "agriculture, forestry and fishing",          "A,B",   "A",
  "ext", "extractive industries",                      "C",     "B",
  "man", "manufacturing",                              "D",     "C",
  "con", "construction",                               "F",     "F",
  "tra", "trade and repair",                           "G",     "G",
  "acc", "accommodation and food",                     "H",     "I",
  "fin", "finance and insurance",                      "J",     "K",
  "pub", "public administration, defence and social security", "L", "O",
  "edu", "education",                                  "M",     "P",
  "hea", "health and social services",                 "N",     "Q",
  "dom", "domestic services",                          "P",     "T",
  "int", "international bodies",                       "Q",     "U",
  "res", "residual: utilities, transport and communications, information, real estate, business and other services",
                                                       "E,I,K,O", "D,E,H,J,L,M,N,R,S"
)

# The division of a CNAE class column: five digits after trimming and
# left-padding (the source drops leading zeros in 2023-2024 and carries a
# trailing space on one code).
rais_division_sql <- function(col) {
  sprintf("LEFT(LPAD(TRIM(%s), 5, '0'), 2)", col)
}

# One lookup for both versions: version (1 or 2), division, section and
# harmonized group, from the two directory tables as read from
# _data/raw/cnae/ (every column character).
rais_section_lookup <- function(dir1, dir2, groups = RAIS_SECTION_GROUPS) {
  split_groups <- function(col) {
    groups %>%
      dplyr::select(group, sections = dplyr::all_of(col)) %>%
      tidyr::separate_rows(sections, sep = ",") %>%
      dplyr::rename(section = sections)
  }
  dplyr::bind_rows(
    dir1 %>% dplyr::distinct(division = divisao, section = secao) %>%
      dplyr::mutate(version = 1L) %>%
      dplyr::left_join(split_groups("cnae1"), by = "section"),
    dir2 %>% dplyr::distinct(division = divisao, section = secao) %>%
      dplyr::mutate(version = 2L) %>%
      dplyr::left_join(split_groups("cnae2"), by = "section")
  ) %>%
    dplyr::select(version, division, section, group)
}
