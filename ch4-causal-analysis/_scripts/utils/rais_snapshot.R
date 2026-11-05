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
