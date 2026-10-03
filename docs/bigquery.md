---
role: bigquery-rules
updated: 2026-10-01
---

# BigQuery rules

Operational rules for all BigQuery work in this project. Reading this file is mandatory before any query, from any session. The task-specific workflow for the RAIS re-extraction (query design, confirmation gates, regression rule) is in `ch4-causal-analysis/rebuild/upstream-reform-rais.md` and builds on these rules.

- Billing project: `amz-data-dissertation`. Project quota: 512 GiB
  scanned per day.
- Datasets: `social_employment_1` is READ-ONLY (never write, alter or
  delete there). All new tables, views and query outputs go to
  `social_employment_2`.
- Public data comes from the `basedosdados` project; always use full
  table names, e.g. `basedosdados.br_me_rais.microdados_vinculos`.
- Auth is configured via gcloud application-default credentials; never
  ask for or store credentials. Use `bq query --use_legacy_sql=false`,
  or in R `basedosdados` / `bigrquery` with billing id
  `amz-data-dissertation`.
- Every query, no exceptions: run `--dry_run` first, report estimated
  bytes, then execute with `--maximum_bytes_billed=100000000000`
  (100 GB). In R, pass `maximum_bytes_billed = 1e11`.
- Never `SELECT *` on basedosdados tables. Select only needed columns;
  filter on the partition column (`ano`) and on `sigla_uf` (Amazônia
  Legal) in WHERE.
- Inspect schemas with `bq show --schema`, not with queries.
- Microdata is queried once: aggregate to municipality-year in a single
  query and save the result as a table in `social_employment_2`. All
  later work reads that table or local files in
  `ch4-causal-analysis/_data/raw/<provider>/` (Parquet/CSV; large slices
  are gitignored and covered by the Drive mirror).
- Log every executed query's SQL to `ch4-causal-analysis/_scripts/sql/`.
- Do not enable other Google Cloud APIs or create Compute Engine
  resources.
