-- 2026-11-04 RAIS track, task 3.B step 1 (rebuild/upstream-reform-rais.md):
-- Query 1 rebuilt with the sector, legal-nature and December-wage columns,
-- the regression rule run, the table exported to Parquet. Billing project
-- amz-data-dissertation; every query dry-run first and executed with
-- maximum_bytes_billed 100 GB; day total 36.44 GB. Source table
-- basedosdados.br_me_rais.microdados_vinculos as of BigQuery
-- lastModifiedTime 1780009150642 (2026-05-28 22:59 UTC; 2,163,605,953 rows,
-- 478.5 GB, range-partitioned on ano, clustered on sigla_uf, id_municipio).
-- The existing extraction, social_employment_1.rais_vinculos_deflat
-- (created 2025-11-25, 88,466,643 rows), was read only. Outputs:
-- social_employment_2.rais_vinculos_deflat_v2 (90,156,865 rows, 7.88 GB)
-- and its Parquet export, gs://amz-data-dissertation-social/rais_v2/
-- (252 shards, 1,829,125,548 bytes), placed by the author in
-- _data/raw/rais/rais_vinculos_v2/ (provenance row in
-- _data/aux/provenance.csv, shard hashes in
-- _data/aux/rais_vinculos_v2_manifest.csv). Decision D-2026-11-04-a.

-- Pre-flight A: rows the original Query 1 filter selects in the source now,
-- by year. Dry run 5,146,469,302 bytes. Run 2026-11-04T19:45Z.
SELECT ano, COUNT(*) AS n
FROM `basedosdados.br_me_rais.microdados_vinculos`
WHERE ano BETWEEN 1999 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
  AND vinculo_ativo_3112 = '1'
  AND valor_remuneracao_media IS NOT NULL
  AND valor_remuneracao_media > 0
GROUP BY ano ORDER BY ano;

-- Pre-flight B: the existing table by year. Dry run 707,733,144 bytes.
-- Result: 1999-2022 identical to pre-flight A year by year; 2023
-- 4,558,077 against 4,802,766 (+244,689, +5.4 percent); 2024 3,523,732
-- against 4,969,265 (+1,445,533, +41.0 percent). Totals 88,466,643 and
-- 90,156,865. (Two further probes, MIN/MAX ano and COUNT on the existing
-- table and on export_table_v4, 707,733,144 and 1,392,496 bytes: both
-- 1999-2024.)
SELECT ano, COUNT(*) AS n
FROM `amz-data-dissertation.social_employment_1.rais_vinculos_deflat`
GROUP BY ano ORDER BY ano;

-- Query 1, version 2. The original Query 1 (parked appendix
-- _annex/parked/u-appendix-data-description.qmd, section 2.1.3.6) with an
-- explicit year filter and the columns cnae_1, cnae_2, cnae_2_subclasse,
-- natureza_juridica and valor_remuneracao_dezembro carried through;
-- vinculo_ativo_3112 dropped (constant 1 after the filter); no flag or
-- derived column beyond cbo_unified, which is inherited unchanged. The
-- public-sector flags of Task B are built locally on the snapshot, after
-- the code lists are inspected. Dry run 16,337,288,908 bytes. Run
-- 2026-11-04T20:48:35Z, 7 s; 90,156,865 rows.
CREATE TABLE `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2`
PARTITION BY RANGE_BUCKET(ano, GENERATE_ARRAY(1999, 2025, 1))
CLUSTER BY sigla_uf, id_municipio
OPTIONS (description = 'RAIS active bonds (31/12) with positive average wage, nine Legal Amazon states, 1999-2024, from basedosdados.br_me_rais.microdados_vinculos as of BigQuery lastModifiedTime 1780009150642 (2026-05-28); extends social_employment_1.rais_vinculos_deflat with CNAE 1.0, CNAE 2.0 (class and subclass), natureza juridica and the December wage. Built 2026-11-04 (RAIS track, task 3.B; SQL in ch4-causal-analysis/_scripts/sql/2026-11-04-rais-query1-v2.sql).')
AS
SELECT
  r.ano,
  r.sigla_uf,
  r.id_municipio,
  CASE
    WHEN r.cbo_2002 IS NOT NULL THEN CAST(r.cbo_2002 AS STRING)
    WHEN r.cbo_1994 IS NOT NULL THEN CONCAT(CAST(r.cbo_1994 AS STRING), '0')
    ELSE NULL
  END AS cbo_unified,
  r.cnae_1,
  r.cnae_2,
  r.cnae_2_subclasse,
  r.natureza_juridica,
  r.valor_remuneracao_media,
  r.valor_remuneracao_dezembro,
  d.ipca_deflator,
  SAFE_MULTIPLY(r.valor_remuneracao_media, d.ipca_deflator) AS valor_remuneracao_media_2024
FROM `basedosdados.br_me_rais.microdados_vinculos` AS r
LEFT JOIN `amz-data-dissertation.social_employment_1.deflator_ipca_2024` AS d
  ON r.ano = d.year
WHERE r.ano BETWEEN 1999 AND 2024
  AND r.sigla_uf IN ('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')
  AND r.vinculo_ativo_3112 = '1'
  AND r.valor_remuneracao_media IS NOT NULL
  AND r.valor_remuneracao_media > 0;

-- Regression rule: Query 2 unchanged (same appendix section; the text of
-- _scripts/sql/2026-10-06-export-table-v4.sql reads its stored output) on
-- the rebuilt table, compared cell by cell with
-- _data/raw/rais/export_table_v4.csv on the key year x geocode x
-- workers_agricultural x wage_code. Dry run 2,974,470,341 bytes. Run
-- 2026-11-04T20:49Z; 174,390 rows against 174,062. Result: 1999-2022
-- identical in every cell (157,897 cells, no cell on either side only);
-- 2023: 4,163 cells equal, 3,612 differ, 6 only in the old export, 42 only
-- in the new, 742 municipalities; 2024: 2,210 equal, 5,382 differ, 4 only
-- old, 296 only new, all 808 municipalities plus Boa Esperança do Norte
-- (5101837, 2023-2024 only, the 809th geocode).
SELECT
  ano AS year,
  id_municipio AS geocode,
  CASE
    WHEN LEFT(LPAD(cbo_unified, 6, '0'), 1) = '6' THEN 1
    ELSE 0
  END AS workers_agricultural,
CASE
  WHEN valor_remuneracao_media_2024 <= 1.00 * 1412.00 THEN 1
  WHEN valor_remuneracao_media_2024 > 1.00 * 1412.00 AND valor_remuneracao_media_2024 <= 2.00 * 1412.00 THEN 2
  WHEN valor_remuneracao_media_2024 > 2.00 * 1412.00 AND valor_remuneracao_media_2024 <= 3.00 * 1412.00 THEN 3
  WHEN valor_remuneracao_media_2024 > 3.00 * 1412.00 AND valor_remuneracao_media_2024 <= 4.00 * 1412.00 THEN 4
  WHEN valor_remuneracao_media_2024 > 4.00 * 1412.00 AND valor_remuneracao_media_2024 <= 5.00 * 1412.00 THEN 5
  ELSE 6
END AS wage_code,
  COUNT(*) AS workers_total
FROM
  `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2`
GROUP BY
  year,
  geocode,
  workers_agricultural,
  wage_code
ORDER BY
  year,
  geocode,
  workers_agricultural,
  wage_code;

-- Probe C: column coverage by year on the rebuilt table. Dry run
-- 6,076,147,503 bytes. Result: the deflator joins in every row; cnae_1 is
-- populated in every year (10 nulls in 1999; five characters through 2022,
-- four to six in 2023-2024, Base dos Dados' own warning about non-official
-- codes applies); cnae_2 and cnae_2_subclasse are null before 2006 and
-- populated from 2006 (five and seven characters); natureza_juridica is
-- four characters, null in 880 rows of 1999 and in 193,201, 200,423 and
-- 208,762 rows of 2022-2024 (an empty code in 603,266 rows overall);
-- valor_remuneracao_dezembro is null only in 2023-2024 (237,971 and
-- 257,444 rows) and zero in 2-5 percent of rows every year; cbo_unified is
-- null in up to 15,181 rows a year (2011-2018), counted as non-agricultural
-- by Query 2 as inherited; 762-808 municipalities a year, 809 in 2023-2024.
SELECT ano,
  COUNT(*) AS n,
  COUNTIF(ipca_deflator IS NULL) AS null_deflator,
  COUNTIF(cbo_unified IS NULL) AS null_cbo,
  COUNTIF(cnae_1 IS NULL) AS null_cnae_1,
  COUNTIF(cnae_2 IS NULL) AS null_cnae_2,
  COUNTIF(cnae_2_subclasse IS NULL) AS null_cnae_2_sub,
  COUNTIF(natureza_juridica IS NULL) AS null_natjur,
  COUNTIF(valor_remuneracao_dezembro IS NULL) AS null_wage_dec,
  COUNTIF(SAFE_CAST(valor_remuneracao_dezembro AS FLOAT64) <= 0) AS zero_wage_dec,
  COUNT(DISTINCT id_municipio) AS n_munic,
  COUNT(DISTINCT natureza_juridica) AS n_natjur_codes,
  MIN(LENGTH(natureza_juridica)) AS natjur_len_min, MAX(LENGTH(natureza_juridica)) AS natjur_len_max,
  MIN(LENGTH(cnae_1)) AS cnae1_len_min, MAX(LENGTH(cnae_1)) AS cnae1_len_max,
  MIN(LENGTH(cnae_2)) AS cnae2_len_min, MAX(LENGTH(cnae_2)) AS cnae2_len_max,
  MIN(LENGTH(cnae_2_subclasse)) AS sub_len_min, MAX(LENGTH(cnae_2_subclasse)) AS sub_len_max
FROM `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2`
GROUP BY ano ORDER BY ano;

-- Probe D: legal-nature code list. Dry run 537,321,594 bytes. Result: 108
-- distinct codes, four digits (CONCLA table); the 1xxx range
-- (Administração Pública) holds 31 codes led by 1031 (municipal executive,
-- 14.1 M rows), 1023 (state executive, 10.3 M), 1244 (município, 1.6 M)
-- and 1015 (federal executive, 1.2 M); 2062 (sociedade empresária
-- limitada, 33.7 M) is the largest code overall.
SELECT natureza_juridica, COUNT(*) AS n
FROM `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2`
GROUP BY 1 ORDER BY n DESC;

-- Probe E: the 2023-2024 difference by state, old table against new. Dry
-- run 2,063,012,154 bytes. Result: 2023 up 0.9 (RR) to 13.5 percent (TO);
-- 2024 up 25.7 (MT) to 86.3 percent (RR) in every state, the same
-- municipality counts except MT (141 to 142): the 2024 slice of the
-- 2025-11 extraction was a partial release.
WITH o AS (
  SELECT ano, sigla_uf, COUNT(*) AS n_old, COUNT(DISTINCT id_municipio) AS m_old
  FROM `amz-data-dissertation.social_employment_1.rais_vinculos_deflat`
  WHERE ano >= 2023 GROUP BY 1, 2),
n AS (
  SELECT ano, sigla_uf, COUNT(*) AS n_new, COUNT(DISTINCT id_municipio) AS m_new
  FROM `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2`
  WHERE ano >= 2023 GROUP BY 1, 2)
SELECT n.ano, n.sigla_uf, o.n_old, n.n_new, n.n_new - o.n_old AS diff,
       ROUND(100 * (n.n_new - o.n_old) / o.n_old, 1) AS pct, o.m_old, n.m_new
FROM n LEFT JOIN o USING (ano, sigla_uf) ORDER BY ano, sigla_uf;

-- Probe F: geocodes present only after 2022. Dry run 1,893,294,165 bytes.
-- Result: 5101837 Boa Esperança do Norte (MT), 2023-2024, 1,074 rows.
SELECT id_municipio, sigla_uf, MIN(ano) AS first_year, MAX(ano) AS last_year, COUNT(*) AS n
FROM `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2`
WHERE id_municipio NOT IN (
  SELECT DISTINCT id_municipio FROM `amz-data-dissertation.social_employment_2.rais_vinculos_deflat_v2` WHERE ano <= 2022)
GROUP BY 1, 2;

-- Export (not a query): run 2026-11-04T20:50:19Z, 5 s.
-- bq extract --destination_format PARQUET \
--   amz-data-dissertation:social_employment_2.rais_vinculos_deflat_v2 \
--   "gs://amz-data-dissertation-social/rais_v2/rais_vinculos_deflat_v2-*.parquet"
