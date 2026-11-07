-- 2026-11-06 RAIS track, task 3.C (rebuild/upstream-reform-rais.md): the
-- national reference series of sectoral employment and the two CNAE
-- directory tables. Billing project amz-data-dissertation; every query
-- dry-run first and executed with maximum_bytes_billed 100 GB; day total
-- 66.70 GB processed (66,701,127,625 bytes; the one-query dry run of the
-- same pass, 66,701,127,625 bytes, is the author's amendment of
-- 2026-11-04, so it ran split by year blocks, each under the 50 GB line).
-- Source table basedosdados.br_me_rais.microdados_vinculos as of BigQuery
-- lastModifiedTime 1780009150642 (2026-05-28), the version of the snapshot
-- of 2026-11-04 (D-2026-11-04-a). Outputs:
-- social_employment_2.rais_sector_national (433,077 rows, 14,522,027
-- bytes) and its CSV export, gs://amz-data-dissertation-social/
-- rais_sector_national/rais_sector_national.csv (12,393,305 bytes,
-- sha256 5f73e81a8f63e575d679da1449deb6e78f5404acab63644299b405ab349723be),
-- placed by the author in _data/raw/rais/rais_sector_national.csv; the
-- directory tables basedosdados.br_bd_diretorios_brasil.cnae_1 and cnae_2
-- pulled as CSV and placed by the author in _data/raw/cnae/ (provenance
-- rows in _data/aux/provenance.csv). Decisions D-2026-11-06-a/b.
-- Dataset social_employment_2 after this session: rais_vinculos_deflat_v2,
-- rais_sector_national. Nothing in social_employment_1 touched.

-- Directory tables (schemas inspected with bq show --schema: cnae_1 has
-- 581 rows, class to section; cnae_2 has 1,356 rows, subclass to section).
-- Dry runs 132,288 and 442,622 bytes. Run 2026-11-06T13:20Z with
-- bq query --format=csv --max_rows=100000; outputs cnae_1.csv (582 lines,
-- 129,067 bytes, sha256 0c2e4ca7a1ed770b1cd83d1b9a7557b3b7b3f679b2a2a2bc45ed4c29fd102939)
-- and cnae_2.csv (1,442 lines with quoted line breaks, 400,418 bytes,
-- sha256 b7814e1f5de9d130298d6cb760f9d4b159f3f660385674bddac00ba4d2a52881).
SELECT * FROM `basedosdados.br_bd_diretorios_brasil.cnae_1` ORDER BY 1;
SELECT * FROM `basedosdados.br_bd_diretorios_brasil.cnae_2` ORDER BY 1;

-- Design dry runs of the national pass (the query below over other year
-- spans; not executed): 1999-2024 66,701,127,625 bytes; 1999-2013
-- 32,371,634,241; 2014-2024 34,329,493,384; 1999-2008 17,426,025,167;
-- 2009-2024 49,275,102,458; CNAE 1.0 only 1999-2005 10,328,593,100;
-- CNAE 2.0 only 2006-2024 47,192,661,144. Both class codes are carried in
-- every year so the national series takes the same section mapping as the
-- municipal series (02-social-rais-sector.R) and the same placeholder
-- diagnostics (_audit/rais.qmd Q10).

-- Block 1, 1999-2012: the counts by year, state, CNAE 1.0 class, CNAE 2.0
-- class and the public-sector flag (natureza jurídica 1xxx, an empty code
-- private, D-2026-11-05-a), with the bond filter of Query 1 (active on
-- 31/12, positive average wage) and no state filter. Dry run
-- 29,129,412,311 bytes. Run 2026-11-06T21:17:30Z, job
-- bqjob_r23bd46bdee92b193_000001a1b2b94d38_1, 2.3 s, 29,129,412,311 bytes
-- processed; 218,615 rows.
CREATE TABLE `amz-data-dissertation.social_employment_2.rais_sector_national`
OPTIONS (description = 'RAIS active bonds (31/12) with positive average wage, all Brazil, 1999-2024, counted by year, state, CNAE 1.0 class, CNAE 2.0 class and public-sector flag (natureza juridica 1xxx), from basedosdados.br_me_rais.microdados_vinculos as of BigQuery lastModifiedTime 1780009150642 (2026-05-28). National reference series of the RAIS track, task 3.C (ch4-causal-analysis/_scripts/sql/2026-11-06-rais-sector-national.sql). Built 2026-11-06 in two year blocks, 1999-2012 and 2013-2024.')
AS
SELECT
  ano,
  sigla_uf,
  cnae_1,
  cnae_2,
  COALESCE(natureza_juridica LIKE '1%', FALSE) AS public_sector,
  COUNT(*) AS n_bonds
FROM `basedosdados.br_me_rais.microdados_vinculos`
WHERE ano BETWEEN 1999 AND 2012
  AND vinculo_ativo_3112 = '1'
  AND valor_remuneracao_media IS NOT NULL
  AND valor_remuneracao_media > 0
GROUP BY 1, 2, 3, 4, 5;

-- Block 2, 2013-2024, appended. Dry run 37,571,715,314 bytes. Run
-- 2026-11-06T21:17:36Z, job bqjob_r66b1802738f74214_000001a1b2b96448_1,
-- 2.6 s, 37,571,715,314 bytes processed; 214,462 rows.
INSERT INTO `amz-data-dissertation.social_employment_2.rais_sector_national`
SELECT
  ano,
  sigla_uf,
  cnae_1,
  cnae_2,
  COALESCE(natureza_juridica LIKE '1%', FALSE) AS public_sector,
  COUNT(*) AS n_bonds
FROM `basedosdados.br_me_rais.microdados_vinculos`
WHERE ano BETWEEN 2013 AND 2024
  AND vinculo_ativo_3112 = '1'
  AND valor_remuneracao_media IS NOT NULL
  AND valor_remuneracao_media > 0
GROUP BY 1, 2, 3, 4, 5;

-- Verification by year. Dry run 9,096,915 bytes. Result: 1,058,183,357
-- bonds, 433,077 rows, 27 states plus the code IGNORADO (12,662 bonds in
-- 2001-2007 and 2021-2024, kept in the Brazil scopes); the nine Legal
-- Amazon states sum to the snapshot's count in every year (1,588,912 in
-- 1999 to 4,969,265 in 2024, D-2026-11-04-a).
SELECT ano, COUNT(*) AS rows_, SUM(n_bonds) AS bonds_brazil,
  SUM(IF(sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO'), n_bonds, 0)) AS bonds_la9,
  SUM(IF(public_sector, n_bonds, 0)) AS bonds_public,
  COUNT(DISTINCT sigla_uf) AS n_uf
FROM `amz-data-dissertation.social_employment_2.rais_sector_national`
GROUP BY 1 ORDER BY 1;

-- Export (not a query): run 2026-11-06T21:18:05Z, 4 s, one CSV object.
-- bq extract --destination_format CSV \
--   amz-data-dissertation:social_employment_2.rais_sector_national \
--   gs://amz-data-dissertation-social/rais_sector_national/rais_sector_national.csv
