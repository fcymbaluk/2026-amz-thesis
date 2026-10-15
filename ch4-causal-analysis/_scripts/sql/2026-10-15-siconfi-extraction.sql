-- 2026-10-15 SICONFI/FINBRA extraction for the descriptive sprint (task 4.5,
-- rebuild/06-descriptive-sprint.md section 5). Billing project
-- amz-data-dissertation; every query dry-run first and executed with
-- maximum_bytes_billed 100 GB; day total 8.76 GB. Source tables
-- basedosdados.br_me_siconfi.* as of BigQuery lastModifiedTime
-- 1785589672765 (municipio_despesas_funcao, 22,098,549 rows),
-- 1785588097749 (uf_despesas_funcao, 190,352 rows),
-- 1785589030910 (municipio_receitas_orcamentarias, 19,977,711 rows).
-- Outputs placed by the author in _data/raw/siconfi/ (provenance rows
-- in _data/aux/provenance.csv).

-- Probe A: municipal expenditure account inventory. Dry run 1,577,718,734 bytes.
SELECT estagio_bd, id_conta_bd, conta, MIN(ano) AS ano_min, MAX(ano) AS ano_max,
       COUNT(*) AS n_rows, COUNT(DISTINCT id_municipio) AS n_munic
FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao`
WHERE ano BETWEEN 2000 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
GROUP BY estagio_bd, id_conta_bd, conta ORDER BY estagio_bd, id_conta_bd

-- Probe B: municipal revenue account inventory. Dry run 1,537,645,565 bytes.
SELECT estagio_bd, id_conta_bd, conta, MIN(ano) AS ano_min, MAX(ano) AS ano_max,
       COUNT(*) AS n_rows, COUNT(DISTINCT id_municipio) AS n_munic
FROM `basedosdados.br_me_siconfi.municipio_receitas_orcamentarias`
WHERE ano BETWEEN 2000 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
GROUP BY estagio_bd, id_conta_bd, conta ORDER BY estagio_bd, id_conta_bd

-- Probe C: state expenditure account inventory. Dry run 14,141,528 bytes.
SELECT estagio_bd, id_conta_bd, conta, MIN(ano) AS ano_min, MAX(ano) AS ano_max,
       COUNT(*) AS n_rows, COUNT(DISTINCT sigla_uf) AS n_uf
FROM `basedosdados.br_me_siconfi.uf_despesas_funcao`
WHERE ano BETWEEN 2000 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
GROUP BY estagio_bd, id_conta_bd, conta ORDER BY estagio_bd, id_conta_bd

-- Extraction U -> siconfi_uf_despesas_funcao_amz_2000_2024.csv (57,270 rows; the table starts in 2013). Dry run 26,725,691 bytes.
SELECT ano, sigla_uf, estagio, portaria, conta, estagio_bd, id_conta_bd, conta_bd, valor
FROM `basedosdados.br_me_siconfi.uf_despesas_funcao`
WHERE ano BETWEEN 2000 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
ORDER BY sigla_uf, ano, id_conta_bd, estagio_bd

-- Extraction M -> siconfi_municipio_despesas_funcao_amz_2004_2024.csv (327,674 rows: function-level
-- rows 3.FF.000, the 3.00.000 totals, and the rows without id_conta_bd, i.e. the 2013 totals and the
-- intra-budget rows; 2000-2003 carry an empty-string id and fall outside IS NULL, hence M2). Dry run 2,642,739,108 bytes.
SELECT ano, sigla_uf, id_municipio, estagio, portaria, conta, estagio_bd, id_conta_bd, conta_bd, valor
FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao`
WHERE ano BETWEEN 2000 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
  AND estagio_bd = 'Despesas Empenhadas'
  AND (id_conta_bd IS NULL OR REGEXP_CONTAINS(id_conta_bd, r'^3\.[0-9]{2}\.000$'))
ORDER BY sigla_uf, id_municipio, ano, id_conta_bd, conta

-- Extraction M2 -> siconfi_municipio_despesas_funcao_amz_2000_2003.csv (63,073 rows, all Empenhadas rows). Dry run 41,960,140 bytes.
SELECT ano, sigla_uf, id_municipio, estagio, portaria, conta, estagio_bd, id_conta_bd, conta_bd, valor
FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao`
WHERE ano BETWEEN 2000 AND 2003
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
  AND estagio_bd = 'Despesas Empenhadas'
ORDER BY sigla_uf, id_municipio, ano, conta

-- Extraction R -> siconfi_municipio_receitas_orcamentarias_amz_2000_2024.csv (578,955 rows; stage
-- Receitas Brutas Realizadas; accounts matched by the regex: FPM, SUS, FUNDEF/FUNDEB, FNDE and
-- salário-educação, FNAS, and the totals). Dry run 2,917,699,341 bytes.
SELECT ano, sigla_uf, id_municipio, estagio, portaria, conta, estagio_bd, id_conta_bd, conta_bd, valor
FROM `basedosdados.br_me_siconfi.municipio_receitas_orcamentarias`
WHERE ano BETWEEN 2000 AND 2024
  AND sigla_uf IN ('AC','AM','AP','MA','MT','PA','RO','RR','TO')
  AND estagio_bd = 'Receitas Brutas Realizadas'
  AND REGEXP_CONTAINS(conta, r'(?i)fpm|fundo de participa|\bsus\b|sistema único de saúde|saude fundo|bloco de (manutenção|estruturação)|fundef|fundeb|fundo de manutenção|multigov|fnde|desenvolvimento da educação|pnae|pnate|pdde|salário.educação|salario.educação|educação$|educação -|caminho da escola|fnas|assistência social|assist social|combate.{1,3}fome|^rec orçamentária|^total receitas|^receitas \(exceto|^total das receitas|^rec correntes|^receitas correntes$|^rec transf correntes|^transferências correntes$|transf intergov da união|^transferências da união|^transferências intergovernamentais|participação na receita da união|^rec tributária|^receita tributária|^impostos, taxas e contribuições|^rec de capital|^receitas de capital|^rec transf de capital|^transferências de capital')
ORDER BY sigla_uf, id_municipio, ano, conta
