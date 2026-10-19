-- 2026-10-13 SICONFI feasibility probes (planning session, descriptive sprint)
-- probe 1: dry run 435 MB; probe 2: dry run 277 MB; both executed with the 100 GB cap

SELECT MIN(ano) AS ano_min, MAX(ano) AS ano_max,
       COUNT(DISTINCT id_municipio) AS n_munic, COUNT(*) AS n_rows
FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao`
WHERE ano BETWEEN 2000 AND 2024 AND sigla_uf = 'AC'

SELECT ano, estagio_bd, conta, COUNT(DISTINCT id_municipio) AS n_munic,
       ROUND(SUM(valor)/1e6, 1) AS mi_brl
FROM `basedosdados.br_me_siconfi.municipio_despesas_funcao`
WHERE ano IN (2005, 2012, 2013, 2019) AND sigla_uf = 'AC'
  AND (LOWER(conta) LIKE '%sa_de%' OR LOWER(conta) LIKE '%educa%'
       OR LOWER(conta) LIKE '%assist_ncia social%' OR LOWER(conta) LIKE '%previd%'
       OR LOWER(conta) LIKE '%gest_o ambiental%' OR LOWER(conta) LIKE '%despesas por fun%')
GROUP BY ano, estagio_bd, conta ORDER BY ano, conta, estagio_bd
