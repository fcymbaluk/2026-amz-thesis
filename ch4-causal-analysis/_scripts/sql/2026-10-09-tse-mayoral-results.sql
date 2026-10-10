-- TSE mayoral results (candidate x municipality x year x turno) for the nine
-- Legal Amazon states, elections 2000-2024, from Base dos Dados. Replaces the
-- lost _data/raw-tse-results-2.csv read by 03-data-cleaning-controls.R:409.
-- Run 2026-10-09T23:34:07Z from the sandbox with
-- bq query --use_legacy_sql=false --maximum_bytes_billed=100000000000
-- --format=csv --max_rows=1000000. Dry run: 675,978,042 bytes. Result: 17,374 rows.
-- Output: _data/raw/tse/tse_mayoral_results_2000_2024.csv (provenance row in
-- _data/aux/provenance.csv).
SELECT
  ano,
  turno,
  id_eleicao,
  tipo_eleicao,
  data_eleicao,
  sigla_uf,
  id_municipio,
  id_municipio_tse,
  cargo,
  sequencial_candidato,
  numero_candidato,
  sigla_partido,
  numero_partido,
  resultado,
  SUM(votos) AS votos,
  COUNT(*)   AS n_zonas
FROM `basedosdados.br_tse_eleicoes.resultados_candidato_municipio_zona`
WHERE ano IN (2000, 2004, 2008, 2012, 2016, 2020, 2024)
  AND cargo = 'prefeito'
  AND sigla_uf IN ('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')
GROUP BY ano, turno, id_eleicao, tipo_eleicao, data_eleicao, sigla_uf, id_municipio,
         id_municipio_tse, cargo, sequencial_candidato, numero_candidato,
         sigla_partido, numero_partido, resultado
ORDER BY ano, turno, sigla_uf, id_municipio, numero_candidato
