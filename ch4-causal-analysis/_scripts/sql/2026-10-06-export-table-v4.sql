-- Export of the RAIS municipal aggregate used by script 02 (Query 2 output,
-- social_employment_1.export_table_v4; the lost local file was
-- _data/data_employment_rais). Run 2026-10-07T02:17:11Z from the sandbox with
-- bq query --use_legacy_sql=false --maximum_bytes_billed=100000000000
-- --format=csv --max_rows=1000000. Dry run: 7,136,542 bytes.
-- Output: _data/raw/rais/export_table_v4.csv (provenance row in _data/aux/provenance.csv).
SELECT year, geocode, workers_agricultural, wage_code, workers_total
FROM `amz-data-dissertation.social_employment_1.export_table_v4`
ORDER BY year, geocode, workers_agricultural, wage_code;
