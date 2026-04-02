
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(writexl)
library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls(all = TRUE))

getwd()

#' *2. Social data:* 
#' 
#' *2.1. Employment* 
#'
#' *2.1.1. Understand the data:* The employment data used in this study comes from the Relação Anual de Informações Sociais (RAIS), 
#' an administrative dataset maintained by the Brazilian Ministry of Labor and Employment. RAIS compiles annual information reported 
#' by all formal-sector employers in Brazil, covering private firms, public agencies, and third-sector organizations. The dataset 
#' includes detailed records of each employment bond, such as the worker’s occupation, income, education level, sex, age, and location 
#' of employment. Because reporting to RAIS is mandatory, it provides near-complete coverage of formal employment relationships (vínculos) 
#' in the country.
#'
#' The dataset spans 1985–2024 and is divided into two main components:
#' * Estabelecimentos, containing establishment-level information, and
#' * Vínculos, containing individual employment-bond microdata.
#' 
#' This study utilizes the Microdados Vínculos, where each row corresponds to a unique employment bond in a given year. Variables describe 
#' the characteristics of the worker (e.g., education, sex, age), the job (e.g., wages, occupation), and the establishment. The data are 
#' anonymized. 
#' 
#' Due to its depth and temporal coverage, RAIS enables consistent measurement of formal employment trends at fine geographic scales, including 
#' municipalities within the Brazilian Amazon.
#' 
#' *2.1.2. Import and inspect the data:* 
#' 
#' The RAIS microdata contain more than 2 billion rows, 350 GB, making local processing difficult. All data manipulation was therefore conducted 
#' using Google BigQuery, a distributed cloud data warehouse that enables scalable SQL processing and avoids local storage constraints.
#' 
#' The data were accessed through Base dos Dados (BD), a non-profit open-data infrastructure that republishes harmonized versions of major 
#' Brazilian administrative datasets (Dahis et al., 2022). BD performs several preprocessing procedures to guarantee temporal consistency and 
#' standardization across years, including:
#'   
#' - Converting municipality identifiers to the official 7-digit IBGE geocode;
#' - Standardizing state identifiers to UF acronyms;
#' - Harmonizing classification systems (CNAE, CBO, establishment types) across regulatory changes;
#' - Replacing invalid or placeholder values (e.g., “9999”, “000”) with nulls.
#' 
#' The dataset used corresponds to the BigQuery table basedosdados.br_me_rais.microdados_vinculos, with the latest update as of 2025-08-26 
#' (accessed November 14, 2025). The BD cleaning scripts are publicly available in the associated GitHub repository 
#' (https://github.com/rdahis/clean_RAIS)
#' 
#' Before proceeding to filtering and transformation, an initial inspection of the dataset was performed to check schema consistency, 
#' variable types, missingness patterns, and key descriptive distributions (e.g., number of vínculos by year, UF, and sector). 
#' It was possible to build some visualizations and graphs directly in Google BigQuery, using the Looker Studio. This confirmed the 
#' integrity of the BD-harmonized structure.
#' 
#' *2.1.3. Filter and transform the data:* 
#' 
#' This study constructs annual municipal indicators of formal employment for the Legal Amazon from the RAIS Vínculos microdata. All steps 
#' described below were implemented through SQL queries in BigQuery, following a transparent, reproducible workflow. For clarity, the data 
#' transformation process is organized into six components:
#'   
#' 3.1 Spatial filtering
#' Municipality  (id_municipio column in the original dataset): the variable id_municipio, already standardized to the 7-digit IBGE format by BD, 
#' was used as the primary geographic identifier.
#' State selection (sigla_uf column in the original dataset): only municipalities located in the Legal Amazon were included (AC, AM, AP, MA, MT, 
#' PA, RO, RR, TO).
#' 
#' 3.2 Temporal filtering (ano column in the original dataset): to maintain consistency in key variables, the analysis uses data from 1999 to 2024. 
#' Earlier years were excluded because they present limitations in occupational coding (CBO).
#' 
#' 3.3 Employment-status filtering (vinculo_ativo_3112 column in the original dataset): the variable identifies whether the vínculo was active on 
#' December 31 of each year. Only bonds with vinculo_ativo_3112 = 1 (meaning ‘active’) were retained to avoid duplicates and produce consistency.
#' 
#' 3.4 Occupational categories (cbo_2022 and cbo_1994 columns in the original dataset): RAIS uses two occupational classification systems, the 
#' CBO-1994 (used until 2002), and CBO-2002 (used thereafter). To harmonize them:
#' 1. When available, the 6-digit CBO-2002 code was used directly.
#' 2. When only 5-digit CBO-1994 was available, the code was converted to a 6-digit format by appending a trailing zero.
#' 3. The first number both codes are related to mayor's group of the Brazilian Classification of Occupations/Classificação Brasileira de Ocupações 
#' (CBO). The category “Agricultural, Forestry, and Fishing Worker” has the code 6. 
#' 4. To simplify analysis and produce a consistent agricultural–non-agricultural divide across the study period, a binary occupational variable was 
#' created:
#' * Agricultural workers (1): occupations whose unified CBO code begins with “6”
#' * Non-agricultural workers (0): all other occupations
#' 
#' 3.5 Wage processing, deflation, and wage groups 
#' Wage variable (valor_remuneracao_media column in the original dataset): the variable provides the worker’s annual average wage. Only positive, 
#' non-null wage values were kept.
#' Deflation: to allow temporal comparability, all wage values were deflated to 2024 Brazilian reais using the IPCA national consumer price index 
#' (IBGE).
#  For each year:
#' 
#' Deflator(t) = IPCA(2024) / IPCA(t)
#' 
#' The deflator table (made apart in a simple spreadsheet software) was merged with RAIS, and deflated wages were computed as:
#' 
#' valor_remuneracao_media_2024 = valor_remuneracao_media × Deflator(t)
#' 
#' Data on the IPCA historic series can be found in the IBGE website: 
#' https://www.ibge.gov.br/estatisticas/economicas/precos-e-custos/9256-indice-nacional-de-precos-ao-consumidor-amplo.html?=&t=downloads 
#' (accessed November 14, 2025)
#' 
# Deflated wages were categorized into six groups using the 2024 minimum wage (R$ 1,412.00) as the reference:
# - Up to 1 minimum wage
# - From 1 to 2 minimum wages
# - From 2 to 3 minimum wages
# - From 3 to 4 minimum wages
# - From 4 to 5 minimum wages
# - More than 5 minimum wages
#' 
#' These categories capture the wage distribution at the base of the income pyramid and facilitate the interpretation of a phenomenon that I want 
#' to observe, which is the policy of increasing the minimum wage over the last 25 years.
#' 
#' 3.6 Aggregation
#' A final SQL query aggregated the processed microdata into municipal-year indicators by:
#' * year,
#' * municipality (geocode),
#' * occupational group (agricultural vs. non-agricultural),
#' * wage group,
#' * and total number of formal workers in each subgroup.
#' 
#' Two SQL scripts were used in BigQuery to perform all filtering, harmonization, deflation, and aggregation steps. These scripts are documented 
#' below for reproducibility:
#' 
#' Query 1
#' 
# SELECT
# r.ano,
# r.sigla_uf,
# r.id_municipio,
# CASE
# WHEN r.cbo_2002 IS NOT NULL THEN CAST(r.cbo_2002 AS STRING)
# WHEN r.cbo_1994 IS NOT NULL THEN CONCAT(CAST(r.cbo_1994 AS STRING), '0')
# ELSE NULL
# END AS cbo_unified,
# r.vinculo_ativo_3112,
# r.valor_remuneracao_media,
# d.ipca_deflator,
# SAFE_MULTIPLY(r.valor_remuneracao_media, d.ipca_deflator) AS valor_remuneracao_media_2024
# FROM
# `basedosdados.br_me_rais.microdados_vinculos` AS r
# LEFT JOIN
# `amz-data-dissertation.social_employment_1.deflator_ipca_2024` AS d
# ON
# r.ano = d.year
# WHERE
# r.sigla_uf IN ('AC', 'AM', 'AP', 'MA', 'MT', 'PA', 'RO', 'RR', 'TO')
# AND r.vinculo_ativo_3112 = '1'
# AND r.valor_remuneracao_media IS NOT NULL
# AND r.valor_remuneracao_media > 0;
#' 
#' Query 2
#' 
# SELECT
# ano AS year,
# id_municipio AS geocode,
# CASE
# WHEN LEFT(LPAD(cbo_unified, 6, '0'), 1) = '6' THEN 1
# ELSE 0
# END AS workers_agricultural,
# CASE
# WHEN valor_remuneracao_media_2024 <= 1.00 * 1412.00 THEN 1
# WHEN valor_remuneracao_media_2024 > 1.00 * 1412.00 AND valor_remuneracao_media_2024 <= 2.00 * 1412.00 THEN 2
# WHEN valor_remuneracao_media_2024 > 2.00 * 1412.00 AND valor_remuneracao_media_2024 <= 3.00 * 1412.00 THEN 3
# WHEN valor_remuneracao_media_2024 > 3.00 * 1412.00 AND valor_remuneracao_media_2024 <= 4.00 * 1412.00 THEN 4
# WHEN valor_remuneracao_media_2024 > 4.00 * 1412.00 AND valor_remuneracao_media_2024 <= 5.00 * 1412.00 THEN 5
# ELSE 6
# END AS wage_code,
# COUNT(*) AS workers_total
# FROM
# `amz-data-dissertation.social_employment_1.rais_vinculos_deflat`
# GROUP BY
# year,
# geocode,
# workers_agricultural,
# wage_code
# ORDER BY
# year,
# geocode,
# workers_agricultural,
# wage_code;
#
#
#' *2.2. Construction of Labor Indicators Normalized by PIA* 
#'
#' *2.2.1. Understanding the data:* 
#'
#'To make formal employment indicators comparable across municipalities with very different population 
#'sizes and to analyze their temporal dynamics, we applied a population standardization using Municipal 
#'population data for persons aged 14 years or older were obtained from the Brazilian Institute of Geography 
#'and Statistics (IBGE) Census for the years 2000, 2010, and 2022. These data correspond to the Population 
#'in Active Age - População em Idade Ativa (PIA) in Portuguese -, the number of individuals within each 
#'municipality who are old enough to be considered part of the potential labor force.
#' 
#' *2.2.2. Import, and inspect the data* 
#' 
#' The data was obtained from the IBGE’s Sidra website. 
#' (https://sidra.ibge.gov.br/pesquisa/censo-demografico/demografico-2022/amostra-nupcialidade-e-familia)
#' 
#' *2.2.3. Clean and transform the data*
#' 
#' Because the census data were available exclusively for 2000, 2010, and 2022, to construct a complete annual 
#' series for the entire study period, it was necessary to interpolate and extrapolate population values for all 
#' intermediate and adjacent years. I applied simple linear interpolation for the years between census observations 
#' and linear extrapolation for the years before and after census observations.
#' 
#' To do the interpolation, I used the zoo package to estimate the annual change between known census points, producing 
#' a smooth annual trajectory for each municipality from 2000 to 2022. For the years 1999, 2023 and 2024, I computed the 
#' extraploation trends manually. The value for 1999 was obtained by backward linear extrapolation, and the values for
#' 2023 and 2024 were obtained by forward linear extrapolation.
#' 
#' 
population_pia_2000 <- read_excel("_data/raw-population-pia-censo2000.xlsx")
population_pia_2010 <- read_excel("_data/raw-population-pia-censo2010.xlsx")
population_pia_2022 <- read_excel("_data/raw-population-pia-censo2022.xlsx")

# Inspecting:

head(population_pia_2000)
head(population_pia_2010)
head(population_pia_2022) 

glimpse(population_pia_2000)
glimpse(population_pia_2010)
glimpse(population_pia_2022)

# Transforming:

population_pia_2022 <- population_pia_2022 %>%
  mutate(year = 2022) %>%
  rename(
    id = `Cód.`,
    total = `Total`
  )

population_pia_2010 <- population_pia_2010 %>%
  mutate(year = 2010)

population_pia_2000 <- population_pia_2000 %>%
  mutate(year = 2000)

population_pia <- bind_rows(
  population_pia_2000, 
  population_pia_2010, 
  population_pia_2022
) %>% 
  arrange(id, year)

# Inspecting:

see_pia <- head(population_pia, n=100)
print(see_pia, n=100)
glimpse(population_pia)

population_pia %>%
  count(id, year) %>%
  filter(n > 1)  

# Transforming:

# install.packages("zoo")
library(zoo)  

population_pia <- population_pia %>%
  arrange(id, year) %>%
  group_by(id, state) %>%
  complete(year = 2000:2022)

population_pia <- population_pia %>%
  group_by(id, state) %>%
  mutate(total_interp = zoo::na.approx(total, x = year, na.rm = FALSE))

population_pia <- population_pia %>%
  arrange(id, year) %>%
  group_by(id, state) %>%
  complete(year = 1999:2024)

population_pia  <- population_pia %>%
  group_by(id, state) %>%
  mutate(
    first_year        = min(year[!is.na(total_interp)]),
    second_year       = sort(year[!is.na(total_interp)])[2],
    last_year         = max(year[!is.na(total_interp)]),
    second_last_year  = sort(year[!is.na(total_interp)], decreasing = TRUE)[2],
    
    first_val         = total_interp[year == first_year][1],
    second_val        = total_interp[year == second_year][1],
    last_val          = total_interp[year == last_year][1],
    second_last_val   = total_interp[year == second_last_year][1],
    
    slope_start = (second_val      - first_val)      / (second_year      - first_year),
    slope_end   = (last_val        - second_last_val)/ (last_year        - second_last_year),
    
    Total_full = dplyr::case_when(
      year < first_year ~ first_val + slope_start * (year - first_year),  # linear backward
      year > last_year  ~ last_val  + slope_end   * (year - last_year),   # linear forward
      TRUE              ~ total_interp                                          # keep interior
    )
  ) %>%
  ungroup() %>%
  select(state, id, year, total_interp = Total_full)

population_pia <- population_pia %>%
  mutate(total = total_interp) %>%
  select(-total_interp)

# Inspecting:
  
ggplot(population_pia, aes(x = year, y = total, group = id)) +
  geom_line(alpha = 0.1) +
  theme_minimal()

ggplot(filter(population_pia, id == "1302603"),
       aes(x = year, y = total)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Population over time — ID 1302603")

population_pia %>% 
  filter(id == "1302603") %>% 
  print(n = 26)

#' *2.2.4. Save the data:* I 

write_xlsx(population_pia, "_data/data-population-pia-censo-interpolate.xlsx")

#' *2.3. Labor and Wage Indicators*

#' *2.3.1. Understanding the data*
# 
# I combined the RAIS and PIA datasets to analyze labor-market composition and wage structure at the municipality–year level. The two datasets 
# were merged using year and geocode as keys, producing a unified panel in which all employment variables could be normalized by the population 
# in active age (PIA). Normalizing employment by PIA allows for meaningful comparisons across municipalities with different population sizes and 
# demographic structures.
# 
# To construct the labor indicators, I used RAIS data on the total number of formal workers per municipality, the wage categories extract from 
# RAIS (six groups defined relative to the 2024 minimum wage), and the dummy variable identifying agricultural occupations. For parsimony and 
# analytical clarity, the six wage categories were aggregated into three broader groups (low, middle, and high wage).
# 
# All employment indicators were then computed as ratios between the relevant worker counts and the corresponding PIA value for each 
# municipality-year. These normalized measures form the basis for analyzing the scale and composition of local labor markets across the Amazon region.
# 
#' *2.3.2. Tidy and transform the data* 
# 
# First, I aggregated the RAIS data by municipality, year, and wage code, summing the number of total workers and the number of agricultural workers. 
# This step allowed me to eliminate the agricultural dummy variable and work directly with counts of agricultural and total employment within each wage 
# category.

rm(list = ls(all = TRUE))
getwd()

data_employment_rais <- read.csv ("_data/data_employment_rais")
data_population_pia <- read_excel("_data/data_population_ibge_censo_pia_interpolate.xlsx")

# Inspecting:

glimpse(data_employment_rais)
head(data_employment_rais, n=10)
data_employment_rais %>%
  count(workers_agricultural)

# Transforming: 

totals <- data_employment_rais %>%
  group_by(year, geocode, wage_code) %>%
  summarise(
    workers_total = sum(workers_total, na.rm = TRUE),
    .groups = "drop"
  )

totals_agricultural <- data_employment_rais %>%
  filter(workers_agricultural == 1) %>%     
  group_by(year, geocode, wage_code) %>%
  summarise(
    workers_agric = sum(workers_total, na.rm = TRUE),
    .groups = "drop"
  )

data_employment_rais_totals <- totals %>%
  left_join(totals_agricultural, by = c("year", "geocode", "wage_code")) %>%
  replace_na(list(workers_agric = 0L))

# Inspecting:

data_employment_rais_totals %>%
  filter(geocode == 1100015, year == 1999, wage_code == 3)

head(data_employment_rais_totals, n=10) %>% print(n=10)


# The wage ranges provided by RAIS were collapsed into three broader, substantive wage groups:
#   
# - Low wage: less then 1 minimum wage
# - Middle wage: from 1 to 4 minimum wage
# - High wage: more then 4 minimum wage
# 
# The wage ranges provided by RAIS were collapsed into three broader, substantive wage groups:
# 
# - Low wage: less then 1 minimum wage
# - Middle wage: from 1 to 4 minimum wage
# - High wage: more then 4 minimum wage
# 
# This procedure reduces dimensionality while preserving substantive meaning, facilitating merging with other municipal-year datasets. 
# I also constructed agricultural wage-range indicators, with the same wage-range grouping applied only to agricultural workers.

# Transforming:

rais_wage_groups <- data_employment_rais_totals %>%
  mutate(
    wage_group = case_when(
      wage_code %in% 1 ~ "low",
      wage_code %in% 2:4 ~ "mid",
      wage_code %in% 5:6 ~ "high"
    )
  ) %>%
  group_by(year, geocode, wage_group) %>%
  summarise(
    workers_group = sum(workers_total, na.rm = TRUE),
    .groups = "drop"
  )

data_employment_rais_wage_wide <- rais_wage_groups  %>%
  tidyr::pivot_wider(
    names_from = wage_group,
    values_from = workers_group,
    names_prefix = "workers_"
  ) %>%
  replace_na(list(
    workers_low = 0L,
    workers_mid = 0L,
    workers_high = 0L
  ))

rais_wage_groups_agri <- data_employment_rais_totals %>%
  mutate(
    wage_group = case_when(
      wage_code %in% 1 ~ "low",
      wage_code %in% 2:4 ~ "mid",
      wage_code %in% 5:6 ~ "high"
    )
  ) %>%
  group_by(year, geocode, wage_group) %>%
  summarise(
    workers_group = sum(workers_agric, na.rm = TRUE),
    .groups = "drop"
  )

data_employment_rais_wage_wide_agri <- rais_wage_groups_agri  %>%
  tidyr::pivot_wider(
    names_from = wage_group,
    values_from = workers_group,
    names_prefix = "workers_agri_"
  ) %>%
  replace_na(list(
    workers_low = 0L,
    workers_mid = 0L,
    workers_high = 0L
  ))

# Inspecting:

head(data_employment_rais_wage_wide, n=10) %>% print(n=10)
head(data_employment_rais_wage_wide_agri, n=10) %>% print(n=10)

glimpse(data_employment_rais_wage_wide)
glimpse(data_employment_rais_wage_wide_agri)

#' *2.3.3. Correct and standardize units* 
# 
# Using the ratio between the number of employees and the population in active age, I constructed two types of indicators. 
# The first set consists of employment rates by municipality, year, and wage group, calculated separately for all occupations and for agricultural 
# occupations. The second set consists of labor-market composition indicators, which measure the proportion of workers in each wage group within a 
# municipality-year, again distinguishing between all occupations and agricultural occupations.
# 
#' *2.3.3.1. Employment Rates (Intensity Indicators)* 
# 
# The employment intensity indicators normalize the number of formal workers from RAIS by the Population in Active Age (PIA). The indicator represent 
# the density of formal jobs per active-age resident. I construct these intensity indicator for the following wage-groups:
# 
# - total employment rate (emp_rate_total)
# - low wage employment rate (emp_rate_low)
# - medium wage employment rate (emp_rate_mid)
# - high wage employment rate (emp_rate_high)
# - agricultural employment rate (emp_rate_agric)
# - low wage agricultural employment rate (emp_rate_agri_low)
# - medium wage agricultural employment rate (emp_rate_agri_mid)
# - high wage agricultural employment rate (emp_rate_agri_high)
# 
#' *2.3.3.2. Labor-Market Composition (Share Indicators)* 
# 
# Labor-market composition indicators express the proportion of workers in each wage group relative to total formal employment. These shares 
# were calculated using a simple ratio. 
  
data_employment_rais_tidy <- data_employment_rais_totals %>%
  group_by(year, geocode) %>%
  summarise(
    workers_total = sum(workers_total, na.rm = TRUE),
    workers_agric = sum(workers_agric, na.rm = TRUE),
    .groups = "drop"
  )

# Inspecting:

head(data_employment_rais_totals, n=10) %>% print(n=10)
head(data_employment_rais_tidy, n=10) %>% print(n=10)

# Transforming:

data_employment_rais_full <- data_employment_rais_tidy %>%
  left_join(data_employment_rais_wage_wide,
            by = c("year", "geocode")) %>%
  left_join(data_employment_rais_wage_wide_agri,
            by = c("year", "geocode"))

data_employment_rais_full <- data_employment_rais_full %>%
  relocate(
    workers_low,
    workers_mid,
    workers_high,
    workers_agri_low,
    workers_agri_mid,
    workers_agri_high,
    .after = workers_agric
  )

# Inspecting:

head(data_employment_rais_full, n=10) %>% print(n=10)
glimpse(data_employment_rais_full)

data_population_pia %>% 
  filter(is.na(pia_total)) %>% 
  print(n=30)

data_population_pia <- data_population_pia %>%
  mutate(geocode = as.integer(id)) %>%
  select(geocode, year, pia_total = total)

# Inspecting:

glimpse(data_population_pia)

# Transforming:

data_labor_with_pia <- data_employment_rais_full %>%
  left_join(data_population_pia, by = c("year", "geocode"))

# Inspecting:

glimpse(data_labor_with_pia)
sum(is.na(data_labor_with_pia$pia_total))
data_labor_with_pia %>% summarise_all(~sum(is.na(.)))
colSums(is.na(data_labor_with_pia))[colSums(is.na(data_labor_with_pia)) > 0]
data_labor_with_pia$pia_total %>% filter(!complete.cases(.))

install.packages("naniar")
library(naniar)
vis_miss(data_labor_with_pia)

data_labor_with_pia %>% 
  filter(is.na(pia_total)) %>% 
  print(n=30)

# Transforming:

data_labor_with_pia <- data_labor_with_pia %>%
  mutate(
    emp_rate_total      = workers_total / pia_total,
    emp_rate_agric      = workers_agric / pia_total,
    emp_rate_low        = workers_low / pia_total,
    emp_rate_mid        = workers_mid / pia_total,
    emp_rate_high       = workers_high / pia_total,
    emp_rate_agri_low   = workers_agri_low / pia_total,
    emp_rate_agri_mid   = workers_agri_mid / pia_total,
    emp_rate_agri_high  = workers_agri_high / pia_total,
  )

data_labor_with_pia <- data_labor_with_pia %>%
  mutate(
    share_low_workers   = workers_low  / workers_total,
    share_mid_workers   = workers_mid  / workers_total,
    share_high_workers  = workers_high / workers_total,
    share_agri_low_workers  = workers_agri_low  / workers_agric,
    share_agri_mid_workers  = workers_agri_mid  / workers_agric,
    share_agri_high_workers = workers_agri_high / workers_agric,
    share_agri_workers  = workers_agric / workers_total
  )

# data_labor_with_pia <- data_labor_with_pia %>%
#   mutate(
#     emp_rate_total_z      = scale(emp_rate_total)[,1],
#     emp_rate_agric_z      = scale(emp_rate_agric)[,1],
#     
#     emp_rate_low_z        = scale(emp_rate_low)[,1],
#     emp_rate_mid_z        = scale(emp_rate_mid)[,1],
#     emp_rate_high_z       = scale(emp_rate_high)[,1],
#     
#     emp_rate_agri_low_z   = scale(emp_rate_agri_low)[,1],
#     emp_rate_agri_mid_z   = scale(emp_rate_agri_mid)[,1],
#     emp_rate_agri_high_z  = scale(emp_rate_agri_high)[,1],
#     
#     share_low_workers_z   = scale(share_low_workers)[,1],
#     share_mid_workers_z   = scale(share_mid_workers)[,1],
#     share_high_workers_z  = scale(share_high_workers)[,1],
#     
#     share_agri_low_workers_z      = scale(share_agri_low_workers)[,1],
#     share_agri_mid_workers_z      = scale(share_agri_mid_workers)[,1],
#     share_agri_high_workers_z     = scale(share_agri_high_workers)[,1],
#     
#     share_agri_workers_z  = scale(share_agri_workers)[,1]
#   )

# Inspecting:

glimpse(data_labor_with_pia)
view(data_labor_with_pia, n=100)

ggplot(data_labor_with_pia, aes(x = year, y = workers_total, group = geocode)) +
  geom_line(alpha = 0.15, color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Employment Rate Over Time — All Municipalities",
    x = "Year",
    y = "Employment Rate (workers/PIA)"
  )

#' *2.3.4. Save the data:* I 

write_xlsx(data_labor_with_pia, "_data/outcome_employment.xlsx")
write_csv(data_labor_with_pia, "_data/outcome_employment.csv")
write_rds(data_labor_with_pia, "_data/outcome_employment.rds")

#' *3. Cash transfers - Bolsa Família* 
#' 
#' 




 
# Other data:

#' - PIB-Munic: The Brazilian Institute of Geography and Statistics (IBGE) provides
#' data of Brazilian municipalities on the Gross Domestic Product (GDP) at current
#' prices, taxes, net of subsidies on products, gross value added, total and by economic
#' activity, and respective shares. Data is available from 2002 to 2018.
#' 
#' - PAM: The Municipal Agricultural Production (PAM) is an annual survey conducted
#' by IBGE which provides on the production of crops in Brazilian municipalities.
#' Output is only included in the dataset if the planted area occupies over 1 acre.
#' The data available has a yearly frequency and is available from 1974 to the present.
#' 
#' - PPM: The Municipal Livestock Production (PPM) is an annual survey conducted by
#' IBGE which provides data on the production of livestock in Brazilian municipalities,
#' such inventories (e.g:cattle, pigs and hogs) and production (e.g: milk, eggs, honey).
#' The data available has a yearly frequency and is available from 1974 to the present.
#' 
#' - SIGMINE: The National Mining Agency (ANM) provides data on mining legally activities
#' in Brazilian municipalities. The data includes information on location, status,
#' product being mined and area in square meters etc

