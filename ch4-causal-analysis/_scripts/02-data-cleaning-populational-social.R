
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(writexl)
library(ggplot2)
library(dplyr)
library(stringr)
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

write_csv(population_pia, "_data/outcome_population_pia.csv")

write_rds(population_pia, "_data/outcome_population_pia.rds")

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
data_population_pia <- read_excel("_data/data-population-pia-censo-interpolate.xlsx")
data_population_pea <- read_excel("_data/outcome_population_pea.xlsx")

# Inspecting:

glimpse(data_employment_rais)
head(data_employment_rais, n=10)
data_employment_rais %>%
  count(workers_agricultural)


range(data_employment_rais$year, na.rm = TRUE)

library(dplyr)

data_employment_rais %>%
  group_by(workers_agricultural) %>%
  summarise(
    workers_total = sum(workers_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    share = workers_total / sum(workers_total),
    share_pct = share * 100
  )


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

# Join with PIA data:

data_population_pia

data_population_pia %>% 
  filter(is.na(total)) %>% 
  print(n=30)

data_population_pia <- data_population_pia %>%
#  mutate(geocode = as.integer(id)) %>%
  rename (pia = total) %>%
  select(geocode, year, pia)

data_population_pia

# Prepare PEA (economically active population) the same way, for a second
# normalization. The file already carries geocode, year, pea.
data_population_pea <- data_population_pea %>%
  mutate(geocode = as.integer(geocode)) %>%
  select(geocode, year, pea)

data_population_pea

# Inspecting:

glimpse(data_population_pia)
glimpse(data_population_pea)

# Transforming:

data_labor_with_pia <- data_employment_rais_full %>%
  left_join(data_population_pia, by = c("year", "geocode")) %>%
  left_join(data_population_pea, by = c("year", "geocode"))

print(data_labor_with_pia, n = 500)



# Inspecting:

glimpse(data_labor_with_pia)
sum(is.na(data_labor_with_pia$pia_total))
data_labor_with_pia %>% summarise_all(~sum(is.na(.)))
colSums(is.na(data_labor_with_pia))[colSums(is.na(data_labor_with_pia)) > 0]
data_labor_with_pia$pia %>% filter(!complete.cases(.))

install.packages("naniar")
library(naniar)
vis_miss(data_labor_with_pia)

data_labor_with_pia %>% 
  filter(is.na(pia)) %>% 
  print(n=30)

# Transforming:

# Employment rates normalized by PIA (working-age population)
data_labor_with_pia <- data_labor_with_pia %>%
  mutate(
    emp_rate_total_pia      = workers_total / pia,
    emp_rate_agric_pia      = workers_agric / pia,
    emp_rate_low_pia        = workers_low / pia,
    emp_rate_mid_pia        = workers_mid / pia,
    emp_rate_high_pia       = workers_high / pia,
    emp_rate_agri_low_pia   = workers_agri_low / pia,
    emp_rate_agri_mid_pia   = workers_agri_mid / pia,
    emp_rate_agri_high_pia  = workers_agri_high / pia
  )

# Employment rates normalized by PEA (economically active population)
data_labor_with_pia <- data_labor_with_pia %>%
  mutate(
    emp_rate_total_pea      = workers_total / pea,
    emp_rate_agric_pea      = workers_agric / pea,
    emp_rate_low_pea        = workers_low / pea,
    emp_rate_mid_pea        = workers_mid / pea,
    emp_rate_high_pea       = workers_high / pea,
    emp_rate_agri_low_pea   = workers_agri_low / pea,
    emp_rate_agri_mid_pea   = workers_agri_mid / pea,
    emp_rate_agri_high_pea  = workers_agri_high / pea
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

library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(dplyr)
library(stringr)

#' *2.4. Cash transfers (Bosa Família program)* 
#'
#' *2.4.1. Understand the data:* The cash transfer data used in this study comes from 
#' administrative records of the Bolsa Família program published by Brazil’s 
#' **Ministério do Desenvolvimento e Assistência Social, Família e Combate à Fome (MDS)** 
#' in the **Dados Abertos** portal of the Brazilian Federal Government. (last updated 
#' in March 2026, available at <https://dados.gov.br/dados/conjuntos-dados/bolsa-familia>). 

#' The raw data is organized as monthly municipality-level files (one file per year), 
#' with CSV files for each year from 2004 (the first year of the policy) to 2026. 

#' Because the final dataset is a municipality-year panel, I select a **reference month** to 
#' represent each year. I conduct stability checks on candidate months (see below), and end 
#' up with **February** and **June** as alternative reference months for robustness. I also 
#' restrict the panel to **2004–2019**, excluding 2020 onward due to the COVID-19 period and 
#' program substitution dynamics that generate structural discontinuities in the Bolsa Família 
#' series.

#' The standard structure includes:
#' - `ibge`: IBGE municipality code
#' - `anomes`: reference year-month (format `YYYYMM`)
#' - `qtd_familias_beneficiarias_bolsa_familia`: number of beneficiary families in the municipality-month
#' - `valor_repassado_bolsa_familia`: total value transferred to beneficiary families in the municipality-month

#' *2.4.2. Import and inspect the data:* I store the raw files locally as CSV files with the naming 
#' convention `bf-YYYY.txt`. I read all files into R using a function that applies 
#' consistent parsing rules and adds a `source_file` variable to track the origin of each observation. 

path <- "_data/raw_mds_bolsa_familia" 

files <- list.files(
  path = path,
  pattern = "^bf-\\d{4}\\.txt$",
  full.names = TRUE
)
print(files)

read_bf <- function(file) {
  readr::read_csv(
    file,
    col_types = cols(
      ibge = col_double(),
      anomes = col_double(),
      qtd_familias_beneficiarias_bolsa_familia = col_double(),
      valor_repassado_bolsa_familia = col_double()
    )
  ) %>%
    mutate(
      source_file = basename(file)
    )
}



bf_all <- map_dfr(files, read_bf)

head(bf_all)
str(bf_all)
tail(bf_all)
glimpse(bf_all)

#' *2.4.2.1. Quality assurance checks* I apply a set of integrity checks to ensure that 
#' the two key variables (**beneficiary families** and **total transfers**) are consistently 
#' recorded across yearly files. These include missingness, zeros, negative values, logical 
#' inconsistencies (positive families with zero transfers), and duplicate municipality-month keys.

#' The quality assurance checks reveal that the data is generally complete and 
#' consistent across years, with no negative values or duplicate keys. 
#' It also reveals that the years of 2020 and 2021 have a high number of zero 
#' transfers despite positive beneficiary families, which is consistent 
#' with the known disruption of Bolsa Família payments during the COVID-19 pandemic 
#' and the introduction of emergency cash-transfer programs. 

qa_summary <- bf_all %>%
  group_by(source_file) %>%
  summarise(
    n_rows = n(),
    na_families = sum(is.na(qtd_familias_beneficiarias_bolsa_familia)),
    na_transfers = sum(is.na(valor_repassado_bolsa_familia)),
    zero_families = sum(qtd_familias_beneficiarias_bolsa_familia == 0, na.rm = TRUE),
    zero_transfers = sum(valor_repassado_bolsa_familia == 0, na.rm = TRUE),
    neg_families = sum(qtd_familias_beneficiarias_bolsa_familia < 0, na.rm = TRUE),
    neg_transfers = sum(valor_repassado_bolsa_familia < 0, na.rm = TRUE),
    transfers_pos_families_zero =
      sum(valor_repassado_bolsa_familia > 0 &
            qtd_familias_beneficiarias_bolsa_familia == 0, na.rm = TRUE),
    families_pos_transfers_zero =
      sum(valor_repassado_bolsa_familia > 0 &
            qtd_familias_beneficiarias_bolsa_familia == 0, na.rm = TRUE    ),
    duplicated_ibge_anomes = sum(duplicated(paste(ibge, anomes))),
    .groups = "drop"
  )

print(qa_summary, n = 22)

# Additional checks:

#' Completennes of month variable: checking if all `anomes` values correspond to 
#' valid months (01 to 12) and summarizing the distribution of months across files 
#' to detect any irregularities or patterns in the timing of data collection.

month_coverage <- bf_all %>%
  mutate(
    year = substr(anomes, 1, 4),
    month = substr(anomes, 5, 6)
  ) %>%
  group_by(source_file) %>%
  summarise(
    n_months = n_distinct(anomes),
    min_anomes = min(anomes, na.rm = TRUE),
    max_anomes = max(anomes, na.rm = TRUE),
    invalid_months =
      sum(!(month %in% sprintf("%02d", 1:12))),
    .groups = "drop"
  )
print(month_coverage, n = 22)

#' Distributional diagnostics for the implied average transfer per family 
#' (`valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa familia`) 
#' to detect anomalous tails that may reflect reporting artifacts or program regime changes.

valor_por_familia <- bf_all %>%
  filter(
    qtd_familias_beneficiarias_bolsa_familia > 0
  ) %>%
  mutate(
    valor_por_familia =
      valor_repassado_bolsa_familia /
      qtd_familias_beneficiarias_bolsa_familia
  ) %>%
  group_by(source_file) %>%
  summarise(
    p1 = quantile(valor_por_familia, 0.01, na.rm = TRUE),
    median = median(valor_por_familia, na.rm = TRUE),
    p99 = quantile(valor_por_familia, 0.99, na.rm = TRUE),
    max = max(valor_por_familia, na.rm = TRUE),
    .groups = "drop"
  )

print(valor_por_familia, n = 22)

#' **2.4.2.2. Choosing reference month**: At first, I choose five reference month candidates: February as a 
#' representative of the beginning of the year, June and July as the middle months, and September 
#' and November as representatives of the end of the year. I do not choose January neither December 
#' because they will lack some adjacent months for the stability checks in the first and final years 
#' of the series (2004 and 2019).

#' I applied three criteria to select the reference month among the candidates: 

#' 1. **Stability**: I calculate the relative month-to-month change in
#' both `beneficiary families` and `average transfer per family` for each municipality-year, 
#' comparing candidate months to their adjacent months (month-1 and month+1).

#' 2. **Centrality**: I calculate the relative deviation of candidate reference months to the 
#' yearly central tendencies of both `beneficiary families` and `average transfer per family` 
#' for each municipality-year.

#' 3. **Completeness**: I calculate the share of missing and zero values in both 
#' `beneficiary families` and `average transfer per family` for each candidate month.

#' Based on these criteria, I select June as the primary reference month for the panel, 
#' with February as an alternative reference month for robustness checks.

#' The choice of June is justified by its consistently lower median instability and smaller 
#' share of large month-to-month jumps across municipalities and years, as well as its proximity 
#' to the yearly central tendencies and higher data completeness compared to other candidate months.

candidate_months <- c(2, 6, 7, 9, 11)

bf_m <- bf_all %>%
  filter(anomes >= 200401 & anomes <= 201912) %>%
  mutate(
    year  = as.integer(substr(anomes, 1, 4)),
    month = as.integer(substr(anomes, 5, 6)),
    valor_por_familia = if_else(
      qtd_familias_beneficiarias_bolsa_familia > 0,
      valor_repassado_bolsa_familia /
        qtd_familias_beneficiarias_bolsa_familia,
      NA_real_
    )
  )

tail(bf_m)

stability <- bf_m %>%
  group_by(ibge, year) %>%
  arrange(month) %>%
  mutate(
    fam_lag = lag(qtd_familias_beneficiarias_bolsa_familia),
    val_lag = lag(valor_por_familia),
    lag_month = lag(month)
  ) %>%
  ungroup() %>%
  filter(month %in% candidate_months) %>%
  # keep only true adjacent comparisons (month-1)
  filter(lag_month == month - 1) %>%
  transmute(
    ibge, year, month,
    fam_instability = abs(qtd_familias_beneficiarias_bolsa_familia - fam_lag) / fam_lag,
    val_instability = abs(valor_por_familia - val_lag) / val_lag
  )

stability %>% count(month, sort = TRUE)

stability_month <- stability %>%
  group_by(month) %>%
  summarise(
    fam_instability_median = median(fam_instability, na.rm = TRUE),
    fam_instability_p90    = quantile(fam_instability, 0.90, na.rm = TRUE),
    val_instability_median = median(val_instability, na.rm = TRUE),
    val_instability_p90    = quantile(val_instability, 0.90, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

stability_month

stability_flags <- stability %>%
  group_by(month) %>%
  summarise(
    fam_jump_20 = mean(fam_instability > 0.20, na.rm = TRUE),
    val_jump_20 = mean(val_instability > 0.20, na.rm = TRUE),
    fam_jump_50 = mean(fam_instability > 0.50, na.rm = TRUE),
    val_jump_50 = mean(val_instability > 0.50, na.rm = TRUE),
    .groups = "drop"
  )

print (stability_flags)

centrality <- bf_m %>%
  group_by(ibge, year) %>%
  mutate(
    fam_year_med = median(qtd_familias_beneficiarias_bolsa_familia,
                          na.rm = TRUE),
    val_year_med = median(valor_por_familia, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(month %in% candidate_months) %>%
  group_by(month) %>%
  summarise(
    fam_distance =
      mean(abs(qtd_familias_beneficiarias_bolsa_familia - fam_year_med) /
             fam_year_med, na.rm = TRUE),
    val_distance =
      mean(abs(valor_por_familia - val_year_med) /
             val_year_med, na.rm = TRUE),
    .groups = "drop"
  )

print (centrality)

completeness <- bf_m %>%
  filter(month %in% candidate_months) %>%
  group_by(month) %>%
  summarise(
    na_fam = mean(is.na(qtd_familias_beneficiarias_bolsa_familia)),
    zero_fam = mean(qtd_familias_beneficiarias_bolsa_familia == 0,
                    na.rm = TRUE),
    na_val = mean(is.na(valor_por_familia)),
    .groups = "drop"
  )

print (completeness) 

decision_table <- stability_month %>%
  left_join(stability_flags, by = "month") %>%
  left_join(centrality, by = "month") %>%
  left_join(completeness, by = "month")

print(decision_table)

view(decision_table)

#' *2.4.3. Clean and transform the data:* 

#' To build a municipality-year panel, I extract the year and month from `anomes`, 
#' filter to the analysis window (2004–2019), and select February and June as reference months. 
#' I then construct:

#' - `bf_beneficiary_families`: coverage
#' - `bf_total_transfers_brl`: state effort (nominal BRL)
#' - `bf_avg_transfer_brl`: generosity (nominal BRL per beneficiary family)

rm(list = ls())

path <- "_data/raw_mds_bolsa_familia"

bf_files <- list.files(
  path = path,
  pattern = "^bf-\\d{4}\\.txt$",
  full.names = TRUE
)

print(bf_files)

bf_read <- function(file) {
  readr::read_csv(
    file,
    col_types = cols(
      ibge = col_double(),
      anomes = col_double(),
      qtd_familias_beneficiarias_bolsa_familia = col_double(),
      valor_repassado_bolsa_familia = col_double()
    )
  ) %>%
    mutate(
      source_file = basename(file)
    )
}

data_bf_all <- map_dfr(bf_files, bf_read)

head(data_bf_all)
str(data_bf_all)
tail(data_bf_all)
glimpse(data_bf_all)

data_bf_panel <- data_bf_all %>%
  mutate(
    year  = as.integer(str_sub(as.character(anomes), 1, 4)),
    month = as.integer(str_sub(as.character(anomes), 5, 6))
  ) %>%
  filter(year >= 2004, year <= 2019, month %in% c(2, 6)) %>%
  transmute(
    geocode = as.integer(ibge),
    year = year,
    bf_ref_month = month,
    bf_beneficiary_families = qtd_familias_beneficiarias_bolsa_familia,
    bf_total_transfers = valor_repassado_bolsa_familia,
    bf_avg_transfer = if_else(
      bf_beneficiary_families > 0,
      bf_total_transfers / bf_beneficiary_families,
      NA_real_
    )
  ) %>%
  mutate(
    bf_ref_month = recode(bf_ref_month, `2` = "Feb", `6` = "Jun")
  ) %>%
  arrange(bf_ref_month, year, geocode)

print(data_bf_panel, n = 50)

bf_panel_feb <- data_bf_panel %>% filter(bf_ref_month == "Feb")
bf_panel_jun <- data_bf_panel %>% filter(bf_ref_month == "Jun")

print(bf_panel_feb, n = 50)
print(bf_panel_jun, n = 50)

str(data_bf_panel)
str(bf_panel_feb)
str(bf_panel_jun)

data_bf_panel %>%
  count(bf_ref_month, geocode, year) %>%
  filter(n > 1)

data_bf_panel %>%
  group_by(bf_ref_month, year) %>%
  summarise(
    n = n(),
    na_avg = mean(is.na(bf_avg_transfer)),
    zero_total = mean(bf_total_transfers == 0, na.rm = TRUE),
    .groups = "drop"
  )

bf_panel_feb %>% 
  group_by(bf_ref_month, year) %>%
  summarise(
    n = n(),
    na_avg = mean(is.na(bf_avg_transfer)),
    zero_total = mean(bf_total_transfers == 0, na.rm = TRUE),
    .groups = "drop"
  )

bf_panel_jun %>% 
  group_by(bf_ref_month, year) %>%
  summarise(
    n = n(),
    na_avg = mean(is.na(bf_avg_transfer)),
    zero_total = mean(bf_total_transfers == 0, na.rm = TRUE),
    .groups = "drop"
  )

#' *2.4.4. Correct and standardize units* 

#' *2.4.4.1. Deflation to constant prices (base 2024):* To compare monetary values over time, 
#' I deflate Bolsa Família transfers using an IPCA-based deflator with 
#' *2024 as the base year* (deflator multiplier). I apply the deflator to both 
#' total transfers and the average transfer per family.

#' The *deflator data is the same that I use to deflate labor and wage indicators*. 
#' It is stored in a CSV file with the following structure:
#' - `year`: the year of the deflator value
#' - `ipca_index`: the IPCA index value for that year
#' - `ipca_deflator`: the deflator multiplier to convert nominal values to 2024 BRL 
#' (calculated as `ipca_index_2024 / ipca_index_year`).

#' I read the deflator data into R and merge it with the Bolsa Família panel by year, 
#' then create new variables for the deflated total transfers and average transfer per family.

deflator <- read_csv("_data/deflator_ipca_2024.csv",
                     col_types = cols(
                       year = col_integer(),
                       ipca_index = col_double(),
                       ipca_deflator = col_double()
                     ))

deflator

data_bf_panel_deflate <- data_bf_panel %>%
  left_join(deflator %>% select(year, ipca_deflator), by = "year") %>%
  mutate(
    bf_total_transfers_2024 = bf_total_transfers * ipca_deflator,
    bf_avg_transfer_2024    = bf_avg_transfer * ipca_deflator
  )

print(data_bf_panel_deflate, n = 50)

data_bf_panel_deflate %>%
  summarise(
    n = n(),
    missing_deflator = sum(is.na(ipca_deflator)),
    years_min = min(year),
    years_max = max(year)
  )

data_bf_panel_deflate %>%
  group_by(bf_ref_month, year) %>%
  summarise(
    n = n(),
    na_avg_2024 = mean(is.na(bf_avg_transfer_2024)),
    zero_total_2024 = mean(bf_total_transfers_2024 == 0, na.rm = TRUE),
    .groups = "drop"
  )

#' *2.4.5. Check and validate the data*: I conduct a final set of checks to confirm 
#' that the the dataset is consistent with known patterns of the Bolsa Família program. 
#' I examine the temporal trends of total transfers, beneficiary families, and average 
#' transfer per family across the two reference months to ensure that they align with 
#' expected trajectories based on program history and previous research.

#' The plots of the annual trajectories of the three key indicators are consistent 
#' with the known expansion and stabilization phases of the Bolsa Família program 
#' over the 2004–2019 period as shown by @souza2022 .

bf_effort_yearly <- data_bf_panel_deflate %>%
  group_by(bf_ref_month, year) %>%
  summarise(
    real_total_transfers =
      sum(bf_total_transfers_2024, na.rm = TRUE),
    total_beneficiary_families =
      sum(bf_beneficiary_families, na.rm = TRUE),
    avg_transfer_2024 =
      real_total_transfers / total_beneficiary_families,
    .groups = "drop"
  )
print(bf_effort_yearly, n = 50)

ggplot(bf_effort_yearly,
       aes(x = year,
           y = real_total_transfers,
           linetype = bf_ref_month)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Total real Bolsa Família transfers (2024 BRL)",
    x = "Year",
    y = "Total transfers (2024 BRL)",
    linetype = "Reference month"
  ) +
  theme_minimal()

ggplot(bf_effort_yearly,
       aes(x = year,
           y = total_beneficiary_families,
           linetype = bf_ref_month)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Total Bolsa Família beneficiary families",
    x = "Year",
    y = "Total beneficiary families",
    linetype = "Reference month"
  ) +
  theme_minimal()

ggplot(bf_effort_yearly,
       aes(x = year,
           y = avg_transfer_2024,
           linetype = bf_ref_month)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Bolsa Família transfer per beneficiary family (2024 BRL)",
    subtitle = "Yearly averages using February and June as reference months",
    x = "Year",
    y = "Average transfer per family (2024 BRL)",
    linetype = "Reference month"
  ) +
  theme_minimal()

#' *Standartization*:

library(dplyr)
library(tidyr)
library(stringr)

data_bf_quota <- read_xlsx("_data/raw_mds_bolsa_familia/bf-quotas.xlsx")

data_bf_quota_panel <- data_bf_quota %>%
  pivot_longer(
    cols = starts_with ("quota_"),
    names_to = "year",
    values_to = "bf_quota"
  ) %>% 
  mutate(
    year = str_remove(year, "quota_") |> as.integer(),
    geocode = as.integer(geocode),
    bf_quota = as.numeric(bf_quota)
  ) %>%
  arrange(geocode, year)

view (data_bf_quota_panel, n = 100)  

view(data_bf_panel_deflate, n = 100)

#' I reshaped the municipal quota dataset into a municipality-year panel and then 
#' merged it with the deflated Bolsa Família panel. Before merging, I harmonized 
#' the municipality identifiers across the two sources.
#' Before the merge, I identified an inconsistency in the geographic identifiers: 
#' the quota dataset used **7-digit IBGE municipality codes**, while the deflated 
#' Bolsa Família panel used **6-digit codes**. To make the two datasets compatible, 
#' I dropped the last digit of the 7-digit quota geocode, creating a harmonized 6-digit 
#' identifier (`geocode_6`). I then created the same harmonized field in the Bolsa 
#' Família panel and merged the two datasets by **municipality** and **year**.

data_bf_quota_panel <- data_bf_quota_panel %>%
  filter(geocode != 1)

data_bf_quota_panel <- data_bf_quota_panel %>%
  mutate(
    geocode_6 = geocode %/% 10
  )

data_bf_panel_deflate <- data_bf_panel_deflate %>%
  mutate(
    geocode_6 = geocode
  )

data_bf_panel_final <- data_bf_panel_deflate %>% 
  left_join(data_bf_quota_panel, by = c("geocode_6", "year"))

view (data_bf_panel_final)

# Inspecting:

data_bf_panel_final %>%
  summarise(
    min_code = min(geocode_6),
    max_code = max(geocode_6)
  )

data_bf_panel_final %>%
  summarise(
    missing_quota = sum(is.na(bf_quota)),
    share_missing = mean(is.na(bf_quota))
  )

str(data_bf_panel_final)

data_bf_panel_deflate %>%
  filter(geocode == 150680) %>%
  print(n = 50)

id_santarem <- 150680
id_mojui    <- 150475

bf_check <- data_bf_panel_final %>%
  filter(geocode_6 %in% c(id_santarem, id_mojui),
         bf_ref_month == "Jun") %>%
  group_by(year) %>% 
  print(n=50)

library(dplyr)

str(data_bf_panel_final)
str(data_population_pea_panel)

# Transforming

outcome_bf <- data_bf_panel_final %>%
  filter(bf_ref_month == "Jun") %>%
  rename(
    geocode = geocode.y,
    bf_families_n = bf_beneficiary_families,
    bf_transfers_total_brl_2024 = bf_total_transfers_2024,
    bf_transfers_avg_brl_2024 = bf_avg_transfer_2024
  ) %>%
  select(
    geocode,
    year,
    bf_families_n,
    bf_transfers_total_brl_2024,
    # Generosity
    bf_transfers_avg_brl_2024,
    bf_quota
  ) %>%
  left_join(
    data_population_pea_panel %>% 
      select(geocode, year, pea), 
    by = c("geocode", "year")) %>%
  mutate(
    # Coverage (ratio)
    bf_families_quota_ratio = if_else(
      !is.na(bf_quota) & bf_quota > 0,
      bf_families_n / bf_quota,
      NA_real_
    ),
    # Effort per PEA (in 2024 BRL)
    bf_transfers_pea_brl_2024 = if_else(
      !is.na(pea) & pea > 0,
      bf_transfers_total_brl_2024 / pea,
      NA_real_
    ),
    
    # Effort (in 2024 BRL)
    bf_transfers_quota_brl_2024 = if_else(
      !is.na(bf_quota) & bf_quota > 0,
      bf_transfers_total_brl_2024 / bf_quota,
      NA_real_
    )
  ) %>% 
  arrange(geocode, 
          year)


outcome_bf
glimpse(outcome_bf)
head(outcome_bf)
tail(outcome_bf, n = 100) %>% 
  print(n = 100)
str(outcome_bf)
str(data_bf_panel_final)


outcome_bf %>%
  summarise(
    missing_effort_per_pea = sum(is.na(bf_transfers_pea_brl_2024)),
    missing_coverage_ratio = sum(is.na(bf_families_quota_ratio)),
    missing_generosity = sum(is.na(bf_transfers_avg_brl_2024)),
    missing_effort_per_quota = sum(is.na(bf_transfers_quota_brl_2024))
  )


outcome_bf %>%
  summarise(
    effort_per_pea_p50 = median(bf_transfers_pea_brl_2024, na.rm = TRUE),
    coverage_ratio_p50 = median(bf_families_quota_ratio, na.rm = TRUE),
    generosity_p50 = median(bf_transfers_avg_brl_2024, na.rm = TRUE),
    effort_per_quota_p50 = median(bf_transfers_quota_brl_2024, na.rm = TRUE)
  )

outcome_bf  %>%
  summarise(
    missing_pea = sum(is.na(pea)),
    share_missing = mean(is.na(pea))
  )

outcome_bf %>%
  filter(is.na(pea)) %>%
  count(geocode, sort = TRUE)

range(outcome_bf$year)



outcome_bf_feb <- data_bf_panel_final %>%
  filter(bf_ref_month == "Feb") %>%
  rename(
    geocode = geocode.y,
    bf_families_n = bf_beneficiary_families,
    bf_transfers_total_brl_2024 = bf_total_transfers_2024,
    bf_transfers_avg_brl_2024 = bf_avg_transfer_2024
  ) %>%
  select(
    geocode,
    year,
    bf_families_n,
    bf_transfers_total_brl_2024,
    # Generosity
    bf_transfers_avg_brl_2024,
    bf_quota
  ) %>%
  left_join(
    data_population_pea_panel %>% 
      select(geocode, year, pea), 
    by = c("geocode", "year")) %>%
  mutate(
    # Coverage (ratio)
    bf_families_quota_ratio = if_else(
      !is.na(bf_quota) & bf_quota > 0,
      bf_families_n / bf_quota,
      NA_real_
    ),
    # Effort per PEA (in 2024 BRL)
    bf_transfers_pea_brl_2024 = if_else(
      !is.na(pea) & pea > 0,
      bf_transfers_total_brl_2024 / pea,
      NA_real_
    ),
    
    # Effort (in 2024 BRL)
    bf_transfers_quota_brl_2024 = if_else(
      !is.na(bf_quota) & bf_quota > 0,
      bf_transfers_total_brl_2024 / bf_quota,
      NA_real_
    )
  ) %>% 
  arrange(geocode, 
          year)

glimpse(outcome_bf_feb)
data_bf_panel_final

head(outcome_bf_feb)
head(outcome_bf)

library(writexl)

write_csv(outcome_bf, "_data/outcome_bf.csv")
write_csv(outcome_bf_feb, "_data/outcome_bf_feb.csv")

write_xlsx(outcome_bf, "_data/outcome_bf.xlsx")
write_xlsx(outcome_bf_feb, "_data/outcome_bf_feb.xlsx")

write_rds(outcome_bf, "_data/outcome_bf.rds")
write_rds(outcome_bf_feb, "_data/outcome_bf_feb.rds")


#' **Labor market indicators (PEA and informality)**

library(readxl)
library(zoo)  


population_pea_2000 <- read_excel("_data/raw_censo_labor_population/pea-2000.xlsx")
population_pea_2010 <- read_excel("_data/raw_censo_labor_population/pea-2010.xlsx")
population_pea_2022 <- read_excel("_data/raw_censo_labor_population/pea-2022.xlsx")

population_informal_2000 <- read_excel("_data/raw_censo_labor_population/informality-2000.xlsx")
population_informal_2010 <- read_excel("_data/raw_censo_labor_population/informality-2010.xlsx")
population_informal_2022 <- read_excel("_data/raw_censo_labor_population/informality-2022.xlsx")

# Inspecting:

head(population_pea_2000)
head(population_pea_2010)
head(population_pea_2022) 

glimpse(population_pea_2000)
glimpse(population_pea_2010)
glimpse(population_pea_2022)

head(population_informal_2000)
head(population_informal_2010)
head(population_informal_2022)

glimpse(population_informal_2000)
glimpse(population_informal_2010)
glimpse(population_informal_2022)

# Transforming:

population_pea_2022 <- population_pea_2022 %>%
  mutate(
    year    = 2022,
    geocode = as.integer(gsub("[^0-9]", "", geocode))
  )
population_pea_2010 <- population_pea_2010 %>%
  mutate(year = 2010)
population_pea_2000 <- population_pea_2000 %>%
  mutate(year = 2000)

population_informal_2022 <- population_informal_2022 %>%
  mutate(
    year = 2022,
    geocode = as.integer(gsub("[^0-9]", "", geocode))
  )

population_informal_2010 <- population_informal_2010 %>%
  mutate(
    year = 2010,
    geocode = as.integer(gsub("[^0-9]", "", geocode))
  )

population_informal_2000 <- population_informal_2000 %>%
  mutate(
    year = 2000,
    geocode = as.integer(gsub("[^0-9]", "", geocode))
  )

population_pea <- bind_rows(
  population_pea_2000, 
  population_pea_2010, 
  population_pea_2022
) %>% 
  arrange(geocode, year)

head(population_pea)
glimpse(population_pea)

population_informal <- bind_rows(
  population_informal_2000,
  population_informal_2010,
  population_informal_2022
) %>%
  arrange(geocode, year)

head(population_informal)

glimpse(population_informal)  

print(population_pea, n=100)
glimpse(population_pea)

print(population_informal, n=100)
glimpse(population_informal)

population_pea %>%
  count(geocode, year) %>%
  filter(n > 1)  

population_informal %>%
  count(geocode, year) %>%
  filter(n > 1)  

# Interpolation

# Serching for municipalities that were create after 2000 and before 2022, which will
# have missing values in the first or last year of the series.

population_pea %>%
  mutate(geocode = as.numeric(geocode)) %>%
  filter(geocode != 1) %>%
  count(geocode) %>%
  count(n)

population_pea %>%
  mutate(geocode = as.numeric(geocode)) %>%
  filter(geocode != 1) %>%
  distinct(geocode, year) %>%
  count(geocode) %>%
  filter(n < 3)

population_informal %>%
  mutate(geocode = as.numeric(geocode)) %>%
  filter(geocode != 1) %>%
  distinct(geocode, year) %>%
  count(geocode) %>%
  filter(n < 3)

# Dealing with Mojuí dos Campos

# Mojuí PEA

population_pea %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year)

population_pea_2022 <- population_pea %>%
  filter(year == 2022) %>%
  select(geocode, pea)

pea_pair <- population_pea_2022 %>%
  filter(geocode %in% c(1506807, 1504752))

share_mojui <- pea_pair %>%
  summarise(
    share = pea[geocode == 1504752] /
      (pea[geocode == 1504752] + pea[geocode == 1506807])
  ) %>%
  pull(share)

population_pea_mj_adjusted <- population_pea %>%
  mutate(geocode = as.numeric(geocode)) %>%
  bind_rows(
    tibble(
      geocode = 1504752,
      pea = NA_real_,
      year = c(2000, 2010)
    )
  ) %>%
  distinct(geocode, year, .keep_all = TRUE) %>%
  left_join(
    population_pea %>%
      mutate(geocode = as.numeric(geocode)) %>%
      filter(geocode == 1506807) %>%
      select(year, pea_santarem = pea),
    by = "year"
  ) %>%
  mutate(
    pea = case_when(
      geocode == 1504752 & year < 2013 ~ pea_santarem * share_mojui,
      geocode == 1506807 & year < 2013 ~ pea_santarem * (1 - share_mojui),
      TRUE ~ pea
    )
  ) %>%
  select(-pea_santarem)


population_pea_mj_adjusted %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year)

# Mojuí informality

population_informal %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year)

population_informal_mj_adjusted <- population_informal %>%
  mutate(geocode = as.numeric(geocode)) %>%
  bind_rows(
    tibble(
      geocode = 1504752,
      munic = "Mojuí dos Campos (PA)",
      Formal = NA_real_,
      Informal = NA_real_,
      year = c(2000, 2010)
    )
  ) %>%
  distinct(geocode, year, .keep_all = TRUE) %>%
  left_join(
    population_informal %>%
      mutate(geocode = as.numeric(geocode)) %>%
      filter(geocode == 1506807) %>%
      select(
        year,
        informal_santarem = Informal,
        formal_santarem = Formal
      ),
    by = "year"
  ) %>%
  mutate(
    Informal = case_when(
      geocode == 1504752 & year < 2013 ~ informal_santarem,
      TRUE ~ Informal
    ),
    Formal = case_when(
      geocode == 1504752 & year < 2013 ~ formal_santarem,
      TRUE ~ Formal
    )
  ) %>%
  select(-informal_santarem, -formal_santarem)


population_informal_mj_adjusted

population_informal_mj_adjusted %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year)

# Municipal boundary changes over time were addressed through backward harmonization. In the dataset, the only case of 
# territorial change corresponds to the creation of Mojuí dos Campos (PA) (IBGE code 1504752), which was split from 
# Santarém (PA) (1506807) in 2013. To ensure temporal consistency in the unit of analysis, historical values for Mojuí 
# dos Campos in 2000 and 2010 were reconstructed by proportionally allocating Santarém’s population based on their 
# relative shares observed in the 2022 Census. Correspondingly, Santarém’s historical values were adjusted by subtracting 
# the estimated share attributed to Mojuí dos Campos. This procedure preserves comparability over time, at the cost of 
# assuming that the relative distribution between the two territories remained stable prior to their administrative 
# separation. In the deforestation dataset (MapBiomas), this harmonization is already implemented, and the procedure 
# described here ensures consistency between socioeconomic and environmental variables.

# Transformations

population_pea_panel <- population_pea_mj_adjusted %>%
  mutate(
    geocode = as.numeric(geocode),
    year = as.integer(year)
  ) %>%
  group_by(geocode, year) %>%
  summarise(pea = first(pea), .groups = "drop") %>%
  group_by(geocode) %>%
  complete(year = 2000:2022) %>%
  arrange(geocode, year) %>%
  mutate(
    pea_interp = zoo::na.approx(pea, x = year, na.rm = FALSE)
  ) %>%
  ungroup()


print(population_pea_panel, n = 100)


population_informal_panel <- population_informal_mj_adjusted %>%
  mutate(
    geocode = as.numeric(geocode),
    year = as.integer(year)
  ) %>%
  group_by(geocode, year) %>%
  summarise(
    munic = first(munic),
    informal = first(Informal),
    .groups = "drop"
  ) %>%
  group_by(geocode) %>%
  complete(year = 2000:2022) %>%
  arrange(geocode, year) %>%
  fill(munic, .direction = "downup") %>%
  mutate(
    informal_interp = zoo::na.approx(informal, x = year, na.rm = FALSE),
    formal_interp = 100 - informal_interp
  ) %>%
  ungroup()

print(population_informal_panel, n = 100)


population_pea_panel %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year)

population_pea_panel %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  count(geocode)

population_informal_panel %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year) %>% 
  print(n = 100)

population_informal_panel %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  count(geocode)

# Inspecting:

ggplot(population_pea_panel, aes(x = year, y = pea_interp, group = geocode)) +
  geom_line(alpha = 0.1) +
  theme_minimal()

ggplot(filter(population_pea_panel, geocode == "1504752"),
       aes(x = year, y = pea_interp)) +
  geom_line(size = 1.2, color = "steelblue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Change over time — ID 1504752")

ggplot(filter(population_pea_panel, geocode == "1506807"),
       aes(x = year, y = pea_interp)) +
  geom_line(size = 1.2, color = "blue") +
  geom_point() +
  theme_minimal() +
  labs(title = "Change over time — ID 1506807")


ggplot(filter(population_informal_panel, geocode == "1504752"),
       aes(x = year, y = informal_interp)) +
  geom_line(size = 1.2, color = "salmon") +
  geom_point() +
  theme_minimal() +
  labs(title = "Change over time — ID 1504752")


ggplot(filter(population_informal_panel, geocode == "1506807"),
       aes(x = year, y = informal_interp)) +
  geom_line(size = 1.2, color = "red") +
  geom_point() +
  theme_minimal() +
  labs(title = "Change over time — ID 1506807")


ggplot(filter(population_pea_panel, geocode == "1"),
       aes(x = year, y = pea_interp)) +
  geom_line(size = 1.2, color = "green") +
  geom_point() +
  theme_minimal() +
  labs(title = "Change over time — ID 1506807")


ggplot(filter(population_informal_panel, geocode == "1"),
       aes(x = year, y = informal_interp)) +
  geom_line(size = 1.2, color = "darkgreen") +
  geom_point() +
  theme_minimal() +
  labs(title = "Change over time — ID 1504752")


#saving

library(writexl)
library(readr)

print(population_pea_panel)
print(population_informal_panel)

data_population_pea_panel <- population_pea_panel %>%
  mutate(geocode = as.integer(geocode)) %>%
  select(geocode, year, pea_interp) %>%
  rename(pea = pea_interp)

data_population_informal_panel <- population_informal_panel %>%
  select(geocode, year, informal_interp) %>%
  rename(informal = informal_interp)

population_pea_panel %>%
  select(geocode, year, pea_interp) %>%
  rename(pea = pea_interp) %>%
  write_csv("_data/outcome_population_pea.csv")

population_pea_panel %>%
  select(geocode, year, pea_interp) %>%
  rename(pea = pea_interp) %>%
  write_xlsx("_data/outcome_population_pea.xlsx")

population_pea_panel %>%
  select(geocode, year, pea_interp) %>%
  rename(pea = pea_interp) %>%
  write_rds("_data/outcome_population_pea.rds")

population_informal_panel %>%
  select(geocode, year, informal_interp) %>%
  rename(informal = informal_interp) %>%
  write_csv("_data/outcome_population_informality.csv")

population_informal_panel %>%
  select(geocode, year, informal_interp) %>%
  rename(informal = informal_interp) %>%
  write_xlsx("_data/outcome_population_informality.xlsx")


population_informal_panel %>%
  select(geocode, year, informal_interp) %>%
  rename(informal = informal_interp) %>%
  write_rds("_data/outcome_population_informality.rds")

