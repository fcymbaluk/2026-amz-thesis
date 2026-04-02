
library(tidyverse)
library(magrittr)
library(readxl)
library(readr)
library(writexl)
library(ggplot2)

rm(list = ls(all = TRUE))

getwd()

#' *1.Environmental data*: 
#' 
#' *1.1.Deforestation:* 
#'
#' *1.1.1. Understand the data:* The deforestation data used in this study comes from the
#' *MapBiomas Project*, a Brazilian initiative that provides annual land use
# and land cover data for the entire national territory. The data is derived
# from Landsat satellite imagery with 30-meter resolution, processed pixel by
# pixel to produce consistent information across all Brazilian biomes.
#'
#' I use the *Deforestation and Secondary Vegetation Database*, version 9, which
#' covers the period from 1985 to 2023 (last updated in August 2024, available at
#' https://brasil.mapbiomas.org/estatisticas/).
#'
#' The database provides the area (in hectares) of primary and secondary total
#' vegetation and vegetation supression per year for Brazilian
#' municipalities. Land use is categorized into six classes and organized across
#' four levels of detail. For this research, I filter the data based on four
#' land use classes:
#'
#'   i. *Primary Vegetation:* Indicates absence of deforestation event, i.e., permanence
#'   since the base year in one or more classes of Natural Vegetation
#'
#'   ii. *Secondary Vegetation:* Indicates trajectory with the presence of a recovery
#'   for secondary vegetation event in previous years.
#'
#'   iii. *Primary Vegetation Suppression:* Indicates a deforestation event occurring
#'   in year *t*, in a pixel previously classified as *Primary Vegetation*. In the
#'   following year (*t+1*), the pixel is reclassified as *Anthropic Use*.
#'
#'   iv. *Secondary Vegetation Suppression:* Indicates a deforestation event in
#'   year *t*, in a pixel previously classified as *Secondary Vegetation*, which is
#'   reclassified as *Anthropic Use* in year *t+1*.
#'
#' Regarding the levels of land use detail (e.g., forest formation, savana formation,
#' floodable forest, and grassland formation as secondary levels for Forest), I use
#' only *Level 1*, the most general level, as my objective is to analyze overall
#' patterns of deforestation.
#'
#' *1.1.2. Import and inspect the data:* I download the data of "Deforestation and
#' Secondary Vegetation" from the "Statistics" section of the Mapbiomas website.
#' I inspect the data to ensure that the import function correctly interpret the decimal
#' separator and recognize numbers as numeric or double values. With the initial
#' inspection, I also discover that the values for "Primary Vegetation Suppression"
#' for the years of 1985 and 1986 are all zero, which justifies the exclusion of
#' those years from the dataset.

data_deforestation_raw <- read_xlsx("_data/raw-environ-mapbiomas-deforestation-municipalities.xlsx", sheet = 2)

glimpse(data_deforestation_raw)

data_deforestation_raw %>%
  filter(transition_name == "Supressão Veg. Primária") %>%
  summarise(all_zero = all(`1985` == 0)) #I repeated this check for the years 1985, 1986 and 2023

data_deforestation_raw %>%
  filter(transition_name == "Supressão Veg. Primária") %>%
  summarise(non_zero_count_1985 = sum(`1985` != 0), 
            non_zero_count_1986 = sum(`1986` != 0),
            non_zero_count_1987 = sum(`1987` != 0),
            non_zero_count_2023 = sum(`2023` != 0))

#' *1.1.3. Clean and transform the data:* 
#
#' *3.a. Recode and tyde:* I select relevant columns: state, municipality, geocode, 
#' transition name, and area in hectares for each year from 1987 to 2023, filter 
#' for Amazon states and the specific transitions that I am interested in (primary 
#' and secondary vegetation and their respective suppression), recode transition 
#' names, and reshape the data to a tidy format. I convert the `year` values to integers 
#' to enable numeric operations, and the `geocode` values to characters to prevent 
#' them from being treated as numbers. Finally, I check if all the `geocode`are
#' correctly registered with seven digits. 

data_deforestation_clean <- data_deforestation_raw %>%
  select(
    state, municipality, geocode, transition_name, `1987`:`2023`
  ) %>% 
  filter(
    state %in% c("Acre", "Amapá", "Amazonas", "Maranhão", "Mato Grosso", "Pará", "Rondônia", "Roraima", "Tocantins"),
    transition_name %in%  c("Supressão Veg. Primária", "Supressão Veg. Secundária", "Veg. Primária", "Veg. Secundária")
  ) %>%
  mutate(
    transition_name = recode(
      transition_name,
      "Supressão Veg. Primária" = "loss_of_primary_vegetation",
      "Supressão Veg. Secundária" = "loss_of_secondary_vegetation",
      "Veg. Primária" = "primary_vegetation",
      "Veg. Secundária" = "secondary_vegetation"),
    geocode = as.character(geocode)
  ) %>%
  pivot_longer(
    cols = `1987`:`2023`,
    names_to = "year",
    values_to = "area_ha",
    names_transform = list(year = as.integer)
  )

data_deforestation_clean %>%
  filter(nchar(geocode) == 7) %>% 
  count()

glimpse(data_deforestation_clean)

#' As my interest is in the total forest loss (the sum of primary and 
#' secondary vegetation loss), I create the new column `vegetation_category` where 
#' I categorize the vegetation variables only as `forest` (sum of primary and 
#' secondary vegetation) and `deforestation` (sum of loss of primary and secondary 
#' vegetation). I summarize the total area in hectares for each vegetation category 
#' by municipality, and reshape the data to a wide format.

data_deforestation_tidy <- data_deforestation_clean %>%
  mutate(
    vegetation_category = case_when(
      transition_name %in% c("primary_vegetation", "secondary_vegetation") ~ "forest",
      transition_name %in% c("loss_of_primary_vegetation", "loss_of_secondary_vegetation") ~ "deforestation",
      TRUE ~ NA_character_
    )
  ) %>%  
  filter(!is.na(vegetation_category)) %>% # This line removes any rows where vegetation_category is NA
  group_by(
    state, municipality, geocode, year, vegetation_category
  ) %>%
  summarise(
    total_area_ha = sum(area_ha, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  pivot_wider(
    id_cols = c(state, municipality, geocode, year),
    names_from = vegetation_category,
    values_from = total_area_ha,
    values_fill = 0, # Fill missing values with 0 to avoid NA in the final dataset
    names_prefix = "area_"
  )

glimpse(data_deforestation_tidy)

#' *3.b. Quantify missingness and detect outliers:* I assess the missingness in the 
#' dataset and find no NAs. I also checked the values for Mojuí dos Campos (PA) — geocode 1504752 — 
#' a former district of Santarém (PA) that became an independent municipality only in 2013. The 
#' dataset contains the data for that municipality for all the years, which is correct.
 
sum(is.na(data_deforestation_tidy))

data_deforestation_tidy %>%
  filter(geocode == 1504752) %>% 
  print(n = 40)

#' *3.c. Detect outliers:* To evaluate record errors, I also analyze the dataset 
#' for outliers using summary statistics and graphs. The data is skewed to the right, 
#' which is expected, as the dataset currently contains data for municipalities 
#' across all Amazon states, including many municipalities with very low deforestation 
#' rates. Below are the codes of the analyzes for `area_deforestation`. The same 
#' analysis is done for `area_forest`.

summary(data_deforestation_tidy$area_deforestation) 

quantile(data_deforestation_tidy$area_deforestation, probs = c(0.25, 0.5, 0.75))

ggplot(data_deforestation_tidy, aes(y = area_deforestation)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, quantile(data_deforestation_tidy$area_deforestation, 0.95, na.rm = TRUE))) +
  theme_minimal()

ggplot(data_deforestation_tidy, aes(x = area_deforestation)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(title = "Distribution of Deforestation Area", x = "Area (ha)")

#' *3.d. Check for duplicates:* I check for duplicates in the dataset, which
#' could indicate data entry errors or multiple records for the same municipality
#' and year. There is no duplicates, what signs that the data is clean and each 
#' record is unique.

data_deforestation_tidy %>%
  filter(duplicated(.)) %>%
  nrow()

#' *3.e. Validate Against External Sources:* I cross-validate the dataset values 
#' against the online dashboard data available on the Mapbiomas website 
#' (https://brasil.mapbiomas.org/ hit the "Maps and Data" button, then "Access the 
#' Platform", and "Mapbiomas Land Cover and Use platform"; select "Deforestment" 
#' in the left menu). The intention here is to evaluate whether the downloaded data 
#' is consistent with what is released online by Mapbiomas and whether I have 
#' correctly managed the transformations. I randomly selected municipalities to 
#' compare their values with those shown on the MapBiomas platform. As my dataset 
#' summarize the values for primary and secondary vegetation, I manually calculated 
#' the same sums using the values available on the platform. The validation 
#' confirms that the values in my dataset align with those available on the MapBiomas 
#' platform. 

deforestation_acrelandia_annual <- data_deforestation_tidy %>%
  filter(municipality == "Acrelândia") %>%
  group_by(year) %>%
  summarise(
    total_deforestation_ha = sum(area_deforestation, na.rm = TRUE),
    .groups = "drop"
  )

print(deforestation_acrelandia_annual, digits = 10, n = 40)

deforestation_paragominas_annual <- data_deforestation_tidy %>%
  filter(municipality == "Paragominas") %>%
  group_by(year) %>%
  summarise(
    total_deforestation_ha = sum(area_deforestation, na.rm = TRUE),
    .groups = "drop"
  )

print(deforestation_paragominas_annual, n = 40, digits = 10)

deforestation_annual_total <- data_deforestation_tidy %>%
  group_by(year) %>%
  summarise(
    total_deforestation_ha = sum(area_deforestation, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(data_deforestation_tidy, aes(x = year, y = area_deforestation)) +
  geom_col() +
  labs(title = "Deforestation in the Amazon biome",
       x = "Year",
       y = "Deforestation (ha)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#'*1.1.4. Correct and standardize units:* I perform two standardization transformations 
#'to smooth the cross-sectional variation in deforestation arising from heterogeneity 
#'in municipality size. First, I calculate the deforestation rate as the percentage 
#'of deforested area relative to the total forest area for each municipality-year 
#'observation. This enables a proportional comparison of deforestation intensity 
#'across units with varying forest sizes. Second, I compute the z-scores for 
#'deforestation areas within each municipality, standardizing values by subtracting 
#'the municipal mean and dividing by the municipal standard deviation. Z-scores 
#'indicate how far each value deviates from the municipality’s average in standard 
#'deviation units. Positive values represent above-average deforestation, while 
#'negative values indicate below-average levels.

data_deforestation_standardized <- data_deforestation_tidy %>%
  mutate(
    deforestation_rate = if_else(
      area_forest > 0,
      (area_deforestation / area_forest) * 100,
      NA_real_
    )
  ) %>%
  group_by(geocode) %>%
  mutate(
    deforestation_mean = mean(area_deforestation, na.rm = TRUE),
    deforestation_sd = sd(area_deforestation, na.rm = TRUE),
    deforestation_z = (area_deforestation - deforestation_mean) / deforestation_sd
  ) %>%
  ungroup()

glimpse(data_deforestation_standardized)

print(data_deforestation_standardized, n = 100)

#' *1.1.5. Save the data:* I save the cleaned and standardized dataset in CSV, RDS, 
#' and Excel formats for future use.

write_csv(
  data_deforestation_standardized,
  file = ("_data/data_deforestation_standardized.csv")
)

write_rds(
  data_deforestation_standardized,
  file = ("_data/data_deforestation_standardized.rds")
)

#' *1.2.Biome:* 

#' *1.2.1. Understand the data:* The data source used to determine which municipalities 
#' are included in the dataset is the Predominant Biome by Municipality for Statistical 
#' Purposes list (*Bioma Predominante por Município para Fins Estatísticos*), released 
#' by the Brazilian Institute of Geography and Statistics (IBGE) in June 2024 (avaiable at 
#' https://www.ibge.gov.br/geociencias/cartas-e-mapas/informacoes-ambientais/15842-biomas.html?=&t=downloads)
#
#' This list identifies the predominant biome in each Brazilian municipality, 
#' providing a standardized criterion for aggregating municipal data by biome.
#' Municipalities located in more than one biome are classified according to the 
#' biome that covers the largest proportion of their territorial area. 
#
#' In this study, I use the Amazon biome as a filter to select the municipalities 
#' to be analyzed. According to the IBGE classification, the Amazon is the 
#' predominant biome in 503 Brazilian municipalities.
#
#' *1.2.2. Import and inspect the data:* I import and inspect the data to check the 
#' number of municipalities registered with "Amazônia" as the predominant biome,
#' and correctly find 503 entries. 

data_biome_raw <- read_xlsx("_data/raw-environ-ibge-biome.xlsx")

glimpse(data_biome_raw)

unique(data_biome_raw$`Bioma predominante`)

data_biome_raw %>%
  count(`Bioma predominante`) %>%
  arrange(desc(n))

#' *1.2.3. Clean and tidy the data:* I rename the columns and selected the ones of 
#' interest.

colnames(data_biome_raw)

data_biome_tidy <- data_biome_raw %>%
  rename (
    geocode = `Geocódigo`,
    municipality = `Nome do município`,
    state = `Sigla da UF`,
    biome = `Bioma predominante`
  ) %>%
  mutate(geocode = as.character(geocode))

glimpse(data_biome_tidy)

data_biome_tidy %>%
  filter(biome == "Amazônia") %>%
  distinct(geocode) %>%
  count()

#' *1.2.4. Join Deforestation and Biome datasets:* I join the biome data with the deforestation 
#' dataset using the geocode to match them. I use the `municipality` column to 
#' check if is there mismatch between the municipality information in both datasets, 
#' find only one related to a typo in the spelling of a municipality name and keep 
#' the column with the correct name. I tidy and clean the final dataset and filter 
#' by the `biome` of my interest, the Amazon biome.

data_deforestation_with_biome <- data_deforestation_standardized %>%
  left_join(data_biome_tidy, by = "geocode")

glimpse(data_deforestation_with_biome)

unique(data_deforestation_with_biome$biome)

mismatch_check <- data_deforestation_with_biome %>%
  filter(municipality.x != municipality.y)

view(mismatch_check)

data_deforestation_final <- data_deforestation_with_biome %>%
  rename(
    municipality = municipality.x,
    state = state.x,
    state_abbreviation = state.y,
  ) %>%
  select(-municipality.y) %>% 
  select(
    geocode, 
    municipality, 
    state,
    state_abbreviation, 
    biome, 
    year, 
    area_forest, 
    area_deforestation, 
    deforestation_rate, 
    deforestation_z,
    deforestation_mean,
    deforestation_sd
  ) %>%
  filter(biome == "Amazônia")

glimpse(data_deforestation_final)

unique(data_deforestation_final$biome)

#' I count the number of unique geocodes and municipalities, which was supposed to 
#' be 503, as per the IBGE classification. I correctly find 503 unique geocodes, 
#' but only 499 municipalites. It is because some municipalities in different states 
#' can share the same name. The municipalities with the same name are:
#
#   geocode municipality      state_abbreviation
# 1 2100873 Araguanã          MA                
# 2 1702158 Araguanã          TO                
# 3 1505551 Pau D'Arco        PA                
# 4 1716307 Pau D'Arco        TO                
# 5 2109239 Presidente Médici MA                
# 6 1100254 Presidente Médici RO                
# 7 1200401 Rio Branco        AC                
# 8 5107206 Rio Branco        MT

data_deforestation_final %>%
  distinct(geocode) %>%
  count()

data_deforestation_final %>%
  distinct(municipality) %>%
  count()

data_deforestation_final_municipality <- data_deforestation_final %>%
  distinct(geocode, municipality, state_abbreviation)

data_deforestation_final_duplicated_names <- data_deforestation_final_municipality %>%
  count(municipality) %>%
  filter(n > 1) %>%
  pull(municipality)

data_deforestation_final_municipality %>%
  filter(municipality %in% data_deforestation_final_duplicated_names) %>%
  arrange(municipality)

#' *1.2.6. Save the data:* I save the final dataset in both Excel, CSV and RDS formats for future use.

write_csv(
  data_deforestation_final,
  file = ("_data/outcome_deforestation.csv")
)

write_rds(
  data_deforestation_final,
  file = ("_data/outcome_deforestation.rds")
)

write_xlsx(
  data_deforestation_final,
  "_data/outcome_deforestation.xlsx")


