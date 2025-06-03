# Download data and set up the dataset

# Clean space
rm(list = ls(all = TRUE))

# Start packages
library(tidyverse)
library(magrittr)

#' *Environmental data:*
#' 
#' *- Deforestation:* The deforestation data used in this study comes from the 
#' *MapBiomas Project*, a Brazilian initiative that provides annual land use 
#' and land cover data for the entire national territory. The data is derived 
#' from Landsat satellite imagery with 30-meter resolution, processed pixel by 
#' pixel to produce consistent information across all Brazilian biomes.
#' 
#' I use the *Deforestation and Secondary Vegetation Database*, version 9, which 
#' covers the period from 1985 to 2023 (last updated in August 2024, available at 
#' https://brasil.mapbiomas.org/estatisticas/).
#' 
#' The database provides the area (in hectares) of primary and secondary vegetation 
#' suppression for Brazilian states and municipalities. Land use is categorized into 
#' six classes and organized across four levels of detail. For this research, I 
#' filtered the data based on two land use classes:
#' 
#'   i. *Primary Vegetation Suppression:* Indicates a deforestation event occurring 
#'   in year *t*, in a pixel previously classified as *Primary Vegetation*. In the 
#'   following year (*t+1*), the pixel is reclassified as *Anthropic Use*.
#' 
#'   ii. *Secondary Vegetation Suppression:* Indicates a deforestation event in 
#'   year *t*, in a pixel previously classified as *Secondary Vegetation*, which is 
#'   reclassified as *Anthropic Use* in year *t+1*.
#' 
#' Regarding the levels of land use detail, I use only *Level 1* — the most general 
#' level — as my objective is to analyze overall patterns of deforestation.


#' *- Biome:* The data source used to determine which municipalities are included 
#' in the dataset is the Predominant Biome by Municipality for Statistical Purposes 
#' list (*Bioma Predominante por Município para Fins Estatísticos*), released by the 
#' Brazilian Institute of Geography and Statistics (IBGE) in June 2024 (avaiable at 
#' https://www.ibge.gov.br/geociencias/cartas-e-mapas/informacoes-ambientais/15842-biomas.html?=&t=downloads)
#' 
#' This list identifies the predominant biome in each Brazilian municipality, 
#' providing a standardized criterion for aggregating municipal data by biome.
#' Municipalities located in more than one biome are classified according to the 
#' biome that covers the largest proportion of their territorial area. 
#' 
#' In this study, I use the Amazon biome as a filter to select the municipalities 
#' to be analyzed. According to the IBGE classification, the Amazon is the 
#' predominant biome in 503 Brazilian municipalities.

#' *Social data:*
#' 
#' *- Inequality Idex (modified):* To measure inequality 
#' 
#' 
#' 
#' Includes 
#' information on the overall social
#' progress score, as well as scores for specific components such sanitation, health,
#' and education. The data is available for 2014, 2018, 2021, and 2023.
#' 
							


#' SidraR

library(sidrar)
library(tidyverse)

data_social_water_1_raw <- get_sidra(api = "/t/6804/n6/all/v/1000381/p/all/c301/allxt/c1817/72125/d/v1000381%202")

str(data_social_water)
head(data_social_water)
unique(data_social_water$`Principal forma de abastecimento de água`)

data_social_water_2_classified <- data_social_water %>%
  select(
    value = Valor,
    municipipality_id = `Município (Código)`,
    municipality_name = Município,
    water_supply_type = `Principal forma de abastecimento de água`
  ) %>% 
  mutate(
    water_supply_classification = case_when(
    water_supply_type == "Rede geral de distribuição" |
    water_supply_type == "Poço profundo ou artesiano" ~ "adequate_water_supply_2022",
    TRUE ~ "inadequate_water_supply_2022"
    )
  ) %>% 
  group_by(municipality_id, municipality_name, water_supply_classification) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  filter(water_supply_classification == "inadequate_water_supply_2022")

print(data_social_water_2_classified)

data_social_water_3_inadequate_supply <- data_social_water_2_classified %>%
  select(municipipality_id, municipality_name, inadequate_water_supply_2022_value = total_value)

print(data_social_water_3_inadequate_supply)




#' Social data:
#' - Imigration:
#' 
#' - Employment (unemployment, informality, :
#' 
#' - Household income (per capita income, before and after taxes):
#' 
#' - Inequality (Gini, poverty rate, 20 percentile):
#' 
#' - The Social Progress Index (IPS) is a composite index that measures social
#' progress in Brazilian municipalities. Includes information on the overall social
#' progress score, as well as scores for specific components such sanitation, health,
#' and education. The data is available for 2014, 2018, 2021, and 2023.
#' 
#' - Social transfers (Bolsa Família, Bolsa Verde):





