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


#' *3. Control variables:* 
#' 
#' *3.1. GDP*
#' 

library(readr)
library(dplyr)
install.packages("janitor")
library(janitor)

# Deflator

data_ipca_deflator <- read_csv(
  "_data/deflator_ipca_2024.csv",
  col_types = cols(
    year          = col_integer(),
    ipca_index    = col_double(),
    ipca_deflator = col_double()
  )
) %>%
  select(year, ipca_deflator)

# PIB Municipality

data_pib_munic <- read_csv(
  "_data/raw_ibge_pib/pib_munic.csv", 
  skip = 3,
  n_max = 5570,
  na = c("", "NA", "...", "-")
  )

# Variável - Produto Interno Bruto a preços correntes (Mil Reais)  

head(data_pib_munic)
glimpse (data_pib_munic)
problems(data_pib_munic)

data_pib_munic <- data_pib_munic %>%
  rename(
    geocode = Cód.,
    municipality = Município) %>% 
  mutate(geocode = as.character(geocode))

# Mojuí dos Campos

data_pib_munic %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode)


# Population municipalities

data_pop_2022 <- read_excel("_data/raw_ibge_pib/pop_censo_2022.xlsx")

data_pop_2000_2010 <- read_excel("_data/raw_ibge_pib/pop_censo_2010_2000.xlsx")

glimpse(data_pop_2000_2010)
glimpse(data_pop_2022)


data_pop_2000_2010_long <- data_pop_2000_2010 %>%
  mutate(
    across(c(`2000`, `2010`), ~ parse_number(as.character(.)))
  ) %>%
  pivot_longer(
    cols = c(`2000`, `2010`),
    names_to = "year",
    values_to = "population"
  ) %>%
  mutate(
    year = as.integer(year),
    geocode = as.character(geocode)
  ) %>%
  select(geocode, municipality, year, population)


data_pop_2022_long <- data_pop_2022 %>%
  mutate(
    year = 2022L,
    geocode = as.character(geocode),
    population = as.numeric(population)
  ) %>%
  select(geocode, municipality, year, population)


head(data_pop_2000_2010_long)
head(data_pop_2022_long)

data_population_panel <- bind_rows(
  data_pop_2000_2010_long,
  data_pop_2022_long
) %>%
  arrange(geocode, year)

head(data_population_panel)
glimpse(data_population_panel)
tail(data_population_panel)


# Interpolation

library(zoo)

data_population_panel <- data_population_panel %>%
  group_by(geocode) %>%
  complete(year = 2000:2022) %>%
  ungroup()

print(data_population_panel, n = 100)


data_population_panel <- data_population_panel %>%
  group_by(geocode) %>%
  arrange(year) %>%
  mutate(
    population_interp = zoo::na.approx(
      population,
      x = year,
      na.rm = FALSE
    )
  ) %>%
  ungroup() %>% 
  arrange(geocode, year)

print(data_population_panel, n = 100)


data_population_panel <- data_population_panel %>%
  group_by(geocode) %>%
  fill(municipality, .direction = "downup") %>%
  ungroup()

data_population_panel <- data_population_panel %>%
  select(geocode, year, municipality, population = population_interp)

print(data_population_panel, n = 100)

data_population_panel %>%
  summarise(missing_names = sum(is.na(municipality)))

data_population_panel %>%
  count(geocode, municipality) %>%
  count(geocode) %>%
  summarise(max_names_per_geocode = max(n))

data_population_panel %>%
  group_by(geocode) %>%
  summarise(
    n_na = sum(is.na(population))
  )

data_population_panel %>%
  filter(geocode == "1506807") %>%
  print(n = 30)

glimpse(data_pib_munic)
glimpse(data_population_panel)


# PIB per capita 

data_pib_long <- data_pib_munic %>%
  pivot_longer(
    cols = `2002`:`2023`,
    names_to = "year",
    values_to = "pib_thousand_brl"
  ) %>%
  mutate(
    year = as.integer(year),
    pib_brl = as.numeric(pib_thousand_brl) * 1000,
    geocode = as.character(geocode)
  ) %>%
  left_join(data_ipca_deflator, by = "year") %>%
  mutate(
    pib_brl_2024 = pib_brl * ipca_deflator
  ) %>%
  arrange(geocode, year)
  
head(data_pib_long)
tail(data_pib_long)

# Merge GDP and population

data_pib_per_capita_panel <- data_pib_long %>%
  left_join(
    data_population_panel %>%
      select(geocode, year, population),
    by = c("geocode", "year")
  )

head(data_pib_per_capita_panel)

# Dealing with Monjuí dos Campos

data_pib_per_capita_panel %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year) %>% 
  print(n = 60)


library(dplyr)

id_mojui <- "1504752"
id_santarem <- "1506807"

shares_2022 <- data_pib_per_capita_panel %>%
  filter(year == 2022, geocode %in% c(id_mojui, id_santarem)) %>%
  summarise(
    pop_mojui = population[geocode == id_mojui],
    pop_sant  = population[geocode == id_santarem],
    gdp_mojui = pib_brl[geocode == id_mojui],
    gdp_sant  = pib_brl[geocode == id_santarem],
    gdp_mojui_2024 = pib_brl_2024[geocode == id_mojui],
    gdp_sant_2024  = pib_brl_2024[geocode == id_santarem]
  ) %>%
  mutate(
    pop_share_mojui = pop_mojui / (pop_mojui + pop_sant),
    gdp_share_mojui = gdp_mojui / (gdp_mojui + gdp_sant),
    gdp_share_mojui_2024 = gdp_mojui_2024 / (gdp_mojui_2024 + gdp_sant_2024)
  )
print(shares_2022)

data_pib_adjusted <- data_pib_per_capita_panel %>%
  left_join(shares_2022, by = character()) %>%  # broadcast shares
  group_by(year) %>%
  mutate(
    # Backcast Mojuí (pre-2013)
    pib_brl = case_when(
      geocode == id_mojui & year < 2013 ~
        pib_brl[geocode == id_santarem] * gdp_share_mojui,
      TRUE ~ pib_brl
    ),
    pib_brl_2024 = case_when(
      geocode == id_mojui    & year < 2013 ~
        pib_brl_2024[geocode == id_santarem] * gdp_share_mojui_2024,
      TRUE ~ pib_brl_2024
    ),
    population = case_when(
      geocode == id_mojui & year < 2013 ~
        population[geocode == id_santarem] * pop_share_mojui,
      TRUE ~ population
    )
  ) %>%
    # Subtract from Santarém (pre-2013)
  mutate (
    pib_brl = case_when(
      geocode == id_santarem & year < 2013 ~
        pib_brl * (1 - gdp_share_mojui),
      TRUE ~ pib_brl
    ),
    pib_brl_2024 = case_when(
      geocode == id_santarem & year < 2013 ~
        pib_brl_2024 * (1 - gdp_share_mojui_2024),
      TRUE ~ pib_brl_2024
    ),
    population = case_when(
      geocode == id_santarem & year < 2013 ~
        population * (1 - pop_share_mojui),
      TRUE ~ population
    )
  ) %>%
  ungroup()

head(data_pib_adjusted)

data_pib_adjusted %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year) %>% 
  print(n = 60)


data_pib_adjusted %>%
  filter(geocode == "1504752") %>%
  arrange(year) %>% 
  print(n = 30)


library(dplyr)
library(zoo)

data_pib_adjusted <- data_pib_adjusted %>%
  group_by(geocode) %>%
  arrange(year) %>%
  mutate(
    population_interp = zoo::na.approx(
      population,
      x = year,
      na.rm = FALSE
    )
  ) %>%
  ungroup()

head(data_pib_adjusted)

data_pib_adjusted <- data_pib_adjusted %>%
  mutate(
    population = if_else(
      is.na(population),
      population_interp,
      population
    )
  ) %>%
  select(-population_interp)

data_pib_adjusted <- data_pib_adjusted %>%
  arrange(geocode, year)

print(data_pib_adjusted, n = 100)
  
print(data_pib_per_capita_panel, n = 100)

tail(data_pib_adjusted)
tail(data_pib_per_capita_panel)



data_pib_per_capita_panel %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year) %>% 
  print(n = 60)


data_pib_adjusted %>%
  filter(geocode %in% c(1504752, 1506807)) %>%
  arrange(geocode, year) %>% 
  print(n = 60)

data_pib_adjusted <- data_pib_adjusted %>%
  filter(year <= 2022)

data_pib_adjusted <- data_pib_adjusted %>% 
  select(geocode,
         municipality,
         year,
         pib_brl,
         pib_brl_2024,
         population
         )

data_gdp_per_capita_panel <- data_pib_adjusted %>%
  rename(
    gdp_brl = pib_brl,
    gdp_brl_2024 = pib_brl_2024
  ) %>%
  mutate(
    gdp_per_capita = if_else(
      !is.na(population) & population > 0,
      gdp_brl / population,
      NA_real_
    ),
    log_gdp_per_capita = log(gdp_per_capita)
  ) %>% 
  mutate(
    gdp_per_capita_2024     = if_else(
      !is.na(population) & population > 0,
      gdp_brl_2024 / population,
      NA_real_
    ),
    log_gdp_per_capita_2024 = log(gdp_per_capita_2024)
  ) 


print(data_gdp_per_capita_panel, n = 50)

data_gdp_per_capita_panel %>%
  select(geocode, year, population, gdp_brl, gdp_per_capita, log_gdp_per_capita, gdp_brl_2024, gdp_per_capita_2024, log_gdp_per_capita_2024) %>%
  write_csv("_data/outcome_gdp.csv")

data_gdp_per_capita_panel %>%
  select(geocode, year, population, gdp_brl, gdp_per_capita, log_gdp_per_capita, gdp_brl_2024, gdp_per_capita_2024, log_gdp_per_capita_2024) %>%
  write_xlsx("_data/outcome_gdp.xlsx")


data_gdp_per_capita_panel %>%
  select(geocode, year, population, gdp_brl, gdp_per_capita, log_gdp_per_capita, gdp_brl_2024, gdp_per_capita_2024, log_gdp_per_capita_2024) %>%
  write_rds("_data/outcome_gdp.rds")


data_gdp_per_capita_panel  




#' *3.2. Competititve elections*
#' 

rm(list = ls(all = TRUE)) 

library(readr)
library(dplyr)
library(type.convert)

data_elections_raw <- read_csv(
  "_data/raw-tse-resultados.csv",
  col_types = cols(.default = col_character())
)

data_elections_raw <- data_elections_raw %>%
  filter(ano != "ano") %>%
  type_convert() 

problems(data_elections_raw)

glimpse(data_elections_raw)

data_elections_raw %>%
  count(sigla_uf, tipo_eleicao, sort = TRUE) %>%
  arrange(sigla_uf, desc(n)) %>%
  print(n = Inf)

# Filter and clean
data_mayors <- data_elections_raw %>%
  filter(
    cargo  == "prefeito",
    turno  == "1",          # First round only for cross-municipality consistency
    tipo_eleicao == "eleicao ordinaria"  # Drop by-elections
  ) %>%
  mutate(
    year  = as.integer(ano),
    votes = as.integer(votos)
  )

# Aggregate to candidate × municipality × year 
# Unit of analysis: one row per candidate per municipality per election year

data_candidates <- data_mayors %>%
  group_by(year, id_municipio, sigla_uf, id_candidato_bd,
           numero_candidato, sigla_partido, numero_partido) %>%
  summarise(
    candidate_votes = sum(votes, na.rm = TRUE),
    .groups = "drop"
  )

# Check for duplicate candidates (should all be 1)
data_candidates %>%
  count(year, id_municipio, id_candidato_bd) %>%
  filter(n > 1) %>%
  nrow()
# There is a duplicate, but it is from MG, that we will not use. 

# Compute vote shares 

data_candidates <- data_candidates %>%
  group_by(year, id_municipio) %>%
  mutate(
    total_votes = sum(candidate_votes),
    vote_share  = candidate_votes / total_votes
  ) %>%
  ungroup()

# Build competitiveness variables at municipality × year level
data_competitiveness <- data_candidates %>%
  rename(geocode = id_municipio) %>%
  mutate(geocode = as.character(geocode)) %>%
  group_by(year, geocode, sigla_uf) %>%
  arrange(year, geocode, desc(vote_share)) %>%
  summarise(
    enc          = 1 / sum(vote_share^2),          # Effective Number of Candidates
    mov          = vote_share[1] - vote_share[2],  # Margin of Victory (1st - 2nd)
    winner_share = vote_share[1],                  # Winner's vote share
    n_candidates = n(),                            # Number of candidates
    total_votes  = first(total_votes),             # Total valid votes cast
    winner_party = sigla_partido[1],
    winner_id    = id_candidato_bd[1],
    .groups = "drop"
  ) %>%
  mutate(
    competitive  = as.integer(mov < 0.10),   # Binary: MoV < 10pp
    uncontested  = as.integer(n_candidates == 1),
    mov          = if_else(uncontested == 1, NA_real_, mov),
    enc          = if_else(uncontested == 1, 1,        enc)
  )

# Sanity checks 
# Distribution of key variables
summary(data_competitiveness[, c("enc", "mov", "n_candidates", "total_votes")])

# Uncontested races by year
data_competitiveness %>%
  group_by(year) %>%
  summarise(
    n_municip     = n(),
    n_uncontested = sum(uncontested),
    pct_uncontested = round(100 * n_uncontested / n_municip, 1),
    mean_enc      = round(mean(enc, na.rm = TRUE), 2),
    mean_mov      = round(mean(mov, na.rm = TRUE), 3)
  ) %>%
  print()

data_competitiveness <- data_competitiveness %>% 
  select(-sigla_uf)

glimpse(data_competitiveness)
head(data_competitiveness)


# Create the full municipality × year panel 
# All municipalities present in competitiveness data, all years 2000–2020

panel_shell <- data_competitiveness %>%
  distinct(geocode) %>%
  cross_join(tibble(year = 2000:2020))

#Join competitiveness data onto the full panel
panel_competitiveness <- panel_shell %>%
  left_join(
    data_competitiveness %>%
      select(year, geocode, enc, mov, winner_share,
             n_candidates, total_votes, winner_party, winner_id,
             competitive, uncontested),
    by = c("year", "geocode")
  )

head(panel_competitiveness)
tail(panel_competitiveness)

# Carry election values forward across non-election years
# Values from election year t apply to t+1, t+2, t+3 (mayor's full term)

panel_competitiveness <- panel_competitiveness %>%
  arrange(geocode, year) %>%
  group_by(geocode) %>%
  fill(enc, mov, winner_share, n_candidates, total_votes,
       winner_party, winner_id, competitive, uncontested,
       .direction = "down") %>%
  ungroup()

#Flag election years vs. carry-forward years
election_years <- c(2000, 2004, 2008, 2012, 2016, 2020)

panel_competitiveness <- panel_competitiveness %>%
  mutate(
    election_year = as.integer(year %in% election_years)
  )

head(panel_competitiveness)

# Sanity check 
# Years 2000–2003 should have NAs (no prior election to carry from)

# Spot-check one municipality: values should repeat within each 4-year term
panel_competitiveness %>%
  filter(geocode == first(geocode)) %>%
  select(year, enc, mov, competitive, election_year) %>%
  print(n = 21)


# Overall NA count per variable
panel_competitiveness %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  mutate(pct_na = round(100 * n_na / nrow(panel_competitiveness), 2)) %>%
  filter(n_na > 0) %>%
  arrange(desc(n_na)) %>%
  print()

# NAs by year 
panel_competitiveness %>%
  group_by(year) %>%
  summarise(across(c(enc, mov, competitive, uncontested),
                   ~ sum(is.na(.)), .names = "na_{.col}")) %>%
  print(n = 21)



# Identify which municipalities have NAs and why 
na_municipalities <- panel_competitiveness %>%
  filter(is.na(mov)) %>%
  distinct(geocode, year)

# How many unique municipalities are affected?
na_municipalities %>%
  distinct(geocode) %>%
  nrow()

# Are NAs concentrated in specific years?
na_municipalities %>%
  count(year) %>%
  print()

# Inspect the affected municipalities more closely
# Do they appear in some years but not others? (suggests merging/splitting)
panel_competitiveness %>%
  semi_join(na_municipalities, by = "geocode") %>%
  select(geocode, year, enc, mov, election_year) %>%
  arrange(geocode, year) %>%
  print(n = 400)

colnames(panel_competitiveness)

panel_competitiveness %>%
  write_csv("_data/outcome_electoral_competitiveness.csv")

panel_competitiveness %>%
  write_xlsx("_data/outcome_electoral_competitiveness.xlsx")

panel_competitiveness %>%
  write_rds("_data/outcome_electoral_competitiveness.rds")


