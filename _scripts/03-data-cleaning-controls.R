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
  "_data/raw-tse-results-2.csv",
  col_types = cols(.default = col_character())
)

data_elections_raw <- data_elections_raw %>%
  filter(ano != "ano") %>%
  type_convert() 

problems(data_elections_raw)
glimpse(data_elections_raw)
head(data_elections_raw)
tail(data_elections_raw)
unique(data_elections_raw$cargo)
range(data_elections_raw$ano)
unique(data_elections_raw$ano) %>% 
  count(data_elections_raw, sigla_uf)

data_elections_raw %>%
  select(ano, turno, tipo_eleicao) %>%
  distinct() %>%
  arrange(ano, turno, tipo_eleicao) %>%
  print(n = Inf)

data_elections_raw %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_count"
  ) %>%
  arrange(desc(missing_count))

data_elections_raw %>%
  count(ano, sigla_uf, id_municipio, turno, numero_partido) %>%
  filter(n > 1) %>%
  print(n = Inf)

duplicates_numero_candidato <- data_elections_raw %>%
  count(ano, sigla_uf, id_municipio, turno, numero_partido) %>%
  filter(n > 1) %>%
  print(n = Inf)

duplicates_numero_candidato %>%
  count(sigla_uf, sort = TRUE) %>%
  print(n = 50)

data_elections_raw %>%
  filter(is.na(votos)) %>%
  count()

table(substr(data_elections_raw$id_municipio, 1, 2))      # expect mostly/only "31"

data_elections_raw %>%
  count(sigla_uf, tipo_eleicao, sort = TRUE) %>%
  arrange(sigla_uf, desc(n)) %>%
  print(n = Inf)

data_elections_raw %>%
  distinct(ano, id_municipio) %>%
  count(ano, name = "n_municipalities") %>%
  arrange(ano)

data_elections_raw %>%
  summarise(across(
    where(is.numeric),
    list(
      min = ~ min(.x, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE),
      mean = ~ mean(.x, na.rm = TRUE),
      missing = ~ sum(is.na(.x))
    )
  ))

summary(data_elections_raw$votos)

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

data_candidates

# Check for duplicate candidates (should all be 1)
data_candidates %>%
  count(year, id_municipio, id_candidato_bd) %>%
  filter(n > 1) %>%
  nrow()


data_candidates %>%
  count(year, sigla_uf, id_municipio, id_candidato_bd) %>%
  filter(n > 1) %>%
  print(n = Inf)


# There is duplicates:

6  2004 AM            1300086          937630     2
7  2004 AM            1300631         1240154     2
8  2004 AM            1301001         1403416     2
15  2004 MA            2100709         1170595     2
16  2004 MA            2101202          332248     2
17  2004 MA            2106508         1017773     2
18  2004 MA            2109270          608725     2
27  2004 PA            1505650         1005015     2
28  2004 PA            1508001              NA     2
39  2004 RO            1100304         1582952     2
49  2004 TO            1700400              NA     2
57  2012 PA            1506112              NA     2
62  2016 PA            1506195              NA     2

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
  distinct(geocode, year) %>% 
  print(n = 50)

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



#' *3.3. Municipal Agricultural Production (IBGE-PAM)*

# ============================================================
# Tidy PAM — Tabela 5457, "Área plantada ou destinada à colheita"
# 4 crops (cotton, rice, corn, soy), full series 1988–2024
# Source: IBGE / SIDRA, Produção Agrícola Municipal
# Goal:  long panel (for visuals) + base-year weights (for the index)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(magrittr)

# ---- paths (adjust to your project) ------------------------
in_path  <- "_data/raw-ibge-PAM.xlsx"
out_path <- "_data/data_agriculture_production.csv"

# ---- 1. read the two-row SIDRA header separately -----------
# Row 1 = year, merged across the 4 crop columns -> needs filling.
# Row 2 = geocode | munic | crop | crop | crop | crop | ...
data_raw <- read_excel(in_path, sheet = "Tabela", n_max = 2, col_names = FALSE)
data_raw

year_row <- unlist(data_raw[1, ], use.names = FALSE)
crop_row <- unlist(data_raw[2, ], use.names = FALSE)

# forward-fill the year labels across the merged-cell gaps
for (i in seq_along(year_row)) {
  if (is.na(year_row[i]) && i > 1) year_row[i] <- year_row[i - 1]
}

# build "YEAR__CROP" column names; first two columns are the IDs
col_names    <- paste(year_row, crop_row, sep = "__")
col_names[1] <- "geocode"
col_names[2] <- "munic"

# ---- 2. read the body as TEXT ------------------------------
# Reading as text preserves SIDRA codes ('-', '...') and stops
# readxl from guessing types off a messy first data column.
data_clean <- read_excel(
  in_path, sheet = "Tabela", skip = 2,
  col_names = col_names, col_types = "text"
)

data_clean

# ---- 3. drop the 'Brasil' total row (geocode == "1") -------
data_clean <- filter(data_clean, geocode != "1")

# ---- 4. pivot to long: one row per municipality-year-crop --
data_long <- data_clean |>
  pivot_longer(
    cols      = -c(geocode, munic),
    names_to  = c("year", "crop"),
    names_sep = "__",
    values_to = "area_raw"
  )
print (data_long, n = 50)
tail (data_long)

# ---- 5. recode values + crops, split "Name (UF)" -----------
# SIDRA legend:  "-"  = absolute zero (crop not grown)  -> 0
#                "...", "..", "X" = unavailable / N/A / confidential -> NA
data_tidy <- data_long |>
  mutate(
    year = as.integer(year),
    crop = case_when(
      str_detect(crop, "Algod") ~ "cotton",
      str_detect(crop, "Arroz") ~ "rice",
      str_detect(crop, "Milho") ~ "corn",
      str_detect(crop, "Soja")  ~ "soy",
      TRUE ~ NA_character_
    ),
    area_planted_ha = case_when(
      area_raw == "-"                   ~ 0,
      area_raw %in% c("...", "..", "X") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(area_raw))
    ),
    municipality       = str_trim(str_remove(munic, "\\s*\\([A-Z]{2}\\)$")),
    state_abbreviation = str_match(munic, "\\(([A-Z]{2})\\)$")[, 2]
  ) |>
  select(geocode, municipality, state_abbreviation,
         year, crop, area_planted_ha) |>
  arrange(geocode, year, crop)

print (data_tidy, n = 100)

tail(data_tidy)

# ---- 6. sanity checks (run interactively) ------------------
# Expect 771 municipalities x 37 years x 4 crops = 114,108 rows
nrow(data_tidy)              # -> 114108
n_distinct(data_tidy$geocode)  # -> 771
range(data_tidy$year)          # -> 1988 2024
count(data_tidy, crop)         # -> 28527 each (no NA crop labels)

write_csv(data_tidy, out_path)


library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)   


pam_path  <- "_data/data_agriculture_production.csv"
area_path <- "_data/raw-ibge-area-munics.xlsx"


# ---- 1. municipal areas: keep in km² -----------------------
# IBGE area is already in km². We convert the PAM crop area to km²
# (section 3) instead, so the whole dataset is expressed in km².
data_munic_areas <- read_excel(area_path, sheet = "AR_BR_MUN_2025") |>
  transmute(
    geocode  = as.character(geocode),
    area_munic_km2 = `Area km2`
  )

# ---- 2. read tidy PAM long (geocode as character) + join ---
data_pam <- read_csv(pam_path, col_types = cols(geocode = col_character())) |>
  left_join(data_munic_areas, by = "geocode")

# friendly diagnostic: which municipalities failed to match an area?

unmatched <- data_pam |>
  filter(is.na(area_munic_km2)) |>
  distinct(geocode, municipality) |>
  arrange(geocode, municipality)

if (nrow(unmatched) > 0) {
  warning(
    nrow(unmatched),
    " municipalities had no area match. Inspect the `unmatched` object."
  )
}

print(unmatched)
names(data_pam)

data_pam |>
  summarise(
    n_total = n(),
    n_missing_area = sum(is.na(area_munic_km2)),
    share_missing_area = mean(is.na(area_munic_km2))
  )

data_pam |>
  summarise(
    n_rows = n(),
    n_municipalities = n_distinct(geocode),
    n_missing_area = sum(is.na(area_munic_km2)),
    min_area = min(area_munic_km2, na.rm = TRUE),
    max_area = max(area_munic_km2, na.rm = TRUE)
  )

data_pam |>
  distinct(geocode, municipality, area_munic_km2) |>
  count(geocode, municipality) |>
  filter(n > 1)

data_pam |>
  group_by(geocode, municipality) |>
  summarise(
    n_area_values = n_distinct(area_munic_km2),
    .groups = "drop"
  ) |>
  filter(n_area_values > 1)

# ---- 3. time-varying share (FULL series) -> for graphs -----
# Convert PAM crop area HECTARES -> km² (1 ha = 0.01 km²) so the whole
# dataset is in km². The share is a ratio, so its value is unchanged.
# NOTE: PAM counts successive/simultaneous crops (SIDRA Nota 8), so this
# share CAN exceed 1 (>100%) in heavy double-cropping municipalities.
# That is a real feature of the data — do NOT cap or "fix" it.

glimpse(data_pam)

data_pam_shares <- data_pam |>
  mutate(
    area_planted_km2 = area_planted_ha / 100,
    area_crop_share       = area_planted_km2 / area_munic_km2
  ) |>
  select(geocode, municipality, state_abbreviation,
         year, crop, area_planted_ha, area_planted_km2, area_munic_km2, area_crop_share) |>
  arrange(geocode, crop, year)


glimpse(data_pam_shares)
head(data_pam_shares)
tail(data_pam_shares)

data_pam_shares |>
  summarise(
    n_rows = n(),
    n_municipalities = n_distinct(geocode),
    n_missing_area_share = sum(is.na(area_crop_share)),
    min_area_share = min(area_crop_share, na.rm = TRUE),
    max_area_share = max(area_crop_share, na.rm = TRUE)
  )

write_csv(data_pam_shares, "_data/outcome_agriculture_production_shares.csv")

library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(readr)

write_xlsx(data_pam_shares, "_data/outcome_agriculture_production_shares.xlsx")

write_rds(data_pam_shares, "_data/outcome_agriculture_production_shares.rds")

# ---- 4. base-year weights (avg 2000–2002) -> for the index -
# Predetermined: fixed at the base window so the weights are NOT
# affected by farmers' later responses to prices/policies.
# Municipal area is constant here, so averaging the share over the
# window is equivalent to averaging the crop area then dividing.
data_weights <- data_pam_shares |>
  filter(year %in% 2000:2002) |>
  group_by(geocode, municipality, state_abbreviation, crop) |>
  summarise(crop_weight = mean(area_crop_share, na.rm = TRUE), .groups = "drop")
# These are crop area as a share of MUNICIPAL area, so they do NOT
# sum to 1 across crops — that is the intended "intensity" weighting.

write_csv(data_weights, "_data/data_pam_weights_base_municarea.csv")

write_csv(data_weights, "_data/outcome_crop_weights_base_municarea.csv")

write_xlsx(data_weights, "_data/outcome_crop_weights_base_municarea.xlsx")

write_rds(data_weights, "_data/outcome_crop_weights_base_municarea.rds")

# ---- 5. sanity checks --------------------------------------
summary(data_weights$crop_weight)
data_weights |> filter(is.nan(crop_weight))                  # all-NA base-year cases
data_pam_shares |> filter(area_crop_share > 1) |> count(crop) # >100% double-crop cases

# ---- 6. OPTIONAL — example graph: share evolution ----------
data_pam_shares |>
  filter(state_abbreviation == "MT") |>
  group_by(year, crop) |>
  summarise(mean_share = mean(area_crop_share, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(year, mean_share, colour = crop)) +
  geom_line(linewidth = 0.8) +
  labs(x = NULL, y = "Mean (crop area / municipal area)",
       colour = "Crop", title = "Crop-area share evolution — Mato Grosso") +
  theme_minimal()



# ============================================================
# Cattle density weights from PPM (Tabela 3939, efetivo bovino)
#   (1) full time series   -> for descriptive graphs
#   (2) base-year weights  -> cattle term of the price index (2000–2002 avg)
# weight_i = cattle heads / municipal area (km²), base-year averaged
# Inputs:
#   data/raw/raw-ibge-PPM.xlsx                (PPM herd counts)
#   data/raw/raw-ibge-area-munics.xlsx        (IBGE areas, km², ref. 2025)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)   # only for the optional plot in section 6

# ---- paths -------------------------------------------------
ppm_path  <- "_data/raw-ibge-PPM.xlsx"
area_path <- "_data/raw-ibge-area-munics.xlsx"

# ---- 1. municipal areas (km²) ------------------------------
areas <- read_excel(area_path, sheet = "AR_BR_MUN_2025") |>
  transmute(geocode = as.character(geocode), area_km2 = `Area km2`)

areas

# ---- 2. read PPM as TEXT (preserve SIDRA codes) ------------
# Single header row: "Cód." | "Brasil e Município" | 1974 | 1975 | ...
ppm <- read_excel(ppm_path, sheet = "Tabela", col_types = "text") |>
  rename(geocode = `Cód.`, munic = `Brasil e Município`) |>
  filter(geocode != "1") |>                       # drop the 'Brasil' total
  mutate(geocode = as.character(geocode))

ppm

# ---- 3. pivot long + recode codes + split "Name (UF)" ------
# SIDRA legend: "-" = absolute zero (no herd) -> 0
#               "...", "..", "X" = unavailable / N/A / confidential -> NA
cattle_long <- ppm |>
  pivot_longer(cols = -c(geocode, munic),
               names_to = "year", values_to = "heads_raw") |>
  mutate(
    year = as.integer(year),
    heads = case_when(
      heads_raw == "-"                   ~ 0,
      heads_raw %in% c("...", "..", "X") ~ NA_real_,
      TRUE ~ suppressWarnings(as.numeric(heads_raw))
    ),
    municipality       = str_trim(str_remove(munic, "\\s*\\([A-Z]{2}\\)$")),
    state_abbreviation = str_match(munic, "\\(([A-Z]{2})\\)$")[, 2]
  ) |>
  select(geocode, municipality, state_abbreviation, year, heads)

tail(cattle_long)

# ---- 4. join area, compute density (FULL series) -----------

cattle_density <- cattle_long |>
  left_join(areas, by = "geocode") |>
  mutate(cattle_per_km2 = heads / area_km2) |>
  select(geocode, municipality, state_abbreviation,
         year, heads, area_km2, cattle_per_km2) |>
  arrange(geocode, year)

unmatched <- cattle_density |> filter(is.na(area_km2)) |> distinct(geocode, municipality)
if (nrow(unmatched) > 0) {
  warning(nrow(unmatched), " municipalities had no area match — inspect `unmatched`.")
}

print(unmatched)

tail(cattle_density) 

write_csv(cattle_density, "_data/outcome_cattle_density.csv")

# ---- 5. base-year weights (avg 2000–2002) ------------------
# Predetermined cattle exposure: herd density fixed at the base window.
# This is the weight for the SEPARATE cattle term in build_price_index.R.
cattle_weights <- cattle_density |>
  filter(year %in% 2000:2002) |>
  group_by(geocode, municipality, state_abbreviation) |>
  summarise(weight = mean(cattle_per_km2, na.rm = TRUE), .groups = "drop")
# weight is heads per km² (a density), so unlike the crop shares it is not
# dimensionless — that is fine: the cattle index is a separate regressor,
# and the scale is absorbed under a logged index + municipality FE.

cattle_weights

write_csv(cattle_weights, "_data/outcome_cattle_weights_base_municarea.csv")

# ---- 6. sanity checks --------------------------------------
n_distinct(cattle_weights$geocode)
summary(cattle_weights$weight)
cattle_weights |> filter(is.nan(weight))           # all-NA base-year cases

# optional: cattle density evolution for one state
cattle_density |>
  filter(state_abbreviation == "RO") |>
  group_by(year) |>
  summarise(mean_density = mean(cattle_per_km2, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(year, mean_density)) +
  geom_line(linewidth = 0.8) +
  labs(x = NULL, y = "Mean cattle head / km²",
       title = "Cattle density evolution — Pará") +
  theme_minimal()





# ============================================================
# Municipality-specific agricultural price index
#   prices : World Bank Pink Sheet, REAL (2010 USD), normalized to 2000 = 1
#   crops  : soy, corn, rice, cotton  -> weighted by crop_weights_base_municarea
#   cattle : beef -> separate index, weighted by cattle density (PENDING PPM)
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)   # only for the optional plot in section 4

# ---- paths -------------------------------------------------
pink_path           <- "_data/raw-prices-world bank-pink sheet.xlsx"
weights_path        <- "_data/outcome_crop_weights_base_municarea.csv"
weights_sum1_path   <- "_data/data_pam_weights_sum1.csv"
cattle_weights_path <- "_data/outcome_cattle_weights_base_municarea.csv"

base_yr <- 2000
yr_min  <- 2000
yr_max  <- 2020

# ---- 1. read & clean the REAL price series -----------------
# Layout: commodity names on Excel row 7, units on row 8, data from row 9.
# skip = 6 makes the names row the header; the first data row is then the
# UNITS row, which we drop with slice(-1). Reading turns numbers to text
# because of that units row, so we coerce with as.numeric afterwards.
prices_raw <- read_excel(pink_path, sheet = "Annual Prices (Real)", skip = 6)
names(prices_raw)    <- str_trim(names(prices_raw))
names(prices_raw)[1] <- "year"     # first column is unnamed in the sheet

prices_raw

prices <- prices_raw |>
  slice(-1) |>                                   # drop the units row
  select(year,
         soy    = `Soybeans`,
         corn   = `Maize`,
         rice   = `Rice, Thai 5%`,
         cotton = `Cotton, A Index`,
         beef   = `Beef`) |>
  mutate(across(everything(), as.numeric)) |>
  filter(year >= yr_min, year <= yr_max) |>
  arrange(year)

prices

# ---- 2. normalize each series to base year = 1 -------------
# Pjt / Pj,2000. Different raw units ($/mt vs $/kg) wash out here.
prices_norm <- prices |>
  mutate(across(c(soy, corn, rice, cotton, beef),
                ~ .x / .x[year == base_yr]))

prices_norm
write_csv(prices_norm, "_data/data_prices_real_normalized.csv")

# ---- 3. long form, split crops from beef -------------------
crop_prices <- prices_norm |>
  select(year, soy, corn, rice, cotton) |>
  pivot_longer(-year, names_to = "crop", values_to = "price_index")

beef_prices <- prices_norm |>
  select(year, beef_index = beef)

crop_prices
beef_prices

# ---- 4. OPTIONAL — plot the five normalized real series ----
prices_norm |>
  pivot_longer(-year, names_to = "commodity", values_to = "index") |>
  ggplot(aes(year, index, colour = commodity)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
  labs(x = NULL, y = "Real price index (2000 = 1)", colour = NULL,
       title = "World Bank real commodity prices, normalized to 2000") +
  theme_minimal()

# ============================================================
# 5. CROP price index  (you have everything for this)
#    P_crop_it = sum_j  w_ij * P_jt
#    weights w_ij = crop area / municipal area  (do NOT sum to 1)
# ============================================================
weights_crop <- read_csv(weights_path,
                         col_types = cols(geocode = col_character()))

weights_crop_sum1 <- read_csv(weights_sum1_path,
                              col_types = cols(geocode = col_character()))

weights_crop
crop_prices

crop_index <- weights_crop |>
  inner_join(crop_prices, by = "crop", relationship = "many-to-many") |>
  group_by(geocode, municipality, state_abbreviation, year) |>
  summarise(price_index_crop = sum(crop_weight * price_index, na.rm = TRUE),
            .groups = "drop")

crop_index

# NOTE: because the weights are municipal-area shares (not summing to 1),
# the index in 2000 equals the municipality's total crop-area intensity,
# not 1. That is intended; under a logged index + FE it does not affect
# your coefficient, and the level is meaningful for descriptive plots.

# ============================================================
# 6. CATTLE price index  (PENDING — needs PPM Tabela 3939)
#    P_cattle_it = w_i * P_beef_t
#    weight w_i = cattle heads / municipal area  (base-year 2000–2002 avg)
#    Build cattle_weights_base.csv exactly like the crop weights, but with
#    herd counts from PPM (Tabela 3939) in the numerator. Expected columns:
#    geocode, municipality, state_abbreviation, weight
# ------------------------------------------------------------
# Activate once cattle_weights_base.csv exists:
#
cattle_weights <- read_csv(cattle_weights_path,
                           col_types = cols(geocode = col_character()))

cattle_weights

cattle_index <- cattle_weights |>
  crossing(beef_prices) |>
  mutate(price_index_cattle = weight * beef_index) |>
  select(geocode, municipality, state_abbreviation, year, price_index_cattle)

cattle_index

setwd("/Users/fcymbaluk/Projects/2026-amz-thesis")
getwd()

# ============================================================
# 7. assemble the price panel + save -------------------------
# For now: crop index only. Add the cattle join once section 6 is live.
price_index_panel <- crop_index |>
  left_join(cattle_index,
           by = c("geocode","municipality","state_abbreviation","year")) |>
  arrange(geocode, year) |>
  group_by(geocode) |>
  mutate(price_index_crop_lag1 = lag(price_index_crop)) |>  # for t-1 specs
  ungroup()

price_index_panel

write_csv(price_index_panel, "_data/outcome_price_index_panel.csv")

# ---- 8. sanity checks --------------------------------------
nrow(prices_norm)                                  # 21 years (2000–2020)
prices_norm |> filter(year == base_yr)             # all five should be 1
n_distinct(price_index_panel$geocode)              # municipalities covered
summary(price_index_panel$price_index_crop)
price_index_panel |> filter(is.nan(price_index_crop) | price_index_crop == 0)

