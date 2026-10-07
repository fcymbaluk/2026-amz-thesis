# 02-social-bf.R
# PURPOSE   Build the municipality-year Bolsa Família panel (beneficiary
#           families, transfers in 2024 BRL, municipal quota and the ratios
#           over quota and PEA) for 2004-2019, all municipalities, with June
#           as the reference month and February as the alternative.
#
# INPUTS    _data/raw/mds/bf-YYYY.txt                MDS (SAGI, MI Social),
#                                                     yearly municipal monthly
#                                                     files, downloaded
#                                                     2026-10-06
#           _data/raw/mds/Atende_SIC_PBF_2003 a Out_2021_Acumulado_
#             Benef.Médio.xlsx (one file, BF_QUOTA_FILE)
#                                                     MDS (SENARC), LAI
#                                                     response, municipal
#                                                     quota by period
#           _data/raw/ibge/ibge_munic_id.xlsx         IBGE municipality codes
#           _data/aux/deflator_ipca_2024.csv          IPCA lookup, 2024 = 1
#           _data/interim/census_pea_informal_2000_2022.rds
#                                                     02-social-census.R
#
# OUTPUTS   _data/interim/bf_panel_2004_2019.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd
#            §2.4 until the annex is written; evidence: _audit/bf.qmd;
#            history: DECISIONS.md)
#   D-2026-10-05-a  Reference month June; February kept as the alternative.
#   D-2026-10-05-b  Panel truncated at 2019.
#   D-2026-10-05-c  Transfers deflated to 2024 BRL with the IPCA lookup.
#   D-2026-10-07-g  Quota per year from the four period columns of the LAI
#                   workbook (until 2005, 2006-2008, 2009-2011, 2012 on).
#   D-2026-10-07-h  Six-digit MDS codes mapped to seven-digit geocodes
#                   through the IBGE lookup.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.2  2020-2024 not built; the raw files exist (2023+ in a new
#             layout, no 2022 file).
#   Task 1.4  bf_quota carries the source values for the 14 Mato Grosso
#             municipalities with 2006-07 ratios above 3.
#   Task 1.8  bf_families_quota_ratio is 0 where the source reports zero
#             families against a positive quota (2004-2006).
# RUN ORDER 02-social-census.R must run first (PEA denominator). Feeds
#           04-data-panel.R, which keeps bf_ref_month == BF_REF_MONTH.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

# ---- Constants --------------------------------------------------------------
BF_YEARS      <- 2004:2019                                    # D-2026-10-05-b
BF_REF_MONTH  <- 6L                                           # D-2026-10-05-a
BF_ALT_MONTH  <- 2L                                           # D-2026-10-05-a
BF_DIR        <- "_data/raw/mds"
BF_QUOTA_FILE <- file.path(
  BF_DIR, "Atende_SIC_PBF_2003 a Out_2021_Acumulado_Benef.Médio.xlsx"
)
BF_QUOTA_HEADER_ROW <- 7L
# Period column header (regular expression) to the panel years it covers.
BF_QUOTA_PERIOD_YEARS <- list(                                # D-2026-10-07-g
  "até 2005"    = 2004:2005,
  "2006 a 2008" = 2006:2008,
  "2009 a 2011" = 2009:2011,
  "2012"        = 2012:2019
)
DEFLATOR_BASE <- 2024L                                        # D-2026-10-05-c
N_MUNIC_BR_EXPECTED    <- 5571L   # IBGE lookup and MDS files
N_MUNIC_QUOTA_EXPECTED <- 5570L
N_BF_MONTHS_EXPECTED   <- 12L

# ---- Read -------------------------------------------------------------------
bf_files <- file.path(BF_DIR, sprintf("bf-%d.txt", BF_YEARS))

read_bf <- function(file) {
  read_csv(
    file,
    col_types = cols(
      ibge   = col_character(),
      anomes = col_integer(),
      qtd_familias_beneficiarias_bolsa_familia = col_double(),
      valor_repassado_bolsa_familia            = col_double()
    ),
    progress = FALSE
  ) %>%
    mutate(source_file = basename(file))
}
bf_raw <- map_dfr(bf_files, read_bf)

quota_raw <- read_excel(BF_QUOTA_FILE, sheet = 1,
                        skip = BF_QUOTA_HEADER_ROW - 1L, col_types = "text",
                        .name_repair = "minimal")

munic_id <- read_xlsx("_data/raw/ibge/ibge_munic_id.xlsx",
                      sheet = "Municípios",
                      col_types = c("text", "numeric", "text", "text"))

deflator <- read_csv("_data/aux/deflator_ipca_2024.csv",
                     col_types = cols(year = col_integer(),
                                      ipca_index = col_double(),
                                      ipca_deflator = col_double()))

census_pea <- read_rds("_data/interim/census_pea_informal_2000_2022.rds") %>%
  select(geocode, year, pea)

# ---- Assert: raw state ------------------------------------------------------
# _audit/bf.qmd Q1, Q5.
geocode_map <- munic_id %>%
  transmute(ibge = str_sub(munic_id, 1, 6), geocode = munic_id)
stopifnot(
  all(file.exists(bf_files)),
  identical(names(bf_raw), c("ibge", "anomes",
                             "qtd_familias_beneficiarias_bolsa_familia",
                             "valor_repassado_bolsa_familia",
                             "source_file")),
  !anyDuplicated(paste(bf_raw$ibge, bf_raw$anomes)),
  all(nchar(bf_raw$ibge) == 6),
  n_distinct(bf_raw$ibge) == N_MUNIC_BR_EXPECTED,
  setequal(bf_raw$anomes %/% 100L, BF_YEARS),
  all(bf_raw$anomes %% 100L %in% 1:12),
  n_distinct(bf_raw$anomes) == length(BF_YEARS) * N_BF_MONTHS_EXPECTED,
  all(bf_raw$qtd_familias_beneficiarias_bolsa_familia >= 0, na.rm = TRUE),
  all(bf_raw$valor_repassado_bolsa_familia >= 0, na.rm = TRUE),
  !anyDuplicated(geocode_map$ibge),
  all(bf_raw$ibge %in% geocode_map$ibge),
  all(deflator$ipca_deflator[deflator$year == DEFLATOR_BASE] == 1),
  all(BF_YEARS %in% deflator$year)
)

# _audit/bf.qmd Q4: header row 7 names the four period columns.
quota_period_cols <- map_chr(names(BF_QUOTA_PERIOD_YEARS), function(p) {
  hit <- names(quota_raw)[str_detect(names(quota_raw), "ESTIMATIVA") &
                            str_detect(names(quota_raw), fixed(p))]
  stopifnot(length(hit) == 1L)
  hit
})
quota_munic <- quota_raw %>%
  select(UF, MUNICÍPIOS, IBGE, all_of(quota_period_cols)) %>%
  filter(!is.na(IBGE), str_detect(IBGE, "^[0-9]{7}$"))
stopifnot(
  identical(names(quota_raw)[1:4], c("UF", "MUNICÍPIOS", "IBGE", "REGIÃO")),
  nrow(quota_munic) == N_MUNIC_QUOTA_EXPECTED,
  !anyDuplicated(quota_munic$IBGE),
  all(quota_munic$IBGE %in% geocode_map$geocode),
  !anyNA(suppressWarnings(as.numeric(unlist(quota_munic[, quota_period_cols]))))
)

# ---- Transform --------------------------------------------------------------
deflator_years <- deflator %>% select(year, ipca_deflator)

bf_months <- bf_raw %>%
  mutate(year  = anomes %/% 100L,
         month = anomes %% 100L) %>%
  filter(month %in% c(BF_REF_MONTH, BF_ALT_MONTH)) %>%
  left_join(geocode_map, by = "ibge") %>%                     # D-2026-10-07-h
  left_join(deflator_years, by = "year") %>%
  transmute(
    geocode,
    year,
    bf_ref_month  = month,
    bf_families_n = qtd_familias_beneficiarias_bolsa_familia,
    bf_transfers_total_brl_2024 = valor_repassado_bolsa_familia *
      ipca_deflator,                                          # D-2026-10-05-c
    bf_transfers_avg_brl_2024 = if_else(
      bf_families_n > 0,
      bf_transfers_total_brl_2024 / bf_families_n,
      NA_real_
    )
  )

bf_quota <- quota_munic %>%
  select(geocode = IBGE, all_of(quota_period_cols)) %>%
  pivot_longer(-geocode, names_to = "period", values_to = "bf_quota") %>%
  mutate(
    bf_quota = as.numeric(bf_quota),
    years = BF_QUOTA_PERIOD_YEARS[match(period, quota_period_cols)]
  ) %>%
  select(-period) %>%
  unnest(years) %>%
  rename(year = years)

bf_panel <- bf_months %>%
  left_join(bf_quota, by = c("geocode", "year")) %>%
  left_join(census_pea, by = c("geocode", "year")) %>%
  mutate(
    bf_families_quota_ratio = if_else(
      !is.na(bf_quota) & bf_quota > 0,
      bf_families_n / bf_quota,
      NA_real_
    ),
    bf_transfers_pea_brl_2024 = if_else(
      !is.na(pea) & pea > 0,
      bf_transfers_total_brl_2024 / pea,
      NA_real_
    ),
    bf_transfers_quota_brl_2024 = if_else(
      !is.na(bf_quota) & bf_quota > 0,
      bf_transfers_total_brl_2024 / bf_quota,
      NA_real_
    )
  ) %>%
  select(geocode, year, bf_ref_month, bf_families_n,
         bf_transfers_total_brl_2024, bf_transfers_avg_brl_2024, bf_quota,
         bf_families_quota_ratio, bf_transfers_pea_brl_2024,
         bf_transfers_quota_brl_2024) %>%
  arrange(bf_ref_month, geocode, year)

# ---- Assert: output state ---------------------------------------------------
# _audit/bf.qmd Q1, Q4.
stopifnot(
  nrow(bf_panel) == N_MUNIC_BR_EXPECTED * length(BF_YEARS) * 2L,
  !anyDuplicated(paste(bf_panel$geocode, bf_panel$year,
                       bf_panel$bf_ref_month)),
  !anyNA(bf_panel$geocode),
  all(nchar(bf_panel$geocode) == 7),
  all(bf_panel$bf_ref_month %in% c(BF_REF_MONTH, BF_ALT_MONTH)),
  all(bf_panel$bf_families_quota_ratio >= 0, na.rm = TRUE),
  all(bf_panel$bf_transfers_pea_brl_2024 >= 0, na.rm = TRUE),
  all(bf_panel$bf_transfers_quota_brl_2024 >= 0, na.rm = TRUE),
  identical(is.na(bf_panel$bf_quota),
            !bf_panel$geocode %in% quota_munic$IBGE),
  is.character(bf_panel$geocode),
  is.integer(bf_panel$year),
  is.integer(bf_panel$bf_ref_month),
  is.double(bf_panel$bf_families_n),
  is.double(bf_panel$bf_quota),
  is.double(bf_panel$bf_transfers_total_brl_2024)
)

# ---- Write ------------------------------------------------------------------
write_rds(bf_panel, "_data/interim/bf_panel_2004_2019.rds")
write_csv(bf_panel, "_data/interim/bf_panel_2004_2019.csv")
