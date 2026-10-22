# 02-social-bf.R
# PURPOSE   Build the municipality-year Bolsa Família panel (beneficiary
#           families, transfers in 2024 BRL, municipal quota, the ratios
#           over quota and PEA, and the programme regime of the stored
#           month) for 2004-2024, all municipalities, with June as the
#           reference month and February as the alternative.
#
# INPUTS    _data/raw/mds/bf-YYYY.txt                MDS (SAGI, MI Social),
#                                                     yearly municipal monthly
#                                                     files, downloaded
#                                                     2026-10-06; 2004-2021
#                                                     in the old layout,
#                                                     2023-2024 in the new
#                                                     one; no 2022 file
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
# OUTPUTS   _data/interim/bf_panel_2004_2024.rds / .csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd
#            §2.4 until the annex is written; evidence: _audit/bf.qmd;
#            history: DECISIONS.md)
#   D-2026-10-05-a  Reference month June; February kept as the alternative.
#   D-2026-10-05-c  Transfers deflated to 2024 BRL with the IPCA lookup.
#   D-2026-10-07-g  Quota per year from the four period columns of the LAI
#                   workbook (until 2005, 2006-2008, 2009-2011, 2012 on).
#   D-2026-10-07-h  Six-digit MDS codes mapped to seven-digit geocodes
#                   through the IBGE lookup.
#   D-2026-10-21-a  Mato Grosso estimates of the LAI workbook realigned to
#                   their municipalities (phase 2, step 2.1; task 1.4).
#   D-2026-10-22-a  Series extended to 2024 across the two file layouts;
#                   the 2004-2024 grid completed, 2022 empty (phase 2,
#                   step 2.2; task 1.2; supersedes D-2026-10-05-b).
#   D-2026-10-22-b  Auxílio Emergencial substitution months stored as
#                   reported and flagged by bf_regime (step 2.2).
#   D-2026-10-22-c  Quota carried to 2021 by the workbook's last period,
#                   NA from 2022 (step 2.2).
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   Task 1.8  bf_families_quota_ratio is 0 where the source reports zero
#             families against a positive quota (2004-2006).
# RUN ORDER 02-social-census.R must run first (PEA denominator). Feeds
#           04-panel-assemble.R, which keeps bf_ref_month == BF_REF_MONTH.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

# ---- Constants --------------------------------------------------------------
BF_YEARS      <- 2004:2024                                    # D-2026-10-22-a
BF_REF_MONTH  <- 6L                                           # D-2026-10-05-a
BF_ALT_MONTH  <- 2L                                           # D-2026-10-05-a
BF_DIR        <- "_data/raw/mds"
# Yearly files by layout (D-2026-10-22-a): the old layout ends with 2021,
# the new one starts with 2023; no file exists for 2022.
BF_OLD_LAYOUT_YEARS <- 2004:2021
BF_NEW_LAYOUT_YEARS <- 2023:2024
BF_FILE_YEARS       <- c(BF_OLD_LAYOUT_YEARS, BF_NEW_LAYOUT_YEARS)
BF_GAP_YEARS        <- setdiff(BF_YEARS, BF_FILE_YEARS)       # 2022
BF_NEW_FIRST_ANOMES <- 202303L   # the re-created programme's first month
BF_QUOTA_FILE <- file.path(
  BF_DIR, "Atende_SIC_PBF_2003 a Out_2021_Acumulado_Benef.Médio.xlsx"
)
BF_QUOTA_HEADER_ROW <- 7L
# Period column header (regular expression) to the panel years it covers.
# The last period runs to the workbook's end, October 2021 (D-2026-10-22-c).
BF_QUOTA_PERIOD_YEARS <- list(                                # D-2026-10-07-g
  "até 2005"    = 2004:2005,
  "2006 a 2008" = 2006:2008,
  "2009 a 2011" = 2009:2011,
  "2012"        = 2012:2021
)
BF_QUOTA_LAST_YEAR <- max(unlist(BF_QUOTA_PERIOD_YEARS))      # 2021
DEFLATOR_BASE <- 2024L                                        # D-2026-10-05-c
N_MUNIC_BR_EXPECTED     <- 5571L   # IBGE lookup and old-layout files
N_MUNIC_BR_NEW_EXPECTED <- 5570L   # new-layout files until October 2025
N_MUNIC_QUOTA_EXPECTED  <- 5570L
BF_NEW_MISSING_IBGE     <- "510183"  # Boa Esperança do Norte (MT), 2025
N_BF_MONTHS_EXPECTED    <- 12L
# Programme regime by month (D-2026-10-22-b): families paid the Auxílio
# Emergencial left the Bolsa Família payroll in April-December 2020 and
# April-October 2021; Auxílio Brasil replaced the programme from November
# 2021 to February 2023; the re-created programme pays from March 2023.
BF_REGIME_LEVELS <- c("pbf", "pbf_emergency_aid", "auxilio_brasil",
                      "pbf_2023")
BF_EMERGENCY_ANOMES <- c(202004:202012, 202104:202110)
BF_AUXILIO_BRASIL_ANOMES <- c(202111:202112, 202201:202212, 202301:202302)
# Phase 2, step 2.1 (D-2026-10-21-a): the Mato Grosso block of the LAI
# workbook lists two municipalities ahead of the alphabetical order while its
# four estimate columns follow that order; see the realignment block below.
BF_QUOTA_REALIGN_UF        <- "MT"
BF_QUOTA_REALIGN_N         <- 141L
BF_QUOTA_REALIGN_DISPLACED <- c("5107743", "5108303")  # Santa Cruz do Xingu,
                                                       # União do Sul
BF_QUOTA_REALIGN_SHIFT     <- c(`-2` = 113L, `-1` = 21L, `0` = 5L)

# ---- Read -------------------------------------------------------------------
bf_files_old <- file.path(BF_DIR, sprintf("bf-%d.txt", BF_OLD_LAYOUT_YEARS))
bf_files_new <- file.path(BF_DIR, sprintf("bf-%d.txt", BF_NEW_LAYOUT_YEARS))

read_bf_old <- function(file) {
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
# New layout (D-2026-10-22-a): renamed key columns, "_s" suffixes on the
# value columns and a fifth column, the provider's average benefit, which is
# not carried. Every column is read as text and parsed explicitly.
read_bf_new <- function(file) {
  read_csv(file, col_types = cols(.default = col_character()),
           progress = FALSE) %>%
    transmute(
      ibge   = codigo_ibge,
      anomes = as.integer(anomes_s),
      qtd_familias_beneficiarias_bolsa_familia =
        as.numeric(qtd_familias_beneficiarias_bolsa_familia_s),
      valor_repassado_bolsa_familia =
        as.numeric(valor_repassado_bolsa_familia_s),
      source_file = basename(file)
    )
}
bf_raw_old <- map_dfr(bf_files_old, read_bf_old)
bf_raw_new <- map_dfr(bf_files_new, read_bf_new)
bf_raw     <- bind_rows(bf_raw_old, bf_raw_new)

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
# _audit/bf.qmd Q1, Q5, Q6.
geocode_map <- munic_id %>%
  transmute(ibge = str_sub(munic_id, 1, 6), geocode = munic_id)
new_first_file <- names(read_csv(bf_files_new[1], n_max = 0,
                                 col_types = cols(.default = col_character())))
stopifnot(
  all(file.exists(c(bf_files_old, bf_files_new))),
  !any(file.exists(file.path(BF_DIR, sprintf("bf-%d.txt", BF_GAP_YEARS)))),
  identical(names(bf_raw), c("ibge", "anomes",
                             "qtd_familias_beneficiarias_bolsa_familia",
                             "valor_repassado_bolsa_familia",
                             "source_file")),
  identical(new_first_file,
            c("codigo_ibge", "anomes_s",
              "qtd_familias_beneficiarias_bolsa_familia_s",
              "valor_repassado_bolsa_familia_s", "pbf_vlr_medio_benef_f")),
  !anyDuplicated(paste(bf_raw$ibge, bf_raw$anomes)),
  all(nchar(bf_raw$ibge) == 6),
  n_distinct(bf_raw_old$ibge) == N_MUNIC_BR_EXPECTED,
  n_distinct(bf_raw_new$ibge) == N_MUNIC_BR_NEW_EXPECTED,
  identical(setdiff(bf_raw_old$ibge, bf_raw_new$ibge), BF_NEW_MISSING_IBGE),
  setequal(bf_raw$anomes %/% 100L, BF_FILE_YEARS),
  all(bf_raw$anomes %% 100L %in% 1:12),
  n_distinct(bf_raw_old$anomes) ==
    length(BF_OLD_LAYOUT_YEARS) * N_BF_MONTHS_EXPECTED,
  min(bf_raw_new$anomes) == BF_NEW_FIRST_ANOMES,
  max(bf_raw_new$anomes) == max(BF_NEW_LAYOUT_YEARS) * 100L + 12L,
  # The new layout parses without loss: no NA where the text is non-empty.
  !anyNA(bf_raw_new$qtd_familias_beneficiarias_bolsa_familia),
  !anyNA(bf_raw_new$valor_repassado_bolsa_familia),
  all(bf_raw$qtd_familias_beneficiarias_bolsa_familia >= 0, na.rm = TRUE),
  all(bf_raw$valor_repassado_bolsa_familia >= 0, na.rm = TRUE),
  !anyDuplicated(geocode_map$ibge),
  all(bf_raw$ibge %in% geocode_map$ibge),
  all(deflator$ipca_deflator[deflator$year == DEFLATOR_BASE] == 1),
  all(BF_YEARS %in% deflator$year),
  max(BF_YEARS) == DEFLATOR_BASE
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

# ---- Fix: realign the Mato Grosso estimates (D-2026-10-21-a) ----------------
# _audit/bf.qmd Q4b. In the workbook's Mato Grosso block the municipality
# rows list Santa Cruz do Xingu and União do Sul first and the other 139 in
# alphabetical order, while the four estimate columns run in strict
# alphabetical order over all 141. The estimate on the i-th Mato Grosso row
# therefore belongs to the i-th Mato Grosso municipality by name: two rows up
# for the 113 municipalities before Santa Cruz do Xingu's slot, one row up for
# the 21 before União do Sul's slot, none for the last five. The other 26
# states are consistent as stored. The raw file is untouched.
mt_rows  <- which(quota_munic$UF == BF_QUOTA_REALIGN_UF)
mt_alpha <- mt_rows[order(quota_munic$MUNICÍPIOS[mt_rows], method = "radix")]
mt_shift <- match(mt_rows, mt_alpha) - seq_along(mt_rows)
mt_shift_counts <- as.integer(
  table(factor(mt_shift[-(1:2)], levels = names(BF_QUOTA_REALIGN_SHIFT)))
)
stopifnot(
  length(mt_rows) == BF_QUOTA_REALIGN_N,
  identical(quota_munic$IBGE[mt_rows[1:2]], BF_QUOTA_REALIGN_DISPLACED),
  identical(order(quota_munic$MUNICÍPIOS[mt_rows[-(1:2)]], method = "radix"),
            seq_len(BF_QUOTA_REALIGN_N - 2L)),
  identical(mt_shift_counts, unname(BF_QUOTA_REALIGN_SHIFT)),
  all(mt_shift[1:2] > 0)
)
quota_munic[mt_alpha, quota_period_cols] <- quota_munic[mt_rows, quota_period_cols]

# ---- Transform --------------------------------------------------------------
deflator_years <- deflator %>% select(year, ipca_deflator)

# Regime of a stored month (D-2026-10-22-b).
bf_regime_of <- function(anomes) {
  case_when(
    anomes %in% BF_EMERGENCY_ANOMES      ~ "pbf_emergency_aid",
    anomes %in% BF_AUXILIO_BRASIL_ANOMES ~ "auxilio_brasil",
    anomes >= BF_NEW_FIRST_ANOMES        ~ "pbf_2023",
    TRUE                                 ~ "pbf"
  )
}

# The full grid, both months, every municipality of the lookup and every
# year 2004-2024, so that 2022, February 2023 and the municipality absent
# from the new layout carry explicit NA rows with their regime
# (D-2026-10-22-a).
bf_grid <- expand_grid(
  geocode = geocode_map$geocode,
  year    = BF_YEARS,
  month   = c(BF_REF_MONTH, BF_ALT_MONTH)
) %>%
  mutate(anomes = year * 100L + month)

bf_months <- bf_grid %>%
  left_join(bf_raw %>% select(-source_file) %>%
              left_join(geocode_map, by = "ibge") %>%          # D-2026-10-07-h
              select(-ibge),
            by = c("geocode", "anomes")) %>%
  left_join(deflator_years, by = "year") %>%
  transmute(
    geocode,
    year,
    bf_ref_month  = month,
    bf_regime     = bf_regime_of(anomes),                     # D-2026-10-22-b
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
  left_join(bf_quota, by = c("geocode", "year")) %>%          # D-2026-10-22-c
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
  select(geocode, year, bf_ref_month, bf_regime, bf_families_n,
         bf_transfers_total_brl_2024, bf_transfers_avg_brl_2024, bf_quota,
         bf_families_quota_ratio, bf_transfers_pea_brl_2024,
         bf_transfers_quota_brl_2024) %>%
  arrange(bf_ref_month, geocode, year)

# ---- Assert: output state ---------------------------------------------------
# _audit/bf.qmd Q1, Q4, Q4b (the realigned Mato Grosso quota tracks the
# June 2006 beneficiary count like every other state's), Q6 (the extension:
# the regime by year and month, the empty 2022, the substitution months'
# transfer per family, the quota endpoint).
mt_check <- bf_panel %>%
  filter(geocode %in% quota_munic$IBGE[mt_rows], year == 2006L,
         bf_ref_month == BF_REF_MONTH)
value_cols <- c("bf_families_n", "bf_transfers_total_brl_2024",
                "bf_transfers_avg_brl_2024", "bf_families_quota_ratio",
                "bf_transfers_pea_brl_2024", "bf_transfers_quota_brl_2024")
regime_by_month <- bf_panel %>%
  distinct(year, bf_ref_month, bf_regime) %>%
  arrange(bf_ref_month, year)
ref_rows <- bf_panel$bf_ref_month == BF_REF_MONTH
june_per_family <- bf_panel %>%
  filter(ref_rows, bf_families_n > 0) %>%
  group_by(year, bf_regime) %>%
  summarise(per_family = sum(bf_transfers_total_brl_2024) /
              sum(bf_families_n), .groups = "drop")
per_family_2019 <- june_per_family$per_family[june_per_family$year == 2019L]
stopifnot(
  nrow(mt_check) == BF_QUOTA_REALIGN_N,
  cor(mt_check$bf_families_n, mt_check$bf_quota) > 0.95,
  all(mt_check$bf_families_quota_ratio < 3, na.rm = TRUE),
  nrow(bf_panel) == N_MUNIC_BR_EXPECTED * length(BF_YEARS) * 2L,
  !anyDuplicated(paste(bf_panel$geocode, bf_panel$year,
                       bf_panel$bf_ref_month)),
  !anyNA(bf_panel$geocode),
  all(nchar(bf_panel$geocode) == 7),
  setequal(bf_panel$year, BF_YEARS),
  all(bf_panel$bf_ref_month %in% c(BF_REF_MONTH, BF_ALT_MONTH)),
  # One regime per stored month, every level used, the sequence as designed.
  nrow(regime_by_month) == length(BF_YEARS) * 2L,
  setequal(bf_panel$bf_regime, BF_REGIME_LEVELS),
  all(bf_panel$bf_regime[bf_panel$year <= 2019L] == "pbf"),
  identical(regime_by_month$bf_regime[regime_by_month$bf_ref_month ==
                                        BF_REF_MONTH &
                                        regime_by_month$year %in% 2020:2024],
            c("pbf_emergency_aid", "pbf_emergency_aid", "auxilio_brasil",
              "pbf_2023", "pbf_2023")),
  identical(regime_by_month$bf_regime[regime_by_month$bf_ref_month ==
                                        BF_ALT_MONTH &
                                        regime_by_month$year %in% 2020:2024],
            c("pbf", "pbf", "auxilio_brasil", "auxilio_brasil", "pbf_2023")),
  # The gap years and the Auxílio Brasil months carry no values.
  all(is.na(unlist(bf_panel[bf_panel$bf_regime == "auxilio_brasil",
                            value_cols]))),
  all(is.na(bf_panel$bf_families_n[bf_panel$year %in% BF_GAP_YEARS])),
  # The substitution months are disbursement artifacts: the June transfer
  # per family is below half of 2019's in both years and below a tenth in
  # 2020, when the first Auxílio Emergencial round took most families off
  # the payroll.
  all(june_per_family$per_family[june_per_family$bf_regime ==
                                   "pbf_emergency_aid"] <
        0.5 * per_family_2019),
  june_per_family$per_family[june_per_family$year == 2020L] <
    0.1 * per_family_2019,
  # The re-created programme pays more than twice 2019's transfer per
  # family in 2024 BRL.
  all(june_per_family$per_family[june_per_family$bf_regime == "pbf_2023"] >
        2 * per_family_2019),
  # The municipality absent from the new layout is NA there and only there
  # among the file years.
  all(is.na(bf_panel$bf_families_n[bf_panel$year %in% BF_NEW_LAYOUT_YEARS &
                                     bf_panel$geocode ==
                                       geocode_map$geocode[geocode_map$ibge ==
                                                             BF_NEW_MISSING_IBGE]])),
  all(bf_panel$bf_families_quota_ratio >= 0, na.rm = TRUE),
  all(bf_panel$bf_transfers_pea_brl_2024 >= 0, na.rm = TRUE),
  all(bf_panel$bf_transfers_quota_brl_2024 >= 0, na.rm = TRUE),
  # Quota: present for the workbook's municipalities through 2021, NA after
  # (D-2026-10-22-c).
  identical(is.na(bf_panel$bf_quota),
            !bf_panel$geocode %in% quota_munic$IBGE |
              bf_panel$year > BF_QUOTA_LAST_YEAR),
  is.character(bf_panel$geocode),
  is.integer(bf_panel$year),
  is.integer(bf_panel$bf_ref_month),
  is.character(bf_panel$bf_regime),
  is.double(bf_panel$bf_families_n),
  is.double(bf_panel$bf_quota),
  is.double(bf_panel$bf_transfers_total_brl_2024)
)

# ---- Write ------------------------------------------------------------------
write_rds(bf_panel, "_data/interim/bf_panel_2004_2024.rds")
write_csv(bf_panel, "_data/interim/bf_panel_2004_2024.csv")
