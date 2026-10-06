# 01-env-ppcdam.R
# PURPOSE   Build the municipality-year PPCDAm priority-list indicator
#           (ppcdam_list, 0/1) for the listed municipalities, 2004-2024.
#
# INPUTS    _data/raw/ppcdam/ppcdam_lists.xlsx   Author's compilation of the MMA
#                                                ordinances, sheet Lista
#           _data/raw/ibge/ibge_munic_id.xlsx    IBGE municipality codes and
#                                                names, sheet Municípios
#
# OUTPUTS   _data/interim/ppcdam_list_2004_2024.rds
#           _data/interim/ppcdam_list_2004_2024.csv
#
# DECISIONS IMPLEMENTED
#           (justification: _annex/parked/u-appendix-data-description.qmd §1.3
#            until the annex is written; evidence: _audit/ppcdam.qmd;
#            history: DECISIONS.md)
#   D-2026-10-06-f  Geocodes attached from the IBGE lookup by municipality name
#                   and state; the recovered compilation carries none.
#   D-2026-10-06-g  Sheet Lista is the indicator; its single missing cell
#                   (Grajaú, MA, 2024) stays NA.
#
# KNOWN DEFECTS (phase 1 only; delete this block when the fix lands)
#   none
# RUN ORDER First script; no pipeline inputs. Feeds 04-data-panel.R, which
#           codes unlisted municipality-years as 0.

library(tidyverse)  # magrittr %>% throughout, never the native pipe
library(readxl)

# ---- Constants --------------------------------------------------------------
PPCDAM_SHEET <- "Lista"                                       # D-2026-10-06-g
PPCDAM_YEARS <- 2004:2024
PPCDAM_STATE_UF <- c(Acre = "AC", Amazonas = "AM",            # D-2026-10-06-f
                     Maranhão = "MA", `Mato Grosso` = "MT",
                     Pará = "PA", Rondônia = "RO", Roraima = "RR")
N_PPCDAM_MUNIC_EXPECTED  <- 92L
N_PPCDAM_LISTED_EXPECTED <- 805L  # municipality-years listed, 2008-2024
N_PPCDAM_NA_EXPECTED     <- 1L                                # D-2026-10-06-g

# ---- Read -------------------------------------------------------------------
ppcdam_wide <- read_xlsx("_data/raw/ppcdam/ppcdam_lists.xlsx",
                         sheet = PPCDAM_SHEET)

munic_id <- read_xlsx("_data/raw/ibge/ibge_munic_id.xlsx",
                      sheet = "Municípios",
                      col_types = c("text", "numeric", "text", "text"))

# ---- Assert: raw state ------------------------------------------------------
year_cols <- as.character(PPCDAM_YEARS)

# _audit/ppcdam.qmd Q1, Q2, Q5.
stopifnot(
  identical(names(ppcdam_wide), c("Município", "Estado", year_cols)),
  nrow(ppcdam_wide) == N_PPCDAM_MUNIC_EXPECTED,
  !anyDuplicated(paste(ppcdam_wide$Município, ppcdam_wide$Estado)),
  all(unlist(ppcdam_wide[, year_cols]) %in% c(0, 1, NA)),
  sum(is.na(ppcdam_wide[, year_cols])) == N_PPCDAM_NA_EXPECTED,
  all(ppcdam_wide$Estado %in% names(PPCDAM_STATE_UF)),
  identical(names(munic_id), c("uf", "uf_id", "munic_id", "munic")),
  all(nchar(munic_id$munic_id) == 7),
  !anyDuplicated(munic_id$munic_id),
  !anyDuplicated(paste(munic_id$munic, munic_id$uf))
)

# ---- Transform --------------------------------------------------------------
ppcdam_long <- ppcdam_wide %>%
  mutate(uf = unname(PPCDAM_STATE_UF[Estado])) %>%
  left_join(munic_id %>% select(munic, uf, geocode = munic_id),
            by = c("Município" = "munic", "uf")) %>%
  select(geocode, municipality = Município, all_of(year_cols)) %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "ppcdam_list",
    names_transform = list(year = as.integer),
    values_transform = list(ppcdam_list = as.integer)
  ) %>%
  arrange(geocode, year)

# ---- Assert: output state ---------------------------------------------------
# _audit/ppcdam.qmd Q2, Q4, Q5.
stopifnot(
  nrow(ppcdam_long) == N_PPCDAM_MUNIC_EXPECTED * length(PPCDAM_YEARS),
  !anyNA(ppcdam_long$geocode),
  n_distinct(ppcdam_long$geocode) == N_PPCDAM_MUNIC_EXPECTED,
  all(nchar(ppcdam_long$geocode) == 7),
  !anyDuplicated(paste(ppcdam_long$geocode, ppcdam_long$year)),
  sum(ppcdam_long$ppcdam_list, na.rm = TRUE) == N_PPCDAM_LISTED_EXPECTED,
  sum(is.na(ppcdam_long$ppcdam_list)) == N_PPCDAM_NA_EXPECTED,
  is.character(ppcdam_long$geocode),
  is.integer(ppcdam_long$year),
  is.integer(ppcdam_long$ppcdam_list)
)

# ---- Write ------------------------------------------------------------------
write_rds(ppcdam_long, "_data/interim/ppcdam_list_2004_2024.rds")
write_csv(ppcdam_long, "_data/interim/ppcdam_list_2004_2024.csv")
