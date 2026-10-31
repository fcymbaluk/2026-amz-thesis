# derived_variables.R
# Purpose  Shared constructions of the derived variables the storage audit
#          (phase 2R, task 2.2) left to the analysis scripts, so that every
#          script builds them the same way as the pipeline builds the stored
#          main versions: a z-score on a reference population (pooled or
#          cross-sectional), a PPCDAm treatment flag on a listing window,
#          and the within-municipality normalised outcome on a reference
#          window. Sourced by scripts 05 and 06 and by the analysis scripts.
# Inputs   Vectors or a municipality-year data frame with `geocode`, `year`
#          and the named columns; windows as integer vectors of years.
# Outputs  z_on(), listed_within(), norm_within(). Definitions only.
# Status   Phase 2R, 2026-10-30 (D-2026-10-30-b, -c, -d, -e). z_on() is the
#          z_cross() of script 05 and the pooled standardisation of script
#          04; listed_within() and norm_within() are the functions of
#          scripts 05 and 06 with the column as an argument.

# Centre and spread taken on the rows where `ref` is TRUE and applied to
# every row; `ref` defaults to the whole vector (pooled). An analysis script
# passes its estimation sample.
z_on <- function(x, ref = rep(TRUE, length(x))) {
  (x - mean(x[ref], na.rm = TRUE)) / sd(x[ref], na.rm = TRUE)
}

# Treated means listed in any year of the window, so a municipality first
# listed after the window is a not-yet-treated control in that design
# (D-2026-10-12-b). Returns one row per geocode with the 0/1 column `name`.
listed_within <- function(d, years, name, listed = "ppcdam_list") {
  d %>%
    dplyr::filter(year %in% years) %>%
    dplyr::group_by(geocode) %>%
    dplyr::summarise(!!name := as.integer(max(.data[[listed]]) == 1L),
                     .groups = "drop")
}

# Each municipality centred and scaled on its own values over the reference
# years; a reference sd at or below `sd_floor` gives NA, never Inf
# (D-2026-10-12-f). `x` is the column name; the result is added as `name`.
norm_within <- function(d, x, years, name, sd_floor = 1e-9) {
  ref <- d %>%
    dplyr::filter(year %in% years) %>%
    dplyr::group_by(geocode) %>%
    dplyr::summarise(ref_mean = mean(.data[[x]]),
                     ref_sd   = sd(.data[[x]]),
                     .groups = "drop")
  d %>%
    dplyr::left_join(ref, by = "geocode") %>%
    dplyr::mutate(
      !!name := dplyr::if_else(ref_sd > sd_floor,
                               (.data[[x]] - ref_mean) / ref_sd,
                               NA_real_)
    ) %>%
    dplyr::select(-ref_mean, -ref_sd)
}
