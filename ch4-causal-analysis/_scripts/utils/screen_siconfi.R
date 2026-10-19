# screen_siconfi.R
# Purpose  D-2026-10-16-b: order-of-magnitude screen for SICONFI municipal
#          filings in the analysis layer. A municipality-year whose per-capita
#          total (expenditure, or current revenue) is more than `factor` times
#          or less than 1/`factor` of the municipality's own median over the
#          years it reports is a filing in the wrong unit, and is treated as a
#          reporting gap (NA on every column of that side), never corrected.
#          Definitions only; sourced by scripts 10 and 11. The interim files
#          of script 09 are not changed.
# Inputs   none
# Outputs  siconfi_flags(), siconfi_screen()
# Status   2026-10-16, sprint day 4

SICONFI_SCREEN_FACTOR <- 10

# Flagged municipality-years of one side, with the ratio to the own median.
siconfi_flags <- function(df, total_col, factor = SICONFI_SCREEN_FACTOR) {
  df %>%
    dplyr::filter(!is.na(.data[[total_col]])) %>%
    dplyr::group_by(geocode) %>%
    dplyr::mutate(ratio_to_median = .data[[total_col]] /
                    stats::median(.data[[total_col]])) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ratio_to_median > factor | ratio_to_median < 1 / factor) %>%
    dplyr::select(geocode, year, ratio_to_median)
}

# Set `cols` to NA on the flagged municipality-years of `df`.
siconfi_screen <- function(df, flags, cols) {
  key <- paste(df$geocode, df$year) %in% paste(flags$geocode, flags$year)
  df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(cols),
                                ~ dplyr::if_else(key, NA_real_, .x)))
}
