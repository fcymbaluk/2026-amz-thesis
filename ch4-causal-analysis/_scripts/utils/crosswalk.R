# crosswalk.R
# Purpose  The variable rename of phase 2R (rebuild/naming-convention.md,
#          D-2026-10-30-a): read the crosswalk, rename a dataset through it,
#          and guard the convention so that any column outside the grammar
#          fails loudly. Sourced by scripts 05 (municipality store) and 06
#          (final panel); the analysis scripts may use the same functions.
# Inputs   _data/aux/crosswalk.csv (old_name, new_name, the grammar slots,
#          `panel` and `store` flags). Definitions only at source time.
# Outputs  Functions: read_crosswalk(), rename_through_crosswalk(),
#          assert_naming_convention(). The prefixes and keys as constants.
# Status   Phase 2R, 2026-10-30.

NAME_KEYS     <- c("geocode", "year", "municipality", "state", "uf")
NAME_PREFIXES <- c("env", "enf", "sop", "soc", "lab", "dem", "ecn", "agr",
                   "mkt", "pol", "geo", "smp")
NAME_MAX_CHAR <- 40L
CROSSWALK_PATH <- "_data/aux/crosswalk.csv"

read_crosswalk <- function(path = CROSSWALK_PATH) {
  xwalk <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character(),
                            panel = readr::col_logical(),
                            store = readr::col_logical()),
    show_col_types = FALSE, progress = FALSE
  )
  stopifnot(
    all(c("old_name", "new_name", "panel", "store") %in% names(xwalk)),
    !anyDuplicated(xwalk$old_name),
    !anyDuplicated(xwalk$new_name),
    !anyNA(xwalk$panel), !anyNA(xwalk$store)
  )
  xwalk
}

# Renames every column of `df` found in old_name; a column already under
# its new name, or absent from the crosswalk, is left as it is.
rename_through_crosswalk <- function(df, xwalk) {
  map <- xwalk[xwalk$old_name %in% names(df) &
                 xwalk$old_name != xwalk$new_name, ]
  new_names <- stats::setNames(map$old_name, map$new_name)
  dplyr::rename(df, dplyr::any_of(new_names))
}

# The convention guard (naming-convention.md, section 6, item 4): keys or a
# domain prefix, snake_case ASCII, at most NAME_MAX_CHAR characters, no
# duplicate.
assert_naming_convention <- function(nms, keys = NAME_KEYS,
                                     prefixes = NAME_PREFIXES) {
  prefix_re <- paste0("^(", paste(prefixes, collapse = "|"), ")_")
  stopifnot(
    all(nms %in% keys | grepl(prefix_re, nms)),
    all(grepl("^[a-z][a-z0-9_]*$", nms)),
    !any(grepl("__", nms)),
    all(nchar(nms) <= NAME_MAX_CHAR),
    !anyDuplicated(nms)
  )
  invisible(TRUE)
}
