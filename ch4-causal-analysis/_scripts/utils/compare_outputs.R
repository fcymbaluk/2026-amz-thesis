# compare_outputs.R
# Purpose  Acceptance comparator for phases 1 and 2: joins a new output to its
#          reference on the panel keys and reports, per variable, cells that
#          differ beyond a tolerance, rows present in one file only, and the
#          municipalities and years affected. Type mismatches (numeric in one
#          file, text in the other) are reported separately, never coerced away.
# Inputs   Two files (.csv or .rds); optional name map (old_name, new_name) for
#          comparisons across the phase 2R rename.
# Outputs  A `compare_report` list; optionally the per-variable summary as csv.
#          Definitions only when sourced. CLI when run with Rscript:
#            Rscript _scripts/utils/compare_outputs.R new.csv ref.csv \
#              [report.csv]
# Status   Phase 0 self-tested on the reference against itself, 2026-10-05.

suppressPackageStartupMessages(library(tidyverse))  # magrittr %>%, never |>

read_output <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    read_rds(path)
  } else if (ext == "csv") {
    # Every column as text: a type comparison must see what the file holds,
    # not what readr guesses from the first rows.
    read_csv(path, col_types = cols(.default = col_character()),
             na = character(), show_col_types = FALSE, progress = FALSE)
  } else {
    stop("compare_outputs: unsupported file type: ", path)
  }
}

# A text column counts as numeric when every non-missing cell parses as a
# number.
# A typed column (from .rds) counts as numeric when R says so.
numeric_parse <- function(x) {
  if (is.numeric(x) || is.logical(x)) return(as.numeric(x))
  y <- as.character(x)
  y[y %in% c("", "NA")] <- NA_character_
  parsed <- suppressWarnings(as.numeric(y))
  if (all(is.na(parsed) == is.na(y))) parsed else NULL
}

as_text <- function(x) {
  y <- as.character(x)
  y[y %in% c("", "NA")] <- NA_character_
  y
}

compare_outputs <- function(new, ref, keys = c("geocode", "year"), tol = 1e-8,
                            name_map = NULL, report_path = NULL) {
  new_df <- if (is.character(new)) read_output(new) else as_tibble(new)
  ref_df <- if (is.character(ref)) read_output(ref) else as_tibble(ref)

  if (!is.null(name_map)) {
    stopifnot(all(c("old_name", "new_name") %in% names(name_map)))
    map <- name_map %>% filter(new_name %in% names(new_df))
    new_df <- new_df %>% rename(!!!set_names(map$new_name, map$old_name))
  }

  missing_keys <- setdiff(keys, intersect(names(new_df), names(ref_df)))
  if (length(missing_keys) > 0) {
    stop("compare_outputs: key column(s) absent from one file: ",
         paste(missing_keys, collapse = ", "))
  }

  new_df <- new_df %>%
    mutate(across(all_of(keys), ~ str_trim(as.character(.x))))
  ref_df <- ref_df %>%
    mutate(across(all_of(keys), ~ str_trim(as.character(.x))))

  dup_new <- new_df %>% count(across(all_of(keys))) %>% filter(n > 1)
  dup_ref <- ref_df %>% count(across(all_of(keys))) %>% filter(n > 1)
  if (nrow(dup_new) > 0 || nrow(dup_ref) > 0) {
    stop("compare_outputs: duplicate keys (new: ", nrow(dup_new),
         ", ref: ", nrow(dup_ref), "); the join would be ambiguous.")
  }

  cols_only_new <- setdiff(names(new_df), names(ref_df))
  cols_only_ref <- setdiff(names(ref_df), names(new_df))
  shared <- setdiff(intersect(names(new_df), names(ref_df)), keys)

  rows_only_new <- anti_join(new_df, ref_df, by = keys) %>% select(all_of(keys))
  rows_only_ref <- anti_join(ref_df, new_df, by = keys) %>% select(all_of(keys))

  joined <- inner_join(new_df, ref_df, by = keys, suffix = c(".new", ".ref"))

  compare_one <- function(v) {
    x <- joined[[paste0(v, ".new")]]
    y <- joined[[paste0(v, ".ref")]]
    xn <- numeric_parse(x)
    yn <- numeric_parse(y)
    if (!is.null(xn) && !is.null(yn)) {
      type_status <- "both_numeric"
      both_na <- is.na(xn) & is.na(yn)
      one_na  <- (is.na(xn) | is.na(yn)) & !both_na
      abs_diff <- abs(xn - yn)
      differs <- one_na | (!both_na & !one_na & abs_diff > tol)
      max_abs <- if (any(!is.na(abs_diff))) {
        max(abs_diff, na.rm = TRUE)
      } else {
        NA_real_
      }
    } else {
      type_status <- if (is.null(xn) == is.null(yn)) {
        "both_character"
      } else {
        "type_mismatch"
      }
      xt <- as_text(x)
      yt <- as_text(y)
      both_na <- is.na(xt) & is.na(yt)
      one_na  <- (is.na(xt) | is.na(yt)) & !both_na
      differs <- one_na | (!both_na & !one_na & xt != yt)
      max_abs <- NA_real_
    }
    affected <- joined[differs, keys, drop = FALSE] %>%
      mutate(variable = v, .before = 1)
    summary <- tibble(
      variable        = v,
      type_status     = type_status,
      n_diff          = sum(differs),
      n_na_new_only   = sum(is.na(as_text(x)) & !is.na(as_text(y))),
      n_na_ref_only   = sum(!is.na(as_text(x)) & is.na(as_text(y))),
      max_abs_diff    = max_abs,
      n_munic         = n_distinct(affected[[keys[1]]]),
      years           = if (length(keys) > 1) {
        collapse_years(affected[[keys[2]]])
      } else {
        NA_character_
      }
    )
    list(summary = summary, affected = affected)
  }

  results <- map(shared, compare_one)
  summary <- map_dfr(results, "summary")
  affected <- map_dfr(results, "affected")

  schema_ok <- length(cols_only_new) == 0 && length(cols_only_ref) == 0
  rows_ok   <- nrow(rows_only_new) == 0 && nrow(rows_only_ref) == 0
  values_ok <- nrow(summary) == 0 || all(summary$n_diff == 0)
  types_ok  <- nrow(summary) == 0 || all(summary$type_status != "type_mismatch")

  report <- list(
    summary       = summary,
    affected      = affected,
    rows_only_new = rows_only_new,
    rows_only_ref = rows_only_ref,
    cols_only_new = cols_only_new,
    cols_only_ref = cols_only_ref,
    n_rows        = c(new = nrow(new_df), ref = nrow(ref_df),
                      joined = nrow(joined)),
    keys          = keys,
    tol           = tol,
    pass          = schema_ok && rows_ok && values_ok && types_ok
  )
  class(report) <- c("compare_report", "list")

  if (!is.null(report_path)) write_csv(summary, report_path)
  report
}

collapse_years <- function(y) {
  y <- sort(unique(suppressWarnings(as.integer(y))))
  if (length(y) == 0) return(NA_character_)
  breaks <- c(0, which(diff(y) != 1), length(y))
  runs <- map_chr(seq_len(length(breaks) - 1), function(i) {
    r <- y[(breaks[i] + 1):breaks[i + 1]]
    if (length(r) == 1) as.character(r) else paste0(r[1], "-", r[length(r)])
  })
  paste(runs, collapse = ", ")
}

print.compare_report <- function(x, n_top = 15, ...) {
  cat("compare_outputs:", if (x$pass) "PASS" else "FAIL",
      sprintf("(keys: %s; tol = %g)\n", paste(x$keys, collapse = ", "), x$tol))
  cat(sprintf(paste0("rows: new %d, ref %d, joined %d; ",
                     "only in new %d, only in ref %d\n"),
              x$n_rows[["new"]], x$n_rows[["ref"]], x$n_rows[["joined"]],
              nrow(x$rows_only_new), nrow(x$rows_only_ref)))
  cat(sprintf("columns compared: %d; only in new: %d; only in ref: %d\n",
              nrow(x$summary), length(x$cols_only_new),
              length(x$cols_only_ref)))
  if (length(x$cols_only_new) > 0) {
    cat("  only in new:", paste(x$cols_only_new, collapse = ", "), "\n")
  }
  if (length(x$cols_only_ref) > 0) {
    cat("  only in ref:", paste(x$cols_only_ref, collapse = ", "), "\n")
  }
  mism <- x$summary %>% filter(type_status == "type_mismatch")
  cat(sprintf("type mismatches: %d\n", nrow(mism)))
  if (nrow(mism) > 0) cat("  ", paste(mism$variable, collapse = ", "), "\n")
  diffs <- x$summary %>% filter(n_diff > 0) %>% arrange(desc(n_diff))
  cat(sprintf("variables with differences: %d of %d\n",
              nrow(diffs), nrow(x$summary)))
  if (nrow(diffs) > 0) {
    print(head(diffs %>% select(variable, type_status, n_diff, n_munic,
                                years, max_abs_diff), n_top))
  }
  invisible(x)
}

# CLI entry: runs only under Rscript, never when sourced.
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2) {
    stop("usage: Rscript compare_outputs.R <new> <ref> [report.csv]")
  }
  report <- compare_outputs(args[1], args[2],
                            report_path = if (length(args) >= 3) {
                              args[3]
                            } else {
                              NULL
                            })
  print(report)
  quit(status = if (report$pass) 0L else 1L)
}
