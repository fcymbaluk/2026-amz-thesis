# read_sidra.R
# Purpose  Read a SIDRA agregados API v3 JSON file (one variable, any number
#          of classification cuts, municipality level) into a long tibble.
# Inputs   A file under _data/raw/ibge/sidra/ as served by the API.
# Outputs  read_sidra_json(): tibble with geocode (chr), municipality (chr),
#          year (int), category_id (chr) and category (chr) of the first
#          classification, categories (chr, every classification's category
#          pasted with " | "), value (chr, as
#          served: digits, "-" for zero, "..." for not available, "X" for
#          omitted). A table served without a classification (5938, 6579)
#          carries NA in category_id, category and categories.
#          sidra_value(): the numeric reading of `value` under an
#          explicit symbol rule. Definitions only; sourced by scripts.
# Status   Phase 1, script 02 (2026-10-07); no-classification guard and
#          vectorised series loop (same output) at script 03 (2026-10-10).

read_sidra_json <- function(path) {
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  stopifnot(length(json) == 1L)
  purrr::map_dfr(json[[1]]$resultados, function(result) {
    has_class <- length(result$classificacoes) > 0L
    names_all <- if (has_class) {
      purrr::map_chr(result$classificacoes,
                     function(k) unlist(k$categoria)[[1]])
    } else {
      NA_character_
    }
    first_id <- if (has_class) {
      names(result$classificacoes[[1]]$categoria)[[1]]
    } else {
      NA_character_
    }
    categories <- if (has_class) {
      paste(names_all, collapse = " | ")
    } else {
      NA_character_
    }
    n_years <- purrr::map_int(result$series, function(s) length(s$serie))
    tibble::tibble(
      geocode      = rep(purrr::map_chr(result$series,
                                        function(s) s$localidade$id),
                         n_years),
      municipality = rep(purrr::map_chr(result$series,
                                        function(s) s$localidade$nome),
                         n_years),
      year         = as.integer(unlist(purrr::map(result$series,
                                                  function(s) names(s$serie)),
                                       use.names = FALSE)),
      category_id  = first_id,
      category     = names_all[[1]],
      categories   = categories,
      value        = unlist(purrr::map(result$series, "serie"),
                            use.names = FALSE)
    )
  })
}

# Symbols listed in `zero_symbols` become 0; any other non-numeric symbol
# becomes NA. The caller states the rule, because "-" is a true zero in
# SIDRA while "..." is "not available".
sidra_value <- function(x, zero_symbols = "-") {
  out <- suppressWarnings(as.numeric(x))
  out[x %in% zero_symbols] <- 0
  out
}
