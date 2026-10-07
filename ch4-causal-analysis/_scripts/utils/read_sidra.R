# read_sidra.R
# Purpose  Read a SIDRA agregados API v3 JSON file (one variable, any number
#          of classification cuts, municipality level) into a long tibble.
# Inputs   A file under _data/raw/ibge/sidra/ as served by the API.
# Outputs  read_sidra_json(): tibble with geocode (chr), municipality (chr),
#          year (int), category_id (chr) and category (chr) of the first
#          classification, categories (chr, every classification's category
#          pasted with " | "), value (chr, as
#          served: digits, "-" for zero, "..." for not available, "X" for
#          omitted). sidra_value(): the numeric reading of `value` under an
#          explicit symbol rule. Definitions only; sourced by scripts.
# Status   Phase 1, script 02 (2026-10-07).

read_sidra_json <- function(path) {
  json <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  stopifnot(length(json) == 1L)
  purrr::map_dfr(json[[1]]$resultados, function(result) {
    names_all <- purrr::map_chr(result$classificacoes,
                                function(k) unlist(k$categoria)[[1]])
    first_id <- names(result$classificacoes[[1]]$categoria)[[1]]
    purrr::map_dfr(result$series, function(s) {
      tibble::tibble(
        geocode      = s$localidade$id,
        municipality = s$localidade$nome,
        year         = as.integer(names(s$serie)),
        category_id  = first_id,
        category     = names_all[[1]],
        categories   = paste(names_all, collapse = " | "),
        value        = unlist(s$serie, use.names = FALSE)
      )
    })
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
