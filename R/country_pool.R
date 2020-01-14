#' country_pool
#'
#' @return .
#' @export .
country_pool <- function() {
  wto_crawler <- new_wto_crawler()
  import_list <- get_import_country(wto_crawler)$value
  tmp <- expand.grid(import = import_list, export = import_list, stringsAsFactors = FALSE)
  tmp <- dplyr::filter(tmp, import != export)
  tibble::as_tibble(arrange(tmp, import))
}
