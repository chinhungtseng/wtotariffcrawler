get_import_country <- function(x) UseMethod("get_import_country")
get_export_country <- function(x) UseMethod("get_export_country")

get_hs2_table <- function(x) UseMethod("get_hs2_table")
get_hs4_table <- function(x) UseMethod("get_hs4_table")
get_hs6_table <- function(x) UseMethod("get_hs6_table")

get_import_country.wto <- function(x) {
  country_table <- rvest::html_nodes(httr::content(x$session$response, "parsed"),
    css = ".row div:nth-child(1) div select option")

  value <- unlist(rvest::html_attrs(country_table))
  value <- value[names(value) == "value"][-1]

  name <- rvest::html_text(country_table)
  name <- gsub("\\r\\n", "", name)
  name <- name[!grepl("^-.*-$", name)]

  return(data.frame(value = value, name = name, stringsAsFactors = FALSE))
}

get_export_country.wto <- function(x) {
  country_table <- rvest::html_nodes(httr::content(x$session$response, "parsed"),
    css = ".row div:nth-child(2) div select option")
  return(extract_node_table(country_table))
}

get_hs2_table.wto <- function(x) {
  hstable <- rvest::html_nodes(httr::content(x$session$response, "parsed"),
    css = "div:nth-child(3) select option")
  return(extract_node_table(hstable))
}

get_hs4_table.wto <- function(x) {
  hstable <- rvest::html_nodes(httr::content(x$session$response, "parsed"),
    css = ".row div:nth-child(4) select option")
  return(extract_node_table(hstable))
}

get_hs6_table.wto <- function(x) {
  response <- httr::content(x$session$response, "parsed")
  tariff_data <- rvest::html_text(rvest::html_nodes(response, css = "td"))
  tariff_name <- rvest::html_text(rvest::html_nodes(response,
    css = "th:nth-child(3),th:nth-child(2),th:nth-child(1)"))

  tariff_tbl <- as.data.frame(matrix(tariff_data, ncol = 6, byrow = TRUE), stringsAsFactors = FALSE)
  names(tariff_tbl) <- tariff_name
  return(tariff_tbl)
}

extract_node_table <- function(x) {
  value <- unlist(rvest::html_attrs(x))
  value <- value[names(value) == "value"][-1]
  name <- rvest::html_text(x)
  name <- gsub("\\r\\n", "", name)
  name <- name[!grepl("^-.*-$", name)]
  return(data.frame(value = value, name = name, stringsAsFactors = FALSE))
}
