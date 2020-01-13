get_proxies <- function() {
  # 1) send request to server.
  # download proxy lists form => `https://www.us-proxy.org/`
  response <- httr::GET(url = "https://www.us-proxy.org/")

  # 2) parsed talbe's name.
  content_parsed <- httr::content(response, "parsed")
  ths <- rvest::html_text(rvest::html_nodes(content_parsed, css = "#proxylisttable th"))
  proxy_name <- ths[ths != ""]
  proxy_name <- gsub(" " ,"_", tolower(proxy_name))

  # 3) parse data.
  trs <- rvest::html_nodes(content_parsed, css = "#proxylisttable tr")
  proxy_data <- unlist(lapply(trs, function(x) {tds <- rvest::html_text(rvest::html_nodes(x, "td"))}))
  proxy_data <- tibble::as_tibble(as.data.frame(matrix(proxy_data, ncol = 8, byrow = TRUE), stringsAsFactors = FALSE))
  names(proxy_data) <- proxy_name

  dplyr::filter(proxy_data, https == "yes")
}


proxy_pool <- function(x) {
  tmp <- x[sample(nrow(x), 1), ]
  list(ip = tmp$ip_address, port = as.numeric(tmp$port))
}

test_proxy <- function(proxy, proxyport) {
  tryCatch({
    proxy_ip_test <- httr::GET(url = "https://httpbin.org/ip",
      httr::use_proxy(url = proxy, port = as.numeric(proxyport)), httr::timeout(10))
    if (httr::status_code(proxy_ip_test) == 200L) {
      results <- httr::content(proxy_ip_test, "parsed")$origin
      results <- unlist(strsplit(results, ", "))
      if (results[1] == results[2]) {
        list(proxy = proxy, proxyport = proxyport)
      } else {
        NULL
      }
    }
  }, error = function(cond) return(NULL)
  )
}
