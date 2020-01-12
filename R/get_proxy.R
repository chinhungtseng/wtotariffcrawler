get_proxy <- function() {
  tmp <- .proxy_list[sample(nrow(.proxy_list), 1), ]
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
