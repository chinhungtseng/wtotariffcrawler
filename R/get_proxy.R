get_proxy <- function() {
  tmp <- .proxy_list[sample(nrow(.proxy_list), 1), ]
  list(ip = tmp$ip_address, port = as.numeric(tmp$port))
}
