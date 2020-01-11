library(httr)
library(rvest)
library(future)
library(doParallel)

# 1) send request to server.
# download proxy lists form => `https://www.us-proxy.org/`
response <- httr::GET(url = "https://www.us-proxy.org/")

# 2) parsed talbe's name.
content_parsed <- httr::content(response, "parsed")
ths <- rvest::html_nodes(content_parsed, css = "#proxylisttable th")
ths <- rvest::html_text(ths)
proxy_name <- ths[ths != ""]
proxy_name <- gsub(" " ,"_", tolower(proxy_name))

# 3) parse data.
trs <- rvest::html_nodes(content_parsed, css = "#proxylisttable tr")
proxy_data <- unlist(lapply(trs, function(x) {tds <- html_text(html_nodes(x, "td"))}))
proxy_data <- tibble::as_tibble(as.data.frame(matrix(proxy_data, ncol = 8, byrow = TRUE), stringsAsFactors = FALSE))
names(proxy_data) <- proxy_name

# 4) test proxy ip is valid and can be used.
# use https://httpbin.org for my ip testing.
cores <- parallel::detectCores()
doParallel::registerDoParallel(cores = (cores - 1))
output_valided <- foreach::foreach(i = seq_len(nrow(proxy_data)),
  .verbose = FALSE, .combine = rbind, .export = c("proxy_data"),
  .packages = c("httr", "rvest", "wtotariffcrawler")) %dopar% {
    if (i >= 1) Sys.sleep.random()
    cat("[test] (", i, ") proxy ip: ", proxy_data$ip_address[i], " port: ", proxy_data$port[i], "\n", sep = "")
    tryCatch({
      proxy_ip_test <- httr::GET(url = "https://httpbin.org/ip",
        use_proxy(url = proxy_data$ip_address[i], port = as.numeric(proxy_data$port[i])), timeout(10))
      if (httr::status_code(proxy_ip_test) == 200L) {
        results <- httr::content(proxy_ip_test, "parsed")$origin
        results <- unlist(strsplit(results, ", "))
        if (results[1] == results[2]) {
          cat(paste0("* Get ", proxy_data$ip_address[i], " ", port = as.numeric(proxy_data$port[i]), " succed!\n"))
          return(proxy_data[i, ])
        }
      }
    }, error = function(cond) NULL)
  }

# 5) output the date that can be used.
saveRDS(valid_proxy_ips, "inst/extdata/proxy.rds")
