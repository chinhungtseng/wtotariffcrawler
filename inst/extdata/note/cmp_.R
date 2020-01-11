library(foreach)
library(doParallel)

with_par <- function() {
  s0 <-Sys.time()
  cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = (cores - 1))
  output_valided <- foreach::foreach(
    i = seq_len(nrow(proxy_data)),
    .verbose = FALSE,
    .combine = rbind,
    .export = c("proxy_data"),
    .packages = c("httr", "rvest", "wtotariffcrawler")) %dopar% {
      if (i >= 1) Sys.sleep(1)#Sys.sleep.random()
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
  s1 <- Sys.time()
  time_diff <- as.numeric((s1 - s0))
  cat(time_diff, "\n")
  output_valided
}

with_no_par <- function() {
  s0 <-Sys.time()
  output_valided <- lapply(seq_len(nrow(proxy_data)), function(i) {
    if (i >= 1) Sys.sleep(1)#Sys.sleep.random()
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
  })
  s1 <- Sys.time()
  time_diff <- as.numeric((s1 - s0))
  cat(time_diff, "/n")
  Reduce(rbind, output_valided)
}

s1 <- with_par()
s2 <- with_no_par()


