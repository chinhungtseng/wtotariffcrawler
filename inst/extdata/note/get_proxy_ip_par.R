library(foreach)
library(doParallel)

cores <- parallel::detectCores()
doParallel::registerDoParallel(cores = (cores - 1))

my_output <- foreach::foreach(
  i = seq_len(nrow(proxy_data)),
  .combine = rbind,
  .export = c("proxy_data"),
  .packages = c("httr", "rvest", "wtotariffcrawler")) %dopar% {
    cat("[test] proxy ip: ", proxy_data$ip_address[i], " port: ", proxy_data$port[i], "\n", sep = "")

    tryCatch({
      proxy_ip_test <- httr::GET(
        url = "https://httpbin.org/ip",
        use_proxy(
          url = proxy_data$ip_address[i],
          port = as.numeric(proxy_data$port[i])
        ), verbose(), timeout(10)
      )

      if (httr::status_code(proxy_ip_test) == 200L) {
        results <- httr::content(proxy_ip_test, "parsed")$origin
        results <- unlist(strsplit(results, ", "))

        if (results[1] == results[2]) {
          return(proxy_data[i, ])
        }
      }
    },
      error = function(cond) {
        print(cond)
      })

    Sys.sleep.random()
  }
