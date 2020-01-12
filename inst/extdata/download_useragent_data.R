# ref url: https://stackoverflow.com/questions/56082653/trying-to-fake-and-rotating-user-agents
url <- "http://51.158.74.109/useragents/?format=json"
useragent_list <- tibble::as_tibble(jsonlite::fromJSON(url))
useragent_list <- useragent_list[useragent_list$device_family == "Other", ]
saveRDS(useragent_list, "inst/extdata/useragent.rds")
