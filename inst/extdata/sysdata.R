.proxy_list <- readRDS("inst/extdata/proxy.rds")
.useragent_list <- readRDS("inst/extdata/useragent.rds")


save(
  .proxy_list, .useragent_list,
  file = "R/sysdata.rda", compress='xz'
)
