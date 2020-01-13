.useragent_list <- readRDS("inst/extdata/useragent.rds")
save(.useragent_list, file = "R/sysdata.rda", compress='xz')
