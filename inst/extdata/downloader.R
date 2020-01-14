countries <- country_pool()

for (i in seq_len(nrow(countries))) {
  import <- countries$import[i]
  export <- countries$export[i]
  tariff_downloader(import, export)
  Sys.sleep(3600)
}
