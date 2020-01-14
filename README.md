
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wtotariffcrawler

<!-- badges: start -->

<!-- badges: end -->

The goal of wtotariffcrawler is to …

## Installation

``` r
remotes::install_github("chinhungtseng/wtotariffcrawler")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wtotariffcrawler)

countries <- country_pool()
for (i in seq_len(nrow(countries))) {
  import <- countries$import[i]
  export <- countries$export[i]
  tariff_downloader(import, export)
  Sys.sleep(3600)
}
```
