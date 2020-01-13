library(foreach)
library(doParallel)

# 1) get import country list
wto_crawler <- new_wto_crawler()
import_country_tbl <- get_import_country(wto_crawler)
import_list <- import_country_tbl$value

# 2)
# cores <- parallel::detectCores()
# doParallel::registerDoParallel(cores = (cores - 1))
doParallel::registerDoParallel(cores = 6)

foreach::foreach(
  import = import_list,
  .export = c("import_list"),
  .packages = c("httr", "rvest", "parallel", "foreach", "doParallel",
                "dplyr", "tibble", "purrr", "readr", "fs", "wtotariffcrawler"),
  .verbose = TRUE) %dopar% {
    # create wto crawler
    wto_crawler <- new_wto_crawler()

    Sys.sleep.random()
    import_session <- request_wto_post(
      wto_crawler,
      params = list(import = import, export = "", hs2 = "", hs4 = "", hs6 = "", submit = ""),
      verbose = TRUE
    )

    export_country_tbl <- get_export_country(import_session)
    export_list <- export_country_tbl$value

    for (export in export_list) {
      tmp_dir_name <- paste0("data/", import, "-", export)
      fs::dir_create(tmp_dir_name)

      Sys.sleep.random()
      export_session <- request_wto_post(
        import_session,
        params = list(import = import, export = export, hs2 = "", hs4 = "", hs6 = "", submit = ""),
        verbose = FALSE
      )

      hs2_tbl <- get_hs2_table(export_session)
      hscode2_list <- hs2_tbl$value

      for (hscode2 in hscode2_list) {
        Sys.sleep.random()
        hs2_session <- request_wto_post(
          export_session,
          params = list(import = import, export = export, hs2 = hscode2, hs4 = "", hs6 = "", submit = ""),
          verbose = FALSE
        )

        Sys.sleep.random()
        cat(paste0("* Try get data { import: ", import, " export: ", export, " hs2: ", hscode2, " }\n"))

        hs4_session <- request_wto_post(
          hs2_session,
          params = list(import = import, export = export, hs2 = hscode2, hs4 = hscode2, hs6 = "", submit = "查詢"),
          verbose = FALSE
        )

        hs6_tbl <- get_hs6_table(hs4_session)

        hs6_tbl %>%
          tibble::as_tibble() %>%
          dplyr::mutate(import = import, export = export, hs2 = hscode2) %>%
          readr::write_tsv(paste0(tmp_dir_name, "/", import, "-", export, "-", hscode2, ".tsv"), na = "")
      }

      files <- fs::dir_ls(tmp_dir_name)

      try({
        purrr::map(files, readr::read_tsv) %>%
          purrr::reduce(dplyr::bind_rows) %>%
          readr::write_tsv(paste0(tmp_dir_name, ".tsv"))

        if (fs::file_exists(paste0(tmp_dir_name, ".tsv"))) {
          fs::file_delete(files)
          fs::dir_delete(tmp_dir_name)
        }
      })
    }

    Sys.sleep(3600)

  }
