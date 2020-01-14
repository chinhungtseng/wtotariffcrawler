#' tariff_downloader
#'
#' @param import .
#' @param export .
#' @param hs2 .
#' @param hs4 .
#'
#' @return .
#' @export
tariff_downloader <- function(import, export, hs2, hs4) {

  if (missing(import) | missing(export)) {
    stop(paste0("import or export must have value!"), call. = FALSE)
  }

  args_check <- function(x) ifelse (missing(x), "", x)
  hs2 <- args_check(hs2)
  hs4 <- args_check(hs4)

  # 1) create wto crawler and get imoprt countries
  wto_crawler <- new_wto_crawler()

  import_list <- get_import_country(wto_crawler)$value
  # check input import in export_list
  cat("* checking import code `", import, "` is valid... ", sep = "", append = TRUE)
  if (!(import %in% import_list)) {
    cat("incorrect!\n")
    stop(paste0("* `", import, "` not in import code list"), call. = FALSE)
  } else {
    cat("correct!\n")
  }

  # 2) get export countries
  Sys.sleep.random()
  import_session <- request_wto_post(
    wto_crawler,
    params = list(import = import, export = "", hs2 = "", hs4 = "", hs6 = "", submit = ""),
    verbose = FALSE
  )

  export_list <- get_export_country(import_session)$value
  # check input export in export_list
  cat("* checking export code `", export, "` is valid... ", sep = "", append = TRUE)
  if (!(export %in% export_list)) {
    cat("incorrect!\n")
    stop(paste0("* `", export, "` not in export code list"), call. = FALSE)
  } else {
    cat("correct!\n")
  }

  tmp_dir_name <- paste0("data/", import, "-", export)
  fs::dir_create(tmp_dir_name)

  # 3) get hs2 list
  Sys.sleep.random()
  export_session <- request_wto_post(
    import_session,
    params = list(import = import, export = export,
                  hs2 = "", hs4 = "", hs6 = "", submit = ""),
    verbose = FALSE
  )

  hs2_list <- get_hs2_table(export_session)$value
  # check input export in hs2_list
  if (hs2 != "") {
    cat("* checking hs2 code `", hs2, "` is valid... ", sep = "", append = TRUE)
    if (!(hs2 %in% hs2_list)) {
      cat("incorrect!\n")
      stop(paste0("* `", hs2, "` not in hs2 code list"), call. = FALSE)
    } else {
      cat("correct!\n")
      hs2_list <- hs2
    }
  }

  cores <- parallel::detectCores()
  doParallel::registerDoParallel(cores = (cores - 1))

  foreach::foreach(
    hscode2 = hs2_list,
    .export = c("export_session", "hs2_list", "export", "import", "hs2", "hs4"),
    .verbose = TRUE,
    .packages = c("httr", "rvest", "parallel", "foreach", "doParallel",
                  "dplyr", "tibble", "purrr", "readr", "fs", "wtotariffcrawler")
  ) %do% {
    cat(paste0("* Try get data { import: ", import, " export: ", export, " hs2: ", hscode2, " }\n"))

    Sys.sleep.random()
    hs2_session <- request_wto_post(
      export_session,
      params = list(import = import, export = export, hs2 = hscode2, hs4 = "", hs6 = "", submit = ""),
      verbose = FALSE
    )

    hs4_list <- get_hs4_table(hs2_session)$value
    # check input export in hscode4_list
    if (hs4 != "") {
      cat("* checking hs4 code `", hs4, "` is valid... ", sep = "", append = TRUE)
      if (!(hs4 %in% hs4_list)) {
        cat("incorrect!\n")
        stop(paste0("* `", hs4, "` not in hs4 code list"), call. = FALSE)
      } else {
        cat("correct!\n")
        hs4_list <- hs4
      }
    }

    if (hscode2 %in% hs4_list) {
      Sys.sleep.random()
      hs4_session <- request_wto_post(
        hs2_session,
        params = list(import = import, export = export, hs2 = hscode2, hs4 = hscode2, hs6 = "", submit = "查詢"),
        verbose = FALSE
      )
      hs6_tbl <- get_hs6_table(hs4_session)
    } else {
      hs6_tbl <- Reduce(rbind, lapply(seq_along(hs4_list), function(hscode4) {
        Sys.sleep.random()
        hs4_session <- request_wto_post(
          hs2_session,
          params = list(import = import, export = export, hs2 = hscode2, hs4 = hscode4, hs6 = "", submit = "查詢"),
          verbose = FALSE
        )
        get_hs6_table(hs4_session)
      }))
    }

    hs6_tbl %>%
      tibble::as_tibble() %>%
      dplyr::mutate(import = import, export = export, hs2 = hscode2) %>%
      readr::write_tsv(paste0(tmp_dir_name, "/", import, "-", export, "-", hscode2, ".tsv"), na = "")
  }

  files <- fs::dir_ls(tmp_dir_name)
  try({
    purrr::map(files, readr::read_tsv, col_types = "ccccccccc") %>%
      purrr::reduce(dplyr::bind_rows) %>%
      readr::write_tsv(paste0(tmp_dir_name, ".tsv"))

    if (fs::file_exists(paste0(tmp_dir_name, ".tsv"))) {
      fs::file_delete(files)
      fs::dir_delete(tmp_dir_name)
    }
  })
}
