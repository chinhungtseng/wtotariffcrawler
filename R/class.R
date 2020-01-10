#' new_wto_crawler
#'
#' @param verbose TRUE or FALSE
#'
#' @return list
#' @export
new_wto_crawler <- function(verbose = FALSE) {
  ATTEMPTS <- 0
  MAXTRY <- 5

  while (ATTEMPTS < MAXTRY) {

    tryCatch({
      url <- "http://db2.wtocenter.org.tw/tariff/Search_byHSCode.aspx"
      header_params <- c(
        "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
        "Accept-Encoding" = "gzip, deflate",
        "Accept-Language" = "en-US,en;q=0.9,zh-TW;q=0.8,zh;q=0.7",
        "Cache-Control"   = "max-age=0",
        "Connection"      = "keep-alive",
        "Content-Type"    = "application/x-www-form-urlencoded",
        "Host"    = "db2.wtocenter.org.tw",
        "Origin"  = "http://db2.wtocenter.org.tw",
        "Referer" = "http://db2.wtocenter.org.tw/",
        "Upgrade-Insecure-Requests" = "1"
      )
      userAgent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.88 Safari/537.36"

      t0 <- Sys.time()
      session <- rvest::html_session(
        url,
        httr::add_headers(header_params),
        httr::user_agent(userAgent),
        httr::timeout(10),
        {if (verbose) {httr::verbose()}}
      )
      t1 <- Sys.time()
      session <- if (httr::status_code(session) != 200L) NULL else session
      ATTEMPTS <- ATTEMPTS + 1

      objs <- structure(list(
        url = url,
        config = session$config,
        status_code = httr::status_code(session),
        session = session,
        connet_times = ATTEMPTS,
        response_delay = as.numeric(t1 - t0)
      ), class = c("wto", class(session)))

      message(paste0("request `", url, "` succeed."))
      return(objs)
    }, error = function(cond) {
      ATTEMPTS <- ATTEMPTS + 1
      message(paste0("[error] request `", url, "` failed, we will try again later."))
      Sys.sleep(5)
    })
  }
  stop(paste0("[error] request `", url, "` failed too many times, stop program."), call. = FALSE)
}

print.wto <- function(x, ...) {
  cat("<wto crawler conifg>\n")
  cat("* url: ", x$url, "\n", sep = "")
  cat("* status code: ", x$status_code, "\n", sep = "")
  cat("* connet times: ", x$connet_times, "\n", sep = "")
  cat("* response delay: ", x$response_delay, "\n", sep = "")
}
