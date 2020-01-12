request_wto_post.wto <- function(x, params, verbose = FALSE, timeout = 60) {
  ATTEMPTS <- 0
  MAXTRY <- 5

  params_check(params)

  response <- x$session$response
  content_parsed <- httr::content(response, "parsed")
  forms_fields <- rvest::html_form(content_parsed)[[1]]$fields

  viewstate          <- forms_fields$`__VIEWSTATE`$value
  viewstategenerator <- forms_fields$`__VIEWSTATEGENERATOR`$value
  evenvalidation     <- forms_fields$`__EVENTVALIDATION`$value

  my_vars <- c("eventtarget", "import", "export", "hs2", "hs4", "hs6", "submit")
  for (i in my_vars) assign(i, "")
  for (i in my_vars[-1]) assign(i, unname(unlist(params)[i]))

  if (params$import != "" & params$export == "") {
    eventtarget <- "ctl00$ContentPlaceHolder1$selectImport"

  } else if (params$export != "" & params$hs2 == "") {
    eventtarget <- "ctl00$ContentPlaceHolder1$selectExport"

  } else if (params$hs2 != "" & params$hs4 == "") {
    eventtarget <- "ctl00$ContentPlaceHolder1$selectHS2Code"

  } else if (params$hs4 != "" & params$hs6 == "") {
    eventtarget <- "ctl00$ContentPlaceHolder1$selectHS4Code"
  }

  while (ATTEMPTS < MAXTRY) {

    tryCatch({

      t0 <- Sys.time()
      session <- rvest:::request_POST(x$session, x$url,
        body = list(
          "__EVENTTARGET"        = eventtarget,
          "__EVENTARGUMENT"      = "",
          "__LASTFOCUS"          = "",
          "__VIEWSTATE"          = viewstate,
          "__VIEWSTATEGENERATOR" = viewstategenerator,
          "__EVENTVALIDATION"    = evenvalidation,
          "ctl00$ContentPlaceHolder1$selectImport"  = import,
          "ctl00$ContentPlaceHolder1$selectExport"  = export,
          "ctl00$ContentPlaceHolder1$selectHS2Code" = hs2,
          "ctl00$ContentPlaceHolder1$selectHS4Code" = hs4,
          "ctl00$ContentPlaceHolder1$selectHS6Code" = hs6,
          "ctl00$ContentPlaceHolder1$btn_serarch"   = submit
        ), encode = "form", httr::timeout(timeout),
        {if (verbose) {httr::verbose()}}
      )
      t1 <- Sys.time()

      session <- if (httr::status_code(session) != 200L) NULL else session
      ATTEMPTS <- ATTEMPTS + 1

      objs <- structure(list(
        url = x$url,
        config = session$config,
        status_code = httr::status_code(session),
        session = session,
        connet_times = ATTEMPTS,
        response_delay = as.numeric(t1 - t0)
      ), class = c("wto", class(session)))

      message(paste0("request `", x$url, "` succeed."))
      return(objs)
    },
      error = function(cond) {
        ATTEMPTS <- ATTEMPTS + 1
        message(paste0("[error] request `", x$url, "` failed, we will try again later."))
        Sys.sleep(5)
      })
  }

  stop(paste0("[error] request `", url, "` failed too many times, stop program."), call. = FALSE)
}
