request_wto_post <- function(x, params, verbose, timeout) {
  UseMethod("request_wto_post")
}

request_wto_post.wto <- function(x, params, verbose = FALSE, proxy = FALSE, timeout = 60) {
  ATTEMPTS <- 0
  MAXTRY <- 10

  params_check(params)

  response <- x$session$response
  content_parsed <- httr::content(response, "parsed")
  forms_fields <- rvest::html_form(content_parsed)[[1]]$fields
  userAgent <- x$user_agent

  if (proxy) {
    proxy_ip <- x$proxy$proxy
    proxy_port <- x$proxy$proxyport
  }

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
        ),
        httr::user_agent(userAgent),
        {if (proxy) {httr::use_proxy(proxy_ip, proxy_port)}},
        encode = "form", httr::timeout(timeout),
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
        user_agent = session$response$request$options$useragent,
        proxy = ifelse(!proxy, list(proxy = NULL, proxyport = NULL), list(
          proxy     = session$response$request$options$proxy,
          proxyport = session$response$request$options$proxyport
        )),
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

params_check <- function(x) {
  # 1) check params value type
  stopifnot(all(unlist(lapply(x, is.character))))

  # 2) check submit value
  stopifnot(any(c("", "查詢") %in% x$submit))

  # 3) hierachy check
  empty_char <- unlist(lapply(x, function(x) x == ""))

  arg1 <- empty_char[c("import", "export", "hs2", "hs4")]
  if ((x$submit == "查詢") & any(arg1)) {
    invalid_args <- names(arg1[arg1])
    stop(paste0(
      "Invalid arugments: ", paste0("`", invalid_args, "`", collapse = ", "), " must not empty!"
    ), call. = FALSE)

  } else {
    if (!empty_char["export"]) {
      arg2 <- empty_char[c("import")]
      if (any(arg2)) {
        invalid_args <- names(arg2[arg2])
        stop(paste0(
          "Invalid arugments: ", paste0("`", invalid_args, "`", collapse = ", "), " must not empty!"
        ), call. = FALSE)
      }
    } else if (!empty_char["hs2"]) {
      arg3 <- empty_char[c("import", "export")]
      if (any(arg3)) {
        invalid_args <- names(arg3[arg3])
        stop(paste0(
          "Invalid arugments: ", paste0("`", invalid_args, "`", collapse = ", "), " must not empty!"
        ), call. = FALSE)
      }
    } else if (!empty_char["hs4"]) {
      arg4 <- empty_char[c("import", "export", "hs2")]
      if (any(arg4)) {
        invalid_args <- names(arg4[arg4])
        stop(paste0(
          "Invalid arugments: ", paste0("`", invalid_args, "`", collapse = ", "), " must not empty!"
        ), call. = FALSE)
      }
    }
  }
}
