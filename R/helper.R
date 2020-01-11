status_code.wto <- function(x) x$status_code

Sys.sleep.random <- function() {
  st <- runif(1, 1, 10)
  cat("sleep ", st, " seconds...\n", sep = "")
  Sys.sleep(st)
}

