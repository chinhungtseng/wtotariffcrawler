get_useragent <- function() {
  sample(.useragent_list$useragent, 1)
}
