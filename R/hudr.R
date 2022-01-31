# Creating global environment to set default values to some
# of parameters in the web queries.
pkg.env <- new.env(parent = emptyenv())
pkg.env$curr.year <- c(strsplit(toString(Sys.Date()), "-")[[1]][1])
pkg.env$curr.key <- NULL
pkg.env$curr.quarter <- c("1","2","3","4")


#' hudsetkey
#' @name hudsetkey
#' @title hudsetkey
#' @description The function will save the key into your library download. You will need to
#' update this for new library(hudr) call. You can also set a new key by using this function with a new one.
#' @param key The token given by USPS
#' @export
hudsetkey <- function(key) {
  pkg.env$curr.key <- key
}
