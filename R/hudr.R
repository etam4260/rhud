#' @import rvest

# Creating global environment to set default values to some
# of parameters in the web queries.
pkg.env <- new.env(parent = emptyenv())
pkg.env$curr.year <- c(strsplit(toString(Sys.Date()), "-")[[1]][1])
pkg.env$curr.quarter <- c("1","2","3","4")
pkg.env$curr.key <- NULL

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

#' hudgetkey
#' @name hudgetkey
#' @title hudgetkey
#' @description  Will return the most recent key set. If no key is set it will return NULL.
#' @returns Will return the most recent key set. If no key is set it will return NULL.
#' @export
hudgetkey <- function() {
  return(pkg.env$curr.key)
}


# Need to add associated links to functions to access data as well as the functions
# themself as well as datasets that are supported by this library.

#' huddatasets
#' @name huddatasets
#' @title huddatasets
#' @description Scrapes the dataset information page at https://www.huduser.gov/portal/datasets/update-schedule.html.
#' @returns Dataframe with information about the various datasets that HUD has, update information, and corresponding functions and documentation to access a dataset using the package.
#' @export
huddatasets <- function() {
  html <- read_html("https://www.huduser.gov/portal/datasets/update-schedule.html")
  table <- as.data.frame(html_table(html_nodes(html, 'table')[[1]]))

  # Clean up procedures. Seems like rvest can't read the table perfectly?
  table[4] <- NULL
  table <- table[-5, ]
  table[4,2] <- table[4,3]
  table[4,3] <- ""

  return(table)
}
