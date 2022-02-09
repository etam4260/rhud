#' @import rvest


#' @name hud_get_key
#' @title hud_get_key
#' @description  Return most recent key set in the HUD_KEY environment variable.
#' If no key is set, return "".
#' @returns Returns a string.
#' @export
hud_get_key <- function() {
  return(Sys.getenv("HUD_KEY"))
}


#' @name hud_data_sets
#' @title hud_data_sets
#' @description Scrapes the dataset information page at
#' https://www.huduser.gov/portal/datasets/update-schedule.html.
#' @returns Dataframe with information about the various datasets that HUD has,
#' update information, and corresponding functions/documentation to access a
#' dataset.
#' @export
hud_data_sets <- function() {
  # Need to add associated links to functions to access data as well as
  # the functions themself as well as datasets that are supported by this library.
  # Maintenance might be an issue if HUD decides to modify their website...

  html <- read_html("https://www.huduser.gov/portal/datasets/update-schedule.html")
  table <- as.data.frame(html_table(html_nodes(html, 'table')[[1]]))

  # Clean up procedures. Seems like rvest can't read the table perfectly?
  table[4] <- NULL
  table <- table[-5, ]
  table[4,2] <- table[4,3]
  table[4,3] <- ""

  return(table)
}
