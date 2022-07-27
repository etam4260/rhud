#' @name rhud_website
#' @title Access RHUD Web Resources
#' @description Quickly get documentation for the rhud package by opening up
#'   the websites associated with it.
skip_if_no_key <- function() {
  if (Sys.getenv("HUD_KEY") == "") {
    skip("The HUD_KEY variable is empty: will not run the test.")
  }
}
