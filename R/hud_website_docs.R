#' @importFrom curl has_internet
#' @importFrom utils browseURL
#'
#' @name rhud_website
#' @title Access RHUD Web Resources
#' @description Quickly get documentation for the rhud package by opening up
#'   the websites associated with it.
#' @param website A character vector: the websites available.
#'   1) "github-pages"
#'   2) "github"
#' @export
#' @examples
#' \dontrun{
#' rhud_website("github-pages")
#' rhud_website("github")
#' rhud_website()
#' }
rhud_website <- function(website = c("github-pages", "github")) {
  is_internet_available()
  # Check for internet
  if ("github" %in% website) browseURL("https://github.com/etam4260/rhud")
  if ("github-pages" %in% website) browseURL("https://etam4260.github.io/rhud/")
}
