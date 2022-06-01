#' @importFrom curl has_internet

#' @name rhud_website
#' @title rhud_website
#' @description Quickly get documentation for the hudr package by opening up
#'   the websites associated with it. Currently supports Unix and Windows OS.
#' @param website The websites available.
#'   1) "github-pages"
#'   2) "github"
#' @export
#' @examples
#' \dontrun{
#' library(rhud)
#' rhud_website("github-pages")
#' rhud_website("github")
#' rhud_website()
#' }
rhud_website <- function(website = c("github-pages", "github")) {
  if(!curl::has_internet()) stop("You currently do not have internet access.")
  # Check for internet

  github_pages <- "https://etam4260.github.io/rhud/"
  github <- "https://github.com/etam4260/rhud"

  if (.Platform$OS.type == "unix") {
    if ("github" %in% website) system(paste("open", github, sep = " "))
    if ("github-pages" %in% website) system(paste("open", github_pages,
                                                 sep = " "))
  } else if (.Platform$OS.type == "windows") {
    if ("github" %in% website) system(paste("start", github, sep = " "))
    if ("github-pages" %in% website) system(paste("start", github_pages,
                                                 sep = " "))
  }
}
