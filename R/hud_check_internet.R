#' @name is_internet_available
#' @title Check if Internet is Available
#' @description A wrapper around curl::has_internet() to check
#'   if internet is available.
#' @returns TRUE if internet is available, halts execution if not.
is_internet_available <- function() {
  if (!curl::has_internet()) {
    stop("\nYou currently do not have internet access.", call. = FALSE)
  }
  TRUE
}
