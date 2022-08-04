#' @name digits_only
#' @title All Digits In Character vector
#' @description Detect whether character vector contains only numbers.
#' @param x A character vector.
#' @returns TRUE if character vector is all digits, FALSE if not.
#' @noRd
#' @noMd
digits_only <- function(x) !grepl("\\D", x)
