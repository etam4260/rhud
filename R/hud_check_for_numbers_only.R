#' @name numbers_only
#' @title All Numbers in Character vector
#' @description Detect whether character vector contains only numbers.
#' @param x A character vector.
#' @returns TRUE if character vector is all numbers, FALSE if not.
#' @noRd
#' @noMd
numbers_only <- function(x) !grepl("\\D", x)
