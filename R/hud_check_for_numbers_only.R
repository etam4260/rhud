#' @name numbers_only
#' @title numbers_only
#' @description Detect whether string contains only numbers.
#' @param x A string.
#' @returns TRUE if string is all numbers, FALSE if not.
#' @noRd
#' @noMd
numbers_only <- function(x) !grepl("\\D", x)
