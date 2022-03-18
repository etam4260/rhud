#' @name numbers_only
#' @title numbers_only
#' @description Detect whether string contains only numbers.
#' @param x A string.
#' @returns TRUE if string is all numbers, FALSE if not.
#' @noRd
#' @noMd
numbers_only <- function(x) !grepl("\\D", x)

#' @name fivenumbers
#' @title fivenumbers
#' @description Detect whether data contains five numbers or not.
#' @param x A input string.
#' @returns TRUE if string is 5 numbers, FALSE if not.
#' @noRd
#' @noMd
fivenumbers <- function(x) !grepl("[0-9]{5}", x)

#' @name fivenumsthenfournums
#' @title fivenumsthenfournums
#' @description Detect whether string contains only 5 numbers a delimiter and then 4 numbers.
#' @param x A string.
#' @returns TRUE if string is 5 numbers, delimiter, then 4 numbers, FALSE if not.
#' @noRd
#' @noMd
fivenumsthenfournums <- function(x) grepl("[0-9]{5}.?[0-9]{4}", x)

#' @name elevennumbers
#' @title elevennumbers
#' @description Detect whether string contains 11 numbers.
#' @param x A string.
#' @returns TRUE if string is 11 numbers, FALSE if not.
#' @noRd
#' @noMd
elevennumbers <- function(x) !grepl("[0-9]{11}", x)


#' @name fournumbers
#' @title fournumbers
#' @description Detect whether string contains 4 numbers
#' @param x A string.
#' @returns TRUE if string is 4 numbers, FALSE if not.
#' @noRd
#' @noMd
fournumbers <- function(x) !grepl("[0-9]{4}", x)


#' @name tennumbers
#' @title tennumbers
#' @description Detect whether string contains 10 numbers.
#' @param x A string.
#' @returns TRUE if string is 10 numbers, FALSE if not.
#' @noRd
#' @noMd
tennumbers <- function(x) !grepl("[0-9]{10}",x)
