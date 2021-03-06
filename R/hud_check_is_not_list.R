#' @name check_is_not_list
#' @title Check that Dataframe Columns are Not Lists.
#' @param dataframe A dataframe to determine whether columns are not lists,
#'   but is constrained to character, integer, or numeric.
#' @returns TRUE if passing, FALSE if not
#' @noRd
#' @noMd
check_is_not_list <- function(dataframe) {
  if (typeof(dataframe) != "list") return(FALSE)

  for (i in seq_len(ncol(dataframe))) {
    if (typeof(dataframe[, i]) != "character" &&
       typeof(dataframe[, i]) != "integer" &&
       typeof(dataframe[, i]) != "numeric" &&
       typeof(dataframe[, i]) != "double") {
      return(FALSE)
    }
  }

  return(TRUE)
}
