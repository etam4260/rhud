#' @name is_valid_rhud_df()
#' @title Check that Dataframe Columns are Not Made of Lists.
#' @param dataframe A dataframe to determine whether columns are not lists,
#'   but are constrained to character, integer, or numeric.
#' @returns TRUE if all columns are made of vectors, FALSE if not.
#' @noRd
#' @noMd
is_valid_rhud_df <- function(dataframe) {
  types <- c("character", "integer", "double")
  typeof(dataframe) == "list" && all(apply(dataframe, 2, typeof) %in% types)
}
