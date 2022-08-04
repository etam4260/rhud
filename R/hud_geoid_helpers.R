#' @name geoid_add_leading_zeros
#' @title Add Leading Zeros To Character Vector
#' @description Sometimes when loading in data from an excel file it
#'   truncates the leading 0's which is important in determining the proper
#'   geoid. This attempts to re-add those.
#' @param geoid_type What geoid type to add leading 0s to
#'   1) zip -> Must be 5 digit zip code
#'   2) county -> Must be a 5 digit county fips code
#'   3) cbsa -> Must be a 5 digit cbsa code
#'   4) cbsadiv -> Must be a 5 digit cbsadiv code
#    5) countysub -> Must be a 10 digit countysub
#'   6) cd -> Must be a 4 digit cd code.
#'   If you specify a number it will
#'   make the geoid of that length.
#' @param input A character or numeric vector
#' @returns A character vector with leading zeros added.
#' @noRd
#' @noMd
#' @examples
#' \dontrun{
#' zip <- c(02102, 11032, 01232)
#'
#' zip <- geoid_add_leading_zeros("zip", zip)
#'
#' cbsa <- c(02102, 11032, 01232)
#'
#' cbsa <- geoid_add_leading_zeros("cbsa", cbsa)
#'
#' some_geoid <- c(213113, 6526266, 1316455)
#'
#' some_geoid <- geoid_add_leading_zeros(10, some_geoid)
#' }
geoid_add_leading_zeros <- function(geoid_type = "zip", input) {
  res <- NULL
  geoid_type <- as.character(geoid_type)

  geoid_type <- switch(
    geoid_type,
    "zip" = 5,
    "county" = 5,
    "cbsa" = 5,
    "cbsadiv" = 5,
    "countysub" = 10,
    "cd" = 4,
    "tract" = 11,
    geoid_type
  )

  if (digits_only(geoid_type)) {
    geoid_type <- as.integer(geoid_type)
  }

  res <- left_pad_zeros(num_char = geoid_type, geoids = input)

  res
}


#' @name left_pad_zeros
#' @title Add Leading Zeros back to Character Vector Helper
#' @description This is helper function for adding_leading_zeros() back to a
#'   character vector.
#' @param geoid_type What geoid type to add leading 0s to
#'   1) zip -> Must be 5 digit zip code
#'   2) county -> Must be a 5 digit county fips code
#'   3) cbsa -> Must be a 5 digit cbsa code
#'   4) cbsa-div -> Must be a 5 digit cbsa code
#'   5) countysub -> Must be a 10 digit countysub code
#'   6) cd -> Must be a 4 digit cd code.
#' @param num_chars The number of leading zeros this geoid needs.
#' @param geoids A character or numeric vector
#' @returns A character vector with the corrected geoids.
#' @noRd
#' @noMd
left_pad_zeros <- function(num_char, geoids) {

  for (i in seq_len(length(geoids))) {

      diff <- num_char - nchar(geoids[i])
      geoids[i] <- paste(paste(rep(0, diff), collapse = ""),
                         geoids[i], sep = "")
  }

  geoids
}


#' @name remove_delimiters
#' @title remove_delimiters
#' @description This is helper function for remove the most common delimiters
#'   used in character vector data.
#' @param input A character or numeric
#'   vector
#' @returns A character vector with delimiters removed.
#' @noRd
#' @noMd
remove_delimiters <- function(input) {
  gsub("[^1-9A-Za-z]*", "", input)
}
