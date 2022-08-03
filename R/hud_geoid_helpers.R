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
#'   If you specify a number, it will
#'   make the geoid of that length.
#' @param input A vector or column of character vectors that need
#'   leading zeros to be processed properly.
#' @returns A vector with leading zeros added.
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

  geoid_type <- assert_geoid_type(geoid_type)

  num_char <- switch(
    geoid_type,
    "countysub" =10,
    "cd" = 4,
    "tract" = 11,
    5
  )

  if (!numbers_only(geoid_type)) {

    if (geoid_type == "zip") {

      res <- left_pad_zeros(num_char = 5, geoids = input)

    } else if (geoid_type == "county") {

      res <- left_pad_zeros(num_char = 5, geoids = input)

    } else if (geoid_type == "cbsa") {

      res <- left_pad_zeros(num_char = 5, geoids = input)

    } else if (geoid_type == "cbsadiv") {

      res <- left_pad_zeros(num_char = 5, geoids = input)

    } else if (geoid_type == "countysub") {

      res <- left_pad_zeros(num_char = 10, geoids = input)

    } else if (geoid_type == "cd") {

      res <- left_pad_zeros(num_char = 4, geoids = input)

    } else if (geoid_type == "tract") {

      res <- left_pad_zeros(num_char = 11, geoids = input)

    }

  } else if (numbers_only(geoid_type)) {

    res <- left_pad_zeros(num_char = geoid_type, geoids = input)

  } else {

    stop(paste("Not a valid input argument for geoid_type. ",
          "You can specify either a number as the expected ",
          "length of the geoid or the name of the geoid lowercase.",
          sep = ""), call. = FALSE)

  }

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
#' @param geoids A vector or column of geoids that need leading zeros to be
#'   processed properly.
#' @param num_chars The number of leading zeros this geoid needs.
#' @returns A vector with the corrected geoids.
#' @noRd
#' @noMd
left_pad_zeros <- function(num_char, geoids) {

  diff <- num_char - nchar(geoids[i])

  for (i in seq_len(length(geoids))) {
      diff <- num_char - nchar(geoids[i])
      geoids[i] <- paste(paste(rep(0, diff), collapse = ""),
                         geoids[i], sep = "")
  }

  geoids
}


#' @name assert_geoid_type
#' @title assert_geoid_type
#' @description This is helper function for remove the most common delimiters
#'   used in character vector data.
#' @param geoid_type A vector or column of characters that need needs delimiters
#'   removed.
#' @returns A vector with delimiters removed.
#' @noRd
#' @noMd
assert_geoid_type <- function (geoid_type) {

}

#' @name remove_delimiters
#' @title remove_delimiters
#' @description This is helper function for remove the most common delimiters
#'   used in character vector data.
#' @param input A vector or column of characters that need needs delimiters
#'   removed.
#' @returns A vector with delimiters removed.
#' @noRd
#' @noMd
remove_delimiters <- function(input) {
  gsub("[^1-9A-Za-z]*", "", input)
}
