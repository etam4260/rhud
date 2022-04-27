#' @name add_leading_zeros
#' @title add_leading_zeros
#' @description Sometimes when loading in data from an excel file it
#'   truncates the leading 0's. This attempts to re-add those.
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
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#' zip <- c(02102, 11032, 01232)
#'
#' zip <- hud_add_leading_zeros("zip", zip)
#'
#' cbsa <- c(02102, 11032, 01232)
#'
#' cbsa <- hud_add_leading_zeros("cbsa", cbsa)
#'
#' some_geoid <- c(213113, 6526266, 1316455)
#'
#' some_geoid <- hud_add_leading_zeros(10, some_geoid)
#' }
add_leading_zeros <- function(geoid_type = "zip", input) {
  if (!numbers_only(geoid_type)) {
    if (geoid_type == "zip") {
      return(fix_geoid(input, 5))
    } else if (geoid_type == "county") {
      return(fix_geoid(input, 5))
    } else if (geoid_type == "cbsa") {
      return(fix_geoid(input, 5))
    } else if (geoid_type == "cbsadiv") {
      return(fix_geoid(input, 5))
    } else if (geoid_type == "countysub") {
      return(fix_geoid(input, 10))
    } else if (geoid_type == "cd") {
      return(fix_geoid(input, 4))
    } else if (geoid_type == "tract") {
      return(fix_geoid(input, 11))
    }
  } else if (numbers_only(geoid_type)) {
    return(fix_geoid(input, geoid_type))
  } else {
    stop(paste("Not a valid input argument for geoid_type. ",
          "You can specify either a number as the expected ",
          "length of the geoid or the name of the geoid lowercase.",
          sep = ""))
  }
}

#' @name remove_leading_zeros
#' @title remove_leading_zeros
#' @description Remove leading zeros from character vector.
#' @param input A vector or column of character vectors that need leading zeros
#'   removed.
#' @returns A vector with leading zeros removed.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#' zip <- c(02102, 11032, 01232)
#'
#' zip <- hud_remove_leading_zeros("zip", zip)
#'
#' cbsa <- c(02102, 11032, 01232)
#'
#' cbsa <- hud_remove_leading_zeros("cbsa", cbsa)
#' }
remove_leading_zeros <- function(input) {
  return(sub("^0+", "", input))
}

#' @name remove_delimiters
#' @title remove_delimites
#' @description This is helper function for remove the most common delimiters
#'   used in character vector data.
#' @param input A vector or column of characters that need needs delimiters
#'   removed.
#' @returns A vector with delimiters removed.
remove_delimiters <- function(input) {
  return(gsub("[^1-9A-Za-z]*", "", input))
}


#' @name remove_delimiters
#' @title remove_delimites
#' @description This is helper function for remove the most common delimiters
#'   used in character vector data.
#' @param input A vector or column of characters that need needs delimiters
#'   removed.
#' @returns A vector with delimiters removed.
#' @noRd
#' @noMd
add_delimiters <- function(input) {

}


#' @name fix_geoid
#' @title fix_geoid
#' @description This is helper function for adding leading zeros back to a
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
#' @param num_chars The number of leading zeros thyis geoid needs.
#' @returns A vector with the corrected geoids.
#' @noRd
#' @noMd
fix_geoid <- function(geoids, num_char) {
  for (i in seq_len(length(geoids))) {
      diff <- num_char - nchar(geoids[i])
      geoids[i] <- paste(paste(rep(0, diff), collapse = ""),
                         geoids[i], sep = "")
  }
  return(geoids)
}




# Auxiliary functions for splitting GEOIDS into their constituent parts
# as per the US Census definition of them...

#' @name split_zip
#' @title split_zip
#' @param zip A vector of zip codes
#' @returns Returns a data frame containing the
#'   decomposed elements of zip codes.
#' @noRd
#' @noMd
split_zip <- function(zip) {

}


#' @name split_county
#' @title split_county
#' @param county A vector of county fip codes
#' @returns Returns a data frame containing the
#'   decomposed elements of county fip codes.
#' @noRd
#' @noMd
split_county <- function(county) {

}


#' @name split_cbsa
#' @title split_cbsa
#' @param zip A vector of cbsa codes
#' @returns Returns a data frame containing the
#'   decomposed elements of cbsa codes.
#' @noRd
#' @noMd
split_cbsa <- function(cbsa) {

}


#' @name split_cbsadiv
#' @title split_cbsadiv
#' @param zip A vector of cbsadiv codes
#' @returns Returns a data frame containing the
#'   decomposed elements of cbsadiv codes.
#' @noRd
#' @noMd
split_cbsadiv <- function(cbsadiv) {

}


#' @name split_cd
#' @title split_cd
#' @param zip A vector of cd codes
#' @returns Returns a data frame containing the
#'   decomposed elements of a cd codes.
#' @noRd
#' @noMd
split_cd <- function(cd) {

}


#' @name split_countysub
#' @title split_countysub
#' @param zip A vector of countysub codes
#' @returns Returns a data frame containing the
#'   decomposed elements of countysub codes.
#' @noRd
#' @noMd
split_countysub <- function(countysub) {

}


#' @name split_tract
#' @title split_tract
#' @param zip A vector of tract codes
#' @returns Returns a data frame containing the
#'   decomposed elements of a tract code.
#' @noRd
#' @noMd
split_tract <- function(tract) {

}
