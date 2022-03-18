#' @name add_leading_zeros
#' @title add_leading_zeros
#' @description Sometimes when loading in fips data from an excel file it truncates the leading 0's. This attempts to re-add those.
#' @param geoid_type What geoid type to add leading 0s to
#' 1) zip -> Must be 5 digit zip code
#' 2) county -> Must be a 5 digit county fips code
#' 3) cbsa -> Must be a 5 digit cbsa code
#' 4) cbsa-div -> Must be a 5 digit cbsa code
#' 5) countysub -> Must be a 10 digit countysub code
#' 6) cd -> Must be a 4 digit cd code
#' @param geoids A vector or column of geoids that need leading zeros to be processed properly.
#' @param num_chars If you don't know what geoid_type you are looking at, you can specify the length this geoid should be.
#' @returns A dataframe with the corrected geoids.
add_leading_zeros <- function(geoid_type = "zip", geoids, num_chars = -Inf) {
  if(geoid_type == "zip") {
    return(fix_geoid(geoids, 5))
  } else if(geoid_type == "county") {
    return(fix_geoid(geoids, 5))
  } else if(geoid_type == "cbsa") {
    return(fix_geoid(geoids, 5))
  } else if(geoid_type == "cbsadiv") {
    return(fix_geoid(geoids, 5))
  } else if(geoid_type == "countysub") {
    return(fix_geoid(geoids, 10))
  } else if(geoid_type == "cd") {
    return(fix_geoid(geoids, 4))
  } else if(geoid_type == "tract") {
    return(fix_geoid(geoids, 11))
  }
}


#' @name fix_geoid
#' @title fix_geoid
#' @description This is helper function for adding leading zeros back to a dataset.
#' @param geoid_type What geoid type to add leading 0s to
#' 1) zip -> Must be 5 digit zip code
#' 2) county -> Must be a 5 digit county fips code
#' 3) cbsa -> Must be a 5 digit cbsa code
#' 4) cbsa-div -> Must be a 5 digit cbsa code
#' 5) countysub -> Must be a 10 digit countysub code
#' 6) cd -> Must be a 4 digit cd code
#' @param geoids A vector or column of geoids that need leading zeros to be processed properly.
#' @param num_chars The number of leading zeros this geoid needs.
#' @returns A dataframe with the corrected geoids
#' @noRd
fix_geoid <- function(geoids, num_char) {
  for(i in 1:length(geoids)) {
    if(!is.null(geoids[i]) && !is.na(geoids[i]) && !is.nan(geoids[i]) && numbers_only(geoids[i])) {
      len_geo <- nchar(geoids[i])
      zeros <- num_char - len_geo

      geoids[i] <- paste(rep(0, zeros), geoids[i], collapse = "", sep = "")
    }
  }
  return(geoids)
}
