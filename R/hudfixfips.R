#' @name fix_fips
#' @title fix_fips
#' @description Sometimes when loading in fips data from an excel file it truncates the leading 0's. This attempts to re-add those.
#' @param fips A column or vector of fips data which should be 5 characters long.
#' @returns A dataframe with the corrected fips.
fix_fips <- function(fips) {
  for(i in 1:length(fips)) {
    # Assume that if it is 4 characters long, then it could be truncated fips...
    print(nchar(fips[i]))
    if(!is.na(fips[i]) && !is.null(fips[i]) && !is.nan(fips[i]) && nchar(fips[i]) == 4 && numbers_only(fips[i])) {
      fips[i] <- paste("0", fips[i], sep = "")
    }
  }
  return(fips)
}

#' @name fix_state_fips
#' @title fix_state_fips
#' @description Sometimes when loading in fips data from an excel file it truncates the leading 0's. This attempts to re-add those.
#' @param fips A column or vector of fips data which should be 2 characters long.
#' @returns A dataframe with only the corrected state fips.
fix_state_fips <- function(state_fips) {
  for(i in 1:length(state_fips)) {
    if(!is.na(state_fips[i]) && !is.null(state_fips[i]) && !is.nan(state_fips[i]) && nchar(state_fips[i]) == 1 && numbers_only(fips[i])) {
      state_fips[i] <- paste("0", state_fips[i], sep = "")
    }
  }
  return(state_fips)
}


#' @name fix_county_fips
#' @title fix_county_fips
#' @description Sometimes when loading in fips data from an excel file it truncates the leading 0's. This attempts to re-add those.
#' @param fips A column or vector of fips data which should be 3 characters long.
#' @returns A dataframe with the corrected county fips.
fix_county_fips <- function(county_fips) {
  for(i in 1:length(county_fips)) {
    if(!is.na(county_fips[i]) && !is.null(county_fips[i]) && !is.nan(county_fips[i]) && nchar(county_fips[i]) == 2 && numbers_only(fips[i])) {
      county_fips[i] <- paste("0", county_fips[i], sep = "")
    }
  }
}
