#' @name chas_input_check_cleansing
#' @title chas_input_check_cleansing
#' @description Helper function used to clean user inputted variables for all
#'    decomposed CHAS functions.
#' @param query
#'   The inputted GEOID.
#' @param year The years to query for.
#' @param key The key obtain from HUD USER website.
#' @returns The cleansed input arguments.
#' @noRd
chas_input_check_cleansing <- function(query, year, key) {
  if(!is.vector(year) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""))
  }

  if(length(key) != 1) {
    stop("You only need one key.")
  }

  if(length(key) != 1) stop("There seems to be multiple keys specified.")

  if(key == "") {
    warning(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "sign up and get a token. Save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = " "))


  }

  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  for(i in seq_len(length(year))) {
    if(!any(year %in% c("2014-2018","2013-2017","2012-2016","2011-2015",
                        "2010-2014","2009-2013","2008-2012","2007-2011",
                        "2006-2010"))) {
      stop("One of the years does not fall in the correct range of values.")
    }
  }

  if(!missing(query)) {
    if(!is.vector(query)) {
      stop(paste("Make sure all inputs are of type vector. ",
                 "Check types with typeof([variable]). ",
                 "If list try unlist([variable]); ",
                 "if matrix try as.vector([variable])", sep = ""))
    }

    query <- unique(paste(trimws(as.character(query), which = "both")))
    return(list(query, year, key))
  }
  return(list(year, key))
}



#' @name cw_input_check_cleansing
#' @title cw_input_check_cleansing
#' @description Helper function used to clean user inputted variables for all
#' Crosswalk functions.
#' @param query
#'   The inputted GEOID.
#' @param year The years to query for.
#' @param quarter The quarters to query for.
#' @param key The key obtain from HUD USER website.
#' @returns The cleansed input arguments.
#' @noRd
cw_input_check_cleansing <- function(query, year, quarter, key) {
  if(!is.vector(query) || !is.vector(year) ||
     !is.vector(quarter) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""))
  }

  if(length(key) != 1) {
    stop("You only need one key.")
  }

  if(length(key) != 1) stop("There seems to be multiple keys specified.")

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = " "))

  }

  query <- unique(paste(trimws(as.character(query), which = "both")))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  quarter <- unique(paste(trimws(as.character(quarter), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  if(FALSE %in% numbers_only(query)) stop("Query input must only be numbers.")
  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  if(FALSE %in% numbers_only(quarter)) stop("Quarter input must only be numbers.")

  if(!all(as.character(quarter) %in% c("1","2","3","4"))) {
    stop("Quarters must be from 1 to 4.")
  }

  ifelse(any(as.integer(year) >
               as.integer(strsplit(as.character(Sys.Date()), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")

  return(list(query, as.integer(year), as.integer(quarter), key))
}


#' @name fmr_il_input_check_cleansing
#' @title fmr_il_input_check_cleansing
#' @description Helper function used to clean user inputted variables for all
#'   Fair markets rent and Income Limits datasets.
#' @param query
#'   The inputted GEOID.
#' @param year The years to query for.
#' @param key The key obtain from HUD USER website.
#' @returns The cleansed input arguments.
#' @noRd
fmr_il_input_check_cleansing <- function(query, year, key) {
  if(!is.vector(query) || !is.vector(year) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = " "))
  }

  if(length(key) != 1) {
    stop("You only need one key.")
  }

  if(length(key) != 1) stop("There seems to be multiple keys specified.")

  query <- unique(paste(trimws(as.character(query), which = "both")))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = " "))

  }

  # Try to fix case
  if(all(nchar(query) == 2)) {
    query <- toupper(query)
  } else if(all(nchar(query) > 2)) {
    query <- capitalize(query)
  }

  # Try to convert input into state code.
  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_nation_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  if(nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(query),]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_name %in%
                             as.character(query),][2]
    query <- unlist(query[[1]])
  }
  if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(query),]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_code %in%
                             as.character(query),][2]
    query <- unlist(query[[1]])
  }
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(query),]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_num %in%
                             as.character(query),][2]
    query <- unlist(query[[1]])
  }

  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  # Check year and query input to see if they fit within
  # the "range" of acceptable values.
  ifelse(any(as.integer(year) >
               as.integer(strsplit(as.character(Sys.Date()), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")

  # Assume length of inputted query as indicator that this is a certain geoid.
  if(all(nchar(as.character(query)) == 10)) {
    querytype <- "county"
    # Coerce back to integer for user processing...
  } else if(all(nchar(as.character(query)) == 2)) {
    querytype <- "state"
  } else if(all(nchar(as.character(query)) == 16)) {
    querytype <- "cbsa"
  } else {
    stop("There is no matching code for this inputted state.")
  }

  # Make sure to coerce year back to character for user processing.
  return(list(query, as.integer(year), key, querytype))
}



#' @name crosswalk_a_dataset_input_check_cleansing
#' @title crosswalk_a_dataset_input_check_cleansing
#' @description Helper function used to clean inputs for the
#'   crosswalk() function.
#' @param data A dataset with rows describing measurements at a zip, county,
#'   countysub, cd,
#'   tract, cbsa, or cbsadiv geographic level.
#' @param geoid The current geoid that the dataset is described in: must be zip,
#'   county, countysub, cd,
#'   tract, cbsa, or cbsadiv geographic level.
#' @param geoid_col The column containing the geographic identifier;
#'   must be zip, county, countysub, cd,
#'   tract, cbsa, or cbsadiv geographic level.
#'   Supply either the name of the column
#'   or the index.
#' @param cw_geoid The geoid to crosswalk the dataset to.
#' @param method The allocation method to use: residential,
#'   business, other, or total
#' @param year The year measurement was taken.
#' @param quarter The quarter of year measurement was taken.
#' @param key The key obtain from HUD USER website.
#' @returns The cleansed input arguments.
#' @noRd
crosswalk_a_dataset_input_check_cleansing <- function(data, geoid, geoid_col,
                                                      cw_geoid, cw_geoid_col,
                                                      method,
                                                      year,
                                                      quarter, key) {
  # For now only support a single year, single quarter, single key, and single method
  # and single column, single geoid, single cw_geoid, and single data.
  if(!is.vector(data) && !is.vector(geoid) && !is.vector(geoid_col)
     && !is.vector(cw_geoid) && !is.vector(year)
     && !is.vector(quarter)
     && !is.vector(key)) {
     stop("Make sure all inputs are of type vector. Try using as.vector() on
          input arguments. i.e 'as.vector(year)'")
  }

  geoid <- unique(paste(trimws(as.character(geoid), which = "both")))
  geoid_col <- unique(paste(trimws(as.character(geoid_col), which = "both")))
  cw_geoid <- unique(paste(trimws(as.character(cw_geoid), which = "both")))

  if(length(key) > 1) {
    stop("There seems to be multiple keys specified.")
  }

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = " "))
  }

  if(length(data) > 1 && length(geoid) > 1 &&  length(geoid_col) > 1
     && length(cw_geoid) > 1 && length(year) > 1 && length(quarter) > 1) {
    stop("This function currently only supports crosswalking one dataset
          at a time. Make sure all input arguments are of length 1.")
  }


  args <- cw_input_check_cleansing(query = data[,geoid_col], year = year,
                                   quarter = quarter, key = key)

  return(list(geoid, geoid_col, cw_geoid, cw_geoid_col, method, args[2], args[3], args[4]))
}


#' @name capitalize
#' @title capitalize
#' @description Returns first character capitalized in string.
#'   Helper function for dealing with state inputs that are lowercase such as 'california'.
#' @param string A character
#' @returns A string with only first letter in string capitalized.
#'   Does not capitalize all words in a sentence.
#' @noRd
capitalize <- function(string) {
  return(paste(toupper(substr(string,1,1)),
               substr(string, 2, nchar(string)), sep = ""))
}
