#' @name chas_input_check_cleansing
#' @title chas_input_check_cleansing
#' @description Helper function used to clean user inputted variables for all
#' decomposed CHAS functions.
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
  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = " "))
  }

  if(length(key) != 1) stop("There seems to be multiple keys specified.")

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

    query <- paste(trimws(as.character(query), which = "both"))
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

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = " "))
  }
  if(length(key) != 1) stop("There seems to be multiple keys specified.")

  query <- paste(trimws(as.character(query), which = "both"))
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

  return(list(query, year, quarter, key))
}


fmr_il_input_check_cleansing <- function(query, year, key) {
  if(!is.vector(query) || !is.vector(year) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = " "))
  }

  query <- paste(trimws(as.character(query), which = "both"))
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
  if(length(key) != 1) stop("There seems to be multiple keys specified.")


  # Try to fix case
  if(all(nchar(query) == 2)) {
    query <- toupper(query)
  } else if(all(nchar(query) > 2)) {
    query <- capitalize(query)
  }

  # Try to convert input into state code.
  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  }

  if(nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(query),]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_name %in%
                             as.character(query),][2]
  }
  if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(query),]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_code %in%
                             as.character(query),][2]
  }
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(query),]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_num %in%
                             as.character(query),][2]
  }

  query <- unlist(query[[1]])

  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  # Check year and query input to see if they fit within
  # the "range" of acceptable values.
  ifelse(any(as.integer(year) >
               as.integer(strsplit(as.character(Sys.Date()), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")

  # Assume length of inputted query as indicator that this is a certain geoid.
  if(all(nchar(as.character(query)) == 10)) {
    querytype <- "county"
  } else if(all(nchar(as.character(query)) == 2)) {
    querytype <- "state"
  } else if(all(nchar(as.character(query)) == 16)) {
    querytype <- "cbsa"
  } else {
    stop("There is no matching code for this inputted state.")
  }

  return(list(query, year, key, querytype))
}




#' @name capitalize
#' @title capitalize
#' @description Returns first character capitalized in string. Helper function for dealing with state inputs that are lowercase such as 'california'.
#' @param string A character
#' @returns A string with only first letter in string capitalized. Does not capitalize all words in a sentence.
#' @noRd
capitalize <- function(string) {
  return(paste(toupper(substr(string,1,1)),
               substr(string, 2, nchar(string)), sep = ""))
}
