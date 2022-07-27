#' @name chas_input_check_cleansing
#' @title Input Cleansing for Comprehensive Housing and
#'   Affordability Strategy Queries Helper
#' @description Helper function used to clean user inputted variables for all
#'    decomposed (Comprehensive Housing and Affordability Strategy)
#'    CHAS functions.
#' @param query
#'   The inputted geographic identifiers to query for.
#' @param year The years to query for.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @returns The cleansed input arguments.
#' @noRd
chas_input_check_cleansing <- function(query, year, key) {
   res <- NULL

   if (!is.vector(year) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (length(key) != 1) {
    stop("\nYou need one key and only 1 key", call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)


  }

  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  for (i in seq_len(length(year))) {
    if (!any(year %in% c("2014-2018", "2013-2017", "2012-2016", "2011-2015",
                        "2010-2014", "2009-2013", "2008-2012", "2007-2011",
                        "2006-2010"))) {
      stop("\nOne of the years does not fall in the correct range of values.",
           call. = FALSE)
    }
  }

  if (!missing(query)) {

    if (!is.vector(query)) {
      stop(paste("\nMake sure all inputs are of type vector. ",
                 "Check types with typeof([variable]). ",
                 "If list try unlist([variable]); ",
                 "if matrix try as.vector([variable])", sep = ""),
           call. = FALSE)
    }

    query <- unique(paste(trimws(as.character(query), which = "both")))

    res <- list(query, year, key)
  } else {
    res <- list(year, key)
  }

  res
}



#' @name cw_input_check_cleansing
#' @title Input Cleansing for USPS Crosswalk Queries Helper
#' @description Helper function used to clean user inputted variables for all
#'   (United States Postal Service) USPS Crosswalk functions.
#' @param primary_geoid
#'   The first geoid used in the function call. For example, in
#'   hud_cw_zip_tract(), the first geoid would be zip.
#' @param secondary_geoid
#'   The second geoid used in the function call. For example, in
#'   hud_cw_zip_tract(), the second geoid would be tract.
#' @param query
#'   The inputted geographic identifiers to query for.
#' @param year The years to query for.
#' @param quarter The quarters to query for.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @returns The cleansed input arguments.
#' @noRd
cw_input_check_cleansing <- function(primary_geoid, secondary_geoid,
                                     query, year,
                                     quarter,
                                     key) {
  if (!is.vector(query) || !is.vector(year) ||
     !is.vector(quarter) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (primary_geoid == "cbsadiv" || secondary_geoid == "cbsadiv") {
    min_year <- 2017
    min_quarter <- 4
  } else if (primary_geoid == "countysub" || secondary_geoid == "countysub") {
    min_year <- 2018
    min_quarter <- 2
  } else {
    min_year <- 2010
    min_quarter <- 1
  }

  if (length(key) != 1) {
    stop("\nYou need a key and only 1 key.", call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)

  }

  query <- unique(paste(trimws(as.character(query), which = "both")))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  quarter <- unique(paste(trimws(as.character(quarter), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  if (FALSE %in% numbers_only(query)) stop(
    "\nQuery input must only be numbers.",
    call. = FALSE)

  if (FALSE %in% numbers_only(year)) stop("\nYear input must only be numbers.",
                                          call. = FALSE)
  if (FALSE %in% numbers_only(quarter)) {
    stop("\nQuarter input must only be numbers.", call. = FALSE)
  }

  if (!all(as.character(quarter) %in% c("1", "2", "3", "4"))) {
    stop("\nQuarters must be from 1 to 4.", call. = FALSE)
  }

  ifelse(any(as.integer(year) >
               as.integer(strsplit(as.character(Sys.Date()), "-")[[1]][1])),
         stop("\nA year specified seems to be in the future?",
              call. = FALSE), "")

  ifelse(any(as.integer(year) < min_year),
         stop(paste("\nOne of the years is below the min year of this query: ",
                    min_year,
                    sep = ""),
              call. = FALSE), "")

  ifelse(any(year == min_year) && any(as.integer(quarter) < min_quarter),
         stop(paste("\nOne of the quarter(s) is below the
                    minimum quarter for the minimum year: ",
                    min_quarter,
                    sep = ""),
              call. = FALSE), "")


  list(query, as.integer(year), as.integer(quarter), key)
}


#' @name fmr_il_input_check_cleansing
#' @title Input Cleansing for Fair Markets Rent and Income Limits Queries Helper
#' @description Helper function used to clean user inputted variables for all
#'   Fair Markets Rent and Income Limits APIs.
#' @param query
#'   The inputted geographic identifiers to query for.
#' @param year The years to query for.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @returns The cleansed input arguments.
#' @noRd
fmr_il_input_check_cleansing <- function(query, year, key) {
  if (!is.vector(query) || !is.vector(year) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (length(key) != 1) {
    stop("\nYou need a key and only 1 key.", call. = FALSE)
  }

  query <- unique(paste(trimws(as.character(query), which = "both")))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)

  }

  # Try to fix case
  if (all(nchar(query) == 2)) {
    query <- toupper(query)
  } else if (all(nchar(query) > 2)) {
    query <- capitalize(query)
  }

  # Try to convert input into state code.
  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(hud_nation_states_territories(key = key))
  }

  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(query), ]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_name %in%
                             as.character(query), ][2]
    query <- unlist(query[[1]])
  }

  if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(query), ]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_code %in%
                             as.character(query), ][2]
    query <- unlist(query[[1]])
  }
  if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(query), ]) != 0) {
    query <- pkg.env$state[pkg.env$state$state_num %in%
                             as.character(query), ][2]
    query <- unlist(query[[1]])
  }

  if (FALSE %in% numbers_only(year)) stop("\nYear input must only be numbers.",
                                          call. = FALSE)
  # Check year and query input to see if they fit within
  # the "range" of acceptable values.
  ifelse(any(as.integer(year) >
               as.integer(strsplit(as.character(Sys.Date()), "-")[[1]][1])),
         stop("\nA year specified seems to be in the future?", call. = FALSE),
              "")


  min_year <- 2017
  ifelse(any(as.integer(year) < min_year),
         stop(paste("\nOne of the years is below the min year of this query:",
                    min_year,
                    sep = ""),
              call. = FALSE), "")

  # Assume length of inputted query as indicator that this is a certain geoid.
  if (all(nchar(as.character(query)) == 10)) {
    querytype <- "county"
    # Coerce back to integer for user processing...
  } else if (all(nchar(as.character(query)) == 2)) {
    querytype <- "state"
  } else if (all(nchar(as.character(query)) == 16)) {
    querytype <- "cbsa"
  } else {
    stop("\nThere is no matching code for this inputted state.", call. = FALSE)
  }

  # Make sure to coerce year back to character for user processing.
  list(query, as.integer(year), key, querytype)
}



#' @name crosswalk_a_dataset_input_check_cleansing
#' @title Input Cleansing for crosswalk() Function Helper
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
  # For now only support a single year, single quarter, single key, and
  # single method
  # and single column, single geoid, single cw_geoid, and single data.
  if (!is.vector(data) && !is.vector(geoid) && !is.vector(geoid_col)
     && !is.vector(cw_geoid) && !is.vector(year)
     && !is.vector(quarter)
     && !is.vector(key)) {
     stop(paste("\nMake sure all inputs are of type vector. ",
                "Try using as.vector() on input arguments. ",
                "i.e 'as.vector(year)'", sep = ""), call. = FALSE)
  }


  # Check if geoid_col exists in dataset or is in valid range
  # Check if this column is all numbers too.
  tryCatch({

    data[[geoid_col]]

    },
    error = function(cond) {

      stop(paste("\nIf the geoid_col argument is
                 specified with indexes make sure ",
               "it is within the range of the dataset. If column names are ",
               "specified, make sure the column name exists in the dataset.",
               sep = ""),
           call. = FALSE)
    }
  )

  # Check if cw_geoid_col (s) exist in the dataset or is in valid range
  # Check if this column is all numbers too.
  tryCatch({

    data[[cw_geoid_col]]

    }, error = function(cond) {
      stop(paste("\nIf the cw_geoid_col argument is
                 specified with indexes make ",
                 "sure it is within the range of the dataset.
                 If column names are ",
                 "specified, make sure the column name exists in the dataset.",
                 sep = ""),
           call. = FALSE)
  })


  if (!all(numbers_only(data[[geoid_col]]))) {
    stop("\nMake sure all items in the geoid_col are made of numbers only",
         call. = FALSE)
  }

  if (!all(numbers_only(data[[cw_geoid_col]]))) {
    stop("\nMake sure all items in the cw_geoid_col are made of numbers only",
         call. = FALSE)
  }

  if (length(geoid) > 1 ||  length(geoid_col) > 1
      && length(cw_geoid) > 1 || length(year) > 1 || length(quarter) > 1) {
    stop(paste("\nThis function currently only supports
               crosswalking one dataset ",
               "at a time. Make sure all input arguments are of length 1: ",
               "not including the data or cw_geoid_col arguments", sep = ""),
         call. = FALSE)
  }

  geoid <- unique(paste(trimws(as.character(geoid), which = "both")))
  geoid_col <- unique(paste(trimws(as.character(geoid_col), which = "both")))
  cw_geoid <- unique(paste(trimws(as.character(cw_geoid), which = "both")))

  if (length(key) != 1) {
    stop("\nYou need a key and only 1 key.", call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)
  }


  args <- cw_input_check_cleansing(primary_geoid = geoid,
                                   secondary_geoid = cw_geoid,
                                   query = data[, geoid_col], year = year,
                                   quarter = quarter, key = key)

  list(geoid, geoid_col, cw_geoid, cw_geoid_col,
       method, args[2], args[3], args[4])
}


#' @name capitalize
#' @title capitalize
#' @description Returns first character capitalized in string.
#'   Helper function for dealing with state inputs that are
#'   lowercase such as 'california'.
#' @param string A character
#' @returns A string with only first letter in string capitalized.
#'   Does not capitalize all words in a sentence.
#' @noRd
#' @noMd
capitalize <- function(string) {
  string <- tolower(string)

  paste(toupper(substr(string, 1, 1)),
        substr(string, 2, nchar(string)), sep = "")
}
