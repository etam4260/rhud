#library for http requests
#library to sanitize strings
#' @import httr
#' @import stringr
#' @import tidyverse
#' @import future
#' @import R.utils

# Implementation thought process was adapted from:
# https://github.com/dteck/HUD
# Inspiration to build an API came from:
# https://github.com/ropensci/rnoaa

# An R interface for accessing HUD USER
# (US Department of Housing and Urban Development) APIs.

# HUD USER has four main APIs
# Crosswalk
# Fair Markets Rent
# Income Limits
# Comprehensive and Housing Affordability Strategy

#' @name hud_cw
#' @title hud_cw
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development.
#' @param type Must be a number between 1 and 12 depending on the Crosswalk type.
#'   1) zip-tract
#'   2) zip-county
#'   3) zip-cbsa
#'   4) zip-cbsadiv (Available 4th Quarter 2017 onwards)
#'   5) zip-cd
#'   6) tract-zip
#'   7) county-zip
#'   8) cbsa-zip
#'   9) cbsadiv-zip (Available 4th Quarter 2017 onwards)
#'   10) cd-zip
#'   11) zip-countysub (Available 2nd Quarter 2018 onwards)
#'   12) countysub-zip (Available 2nd Quarter 2018 onwards)
#' @param query
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#'   or
#'   11 digit unique 2000 or 2010 Census tract GEOID consisting of
#'   state FIPS + county FIPS + tract code. Eg: 51059461700  for type 6
#'   or
#'   5 digit unique 2000 or 2010 Census county GEOID consisting of
#'   state FIPS + county FIPS. Eg: 51600 for type 7
#'   or
#'   5 digit CBSA code for Micropolitan and Metropolitan Areas Eg: 10380 for type 8
#'   or
#'   5-digit CBSA Division code which only applies to Metropolitan Areas.
#'   Eg: 35614 for type 9
#'   or
#'   4-digit GEOID for the Congressional District which consists of
#'   state FIPS + Congressional District code. Eg: 7200 for type 10
#'   or
#'   10-digit GEOID for the County sub Eg: 4606720300 for type 12
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw <- function(type, query, year = format(Sys.Date(), "%Y"), quarter = 1,
                   key = Sys.getenv("HUD_KEY")) {

  if(key == "") stop("Did you forget to set the key?")

  alltypes <- c("zip-tract","zip-county","zip-cbsa",
                "zip-cbsadiv","zip-cd","tract-zip",
                "county-zip","cbsa-zip","cbsadiv-zip",
                "cd-zip","zip-countysub","countysub-zip")

  # Allow user to specify the full string too.
  type = switch(tolower(type),
                "zip-tract" = 1,
                "zip-county" = 2,
                "zip-cbsa" = 3,
                "zip-cbsadiv" = 4,
                "zip-cd" = 5,
                "tract-zip" = 6,
                "county-zip" = 7,
                "cbsa-zip" = 8,
                "cbsadiv-zip" = 9,
                "cd-zip" = 10,
                "zip-countysub" = 11,
                "countysub-zip" = 12,
                type
                )

  lhgeoid <- NULL
  rhgeoid <- NULL

  URL <- NULL
  call <- NULL
  cont <- NULL
  res <- NULL
  thisyear <- NULL

  # Removing leading and ending spaces and converting all integer inputs to characters
  type <- paste(str_trim(as.character(type), side = "both"))
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- unique(paste(str_trim(as.character(year), side = "both")))
  quarter <- unique(paste(str_trim(as.character(quarter), side = "both")))
  key <- paste(str_trim(as.character(key), side = "both"))

  numbers_only <- function(x) !grepl("\\D", x)
  if(FALSE %in% numbers_only(type)) stop("Type input must only be numbers.")
  if(FALSE %in% numbers_only(query)) stop("Query input must only be numbers.")
  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  if(FALSE %in% numbers_only(quarter)) stop("Quarter input must only be numbers.")

  if(!all(as.character(quarter) %in% c("1","2","3","4"))) stop("Quarters must be from 1 to 4.")

  lhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][1]
  rhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][2]

  ifelse(any(as.integer(year) > as.integer(str_split(Sys.Date(), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")
  ifelse(as.integer(type) < 1 || as.integer(type) > 12,
         stop("The type input is not in the range of 1-12"), "")

  # Need to make sure query is a zip code of 5 digits.
  if(as.integer(type) >= 1 && as.integer(type) <= 5 || as.integer(type) == 11){
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is a fips code of 5 digits.
  } else if(as.integer(type) == 7) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is a fips code with census tract attached onto it 11 digits.
  } else if(as.integer(type) == 6) {
    if(nchar(query) != 11) stop("Query input is not of length 11")
  # Need to make sure query is CBSA code for micropolitan/metropolitan areas: 5 digits.
  } else if(as.integer(type) == 8) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is CBSA division code for metropolitan areas: 5 digits.
  } else if(as.integer(type) == 9) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is 4 digit GEOID for congressional districts: 4 digits.
  } else if(as.integer(type) == 10) {
    if(nchar(query) != 4) stop("Query input is not of length 4")
  # Need to make sure query is 10 digits for county subdistrict
  } else if(as.integer(type) == 12) {
    if(nchar(query) != 10) stop("Query input is not of length 10")
  }

  allqueries <- expand.grid(year = year, quarter = quarter)
  allqueries$type <- type
  allqueries$query <- query

  # allqueries$url <- paste("https://www.huduser.gov/hudapi/public/usps?type=", type, "&query=", query, as.vector(outer(paste('&year=', year, sep = ""), paste('&quarter=', quarter, sep = ""), paste, sep="")), sep="") #build URL
  # allqueries$key <- key
  # if(nrow(allqueries) < pkg.env$cores) use_cores <- nrow(allqueries) else use_cores <- availableCores() - 1
  # return(parallelize_api_calls(allqueries, use_cores))

  list_res <- c()
  for(i in 1:nrow(allqueries)) {
    URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", type, "&query=", query, "&year=", allqueries$year[i], "&quarter=", allqueries$quarter[i], sep="") #build URL
    call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key))), timeout(30)),silent = TRUE) #try to make call
    cont<-try(content(call), silent = TRUE) #parse returned data
    if('error' %in% names(cont[[1]])) {
      warning(paste("Could not find data for inputted query, year, or quarter where query equals ", query, " ,year equals ",allqueries$year[i], " ,and quarter equals ", allqueries$quarter[i], ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
    } else {
      res <- as.data.frame(do.call(rbind, cont$data$results))
      res$type <- allqueries$type[i]
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      res$quarter <- allqueries$quarter[i]
      list_res[[i]] <- res
    }
  }

  allres <- NULL
  if(length(list_res) != 0) {
    allres <- do.call(rbind, list_res)
    colnames(allres)[1] <- rhgeoid
    colnames(allres)[7] <- lhgeoid
  }
  return(allres)
}


#' @name hud_fmr
#' @title hud_fmr
#' @description This function queries the Fair Markets Rent API provided by
#'   US Department of Housing and Urban Development.
#' @param query Can provide either a 5 digit FIPS code + 99999 at end,
#'   state abbreviation, or CBSA code.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing FAIR MARKETS RENT
#'   data for a particular county or state. For county level data, these
#'   measurements include the county_name, counties_msa, town_name, metro_status,
#'   metro_name, smallarea_status, basicdata, Efficiency, One-Bedroom,
#'   Two-Bedroom, Three-Bedroom, Four-Bedroom, and year For more details about
#'   these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
#'   For state level data, these measurements will be the same as county level
#'   data, but will return a dataframe with the individual measurements for each
#'   individual county within the state.
hud_fmr <- function(query, year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {
  URL <- NULL
  call <- NULL
  cont <- NULL
  querytype <- NULL
  numbers_only <- function(x) !grepl("\\D", x)

  if(key == "") stop("Did you forget to set the key?")
  if(nchar(query) == 2) {
    query = toupper(query)
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
    if(!any(as.character(query) == pkg.env$state)) stop("There is no matching code for this inputted state.")

    if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(query),]) != 0) query <- pkg.env$state[pkg.env$state$state_name == as.character(query),][2]
    if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(query),]) != 0) query <- pkg.env$state[pkg.env$state$state_code == as.character(query),][2]
    if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(query),]) != 0) query <- pkg.env$state[pkg.env$state$state_num == as.character(query),][2]
    query = unlist(query)

  }
  if(nchar(query) > 2) query = capitalize(query)
  # Removing leading and ending spaces and converting all integer inputs
  # to characters
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- unique(paste(str_trim(as.character(year), side = "both")))
  key <- paste(str_trim(as.character(key), side = "both"))


  # Check year and query input to see if they fit within
  # the "range" of acceptable values.
  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")

  ifelse(any(as.integer(year) > as.integer(str_split(Sys.Date(), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")

  if(nchar(as.character(query)) == 10) {
    querytype = "county"
  } else if(nchar(as.character(query)) == 2) {
    querytype = "state"
  } else if(nchar(as.character(query)) == 16) {
    querytype = "cbsa"
  } else {
    stop("Query input does not seem to match the length expected.")
  }

  allqueries <- data.frame(query = query, year = year)
  allqueries$url <- paste("https://www.huduser.gov/hudapi/public/fmr/", if(!numbers_only(query))
    "statedata/" else "data/", query, "?year=", allqueries$year, sep="") #build URL

  #if(nrow(allqueries) < pkg.env$cores) use_cores <- nrow(allqueries)

  list_res <- c()

  for(i in 1:nrow(allqueries)) {
    # Build the URL for querying the data.
    URL <- paste("https://www.huduser.gov/hudapi/public/fmr/", if(querytype == "state")
      "statedata/" else "data/", query, "?year=", allqueries$year[i], sep="") #build URL
    call<-try(GET(URL, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))), timeout(30)),
              silent = TRUE) #try to make call

    cont<-try(content(call), silent = TRUE) #parse returned data
    if('error' %in% names(cont)) {
      warning(paste("Could not find data for inputted query, year, or quarter where query equals ", query, " ,year equals ",allqueries$year[i], ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
    } else {
      if(querytype == "state") res <- as.data.frame(do.call(rbind, cont$data$counties)) else res <- as.data.frame(cont$data)
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      list_res[[i]] <- res
    }
  }

  if(length(list_res) != 0) {
    return(do.call(rbind, list_res))
  }
  return(NULL)
}



#' @name hud_il
#' @title hud_il
#' @description This function queries the Income Limits API provided by
#'   US Department of Housing and Urban Development
#' @param query Can provide either a 5 digit FIPS code + 99999 at end,
#'   state abbreviation, or CBSA code.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Income Limits API
#' @export
#' @returns This function returns a dataframe containing INCOME LIMITS data
#'   for a particular county or state. For county level data, these measurements
#'   include the county_name, counties_msa, town_name, metro_status, metro_name,
#'   year, median_income, very_low+, extremely_low+, and low+. For more details
#'   about these measurements, go to
#'   https://www.huduser.gov/portal/dataset/fmr-api.html
hud_il <- function(query, year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {
  URL <- NULL
  call <- NULL
  cont <- NULL
  querytype <- NULL

  if(key == "") stop("Did you forget to set the key?")
  if(nchar(query) == 2) {
    query = toupper(query)
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
    if(!any(as.character(query) == pkg.env$state)) stop("There is no matching code for this inputted state.")

    if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(query),]) != 0) query <- pkg.env$state[pkg.env$state$state_name == as.character(query),][2]
    if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(query),]) != 0) query <- pkg.env$state[pkg.env$state$state_code == as.character(query),][2]
    if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(query),]) != 0) query <- pkg.env$state[pkg.env$state$state_num == as.character(query),][2]
    query = unlist(query)
  }

  if(nchar(query) > 2) query = capitalize(query)

  # Removing leading and ending spaces and converting all integer inputs to characters
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- unique(paste(str_trim(as.character(year), side = "both")))
  key <- paste(str_trim(as.character(key), side = "both"))
  numbers_only <- function(x) !grepl("\\D", x)

  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  # Check year and query input to see if they fit within
  # the "range" of acceptable values.
  ifelse(any(as.integer(year) > as.integer(str_split(Sys.Date(), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")

  if(nchar(as.character(query)) == 10) {
    querytype = "county"
  } else if(nchar(as.character(query)) == 2) {
    querytype = "state"
  } else if(nchar(as.character(query)) == 16) {
    querytype = "cbsa"
  } else {
    stop("Query input does not seem to match the length expected.")
  }

  allqueries <- data.frame(query = query, year = year)
  allqueries$url <- paste("https://www.huduser.gov/hudapi/public/fmr/", if(!numbers_only(query))
    "statedata/" else "data/", query, "?year=", allqueries$year, sep="") #build URL

  list_res <- c()

  #f(nrow(allqueries) < pkg.env$cores) use_cores <- nrow(allqueries)

  for(i in 1:nrow(allqueries)) {
    # Build the URL for querying the data.
    URL <- paste("https://www.huduser.gov/hudapi/public/il/", if(querytype == "state")
      "statedata/" else "data/", query, "?year=", allqueries$year[i], sep="") #build URL
    call<-try(GET(URL, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))), timeout(30)),
              silent = TRUE) #try to make call

    cont<-try(content(call), silent = TRUE) #parse returned data
    if('error' %in% names(cont)) {
      warning(paste("Could not find data for inputted query, year, or quarter where query equals ",
                    query, " ,year equals ",allqueries$year[i],
                    ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
    } else {

      if(querytype == "state") {
        res <- as.data.frame(do.call(cbind, list(as.data.frame(cont$data$very_low),
                                                 as.data.frame(cont$data$extremely_low),
                                                 as.data.frame(cont$data$very_low))))
      } else {
        res <- as.data.frame(cont$data)
      }
      res$statecode <- cont$data$statecode
      res$stateID <- cont$data$stateID
      res$median_income <- cont$data$median_income
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      list_res[[i]] <- res
    }
  }
  if(length(list_res) != 0) {
    return(do.call(rbind, list_res))
  }
  return(NULL)

}



#' @name hud_chas
#' @title hud_chas
#' @description This function queries the CHAS API provided by US Department
#'   of Housing and Urban Development
#' @param type Queries the data based off:
#'   1 - Nation
#'   2 - State
#'   3 - County
#'   4 - MCD
#'   5 - Place
#' @param stateId For types 2,3,4,5, you must provide a stateId. For 3,4,5
#' @param entityId For types 3,4,5, you must provide a fips code
#' @param year Gets the year that this data was recorded. Defaults to 2014-2018.
#'   There are specific year ranges that are only accepted.
#'   2014-2018
#'   2013-2017
#'   2012-2016
#'   2011-2015
#'   2010-2014
#'   2009-2013
#'   2008-2012
#'   2007-2011
#'   2006-2010
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords Comprehensive Housing Affordability Strategy (CHAS) API
#' @export
#' @returns This function returns a dataframe containing CHAS data for a
#'   particular state. For more details about these measurements, go to
#'   https://www.huduser.gov/portal/dataset/chas-api.html
hud_chas <- function(type, stateId = NULL, entityId = NULL, year = c("2014-2018"),
                     key = Sys.getenv("HUD_KEY")) {
  URL <- NULL
  call <- NULL
  cont <- NULL

  if(key == "") stop("Did you forget to set the key?")

  # Allow user to specify the string too.
  type = switch(tolower(type),
                "Nation" = 1,
                "State" = 2,
                "County" = 3,
                "MCD" = 4,
                "Minor Civil Division" = 4,
                "Place" = 5,
                "City" = 5,
                type
  )

  # Removing leading and ending spaces and converting all integer inputs to characters
  type <- paste(str_trim(as.character(type), side = "both"))
  stateId <- paste(str_trim(as.character(stateId), side = "both"))
  entityId <- paste(str_trim(as.character(entityId), side = "both"))
  year <- unique(paste(str_trim(as.character(year), side = "both")))
  key <- paste(str_trim(as.character(key), side = "both"))
  numbers_only <- function(x) !grepl("\\D", x)

  # Check for if years are proper input
  if(!all(year %in% c("2014-2018","2013-2017","2012-2016","2011-2015","2010-2014",
                      "2009-2013","2008-2012","2007-2011","2006-2010"))) stop("Years specified are not allowed. Check the documentation.")
  ifelse(as.integer(type) < 1 || as.integer(type) > 5,
         stop("The type input is not in the range of 1-5"), "")


  if(type == "1") {
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type,
                 "&year=", year,  sep="") #build URL
    allqueries <- data.frame(url = URL, year = year)
  }
  if(type == "2") {
    if(is.null(stateId)) stop("You need to specify a stateId for this type.")
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type,
                 "&stateId=", stateId, "&year=", year,  sep="") #build URL
    allqueries <- data.frame(url = URL, year = year, stateId = stateId)
  }
  if(type == "3" || type == "4" || type == "5") {
    if(is.null(stateId) || is.null(entityId)) stop("You need to specify a
                                                 stateId and entityId
                                                 for this type.")
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
                 type, "&stateId=", stateId, "&entityId=", entityId,
                 "&year=", year,  sep="") #build URL
    allqueries <- data.frame(url = URL, year = year, stateId = stateId, entityId = entityId)
  }

  # allqueries$url <- paste("https://www.huduser.gov/hudapi/public/fmr/", if(!numbers_only(query))
  #   "statedata/" else "data/", query, "?year=", allqueries$year, sep="") #build URL
  #if(nrow(allqueries) < pkg.env$cores) use_cores <- nrow(allqueries)

  list_res <- c()

  for(i in 1:nrow(allqueries)) {
    # Build the URL for querying the data.
    call<-try(GET(allqueries$url[i], add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))), timeout(30)),
              silent = TRUE) #try to make call

    cont<-try(content(call), silent = TRUE) #parse returned data

    if('error' %in% names(cont)) {
      warning(paste("Could not find data for inputted type where type equals",
                    type, " ,year equals ",allqueries$year[i],
                    ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
    } else {
      list_res[[i]] <- cont[[1]]
    }
  }

  if(length(list_res) != 0) {
    return(as.data.frame(do.call(rbind, list_res)))
  }
  return(NULL)
}
