#' @import httr

# Implementation thought process was adapted from:
# https://github.com/dteck/HUD
# Inspiration to build an API came from:
# https://github.com/ropensci/rnoaa

# An R interface for accessing HUD USER
# (US Department of Housing and Urban Development) APIs.

# HUD USER has four main APIs. These functions declared below follow the
# structure of the API very closely, but provide the additional ability to query
# for multiple geoids as well as multiple years inputs.

# For more intuitive querying withing the need to specify type, check out the
# decomposed functions in the hudcw.R and hudchas.R files.

# Crosswalk
# Fair Markets Rent
# Income Limits
# Comprehensive and Housing Affordability Strategy


#' @name hud_cw
#' @title hud_cw
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development.
#' @param type Must be a number between 1 and 12 depending on the Crosswalk
#'   type. You can also supply the string name.
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
#'   5 digit USPS ZIP code of the data to retrieve.
#'   E.g. 22031 for type 1 to 5 and 11 .
#'   or
#'   11 digit unique 2000 or 2010 Census tract GEOID consisting of
#'   state FIPS + county FIPS + tract code. Eg: 51059461700  for type 6
#'   or
#'   5 digit unique 2000 or 2010 Census county GEOID consisting of
#'   state FIPS + county FIPS. Eg: 51600 for type 7
#'   or
#'   5 digit CBSA code for Micropolitan and Metropolitan Areas
#'   Eg: 10380 for type 8
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
#'   previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_cw(type = 1, query = '35213', year = c('2010', '2011'),
#'    quarter = c('1'))
#'
#' hud_cw(type = '2', query = '35213', year = c('2016', '2020'),
#'    quarter = c('2'))
#'
#' hud_cw(type = 3, query = 35213, year = c('2012', '2011'),
#'    quarter = c('3'))
#'
#' hud_cw(type = 4, query = '22031', year = c('2017', '2019'),
#'    quarter = c('4'))
#'
#' hud_cw(type = '5', query = '35213', year = c(2011, '2012'),
#'    quarter = c('1','2'))
#'
#' hud_cw(type = 6, query = '48201223100', year = c('2017', '2010'),
#'    quarter = c('1','2','3'))
#'
#' hud_cw(type = 7, query = '22031', year = c('2010', '2011'),
#'    quarter = c('1','2','3','4'))
#'
#' hud_cw(type = 8, query = '10140', year = c('2010', '2011'),
#'    quarter = c('2','1'))
#'
#' hud_cw(type = 9, query = '10380', year = c('2017'),
#'    quarter = c('4'))
#'
#' hud_cw(type = 10, query = '2202', year = c('2010', '2011'),
#'    quarter = c('4','3'))
#'
#' hud_cw(type = 11, query = '35213', year = c('2019', '2020'),
#'    quarter = c('2','3'))
#'
#' hud_cw(type = 12, query = '4606720300 ', year = c('2019', '2019', '2019'),
#'    quarter = c('4','4'))
#' }
hud_cw <- function(type, query, year = format(Sys.Date() - 365, "%Y"),
                   quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  args <- cw_input_check_cleansing(query, year, quarter, key)

  query <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  alltypes <- c("zip-tract","zip-county","zip-cbsa",
                "zip-cbsadiv","zip-cd","tract-zip",
                "county-zip","cbsa-zip","cbsadiv-zip",
                "cd-zip","zip-countysub","countysub-zip")

  if(length(type) != 1) stop("Only one crosswalk type can be specified.")

  # Allow user to specify the full string too.
  type <- switch(tolower(type),
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

  # Check if type argument follows proper structure.
  type <- paste(trimws(as.character(type), which = "both"))
  if(FALSE %in% numbers_only(type)) {
    stop("Type input must be a single number or the name of crosswalk file.")
  }
  ifelse(as.integer(type) < 1 || as.integer(type) > 12,
         stop("The type input is not in the range of 1-12"), "")

  lhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][1]
  rhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][2]

  # Need to make sure query is a zip code of 5 digits.
  if(as.integer(type) >= 1 && as.integer(type) <= 5 || as.integer(type) == 11){
    if(any(nchar(query) != 5)) stop("Query input is not of length 5")
    # Need to make sure query is a fips code of 5 digits.
  } else if(as.integer(type) == 7) {
    if(any(nchar(query) != 5)) stop("Query input is not of length 5")
    # Need to make sure query is a fips code
    # with census tract attached onto it 11 digits.
  } else if(as.integer(type) == 6) {
    if(any(nchar(query) != 11)) stop("Query input is not of length 11")
    # Need to make sure query is CBSA code for
    # micropolitan/metropolitan areas: 5 digits.
  } else if(as.integer(type) == 8) {
    if(any(nchar(query) != 5)) stop("Query input is not of length 5")
    # Need to make sure query is CBSA division code for
    # metropolitan areas: 5 digits.
  } else if(as.integer(type) == 9) {
    if(any(nchar(query) != 5)) stop("Query input is not of length 5")
    # Need to make sure query is 4 digit GEOID for
    # congressional districts: 4 digits.
  } else if(as.integer(type) == 10) {
    if(any(nchar(query) != 4)) stop("Query input is not of length 4")
    # Need to make sure query is 10 digits for county subdistrict
  } else if(as.integer(type) == 12) {
    if(any(nchar(query) != 10)) stop("Query input is not of length 10")
  }

  allqueries <- expand.grid(query = query, year = year, quarter = quarter,
                            stringsAsFactors = FALSE)
  allqueries$type <- type[[1]]
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=",
               allqueries$type,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, lhgeoid, rhgeoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, lhgeoid, rhgeoid, key)[[1]])
}


#' @name hud_fmr
#' @title hud_fmr
#' @description This function queries the Fair Markets Rent API provided by
#'   US Department of Housing and Urban Development.
#' @param query Can provide either a 5 digit FIPS code + 99999 at end,
#'   state abbreviation, or CBSA code.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @keywords Fair Markets Rent API
#' @export
#' @returns This function returns a dataframe containing FAIR MARKETS RENT data
#'   for a particular county or state. For county level data, these measurements
#'   include the county_name, counties_msa, town_name, metro_status, metro_name,
#'   smallarea_status, basicdata, Efficiency, One-Bedroom, Two-Bedroom,
#'   Three-Bedroom, Four-Bedroom, and year. For more details about these
#'   measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html For
#'   state level data, these measurements will be the same as county level data,
#'   but will return a dataframe with the individual measurements for each
#'   individual county within the state.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_fmr("VA", year=c(2021))
#'
#' hud_fmr("5100199999", year=c(2021))
#'
#' hud_fmr("METRO47900M47900", year=c(2018))
#' }
hud_fmr <- function(query, year = format(Sys.Date() - 365, "%Y"),
                    key = Sys.getenv("HUD_KEY")) {

  # If state query, will provide data at metro and county level

  args <- fmr_il_input_check_cleansing(query, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]
  querytype <- args[[4]]

  # Create all combinations of query and year...
  allqueries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  # Call helper functions...
  if(querytype == "state") {
      # Merge county level data with metroarea data.
      return(list(counties = hud_fmr_state_counties(query, year, key),
                  metroareas = hud_fmr_state_metroareas(query, year, key)))
  } else if(querytype == "cbsa") {
      # Returns zip level data.
      return(hud_fmr_metroarea_zip(query, year, key))
  } else if(querytype == "county") {
      # Returns zip level data.
      return(hud_fmr_county_zip(query, year, key))
  }

}




#' @name hud_il
#' @title hud_il
#' @description This function queries the Income Limits API provided by
#'   US Department of Housing and Urban Development
#' @param query Can provide either a 5 digit FIPS code + 99999 at end,
#'   state abbreviation, or CBSA code.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
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
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_il("VA", year=c(2021))
#'
#' hud_il("5100199999", year=c(2021))
#'
#' hud_il("METRO47900M47900", year=c(2018))
#' }
hud_il <- function(query, year = format(Sys.Date() - 365, "%Y"),
                   key = Sys.getenv("HUD_KEY")) {
  args <- fmr_il_input_check_cleansing(query, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]
  querytype <- args[[4]]

  errorURLs <- c()

  allqueries <- expand.grid(query = query, year = year, stringsAsFactors = FALSE)

  list_res <- c()
  for(i in seq_len(nrow(allqueries))) {
    # Build the URL for querying the data.
    URL <- paste("https://www.huduser.gov/hudapi/public/il/",
                 if(querytype == "state") "statedata/" else "data/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep="")

    call<-try(GET(URL, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont<-try(content(call), silent = TRUE)

    if('error' %in% names(cont)) {
      errorURLs <- c(errorURLs, URL)
    } else {

      if(querytype == "state") {
        res <- as.data.frame(do.call(cbind,
                                     list(as.data.frame(cont$data$very_low),
                                          as.data.frame(cont$data$extremely_low),
                                          as.data.frame(cont$data$very_low))))
        res$statecode <- cont$data$statecode
        res$stateID <- cont$data$stateID
      } else if(querytype == "county") {
        res <- as.data.frame(cont$data)
      } else {
        res <- as.data.frame(cont$data)
      }

      oth <- data.frame(query = allqueries$query[i],
                        year = allqueries$year[i],
                        median_income = cont$data$median_income, stringsAsFactors = FALSE)
      res <- cbind(oth, res)

      list_res[[i]] <- res
    }
  }


  if(length(errorURLs) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries:", paste(errorURLs, collapse = "\n"),
                  "It is possible that your key maybe invalid,",
                  "there isn't any data for these parameters,",
                  "or you have reached the maximum number of API",
                  "calls per minute. If you think this is wrong please",
                  "report it at https://github.com/etam4260/hudr/issues.",
                  sep = " "))
  }

  if(length(list_res) != 0) {
    res <- do.call(rbind, list_res)
    return(res)
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
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas(1)
#'
#' hud_chas('2', stateId = '56')
#'
#' hud_chas('3','51','199')
#'
#' hud_chas('4', '51', 94087)
#'
#' hud_chas('5', '51', 48996)
#' }
hud_chas <- function(type, stateId = NULL, entityId = NULL,
                     year = c("2014-2018"),
                     key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(type) || !is.vector(year) || !is.vector(key)) {
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
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""))
  }

  # Allow user to specify the string too.
  type <- switch(tolower(type),
                "Nation" = 1,
                "State" = 2,
                "County" = 3,
                "MCD" = 4,
                "Minor Civil Division" = 4,
                "Place" = 5,
                "City" = 5,
                type
  )

  # Removing leading and ending spaces and converting all integer inputs to
  # characters.
  type <- paste(trimws(as.character(type), which = "both"))
  stateId <- paste(trimws(as.character(stateId), which = "both"))
  entityId <- paste(trimws(as.character(entityId), which = "both"))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  # Check for if years are proper input
  if(!all(year %in% c("2014-2018","2013-2017","2012-2016","2011-2015",
                      "2010-2014","2009-2013","2008-2012","2007-2011",
                      "2006-2010"))) {
    stop("Years specified are not allowed. Check the documentation.")
  }

  ifelse(as.integer(type) < 1 || as.integer(type) > 5,
         stop("The type input is not in the range of 1-5"), "")


  if(type == "1") {
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "1",
                 "&year=", year,  sep="")
  }

  if(type == "2") {
    if(is.null(stateId)) stop("You need to specify a stateId for this type.")
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type,
                 "&stateId=", stateId, "&year=", year,  sep="")
    if(!is.vector(stateId)) {
      stop(paste("Make sure all inputs are of type vector. ",
                 "Check types with typeof([variable]). ",
                 "If list try unlist([variable]); ",
                 "if matrix try as.vector([variable])", sep = ""))
    }

    allqueries <- expand.grid(fip_code = stateId, year = year, stringsAsFactors = FALSE)
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "2",
                 "&stateId=", allqueries$fip_code,
                 "&year=", allqueries$year,  sep="")
  }

  if(type == "3" || type == "4" || type == "5") {
    if(is.null(stateId) || is.null(entityId)) stop("You need to specify a
                                                 stateId and entityId
                                                 for this type.")
    if(!is.vector(stateId) || !is.vector(entityId)) {
      stop(paste("Make sure all inputs are of type vector. ",
           "Check types with typeof([variable]). ",
           "If list try unlist([variable]); ",
           "if matrix try as.vector([variable])", sep = ""))
    }

    if(length(stateId) != length(entityId)) {
      stop("You need to make sure stateId and entityId are of same length.")
    }

    allqueries <- expand.grid(state_fip = stateId, year = year,
                              stringsAsFactors = FALSE)
    allqueries$entityId <- entityId

    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type,
                 "&stateId=", allqueries$state_fip,
                 "&entityId=", allqueries$entityId,
                 "&year=", allqueries$year,
                 sep="")
  }
  return(chas_do_query_calls(URL, key = key))
}
