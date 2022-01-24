#library for http requests
#library to sanitize strings
#' @import httr
#' @import stringr

# Implementation thought process was adapted from
# https://github.com/dteck/HUD

# This documentation is for the year of 2022.
# An R interface for accessing HUD (US Department of Housing and Urban Development)
# API.
# The HUD has four main datasets:
# USPS ZIP CODE CROSSWALK
# FAIR MARKETS RENT
# INCOME LIMITS
# COMPREHENSIVE HOUSING AFFORDABILITY STRATEGY



# Creating global environment to set default values to some
# of parameters in the web queries.
pkg.env <- new.env(parent = emptyenv())
pkg.env$curr.year <- strsplit(toString(Sys.Date()), "-")[[1]][1]
pkg.env$curr.quarter <- NULL
if(as.integer(pkg.env$curr.year) > 9) {
  pkg.env$curr.quarter <- "4"
} else if(as.integer(pkg.env$curr.year) > 6) {
  pkg.env$curr.quarter <- "3"
} else if(as.integer(pkg.env$curr.year) > 3) {
  pkg.env$curr.quarter <- "2"
} else {
  pkg.env$curr.quarter <- "1"
}



#' hudcw
#' @name hudcw
#' @title hudcw
#' This function queries the Crosswalks API provided by US Department of Housing and Urban Development
#' @param type Must be a number between 1 and 12 depending on the Crosswalk type.
#' 1) zip-tract
#' 2) zip-county
#' 3) zip-cbsa
#' 4) zip-cbsadiv (Available 4th Quarter 2017 onwards)
#' 5) zip-cd
#' 6) tract-zip
#' 7) county-zip
#' 8) cbsa-zip
#' 9) cbsadiv-zip (Available 4th Quarter 2017 onwards)
#' 10) cd-zip
#' 11) zip-countysub (Available 2nd Quarter 2018 onwards)
#' 12) countysub-zip (Available 2nd Quarter 2018 onwards)
#' @param query
#' 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' or
#' 11 digit unique 2000 or 2010 Census tract GEOID consisting of state FIPS + county FIPS + tract code. Eg: 51059461700  for type 6
#' or
#' 5 digit unique 2000 or 2010 Census county GEOID consisting of state FIPS + county FIPS. Eg: 51600 for type 7
#' or
#' 5 digit CBSA code for Micropolitan and Metropolitan Areas Eg: 10380 for type 8
#' or
#' 5-digit CBSA Division code which only applies to Metropolitan Areas. Eg: 35614 for type 9
#' or
#' 4-digit GEOID for the Congressional District which consists of state FIPS + Congressional District code. Eg: 7200 for type 10
#' or
#' 10-digit GEOID for the County sub Eg: 4606720300 for type 12
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param quarter Gets the quarter of the year that this data was recorded. Defaults to the latest quarter.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a particular GEOID.
#' These measurements include res-ratio, bus-ratio, oth-ratio, tot-ratio. For more details on these measurements, visit https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' cw <- hudwc(type = 7, query = '22031', year = '2021', quarter = '4', key = 'edf23jf834qd72nja')
hudcw <- function(type, query, year = pkg.env$curr.year, quarter = pkg.env$curr.quarter, key) {
  #chop off leading and trailing whitespace in inputs.
  type <- paste(str_trim(as.character(type), side = "both"))
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- paste(str_trim(as.character(year), side = "both"))
  quarter <- paste(str_trim(as.character(quarter), side = "both"))
  key <- paste(str_trim(as.character(key), side = "both"))

  if(as.integer(year) > as.integer(pkg.env$curr.year)) stop("The year specified seems to be in the future?")
  if(as.integer(type) < 1 || as.integer(type) >= 12) stop("The type input is not in the range of 1-12")
  geoid <- NULL

  # Need to make sure query is a zip code of 5 digits.
  if(as.integer(type) >= 1 && as.integer(type) <= 5 || as.integer(type) == 11){
    if(nchar(query) != 5) stop("Query input is not of length 5")
    geoid <- "zip"
  # Need to make sure query is a fips code of 5 digits.
  } else if(as.integer(type) == 7) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
    geoid <- "fips"
  # Need to make sure query is a fips code with census tract attached onto it 11 digits.
  } else if(as.integer(type) == 6) {
    if(nchar(query) != 11) stop("Query input is not of length 11")
    geoid <- "fipstract"
  # Need to make sure query is CBSA code for micropolitan/metropolitan areas: 5 digits.
  } else if(as.integer(type) == 8) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
    geoid <- "CBSA code"
  # Need to make sure query is CBSA division code for metropolitan areas: 5 digits.
  } else if(as.integer(type) == 9) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
    geoid <- "CBSA division"
  # Need to make sure query is 4 digit GEOID for congressional districts: 4 digits.
  } else if(as.integer(type) == 10) {
    if(nchar(query) != 4) stop("Query input is not of length 4")
    geoid <- "Congressional District"
  # Need to make sure query is 10 digits for county subdistrict
  } else if(as.integer(type) == 12) {
    if(nchar(query) != 10) stop("Query input is not of length 10")
    geoid <- "County subdistrict"
  }

  # Build the URL for querying the data.
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", type, "&query=", query, "&year=", year, "&quarter=", quarter, sep="") #build URL

  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call

  cont<-try(content(call), silent = TRUE) #parse returned data

  if (length(cont$data) == 0){ #check to see if results exist - if not warn use of errors in their input values
    stop("The query did not return any results. Please check if these input values follow the rules stated for each parameter. Maybe your authorization key is wrong?")
  } else {
    res<-data.frame(geoid=integer(),zip=integer(),res_ratio=integer(),
                    bus_ratio=integer(),oth_ratio=integer(),
                    tot_ratio=integer(),year=integer(), quarter=integer()) #build df
    colnames(res)[1] <- geoid
    for(i in seq(1,length(cont$data$results),1)){ #iterate over results and append to df

      int<-data.frame(geoid=query,
                      zip=cont$data$results[[i]][["geoid"]],
                      res_ratio=cont$data$results[[i]][["res_ratio"]],
                      bus_ratio=cont$data$results[[i]][["bus_ratio"]],
                      oth_ratio=cont$data$results[[i]][["oth_ratio"]],
                      tot_ratio=cont$data$results[[i]][["tot_ratio"]],
                      year = year,
                      quarter = quarter)
      colnames(int)[1] <- geoid
      res<-rbind.data.frame(res, int) #appends rows to df

    }
    return(res) #returns df as output of function
  }
}




#' hudfmr
#' @name hudfmr
#' @title hudfmr
#' This function queries the Fair Markets Rent API provided by US Department of Housing and Urban Development
#' @param query Can provide either FIPS code, CBSA code for metro areas, or state abbreviation.
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing FAIR MARKETS RENT data for a particular county or state.
#' For county level data, these measurements include the county_name, counties_msa, town_name, metro_status, metro_name,
#' smallarea_status, basicdata, Efficiency, One-Bedroom, Two-Bedroom, Three-Bedroom, Four-Bedroom, and year
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
#' For state level data, these measurements will be the same as county level data, but will return a dataframe with
#' the individual measurements for each individual county within the state.
#' @examples
#' fmr <- hudfmr(query = '22031', year = '2021', key = 'edf23jf834qd72nja')
#' fmr <- hudfmr(query = 'VA', year = '2021', key = 'edf23jf834qd72nja')
hudfmr <- function(query, year = pkg.env$curr.year, key) {
  #chop off leading and trailing whitespace in inputs.
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- paste(str_trim(as.character(year), side = "both"))
  key <- paste(str_trim(as.character(key), side = "both"))

  if(as.integer(year) > as.integer(pkg.env$curr.year)) stop("The year specified seems to be in the future?")
  if(is.na(as.integer(query)) && nchar(query) != 2) stop("The inputted query for state abbreviation is not right.")
  if(!is.na(as.integer(query)) && nchar(query) != 5) stop("The inputted query input is not a 5 digit fips or CBSA code.")
  geoid <- NULL
  if(is.na(as.integer(query))) geoid <- "state"
  if(!is.na(as.integer(query))) geoid <- "county or CBSA"

  # Build the URL for querying the data.
  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/", if(geoid == "State") "statedata/" else "data/", query, "?year=", year, sep="") #build URL

  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call

  cont<-try(content(call), silent = TRUE) #parse returned data

  if (length(cont$data) == 0){ #check to see if results exist - if not warn use of errors in their input values
    stop("The query did not return any results. Please check if these input values follow the rules stated for each parameter. Maybe your authorization key is wrong?")
  } else {
    res<-data.frame(geoid=integer(),zip=integer(),res_ratio=integer(),
                    bus_ratio=integer(),oth_ratio=integer(),
                    tot_ratio=integer(),year=integer(), quarter=integer()) #build df
    colnames(res)[1] <- geoid

    for(i in seq(1,length(cont$data$results),1)){ #iterate over results and append to df

      # int<-data.frame(geoid=query,
      #                 town=cont$data$counties[[i]][["town_name"]],
      #                 county=cont$data$counties[[i]][["res_ratio"]],
      #                 metro=cont$data$counties[[i]][["bus_ratio"]],
      #                 fips=cont$data$counties[[i]][["oth_ratio"]],
      #                 efficiency=cont$data$counties[[i]][["tot_ratio"]],
      #                 onebedroom = ,
      #                 twobedroom = ,
      #                 threebedroom =,
      #                 fourbedroom =,
      #                 fmrpercentile =,
      #                 statename =
      #                 smallareastatus =,
      #                 )
      colnames(int)[1] <- geoid
      res<-rbind.data.frame(res, int) #appends rows to df

    }
    return(res) #returns df as output of function
  }
}




#' hudil
#' @name hudil
#' @title hudil
#' This function queries the Income Limits API provided by US Department of Housing and Urban Development
#' @param query Can provide either FIPS code, CBSA code for metro areas, or state abbreviation.
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Income Limits API
#' @export
#' @returns This function returns a dataframe containing INCOME LIMITS data for a particular county or state.
#' For county level data, these measurements include the county_name, counties_msa, town_name, metro_status, metro_name,
#' year, median_income, very_low+, extremely_low+, and low+.
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
#' For state level data, these measurements will be the same as county level data, but will return a dataframe with
#' the individual measurements for each individual county within the state.
#' @examples
#' il <- hudil(query = '22031', year = '2021', key = 'edf23jf834qd72nja')
#' il <- hudil(query = 'VA', year = '2021', key = 'edf23jf834qd72nja')
hudil <- function(query, year = pkg.env$curr.year, key) {

}



#' hudchas
#' @name hudchas
#' @title hudchas
#' This function queries the Income Limits API provided by US Department of Housing and Urban Development
#' @param type Queries the data based off
#' 1 - Nation
#' 2 - State
#' 3 - County
#' 4 - MCD
#' 5 - Place
#' @param query Can provide either FIPS code, CBSA code for metro areas, or state abbreviation.
#' @param year Gets the year that this data was recorded. Defaults to 2014-2018. There are specific year ranges
#' that are only accepted.
#' 2014-2018
#' 2013-2017
#' 2012-2016
#' 2011-2015
#' 2010-2014
#' 2009-2013
#' 2008-2012
#' 2007-2011
#' 2006-2010
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Comprehensive Housing Affordability Strategy (CHAS) API
#' @export
#' @returns This function returns a dataframe containing FREE MARKETS RENT data for a particular county or state.
#'
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
#' For state level data, these measurements will be the same as county level data, but will return a dataframe with
#' the individual measurements for each individual county within the state.
#' @examples
#' chas <- hudchas(type = '2', query = '56', year = '2014-2018', key = 'edf23jf834qd72nja')
hudchas <- function(type, query, year = "2014-2018", key) {

}
