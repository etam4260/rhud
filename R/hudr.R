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
#' 5-dgit CBSA Division code which only applies to Metropolitan Areas. Eg: 35614 for type 9
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
hudcw <- function(type, query, year = pkg.env$curr.year, quarter = "4", key) {

}





#' hudfmr
#' @name hudfmr
#' @title hudfmr
#' This function queries the Free Markets Rent API provided by US Department of Housing and Urban Development
#' @param query Can provide either FIPS code, CBSA code for metro areas, or state abbreviation.
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing FREE MARKETS RENT data for a particular county or state.
#' For county level data, these measurements include the county_name, counties_msa, town_name, metro_status, metro_name,
#' smallarea_status, basicdata, Efficiency, One-Bedroom, Two-Bedroom, Three-Bedroom, Four-Bedroom, and year
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
#' For state level data, these measurements will be the same as county level data, but will return a dataframe with
#' the individual measurements for each individual county within the state.
#' @examples
#' fmr <- hudfmr(query = '22031', year = '2021', key = 'edf23jf834qd72nja')
#' fmr <- hudfmr(query = 'VA', year = '2021', key = 'edf23jf834qd72nja')
hudfmr <- function(query, year = pkg.env$curr.year, key) {

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
