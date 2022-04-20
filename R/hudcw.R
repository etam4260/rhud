#' @name hud_cw_zip_tract
#' @title hud_cw_zip_tract
#' @description This function queries the Crosswalks API provided by US
#'   Department of Housing and Urban Development. This returns the crosswalk for
#'   zip to tract.
#' @param zip 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_cw_zip_tract(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_tract(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_tract <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                             quarter = 1, minimal = FALSE,
                             key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "tract"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(zip) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = zip, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 1,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  if(!minimal) {
    return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                             allqueries$quarter, primary_geoid,
                             secondary_geoid, key))
  }
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$tract)
}



#' @name hud_cw_zip_county
#' @title hud_cw_zip_county
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to county.
#' @param zip 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_zip_county(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_county(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_county <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                              quarter = 1, minimal = FALSE,
                              key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "county"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(zip) != 5)) stop("Query input is not of length 5")

  # Create dataframe with all queries needed.
  allqueries <- expand.grid(query = zip, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 2,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$county)
}



#' @name hud_cw_zip_cbsa
#' @title hud_cw_zip_cbsa
#' @description This function queries the Crosswalks API provided by US
#'   Department of Housing and Urban Development. This returns the crosswalk for
#'   zip to cbsa.
#' @param zip 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_cw_zip_cbsa(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cbsa(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cbsa <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                            quarter = 1, minimal = FALSE,
                            key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "cbsa"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(zip) != 5)) stop("Query input is not of length 5")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  allqueries <- expand.grid(query = zip, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 3,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$cbsa)
}



#' @name hud_cw_zip_cbsadiv
#' @title hud_cw_zip_cbsadiv
#' @description This function queries the Crosswalks API provided by US
#'   Department of Housing and Urban Development. This returns the crosswalk for
#'   zip to cbsadiv. (Available 4th Quarter 2017 onwards)
#' @param zip 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_zip_cbsadiv(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cbsadiv(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cbsadiv <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                               quarter = 1, minimal = FALSE,
                               key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "cbsadiv"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(zip) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = zip, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 4,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter, sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$cbsadiv)
}



#' @name hud_cw_zip_cd
#' @title hud_cw_zip_cd
#' @description This function queries the Crosswalks API provided by US
#'   Department of Housing and Urban Development. This returns the crosswalk for
#'   zip to congressional district.
#' @param zip 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_cw_zip_cd(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cd(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cd <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                          quarter = 1, minimal = FALSE,
                          key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "cd"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(zip) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = zip, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 5,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$cd)
}



#' @name hud_cw_tract_zip
#' @title hud_cw_tract_zip
#' @description This function queries the Crosswalks API provided by US
#'   Department of Housing and Urban Development. This returns the crosswalk for
#'   tract to zip.
#' @param tract 11 digit unique 2000 or 2010 Census tract GEOID consisting of
#'   state FIPS + county FIPS + tract code. Eg: 51059461700  for type 6
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_cw_tract_zip(tract = 48201223100, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_tract_zip(tract = '48201223100', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_tract_zip <- function(tract, year = format(Sys.Date() - 365, "%Y"),
                             quarter = 1, minimal = FALSE,
                             key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "tract"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(tract, year, quarter, key)
  tract <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(tract) != 11)) stop("Query input is not of length 11")

  allqueries <- expand.grid(query = tract, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 6,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$zip)
}



#' @name hud_cw_county_zip
#' @title hud_cw_county_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for county to zip.
#' @param county
#'   5 digit unique 2000 or 2010 Census county GEOID consisting of
#' state FIPS + county FIPS. Eg: 51600 for type 7
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_county_zip(county = 22031, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_county_zip(county = '22031', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_county_zip <- function(county, year = format(Sys.Date() - 365, "%Y"),
                              quarter = 1, minimal = FALSE,
                              key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "county"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(county, year, quarter, key)
  county <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(county) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = county, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 7,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$zip)
}



#' @name hud_cw_cbsa_zip
#' @title hud_cw_cbsa_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#' returns the crosswalk for cbsa to zip.
#' @param cbsa 5 digit CBSA code for Micropolitan and Metropolitan Areas Eg:
#'   10380 for type 8
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_cbsa_zip(cbsa = 10140, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cbsa_zip(cbsa = '10140', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cbsa_zip <- function(cbsa, year = format(Sys.Date() - 365, "%Y"),
                            quarter = 1, minimal = FALSE,
                            key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "cbsa"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(cbsa, year, quarter, key)
  cbsa <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(cbsa) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = cbsa, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 8,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$zip)
}



#' @name hud_cw_cbsadiv_zip
#' @title hud_cw_cbsadiv_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for cbsadiv to zip.  (Available 4th Quarter 2017 onwards)
#' @param cbsadiv
#'   5-digit CBSA Division code which only applies to Metropolitan Areas.
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_cbsadiv_zip(cbsadiv = 10380, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cbsadiv_zip(cbsadiv = '10380', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cbsadiv_zip <- function(cbsadiv, year = format(Sys.Date() - 365, "%Y"),
                               quarter = 1, minimal = FALSE,
                               key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "cbsadiv"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(cbsadiv, year, quarter, key)
  cbsadiv <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(cbsadiv) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = cbsadiv, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 9,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$zip)
}



#' @name hud_cw_cd_zip
#' @title hud_cw_cd_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for cbsadiv to zip.
#' @param cd
#'  4-digit GEOID for the Congressional District which consists of
#'  state FIPS + Congressional District code. Eg: 7200 for type 10
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#' previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_cd_zip(cd = 2202, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cd_zip(cd = '2202', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cd_zip <- function(cd, year = format(Sys.Date() - 365, "%Y"),
                          quarter = 1, minimal = FALSE,
                          key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "cd"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(cd, year, quarter, key)
  cd <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(cd) != 4)) stop("Query input is not of length 4")

  allqueries <- expand.grid(query = cd, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 10,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$zip)
}



#' @name hud_cw_zip_countysub
#' @title hud_cw_zip_countysub
#' @description This function queries the Crosswalks API provided by US
#'   Department of Housing and Urban Development. This returns the crosswalk for
#'   zip to countysub. (Available 2nd Quarter 2018 onwards)
#' @param zip 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
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
#' hud_cw_zip_countysub(zip = 35213, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_zip_countysub(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_countysub <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                                 quarter = 1, minimal = FALSE,
                                 key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "countysub"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(zip) != 5)) stop("Query input is not of length 5")

  allqueries <- expand.grid(query = zip, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 11,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$countysub)
}


#' @name hud_cw_countysub_zip
#' @title hud_cw_countysub_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for countysub to zip. (Available 2nd Quarter 2018 onwards)
#' @param countysub
#'   10-digit GEOID for the County sub Eg: 4606720300 for type 12
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#' previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' hud_cw_countysub_zip(countysub = '4606720300 ',
#'    year = c('2019', '2019', '2019'), quarter = c('4','4'))
#'
#' hud_cw_countysub_zip(countysub = '4606720300 ',
#'    year = c('2019'), quarter = c('1'), minimal = TRUE)
#' }
hud_cw_countysub_zip <- function(countysub,
                                 year = format(Sys.Date() - 365, "%Y"),
                                 quarter = 1, minimal = FALSE,
                                 key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "countysub"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(countysub, year, quarter, key)
  countysub <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(any(nchar(countysub) != 10)) stop("Query input is not of length 10")

  allqueries <- expand.grid(query = countysub, year = year, quarter = quarter, stringsAsFactors = FALSE)
  URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 12,
               "&query=", allqueries$query,
               "&year=", allqueries$year,
               "&quarter=", allqueries$quarter,
               sep="")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                                        allqueries$quarter, primary_geoid,
                                        secondary_geoid, key))
  return(cw_do_query_calls(URL, allqueries$query, allqueries$year,
                           allqueries$quarter, primary_geoid,
                           secondary_geoid, key)$zip)
}
