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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::crosswalk()]
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing crosswalk data for a
#'   particular geoid. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_zip_tract(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_tract(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_tract <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                             quarter = 1, minimal = FALSE,
                             key = Sys.getenv("HUD_KEY"),
                             to_tibble) {

  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)


  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "zip"
  secondary_geoid <- "tract"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid, zip,
                                   year, quarter, key)

  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(zip) != 5)) stop("\nZipcode inputs are not all of length 5",
                                 call. = FALSE)

  all_queries <- expand.grid(query = zip, year = year,
                            quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 1,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$tract)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_zip_county(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_county(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_county <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                              quarter = 1, minimal = FALSE,
                              key = Sys.getenv("HUD_KEY"),
                              to_tibble) {

  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)


  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "zip"
  secondary_geoid <- "county"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(zip) != 5)) stop("\nZipcode inputs are not all of length 5",
                                 call. = FALSE)

  # Create dataframe with all queries needed.
  all_queries <- expand.grid(query = zip, year = year,
                            quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 2,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract,
  # 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }

  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$county)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_zip_cbsa(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cbsa(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cbsa <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                            quarter = 1, minimal = FALSE,
                            key = Sys.getenv("HUD_KEY"),
                            to_tibble) {

  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "zip"
  secondary_geoid <- "cbsa"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(zip) != 5)) stop("\nZipcode inputs are not all of length 5",
                                 call. = FALSE)

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  all_queries <- expand.grid(query = zip, year = year,
                            quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 3,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$cbsa)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_zip_cbsadiv(zip = '35213', year = c('2017'), quarter = c('4'))
#'
#' hud_cw_zip_cbsadiv(zip = '35213', year = c('2017'), quarter = c('4'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cbsadiv <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                               quarter = 1, minimal = FALSE,
                               key = Sys.getenv("HUD_KEY"),
                               to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)


  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }


  primary_geoid <- "zip"
  secondary_geoid <- "cbsadiv"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(zip) != 5)) stop("\nZipcode inputs are not all of length 5",
                                 call. = FALSE)

  all_queries <- expand.grid(query = zip, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 4,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter, sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract,
  # 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$cbsadiv)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_zip_cd(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cd(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cd <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                          quarter = 1, minimal = FALSE,
                          key = Sys.getenv("HUD_KEY"),
                          to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }


  primary_geoid <- "zip"
  secondary_geoid <- "cd"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(zip) != 5)) stop("\nZipcode inputs are not all of length 5",
                                 call. = FALSE)

  all_queries <- expand.grid(query = zip, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 5,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract,
  # 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$cd)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_tract_zip(tract = 48201223100, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_tract_zip(tract = '48201223100', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_tract_zip <- function(tract, year = format(Sys.Date() - 365, "%Y"),
                             quarter = 1, minimal = FALSE,
                             key = Sys.getenv("HUD_KEY"),
                             to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "tract"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   tract, year, quarter, key)
  tract <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(tract) != 11)) stop("\nTract inputs are not all of length 11",
                                    call. = FALSE)

  all_queries <- expand.grid(query = tract, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 6,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract,
  # 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$zip)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_county_zip(county = 22031, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_county_zip(county = '22031', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_county_zip <- function(county, year = format(Sys.Date() - 365, "%Y"),
                              quarter = 1, minimal = FALSE,
                              key = Sys.getenv("HUD_KEY"),
                              to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "county"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   county, year, quarter, key)
  county <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(county) != 5)) stop("\nCounty inputs are not all of length 5",
                                    call. = FALSE)

  all_queries <- expand.grid(query = county, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 7,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract,
  # 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$zip)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_cbsa_zip(cbsa = 10140, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cbsa_zip(cbsa = '10140', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cbsa_zip <- function(cbsa, year = format(Sys.Date() - 365, "%Y"),
                            quarter = 1, minimal = FALSE,
                            key = Sys.getenv("HUD_KEY"),
                            to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }


  primary_geoid <- "cbsa"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   cbsa, year, quarter, key)
  cbsa <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(cbsa) != 5)) stop("\nCbsa inputs are not all of length 5",
                                  call. = FALSE)

  all_queries <- expand.grid(query = cbsa, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 8,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract,
  # 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$zip)
}



#' @name hud_cw_cbsadiv_zip
#' @title hud_cw_cbsadiv_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for cbsadiv to zip.
#'   (Available 4th Quarter 2017 onwards)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_cbsadiv_zip(cbsadiv = 10380, year = c('2017'), quarter = c('4'))
#'
#' hud_cw_cbsadiv_zip(cbsadiv = '10380', year = c('2017'), quarter = c('4'),
#'    minimal = TRUE)
#' }
hud_cw_cbsadiv_zip <- function(cbsadiv, year = format(Sys.Date() - 365, "%Y"),
                               quarter = 1, minimal = FALSE,
                               key = Sys.getenv("HUD_KEY"),
                               to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "cbsadiv"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   cbsadiv, year, quarter, key)
  cbsadiv <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(cbsadiv) != 5)) stop("\nCbsadiv inputs are not all of length 5",
                                     call. = FALSE)

  all_queries <- expand.grid(query = cbsadiv, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 9,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$zip)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_cd_zip(cd = 2202, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cd_zip(cd = '2202', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cd_zip <- function(cd, year = format(Sys.Date() - 365, "%Y"),
                          quarter = 1, minimal = FALSE,
                          key = Sys.getenv("HUD_KEY"),
                          to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "cd"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   cd, year, quarter, key)
  cd <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(cd) != 4)) stop("\nCd inputs are not all of length 4",
                                call. = FALSE)

  all_queries <- expand.grid(query = cd, year = year, quarter = quarter,
                             stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 10,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$zip)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_zip_countysub(zip = 35213, year = c('2019'), quarter = c('2'))
#'
#' hud_cw_zip_countysub(zip = '35213', year = c('2019'), quarter = c('2'),
#'    minimal = TRUE)
#' }
hud_cw_zip_countysub <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                                 quarter = 1, minimal = FALSE,
                                 key = Sys.getenv("HUD_KEY"),
                                 to_tibble) {

  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                    call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "zip"
  secondary_geoid <- "countysub"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(zip) != 5)) stop("\nZipcode inputs are not all of length 5",
                                 call. = FALSE)

  all_queries <- expand.grid(query = zip, year = year, quarter = quarter,
                             stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 11,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$countysub)
}


#' @name hud_cw_countysub_zip
#' @title hud_cw_countysub_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for countysub to zip.
#'   (Available 2nd Quarter 2018 onwards)
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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @seealso
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a
#'   particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_cw_countysub_zip(countysub = '4606720300',
#'    year = c('2019', '2019', '2019'), quarter = c('4','4'))
#'
#' hud_cw_countysub_zip(countysub = '3706794068',
#'    year = c('2019', '2019', '2019'), quarter = c('4','4'))
#'
#' hud_cw_countysub_zip(countysub = '3711993268',
#'    year = c('2019', '2019', '2019'), quarter = c('4','4'))
#'
#' hud_cw_countysub_zip(countysub = '3910383426',
#'    year = c('2019'), quarter = c('1'), minimal = TRUE)
#' }
hud_cw_countysub_zip <- function(countysub,
                                 year = format(Sys.Date() - 365, "%Y"),
                                 quarter = 1, minimal = FALSE,
                                 key = Sys.getenv("HUD_KEY"),
                                 to_tibble) {
  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  primary_geoid <- "countysub"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   countysub, year, quarter, key)
  countysub <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if (any(nchar(countysub) != 10)) stop("\nCountysub inputs are not all of length 10",
                                        call. = FALSE)

  all_queries <- expand.grid(query = countysub, year = year,
                             quarter = quarter, stringsAsFactors = FALSE)
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=", 12,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to
  # zip_county... The functions in this file should follow that order.
  if (!minimal) {
    return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble))
  }
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, primary_geoid,
                           secondary_geoid, key, to_tibble)$zip)
}
