#' @name hud_cw_zip_tract
#' @title Crosswalk Zip to Tract
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   zip to tract.
#' @param zip A character or numeric vector: 5 digit
#'   (United States Postal Service) USPS zipcode
#'   of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   zip to tract for all combinations of "zip", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_zip_tract(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_tract(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_tract <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                             quarter = 1, minimal = FALSE,
                             key = Sys.getenv("HUD_KEY"),
                             to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  res <- NULL
  primary_geoid <- "zip"
  secondary_geoid <- "tract"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid, zip,
                                   year, quarter, key)

  zip <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)$tract
  }

  res
}



#' @name hud_cw_zip_county
#' @title Crosswalk Zip to County
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   zip to county.
#' @param zip A character or numeric vector: 5 digit
#'   (United States Postal Service) USPS zipcode
#'   of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   zip to county for all combinations of "zip", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_zip_county(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_county(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_county <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                              quarter = 1, minimal = FALSE,
                              key = Sys.getenv("HUD_KEY"),
                              to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  res <- NULL
  primary_geoid <- "zip"
  secondary_geoid <- "county"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble)
  } else {
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                        all_queries$quarter, primary_geoid,
                        secondary_geoid, key, to_tibble)$county
  }

  res
}



#' @name hud_cw_zip_cbsa
#' @title  Crosswalk Zip to CBSA
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   zip to cbsa.
#' @param zip A character or numeric vector: 5 digit
#'   (United States Postal Service) USPS zipcode
#'   of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   zip to cbsa for all combinations of "zip", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_zip_cbsa(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cbsa(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cbsa <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                            quarter = 1, minimal = FALSE,
                            key = Sys.getenv("HUD_KEY"),
                            to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  res <- NULL
  primary_geoid <- "zip"
  secondary_geoid <- "cbsa"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                                        all_queries$quarter, primary_geoid,
                                        secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$cbsa
  }

  res
}



#' @name hud_cw_zip_cbsadiv
#' @title  Crosswalk Zip to CBSAdiv
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   zip to cbsadiv.
#' @param zip A character or numeric vector: 5 digit
#'   (United States Postal Service) USPS zipcode
#'   of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   zip to cbsadiv for all combinations of "zip", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#'
#' hud_cw_zip_cbsadiv(zip = '35213', year = c('2017'), quarter = c('4'))
#'
#' hud_cw_zip_cbsadiv(zip = '35213', year = c('2017'), quarter = c('4'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cbsadiv <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                               quarter = 1, minimal = FALSE,
                               key = Sys.getenv("HUD_KEY"),
                               to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()
  res <- NULL

  primary_geoid <- "zip"
  secondary_geoid <- "cbsadiv"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$cbsadiv
  }

  res
}



#' @name hud_cw_zip_cd
#' @title  Crosswalk Zip to CD
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   zip to cd.
#' @param zip A character or numeric vector: 5 digit
#'   (United States Postal Service) USPS zipcode
#'   of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   zip to cd for all combinations of "zip", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_zip_cd(zip = '35213', year = c('2010'), quarter = c('1'))
#'
#' hud_cw_zip_cd(zip = '35213', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_zip_cd <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                          quarter = 1, minimal = FALSE,
                          key = Sys.getenv("HUD_KEY"),
                          to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "zip"
  secondary_geoid <- "cd"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$cd
  }

  res
}



#' @name hud_cw_tract_zip
#' @title Crosswalk Tract to Zip
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   tract to zip.
#' @param tract A character or numeric vector: 11 digit tract code.
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   tract to zip for all combinations of "tract", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_tract_zip(tract = 48201223100, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_tract_zip(tract = '48201223100', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_tract_zip <- function(tract, year = format(Sys.Date() - 365, "%Y"),
                             quarter = 1, minimal = FALSE,
                             key = Sys.getenv("HUD_KEY"),
                             to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  res <- NULL
  primary_geoid <- "tract"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   tract, year, quarter, key)
  tract <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$zip
  }

  res
}



#' @name hud_cw_county_zip
#' @title Crosswalk County to Zip
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   county to zip.
#' @param county A character or numeric vector: 5 digit county code.
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   county to zip for all combinations of "county", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_county_zip(county = 22031, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_county_zip(county = '22031', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_county_zip <- function(county, year = format(Sys.Date() - 365, "%Y"),
                              quarter = 1, minimal = FALSE,
                              key = Sys.getenv("HUD_KEY"),
                              to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "county"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   county, year, quarter, key)
  county <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$zip
  }

  res
}



#' @name hud_cw_cbsa_zip
#' @title Crosswalk CBSA to Zip
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   cbsa to zip.
#' @param cbsa A character or numeric vector: 5 digit core based
#'   statistical area code.
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   cbsa to zip for all combinations of "cbsa", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_cbsa_zip(cbsa = 10140, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cbsa_zip(cbsa = '10140', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cbsa_zip <- function(cbsa, year = format(Sys.Date() - 365, "%Y"),
                            quarter = 1, minimal = FALSE,
                            key = Sys.getenv("HUD_KEY"),
                            to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "cbsa"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   cbsa, year, quarter, key)
  cbsa <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$zip
  }

  res
}



#' @name hud_cw_cbsadiv_zip
#' @title Crosswalk CBSAdiv to Zip
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   cbsadiv to zip.
#' @param cbsadiv A character or numeric vector: 5 digit core based
#'   statistical area division code.
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   cbsadiv to zip for all combinations of "cbsadiv", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_cbsadiv_zip(cbsadiv = 10380, year = c('2017'), quarter = c('4'))
#'
#' hud_cw_cbsadiv_zip(cbsadiv = '10380', year = c('2017'), quarter = c('4'),
#'    minimal = TRUE)
#' }
hud_cw_cbsadiv_zip <- function(cbsadiv, year = format(Sys.Date() - 365, "%Y"),
                               quarter = 1,
                               minimal = FALSE,
                               key = Sys.getenv("HUD_KEY"),
                              to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "cbsadiv"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   cbsadiv, year, quarter, key)
  cbsadiv <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$zip
  }

  res
}



#' @name hud_cw_cd_zip
#' @title Crosswalk CD to Zip
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   cd to zip.
#' @param cd A character or numeric vector: 4 digit congressional district code.
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   cd to zip for all combinations of "cd", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_cd_zip(cd = 2202, year = c('2017'), quarter = c('1'))
#'
#' hud_cw_cd_zip(cd = '2202', year = c('2010'), quarter = c('1'),
#'    minimal = TRUE)
#' }
hud_cw_cd_zip <- function(cd, year = format(Sys.Date() - 365, "%Y"),
                          quarter = 1, minimal = FALSE,
                          key = Sys.getenv("HUD_KEY"),
                          to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "cd"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   cd, year, quarter, key)
  cd <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-  cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$zip
  }

  res
}



#' @name hud_cw_zip_countysub
#' @title Crosswalk Zip to Countysub
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   zip to countysub.
#' @param zip A character or numeric vector: 5 digit
#'   (United States Postal Service) USPS zipcode
#'   of the data to retrieve. E.g. 22031 for type
#'   1 to 5 and 11 .
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   zip to countysub for all combinations of "zip", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_cw_zip_countysub(zip = 35213, year = c('2019'), quarter = c('2'))
#'
#' hud_cw_zip_countysub(zip = '35213', year = c('2019'), quarter = c('2'),
#'    minimal = TRUE)
#' }
hud_cw_zip_countysub <- function(zip, year = format(Sys.Date() - 365, "%Y"),
                                 quarter = 1, minimal = FALSE,
                                 key = Sys.getenv("HUD_KEY"),
                            to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "zip"
  secondary_geoid <- "countysub"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   zip, year, quarter, key)
  zip <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$countysub
  }

  res
}


#' @name hud_cw_countysub_zip
#' @title Crosswalk Countysub to Zip
#' @description This function queries the USPS Crosswalks API provided by US
#'   Department of Housing and Urban Development (HUD USER).
#'   This returns the crosswalk for
#'   countysub to zip.
#' @param countysub A character or numeric vector:
#'   10 digit county sub division code.
#' @param tract A character or numeric vector: 11 digit tract code.
#' @param year A character or numeric vector: gets the year that this data
#'   was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter A character or numeric vector: gets the quarter of the year
#'   that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal A logical: return just the crosswalked geoids if TRUE.
#'   Otherwise, return
#'   all fields. This does not remove duplicates.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
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
#' @returns This function returns a dataframe containing crosswalk data for
#'   countysub to zip for all combinations of "countysub", "year", and "quarter"
#'   inputs.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
#' @examples
#' \dontrun{
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
                             to_tibble = getOption("rhud_use_tibble", FALSE)) {
  is_internet_available()

  res <- NULL
  primary_geoid <- "countysub"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(primary_geoid, secondary_geoid,
                                   countysub, year, quarter, key)
  countysub <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key

  if (any(nchar(countysub) != 10)) {
    stop("\nCountysub inputs are not all of length 10",
         call. = FALSE)
  }

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
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                             all_queries$quarter, primary_geoid,
                             secondary_geoid, key, to_tibble)
  } else {
    res <-   cw_do_query_calls(urls, all_queries$query, all_queries$year,
                               all_queries$quarter, primary_geoid,
                               secondary_geoid, key, to_tibble)$zip
  }

  res
}
