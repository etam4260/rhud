#' @import httr
#' @import tibble

# An R interface for accessing HUD USER
# (US Department of Housing and Urban Development) APIs.

# HUD USER has four main APIs. These functions declared below follow the
# structure of the API very closely, but provide the additional ability to query
# for multiple geoids as well as multiple years inputs.

# Crosswalk
# Fair Markets Rent
# Income Limits
# Comprehensive and Housing Affordability Strategy

#' @name hud_cw
#' @title Omni-Query for HUD Crosswalk Files API
#' @description This function queries the USPS Crosswalks API provided by
#'   US Department of Housing and Urban Development (HUD USER).
#' @param type Must be a number between 1 and 12 depending on the USPS Crosswalk
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
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing crosswalk data for
#'   one geoid to another for all combinations of "query", "year", and "quarter"
#'   inputs where query depends on type specified.
#'
#'   These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
#'
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
#' @examples
#' \dontrun{
#' hud_cw(type = 1, query = "35213", year = c("2010", "2011"),
#'    quarter = c("1"))
#'
#' hud_cw(type = "2", query = "35213", year = c("2016", "2020"),
#'    quarter = c("2"))
#'
#' hud_cw(type = 3, query = 35213, year = c("2012", "2011"),
#'    quarter = c("3"))
#'
#' hud_cw(type = 4, query = "22031", year = c("2017", "2019"),
#'    quarter = c("4"))
#'
#' hud_cw(type = "5", query = "35213", year = c(2011, "2012"),
#'    quarter = c("1", "2"))
#'
#' hud_cw(type = 6, query = "48201223100", year = c("2017", "2010"),
#'    quarter = c("1", "2", "3"))
#'
#' hud_cw(type = 7, query = "22031", year = c("2010", "2011"),
#'    quarter = c("1", "2", "3"))
#'
#' hud_cw(type = 8, query = "10140", year = c("2010", "2011"),
#'    quarter = c("1", "2"))
#'
#' hud_cw(type = 9, query = "10380", year = c("2017"),
#'    quarter = c("4"))
#'
#' hud_cw(type = 10, query = "2202", year = c("2010", "2011"),
#'    quarter = c("4", "3"))
#'
#' hud_cw(type = 11, query = "35213", year = c("2019", "2020"),
#'    quarter = c("2", "3"))
#'
#' hud_cw(type = 12, query = "4606720300 ", year = c("2019", "2019", "2019"),
#'    quarter = c("4", "4"))
#' }
hud_cw <- function(type, query,
                   year = format(Sys.Date() - 365, "%Y"),
                   quarter = 1,
                   minimal = FALSE,
                   key = Sys.getenv("HUD_KEY"),
                   to_tibble = getOption("rhud_use_tibble", FALSE)) {
  res <- NULL
  is_internet_available()

  alltypes <- c("zip-tract", "zip-county", "zip-cbsa",
                "zip-cbsadiv", "zip-cd", "tract-zip",
                "county-zip", "cbsa-zip", "cbsadiv-zip",
                "cd-zip", "zip-countysub", "countysub-zip")

  if (length(type) != 1) {
    stop("\nOnly one crosswalk type can be specified at a time",
                              call. = FALSE)
  }

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

  if (FALSE %in% digits_only(type)) {
    stop("\nType input must be a single number or the name of crosswalk file.",
         call. = FALSE)
  }

  ifelse(as.integer(type) < 1 || as.integer(type) > 12,
         stop("\nThe type input is not in the range of 1-12",
              call. = FALSE), "")

  lhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][1]
  rhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][2]

  args <- cw_input_check_cleansing(lhgeoid,
                                   rhgeoid,
                                   query,
                                   year,
                                   quarter,
                                   key)

  query <- args$query
  year <- args$year
  quarter <- args$quarter
  key <- args$key


  # Need to make sure query is a zip code of 5 digits.
  if (as.integer(type) >= 1 && as.integer(type) <= 5 ||
      as.integer(type) == 11) {
    if (any(nchar(query) != 5)) stop("\nQuery input is not of length 5",
                                     call. = FALSE)
    # Need to make sure query is a fips code of 5 digits.
  } else if (as.integer(type) == 7) {
    if (any(nchar(query) != 5)) stop("\nQuery input is not of length 5",
                                     call. = FALSE)
    # Need to make sure query is a fips code
    # with census tract attached onto it 11 digits.
  } else if (as.integer(type) == 6) {
    if (any(nchar(query) != 11)) stop("\nQuery input is not of length 11",
                                      call. = FALSE)
    # Need to make sure query is CBSA code for
    # micropolitan/metropolitan areas: 5 digits.
  } else if (as.integer(type) == 8) {
    if (any(nchar(query) != 5)) stop("\nQuery input is not of length 5",
                                     call. = FALSE)
    # Need to make sure query is CBSA division code for
    # metropolitan areas: 5 digits.
  } else if (as.integer(type) == 9) {
    if (any(nchar(query) != 5)) stop("\nQuery input is not of length 5",
                                     call. = FALSE)
    # Need to make sure query is 4 digit GEOID for
    # congressional districts: 4 digits.
  } else if (as.integer(type) == 10) {
    if (any(nchar(query) != 4)) stop("\nQuery input is not of length 4",
                                     call. = FALSE)
    # Need to make sure query is 10 digits for county subdistrict
  } else if (as.integer(type) == 12) {
    if (any(nchar(query) != 10)) stop("\nQuery input is not of length 10",
                                      call. = FALSE)
  }

  all_queries <- expand.grid(query = query, year = year, quarter = quarter,
                            stringsAsFactors = FALSE)

  all_queries$type <- type[[1]]

  urls <- paste(get_hud_host_name(),
               "usps",
               "?type=",
               all_queries$type,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")


  if (!minimal) {
    res <- cw_do_query_calls(urls, all_queries$query,
                            all_queries$year, all_queries$quarter,
                            lhgeoid, rhgeoid, key,
                            to_tibble)
  } else {
    res <- cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, lhgeoid, rhgeoid, key,
                           to_tibble)[[1]]
  }

  res
}


#' @name hud_fmr
#' @title Omni-Query for US Fair Markets Rent API
#' @description This function queries the Fair Markets Rent API provided by
#'   US Department of Housing and Urban Development (HUD USER). Querying a
#'   state provides a list containing two datasets: one with FMR at a county
#'   level resolution and the other FMR at a metroarea level resolution.
#'   Querying a county or (core based statistical areas) cbsa will provide data
#'   at a zip code resolution
#'   if geographic identifiers are classified as a small area.
#' @param query Can provide either a 5 digit FIPS code + 99999 at end,
#'   state abbreviation, or cbsa (core based statistical area) code.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Fair Markets Rent API
#' @export
#' @returns This function returns a dataframe or tibble
#'   containing Fair Markets Rent (FMR) data
#'   for a particular county, metroarea, or state. For county and msa level
#'   data,
#'   these measurements include the county_name, counties_msa, town_name,
#'   metro_status, metro_name, smallarea_status, basicdata, Efficiency,
#'   One-Bedroom, Two-Bedroom, Three-Bedroom, Four-Bedroom, and year for zip
#'   code
#'   level data if it considered a small area. Otherwise, data will be returned
#'   at the level of the queried county or metroarea.
#'
#'   For state level data two dataframes will be returned. One for county level
#'   and the other for metroarea level data. These measurements will be the same
#'   as county/metroarea level data.
#'
#'   For more details about these measurements, go to
#'   https://www.huduser.gov/portal/dataset/fmr-api.html.
#'
#' @examples
#' \dontrun{
#'
#' hud_fmr("VA", year=c(2021))
#'
#' hud_fmr("5100199999", year=c(2021))
#'
#' hud_fmr("METRO47900M47900", year=c(2018))
#' }
hud_fmr <- function(query, year = format(Sys.Date() - 365, "%Y"),
                    key = Sys.getenv("HUD_KEY"),
                    to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  args <- fmr_il_input_check_cleansing(query, year, key)
  query <- args$query
  year <- args$year
  key <- args$key
  querytype <- args$querytype

  all_queries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  res <- fmr_do_query_call(all_queries, key, to_tibble, querytype)

  res
}




#' @name hud_il
#' @title Omni-Query for HUD US Income Limits API
#' @description This function queries the Income Limits API provided by
#'   US Department of Housing and Urban Development (HUD USER).
#' @param query  Querying a
#'   state provides the Income Limits measurement for that state level
#'   resolution.
#'   Querying a county or cbsa will provide data at a county and
#'   cbsa resolution, respectively.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Income Limits API
#' @seealso
#' * [rhud::hud_fmr_state_metroareas()]
#' * [rhud::hud_fmr_state_counties()]
#' * [rhud::hud_fmr_metroarea_zip()]
#' * [rhud::hud_fmr_county_zip()]
#' * [rhud::hud_fmr()]
#' @export
#' @returns This function returns a dataframe or tibble containing
#'   Income Limits data
#'   for a particular county, metroarea, or state. Data is returned at the level
#'   of the queried geoid.
#'
#'   These measurements include the county_name, counties_msa, town_name,
#'   metro_status, metro_name, year, median_income, very_low+, extremely_low+,
#'   and low+. For more details about these measurements, go to
#'   https://www.huduser.gov/portal/dataset/fmr-api.html
#'
#' @examples
#' \dontrun{
#'
#' hud_il("VA", year=c(2021))
#'
#' hud_il("5100199999", year=c(2021))
#'
#' hud_il("METRO47900M47900", year=c(2018))
#' }
hud_il <- function(query, year = format(Sys.Date() - 365, "%Y"),
                   key = Sys.getenv("HUD_KEY"),
                   to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  args <- fmr_il_input_check_cleansing(query, year, key)
  query <- args$query
  year <- args$year
  key <- args$key
  query_type <- args$query_type

  all_queries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  il_do_query_call(all_queries, key, to_tibble, query_type)
}


#' @name hud_chas
#' @title Omni-Query for HUD Comprehensive Housing and Affordability Strategy
#'   API
#' @description This function queries the
#'   Comprehensive Housing Affordability Strategy CHAS API provided by
#'   US Department of Housing and Urban Development (HUD USER). This closely
#'   follow their API implementation. For more intuitive querying, check the
#'   decomposed versions linked below.
#'   The ordering of items in stateid input must match that of the entityid
#'   input.
#' @param type Queries the data based off:
#'   1 - Nation
#'   2 - State
#'   3 - County
#'   4 - MCD
#'   5 - Place
#'
#'   Can either supply the number from 1-5 or the name. Either Nation, State,
#'   County, MCD, or Place
#'
#' @param state_id For types 2,3,4,5, you must provide a state_id.
#' @param entity_id For types 3,4,5, you must provide a fips code, placecode,
#'   or mcd code.
#' @param year Gets the year that this data was recorded. Defaults to 2014-2018.
#'   There are specific year ranges that are only accepted.
#'   * 2014-2018
#'   * 2013-2017
#'   * 2012-2016
#'   * 2011-2015
#'   * 2010-2014
#'   * 2009-2013
#'   * 2008-2012
#'   * 2007-2011
#'   * 2006-2010
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Comprehensive Housing Affordability Strategy (CHAS) API
#' @export
#' @seealso
#' * [rhud::hud_chas_nation()]
#' * [rhud::hud_chas_state()]
#' * [rhud::hud_chas_county()]
#' * [rhud::hud_chas_state_mcd()]
#' * [rhud::hud_chas_state_place()]
#' * [rhud::hud_chas()]
#' @returns This function returns a dataframe or tibble containing
#'   Comprehensive Housing
#'   and Affordability Strategy data for a particular state.
#'   For more details about
#'   these measurements, go to
#'   https://www.huduser.gov/portal/dataset/chas-api.html
#' @examples
#' \dontrun{
#' hud_chas(1)
#'
#' hud_chas('2', state_id = '56')
#'
#' hud_chas('3','51','199')
#'
#' hud_chas('4', '51', 94087)
#'
#' hud_chas('5', '51', 48996)
#' }
hud_chas <- function(type, state_id = NULL, entity_id = NULL,
                     year = c("2014-2018"),
                     key = Sys.getenv("HUD_KEY"),
                     to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  args <- chas_input_check_cleansing(year = year, key = key)
  year <- args$year
  key <- args$key


  if (!is.vector(type)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  # Allow user to specify the string too.
  type <- switch(tolower(type),
                "nation" = 1,
                "state" = 2,
                "county" = 3,
                "mcd" = 4,
                "minor civil division" = 4,
                "place" = 5,
                "city" = 5,
                type
  )

  # Removing leading and ending spaces and converting all integer inputs to
  # characters.
  type <- paste(trimws(as.character(type), which = "both"))
  state_id <- paste(trimws(as.character(state_id), which = "both"))
  entity_id <- paste(trimws(as.character(entity_id), which = "both"))

  # Check for if years are proper input
  if (!all(year %in% c("2014-2018", "2013-2017", "2012-2016", "2011-2015",
                      "2010-2014", "2009-2013", "2008-2012", "2007-2011",
                      "2006-2010"))) {
    stop("\nOne of the years does not fall in the correct range of values.",
         call. = FALSE)
  }

  ifelse(as.integer(type) < 1 || as.integer(type) > 5,
         stop("\nThe type input is not in the range of 1-5", call. = FALSE), "")


  if (type == "1") {
    urls <- paste(get_hud_host_name(),
                 "chas?type=",
                 "1",
                 "&year=",
                 year,
                 sep = "")
  }


  if (type == "2") {
    if (is.null(state_id)) stop("You need to specify a stateId for this type.",
                                call. = FALSE)

    if (!is.vector(state_id)) {
      stop(paste("\nMake sure all inputs are of type vector. ",
                 "Check types with typeof([variable]). ",
                 "If list try unlist([variable]); ",
                 "if matrix try as.vector([variable])", sep = ""),
           call. = FALSE)
    }

    all_queries <- expand.grid(fip_code = state_id, year = year,
                               stringsAsFactors = FALSE)

    urls <- paste(get_hud_host_name(),
                  "chas?type=",
                  "2",
                 "&stateId=",
                 all_queries$fip_code,
                 "&year=",
                 all_queries$year,
                 sep = "")
  }

  if (type == "3" || type == "4" || type == "5") {

    if (is.null(state_id) || is.null(entity_id)) stop("You need to specify a
                                                 stateId and entityId
                                                 for this type.", call. = FALSE)

    if (!is.vector(state_id) || !is.vector(entity_id)) {
      stop(paste("\nMake sure all inputs are of type vector. ",
                 "Check types with typeof([variable]). ",
                 "If list try unlist([variable]); ",
                 "if matrix try as.vector([variable])", sep = ""),
           call. = FALSE)
    }

    if (length(state_id) != length(entity_id)) {
      stop("\nYou need to make sure stateId and entityId are of same length.",
           call. = FALSE)
    }

    all_queries <- expand.grid(state_fip = state_id, year = year,
                              stringsAsFactors = FALSE)

    all_queries$entity_id <- entity_id

    urls <- paste(get_hud_host_name(),
                 "chas?type=",
                 type,
                 "&stateId=",
                 all_queries$state_fip,
                 "&entityId=",
                 all_queries$entity_id,
                 "&year=",
                 all_queries$year,
                 sep = "")
  }

  chas_do_query_calls(urls, key = key, to_tibble)
}
