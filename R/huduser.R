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
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
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
#' library(rhud)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
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
#'    quarter = c("1", "2", "3"))
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
hud_cw <- function(type, query, year = format(Sys.Date() - 365, "%Y"),
                   quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY"),
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

  args <- cw_input_check_cleansing(query, year, quarter, key)

  query <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  alltypes <- c("zip-tract", "zip-county", "zip-cbsa",
                "zip-cbsadiv", "zip-cd", "tract-zip",
                "county-zip", "cbsa-zip", "cbsadiv-zip",
                "cd-zip", "zip-countysub", "countysub-zip")

  if (length(type) != 1) stop("\nOnly one crosswalk type can be specified at a time",
                              call. = FALSE)

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

  if (FALSE %in% numbers_only(type)) {
    stop("\nType input must be a single number or the name of crosswalk file.",
         call. = FALSE)
  }

  ifelse(as.integer(type) < 1 || as.integer(type) > 12,
         stop("\nThe type input is not in the range of 1-12", call. = FALSE), "")

  lhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][1]
  rhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][2]

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
  urls <- paste("https://www.huduser.gov/hudapi/public/usps?type=",
               all_queries$type,
               "&query=", all_queries$query,
               "&year=", all_queries$year,
               "&quarter=", all_queries$quarter,
               sep = "")

  if (!minimal) return(cw_do_query_calls(urls, all_queries$query,
                                         all_queries$year,
                           all_queries$quarter, lhgeoid, rhgeoid, key, to_tibble))
  return(cw_do_query_calls(urls, all_queries$query, all_queries$year,
                           all_queries$quarter, lhgeoid, rhgeoid, key, to_tibble)[[1]])
}


#' @name hud_fmr
#' @title hud_fmr
#' @description This function queries the Fair Markets Rent API provided by
#'   US Department of Housing and Urban Development. Querying a
#'   state provides a list containing two dataframes: one with fmr at a county
#'   level resolution and the other fmr at a metroarea level resolution.
#'   Querying a county or cbsa will provide data at a zip code resolution
#'   if geoids are classified as a small area.
#' @param query Can provide either a 5 digit FIPS code + 99999 at end,
#'   state abbreviation, or CBSA code.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Fair Markets Rent API
#' @export
#' @returns This function returns a dataframe containing fair markets rent data
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
#' library(rhud)
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
                    key = Sys.getenv("HUD_KEY"), to_tibble) {

  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  args <- fmr_il_input_check_cleansing(query, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]
  querytype <- args[[4]]

  # If state query, will provide data at metro and county level
  # Call helper functions...
  if (querytype == "state") {
      # Merge county level data with metroarea data.
      return(list(counties = hud_fmr_state_counties(query, year, key, to_tibble),
                  metroareas = hud_fmr_state_metroareas(query, year, key, to_tibble)))
  } else if (querytype == "cbsa") {
      # Returns zip level data.
      return(hud_fmr_metroarea_zip(query, year, key, to_tibble))
  } else if (querytype == "county") {
      # Returns zip level data.
      return(hud_fmr_county_zip(query, year, key, to_tibble))
  }
}




#' @name hud_il
#' @title hud_il
#' @description This function queries the Income Limits API provided by
#'   US Department of Housing and Urban Development.
#' @param query  Querying a
#'   state provides the IL measurement for that state level resolution. Querying
#'   Querying a county or cbsa will provide data at a county and
#'   cbsa resolution, respectively.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
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
#' library(rhud)
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
                   key = Sys.getenv("HUD_KEY"), to_tibble) {

  if (!curl::has_internet()) stop("\nYou currently do not have internet access.",
                                  call. = FALSE)

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble = getOption("rhud_use_tibble")
    message(paste("Outputted in tibble format",
                  "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble = FALSE
  }

  args <- fmr_il_input_check_cleansing(query, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]
  querytype <- args[[4]]

  error_urls <- c()

  all_queries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  list_res <- c()
  for (i in seq_len(nrow(all_queries))) {
    # Build the urls for querying the data.
    urls <- paste("https://www.huduser.gov/hudapi/public/il/",
                 if (querytype == "state") "statedata/" else "data/",
                 all_queries$query[i], "?year=", all_queries$year[i], sep = "")

    call <- try(GET(urls, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/rhud"), timeout(30)),
              silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, nrow(all_queries))

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {

      if (querytype == "state") {

        res <- as.data.frame(cont$data)

        res$statecode <- cont$data$statecode

        oth <- data.frame(query = all_queries$query[i],
                          year = all_queries$year[i],
                          median_income = cont$data$median_income,
                          stringsAsFactors = FALSE)
        res <- cbind(oth, res)

      } else if (querytype == "county") {
        res <- as.data.frame(cont$data)

        oth <- data.frame(query = all_queries$query[i],
                          stringsAsFactors = FALSE)
        res <- cbind(oth, res)

      } else {
        res <- as.data.frame(cont$data)

        oth <- data.frame(query = all_queries$query[i],
                          stringsAsFactors = FALSE)
        res <- cbind(oth, res)
      }

      list_res[[i]] <- res
    }
  }
  message("\n")

  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries: \n\n",
                  paste(paste("*", error_urls, sep = " "), collapse = "\n"),
                  "\n\nIt is possible that your key maybe invalid or ",
                  "there isn't any data for these parameters, ",
                  "If you think this is wrong please ",
                  "report it at https://github.com/etam4260/rhud/issues.",
                  sep = ""), call. = FALSE)
  }

  if (length(list_res) != 0) {
    res <- do.call(rbind, list_res)
    if (to_tibble == FALSE) {
      return(res)
    } else {
      return(as_tibble(res))
    }
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
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Comprehensive Housing Affordability Strategy (CHAS) API
#' @export
#' @returns This function returns a dataframe containing CHAS data for a
#'   particular state. For more details about these measurements, go to
#'   https://www.huduser.gov/portal/dataset/chas-api.html
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
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

  if (!is.vector(type) || !is.vector(year) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
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
  state_id <- paste(trimws(as.character(state_id), which = "both"))
  entity_id <- paste(trimws(as.character(entity_id), which = "both"))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

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
    urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "1",
                 "&year=", year,  sep = "")
  }

  if (type == "2") {
    if (is.null(state_id)) stop("You need to specify a stateId for this type.")
    urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type,
                 "&stateId=", state_id, "&year=", year,  sep = "")
    if (!is.vector(state_id)) {
      stop(paste("\nMake sure all inputs are of type vector. ",
                 "Check types with typeof([variable]). ",
                 "If list try unlist([variable]); ",
                 "if matrix try as.vector([variable])", sep = ""),
           call. = FALSE)
    }

    all_queries <- expand.grid(fip_code = state_id, year = year,
                              stringsAsFactors = FALSE)
    urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "2",
                 "&stateId=", all_queries$fip_code,
                 "&year=", all_queries$year,  sep = "")
  }

  if (type == "3" || type == "4" || type == "5") {
    if (is.null(state_id) || is.null(entity_id)) stop("You need to specify a
                                                 stateId and entityId
                                                 for this type.")
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

    urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type,
                 "&stateId=", all_queries$state_fip,
                 "&entityId=", all_queries$entity_id,
                 "&year=", all_queries$year,
                 sep = "")
  }
  return(chas_do_query_calls(urls, key = key, to_tibble))
}
