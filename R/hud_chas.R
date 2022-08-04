#' @name hud_chas_nation
#' @title Comprehensive Housing and Affordability Strategy for the US.
#' @param year A character vector with the years to query for.
#'  * year = "2014-2018"
#'  * year = "2013-2017"
#'  * year = "2012-2016"
#'  * year = "2011-2015"
#'  * year = "2010-2014"
#'  * year = "2009-2013"
#'  * year = "2008-2012"
#'  * year = "2007-2011"
#'  * year = "2006-2010"
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @description Returns Comprehensive Housing and Affordability Strategy (CHAS)
#'   data for the entire nation.
#' @returns Returns a data frame with
#'   Comprehensive Housing and Affordability Strategy (CHAS) data for the
#'   entire nation for all
#'   observations supplied in "year" input.
#' @seealso
#' * [rhud::hud_chas_nation()]
#' * [rhud::hud_chas_state()]
#' * [rhud::hud_chas_county()]
#' * [rhud::hud_chas_state_mcd()]
#' * [rhud::hud_chas_state_place()]
#' * [rhud::hud_chas()]
#' @export
#' @examples
#' \dontrun{
#'
#' hud_chas_nation()
#'
#' }
hud_chas_nation <- function(year = c("2014-2018"),
                            key = Sys.getenv("HUD_KEY"),
                            to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  args <- chas_input_check_cleansing(year = year, key = key)
  year <- args$year
  key <- args$key

  urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "1",
               "&year=", year, sep = "")

  chas_do_query_calls(urls, key = key, to_tibble)
}

#' @name hud_chas_state
#' @title Comprehensive Housing and Affordability Strategy for US States.
#' @description Returns Comprehensive Housing and Affordability Strategy
#'   (CHAS) data for state(s).
#' @param state A character or numeric vector: the state(s) to query for.
#'   Can supply as abbreviation, whole name,
#'   or as the fips code.
#' @param year A character vector: the years to query for.
#'  * year = "2014-2018"
#'  * year = "2013-2017"
#'  * year = "2012-2016"
#'  * year = "2011-2015"
#'  * year = "2010-2014"
#'  * year = "2009-2013"
#'  * year = "2008-2012"
#'  * year = "2007-2011"
#'  * year = "2006-2010"
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @returns Returns a data frame with
#'   Comprehensive Housing and Affordability Strategy (CHAS) data for states for
#'   all combinations of "state" and "year" inputs.
#' @seealso
#' * [rhud::hud_chas_nation()]
#' * [rhud::hud_chas_state()]
#' * [rhud::hud_chas_county()]
#' * [rhud::hud_chas_state_mcd()]
#' * [rhud::hud_chas_state_place()]
#' * [rhud::hud_chas()]
#' @export
#' @examples
#' \dontrun{
#' hud_chas_state("CA")
#' hud_chas_state("51")
#' }
hud_chas_state <- function(state, year = c("2014-2018"),
                           key = Sys.getenv("HUD_KEY"),
                           to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()

  args <- chas_input_check_cleansing(state, year, key)
  state <- args$query
  year <- args$year
  key <- args$key

  # Assume abbreviation or fips code if length of 2. Captitalize does not
  # affect numbers. Assume full state name if length more than 2
  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(state)

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(hud_nation_states_territories(
      key = Sys.getenv("HUD_KEY")))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("\nThere is no matching fips code for one of the inputted states.",
           call. = FALSE)
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][3]
  } else if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][3]
  } else if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][3]
  }

  fip_code <- unlist(fip_code)

  all_queries <- expand.grid(fip_code = fip_code, year = year,
                            stringsAsFactors = FALSE)

  urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "2",
               "&stateId=", all_queries$fip_code, "&year=", all_queries$year,
               sep = "")


  chas_do_query_calls(urls, key = key, to_tibble)
}

#' @name hud_chas_county
#' @title Comprehensive Housing and Affordability Strategy for the US Counties
#' @description Returns Comprehensive Housing and Affordability Strategy (CHAS)
#"   data for county(s).
#' @param county A character or numeric vector:
#'   the county(s) to query for. Must supply a geoid. 2 digit state fip
#'   + 3 digit county fip. hud_state_counties() will show an extra 99999 at the
#'   end. Just remove that.
#' @param year The years to query for.
#'  * year = "2014-2018"
#'  * year = "2013-2017"
#'  * year = "2012-2016"
#'  * year = "2011-2015"
#'  * year = "2010-2014"
#'  * year = "2009-2013"
#'  * year = "2008-2012"
#'  * year = "2007-2011"
#'  * year = "2006-2010"
#' @param key A character vector of length 1: the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @returns Returns a data frame with
#'   Comprehensive Housing and Affordability Strategy (CHAS) data for
#'   counties for all combinations of "county" and "year" inputs.
#' @seealso
#' * [rhud::hud_chas_nation()]
#' * [rhud::hud_chas_state()]
#' * [rhud::hud_chas_county()]
#' * [rhud::hud_chas_state_mcd()]
#' * [rhud::hud_chas_state_place()]
#' * [rhud::hud_chas()]
#' @export
#' @examples
#' \dontrun{
#'
#' hud_chas_county(county = c("06105", "06113"))
#'
#' }
hud_chas_county <- function(county, year = c("2014-2018"),
                            key = Sys.getenv("HUD_KEY"),
                            to_tibble = getOption("rhud_use_tibble",
                                                  FALSE)) {

  is_internet_available()

  args <- chas_input_check_cleansing(county, year, key)
  county <- args$query
  year <- args$year
  key <- args$key

  # Try to fix the counties that have lost leading zeros in them...
  county <- geoid_add_leading_zeros(geoid_type = "county", county)

  # The first 2 are state fip. Last 3 are county fip.
  state_fip <- as.integer(substr(county, 1, 2))
  county_fip <- as.integer(substr(county, 3, 5))

  check_county <- paste(county, "99999", sep = "")

  # Grab all possible states if the package doesn't have a state dataset to
  # query from.
  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(
      hud_nation_states_territories(key = Sys.getenv("HUD_KEY")))
  }

  # Check if first two numbers of county code inputted is a valid state.
  if (!all(as.character(state_fip) %in% pkg.env$state$state_num)) {
    stop(paste("\nThere is no matching state FIPs code for",
               "one of the inputted fips",
               sep = ""), call. = FALSE)
  }

  if (!all(as.character(check_county) %in%
           hud_state_counties(state_fip)$fips_code)) {
    stop(paste("\nThere is no matching county FIPs code for",
               "one of the inputted counties",
               sep = ""), call. = FALSE)
  }

  all_queries <- expand.grid(state_fip = state_fip, year = year,
                             stringsAsFactors = FALSE)
  all_queries$county_fip <- county_fip
  urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "3",
               "&stateId=", all_queries$state_fip,
               "&entityId=", all_queries$county_fip,
               "&year=", all_queries$year,
               sep = "")

  chas_do_query_calls(urls, key, to_tibble)
}


#' @name hud_chas_state_mcd
#' @title Comprehensive Housing and Affordability Strategy for
#'   US Minor Civil Divisions
#' @description Returns Comprehensive Housing and Affordability Strategy (CHAS)
#'   data for all mcd in a state.
#' @param state A character or numeric vector:
#'   The states to query for which can be specified using
#'   the state name, abbreviation, or fips code.
#' @param year A character vector: the years to query for.
#'  * year = "2014-2018"
#'  * year = "2013-2017"
#'  * year = "2012-2016"
#'  * year = "2011-2015"
#'  * year = "2010-2014"
#'  * year = "2009-2013"
#'  * year = "2008-2012"
#'  * year = "2007-2011"
#'  * year = "2006-2010"
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical : if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @returns Returns a dataframe with
#'   Comprehensive Housing and Affordability Strategy (CHAS) data for mcds
#'   inside states for all
#'   combinations of "state" and "year" inputs.
#' @seealso
#' * [rhud::hud_chas_nation()]
#' * [rhud::hud_chas_state()]
#' * [rhud::hud_chas_county()]
#' * [rhud::hud_chas_state_mcd()]
#' * [rhud::hud_chas_state_place()]
#' * [rhud::hud_chas()]
#' @export
#' @examples
#' \dontrun{
#'
#' #hud_chas_state_mcd("VA", year = c("2014-2018","2013-2017"))
#'
#' }
hud_chas_state_mcd <- function(state, year = c("2014-2018"),
                               key = Sys.getenv("HUD_KEY"),
                               to_tibble = getOption("rhud_use_tibble",
                                                     FALSE)) {

  is_internet_available()

  args <- chas_input_check_cleansing(state, year = year, key = key)
  year <- args$year
  key <- args$key


  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(state)

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(
      hud_nation_states_territories(key = Sys.getenv("HUD_KEY")))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("\nThere is no matching fips code for one of the inputted states.",
           call. = FALSE)
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][3]
  } else if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][3]
  } else if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                               as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][3]
  }

  fip_code <- unlist(fip_code)

  # Get all MCDs in these states...
  all_mcd_in_states <- suppressMessages(
    hud_state_minor_civil_divisions(fip_code))

  all_queries <- data.frame()

  for (i in year) {
    all_mcd_in_states$year <- year
    all_queries <- rbind(all_queries, all_mcd_in_states)
  }

  # Query for all of them
  urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "4",
               "&stateId=", all_queries$statecode,
               "&entityId=", all_queries$entityId,
               "&year=", all_queries$year,  sep = "")

  chas_do_query_calls(urls, key, to_tibble)
}



#' @name hud_chas_state_place
#' @title Comprehensive Housing and Affordability Strategy for US Places
#' @description Returns Comprehensive Housing and Affordability Strategy (CHAS)
#'   for all places in a state.
#' @param state A character or numeric vector:
#'   The states to query for which can be specified using
#'   the state name, abbreviation, or fips code.
#' @param year A character vector: the years to query for.
#'  * year = "2014-2018"
#'  * year = "2013-2017"
#'  * year = "2012-2016"
#'  * year = "2011-2015"
#'  * year = "2010-2014"
#'  * year = "2009-2013"
#'  * year = "2008-2012"
#'  * year = "2007-2011"
#'  * year = "2006-2010"
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical : if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @returns Returns a dataframe with
#'   Comprehensive Housing and Affordability Strategy (CHAS) data for places
#'   inside states for all
#'   combinations of "state" and "year" inputs.
#' @seealso
#' * [rhud::hud_chas_nation()]
#' * [rhud::hud_chas_state()]
#' * [rhud::hud_chas_county()]
#' * [rhud::hud_chas_state_mcd()]
#' * [rhud::hud_chas_state_place()]
#' * [rhud::hud_chas()]
#' @export
#' @examples
#' \dontrun{
#'
#' #hud_chas_state_place("MD", year = c("2014-2018","2013-2017"))
#'
#' }
hud_chas_state_place <- function(state, year = c("2014-2018"),
                           key = Sys.getenv("HUD_KEY"),
                           to_tibble = getOption("rhud_use_tibble",
                                                 FALSE)) {

  is_internet_available()

  args <- chas_input_check_cleansing(state, year = year, key = key)
  year <- args$year
  key <- args$key

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(
      hud_nation_states_territories(key = Sys.getenv("HUD_KEY")))
  }

  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(state)

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("\nThere is no matching fips code for one of the inputted states.",
           call. = FALSE)
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][3]
  } else if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][3]
  } else if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][3]
  }

  fip_code <- unlist(fip_code)

  # Get all places in these states...
  all_places_in_states <- suppressMessages(hud_state_places(fip_code))
  all_queries <- data.frame()

  for (i in year) {
    all_places_in_states$year <- year
    all_queries <- rbind(all_queries, all_places_in_states)
  }

  urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "5",
               "&stateId=", all_queries$statecode,
               "&entityId=", all_queries$entityId,
               "&year=", all_queries$year,  sep = "")

  chas_do_query_calls(urls, key, to_tibble)
}
