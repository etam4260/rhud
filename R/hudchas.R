#' @name hud_chas_nation
#' @title hud_chas_nation
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
#' @param key The key obtain from HUD USER website.
#' @description Returns CHAS data for the entire nation.
#' @returns Returns a dataframe with CHAS data for the entire nation.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_nation()
#' }
hud_chas_nation <- function(year = c("2014-2018"),
                            key = Sys.getenv("HUD_KEY")) {

  args <- chas_input_check_cleansing(year = year, key = key)
  year <- args[[1]]
  key <- args[[2]]

  urls <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "1",
               "&year=", year, sep = "")
  return(chas_do_query_calls(urls, key = key))
}

#' @name hud_chas_state
#' @title hud_chas_state
#' @description Returns CHAS data for a state.
#' @param state The state to query for. Can supply as abbreviation, whole name,
#'   or as geoid.
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
#' @param key The key obtain from HUD USER website.
#' @returns Returns a dataframe with CHAS data for a particular state.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_state("CA")
#' hud_chas_state("New York")
#' hud_chas_state("51")
#' }
hud_chas_state <- function(state, year = c("2014-2018"),
                           key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(state, year, key)
  state <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  # Assume abbreviation or fips code if length of 2. Captitalize does not
  # affect numbers. Assume full state name if length more than 2
  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- hud_nation_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
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
  return(chas_do_query_calls(urls, key = key))
}

#' @name hud_chas_county
#' @title hud_chas_county
#' @description Returns CHAS data for counties.
#' @param county The county to query for. Must supply a geoid. 2 digit state fip
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
#' @param key The key obtain from HUD USER website.
#' @returns Returns a dataframe with CHAS data for counties.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_county(county = c(06105, 06113))
#' hud_chas_county(county = c("06105", "06113"))
#' }
hud_chas_county <- function(county, year = c("2014-2018"),
                            key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(county, year, key)
  county <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  # Try to fix the counties that have lost leading zeros in them...
  county <- add_leading_zeros(geoid_type = "county", county)

  # The first 2 are state fip. Last 3 are county fip.
  state_fip <- as.integer(substr(county, 1, 2))
  county_fip <- as.integer(substr(county, 3, 5))

  # Don't know what the 99999 means, but it seems like every fips code has this
  # tacked onto the end within hud_counties() function call.
  check_county <- paste(county, "99999", sep = "")

  # Grab all possible states if the package doesn't have a state dataset to
  # query from.
  if (is.null(pkg.env$state)) {
    pkg.env$state <- hud_nation_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  # Check if first two numbers of county code inputted is a valid state.
  if (!all(as.character(state_fip) %in% pkg.env$state$state_num)) {
    stop(paste("There is no matching state FIPs code for",
               "one of the inputted fips",
               sep = ""))
  }

  if (!all(as.character(check_county) %in%
           hud_state_counties(state_fip)$fips_code)) {
    stop(paste("There is no matching county FIPs code for",
               "one of the inputted counties",
               sep = ""))
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

  res <- chas_do_query_calls(urls, key)

  return(res)
}

#' @name hud_chas_state_mcd
#' @title hud_chas_state_mcd
#' @description Returns CHAS data for all mcds in a state.
#' @param state The state name, abbreviation, or fips code.
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
#' @param key The key obtain from HUD USER website.
#' @returns Returns a dataframe with CHAS data for mcds.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_state_mcd("VA", year = c("2014-2018","2013-2017"))
#'
#' }
hud_chas_state_mcd <- function(state, year = c("2014-2018"),
                         key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(year = year, key = key)
  year <- args[[1]]
  key <- args[[2]]


  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- hud_nation_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
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
  all_mcd_in_states <- hud_state_minor_civil_divisions(fip_code)
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

  res <- chas_do_query_calls(urls, key)

  return(res)
}

#' @name hud_chas_state_place
#' @title hud_chas_state_place
#' @description Returns CHAS for all places in a state.
#' @param state The state name, abbreviation, or fips code. Make sure if state
#'   fips is 1 digit number, do not include leading 0.
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
#' @param key The key obtain from HUD USER website.
#' @returns Returns a dataframe with CHAS data for places.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_state_place("MD", year = c("2014-2018","2013-2017"))
#'
#' }
hud_chas_state_place <- function(state, year = c("2014-2018"),
                           key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(year = year, key = key)
  year <- args[[1]]
  key <- args[[2]]

  if (is.null(pkg.env$state)) {
    pkg.env$state <- hud_nation_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- hud_nation_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
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
  all_places_in_states <- hud_state_places(fip_code)
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

  res <- chas_do_query_calls(urls, key)


  return(res)
}
