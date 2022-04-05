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

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "1",
               "&year=", year,  sep="")
  return(chas_do_query_calls(URL, key = key))
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
  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  }

  for(i in seq_len(length(state))) {
    if(!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state),][3]
  } else if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][3]
  } else if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][3]
  }
  fip_code <- unlist(fip_code)

  allqueries <- expand.grid(fip_code = fip_code, year = year)

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "2",
               "&stateId=", allqueries$fip_code, "&year=", allqueries$year,
               sep="")
  return(chas_do_query_calls(URL, key = key))
}

#' @name hud_chas_county
#' @title hud_chas_county
#' @description Returns CHAS data for counties.
#' @param county The county to query for. Must supply a geoid. 2 digit state fip
#'   + 3 digit county fip. hud_counties() will show an extra 99999 at the end.
#'   Just remove that.
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
#' @returns Returns a dataframe with CHAS data for a particular county.
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
  state_fip <- as.integer(substr(county, 1,2))
  county_fip <- substr(county, 3,5)

  # Don't know what the 99999 means, but it seems like every fips code has this
  # tacked onto the end within hud_counties() function call.
  check_county <- paste(county, "99999", sep = "")

  # Grab all possible states if the package doesn't have a state dataset to
  # query from.
  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  }

  # Check if first two numbers of county code inputted is a valid state.
  if(!all(as.character(state_fip) %in% pkg.env$state$state_num)) {
    stop(paste("There is no matching state FIPs code for",
               "one of the inputted fips",
               sep = ""))
  }

  if(!all(as.character(check_county) %in% hud_counties(state_fip)$fips_code)) {
    stop(paste("There is no matching county FIPs code for",
               "one of the inputted counties",
               sep = ""))
  }

  allqueries <- expand.grid(state_fip = state_fip, year = year)
  allqueries$county_fip <- county_fip
  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "3",
               "&stateId=", allqueries$state_fip,
               "&entityId=", allqueries$county_fip,
               "&year=", allqueries$year,
               sep="")

  res <- chas_do_query_calls(URL, key)

  return(res)
}

#' @name hud_chas_mcd
#' @title hud_chas_mcd
#' @description Returns CHAS data for an mcd.
#' @param state The state name, abbreviation, or fips code.
#' @param mcd The mcd to query for. Must supply as 5 digit geoid.
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
#' @returns Returns a dataframe with CHAS data for a particular minor civil
#'   division(mcd).
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_mcd("VA", "94135", year = c("2014-2018","2013-2017"))
#'
#' hud_chas_mcd(c("MD", "VA"), c("90812", "94135"))
#'
#' hud_chas_mcd("California", "93140")
#' }
hud_chas_mcd <- function(state, mcd, year = c("2014-2018"),
                         key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(mcd, year, key)
  mcd <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  if(length(state) != length(mcd)) {
    stop(paste("Length of states specified should",
               "be equal to length of MCDs specified.",
               sep = ""))
  }

  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  }

  for(i in seq_len(length(state))) {
    if(!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state),][3]
  } else if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][3]
  } else if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                               as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][3]
  }

  fip_code <- unlist(fip_code)

  # We have to assume both
  allqueries <- expand.grid(fip_code = fip_code, year = year)
  allqueries$mcd <- mcd

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "4",
               "&stateId=", allqueries$fip_code,
               "&entityId=", allqueries$mcd,
               "&year=", allqueries$year,  sep="")

  res <- chas_do_query_calls(URL, key)

  return(res)
}

#' @name hud_chas_place
#' @title hud_chas_place
#' @description Returns CHAS data for place.
#' @param state The state name, abbreviation, or fips code. Make sure if state
#'   fips is 1 digit number, do not include leading 0.
#' @param place The place(Usually just cities) to query for. Must supply as a 5
#'   digit geoid.
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
#' @returns Returns a dataframe with CHAS data for a particular place (Usually
#'   just cities).
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_chas_place("MD", 53625, year = c("2014-2018","2013-2017"))
#'
#' hud_chas_place("6", "17727")
#'
#' hud_chas_place(51, 48996)
#' }
hud_chas_place <- function(state, place, year = c("2014-2018"),
                           key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(place, year, key)
  place <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  if(length(state) != length(place)) {
    stop(paste("Length of states specified should",
               "be equal to length of places specified.",
               sep = ""))
  }

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  }

  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  }

  for(i in seq_len(length(state))) {
    if(!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state),][3]
  } else if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][3]
  } else if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][3]
  }

  fip_code <- unlist(fip_code)

  allqueries <- expand.grid(fip_code = fip_code, year = year)
  allqueries$place <- place
  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "5",
               "&stateId=", allqueries$fip_code,
               "&entityId=", allqueries$place,
               "&year=", allqueries$year,  sep="")

  res <- chas_do_query_calls(URL, key)

  return(res)
}
