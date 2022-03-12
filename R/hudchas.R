#' @import httr

#' @name chas_input_check_cleansing
#' @title chas_input_check_cleansing
#' @description Helper function used to clean user inputted variables for all
#' CHAS functions.
#' @param query
#'   The inputted GEOID
#' @param year The years to query for.
#' @param key The key obtain from HUD USER website.
#' @returns The cleansed input arguments.
#' @noRd
chas_input_check_cleansing <- function(query, year, key) {
  if(!is.vector(year) || !is.vector(key)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")
  if(key == "") stop("Did you forget to set the key?")

  year <- unique(paste(trimws(as.character(year), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))
  if(!all(year %in% c("2014-2018","2013-2017","2012-2016","2011-2015","2010-2014",
                      "2009-2013","2008-2012","2007-2011","2006-2010"))) stop("Years specified are not allowed. Check the documentation.")

  if(!missing(query)) {
    if(!is.vector(query)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")

    query <- paste(trimws(as.character(query), which = "both"))
    return(list(query, year, key))
  }
  return(list(year, key))
}


#' @name chas_do_query_calls
#' @title chas_do_query_calls
#' @description Helper function for making the query calls to CHAS
#' API endpoint.
#' @param allqueries All the queries need to be made.
#' @param key The key obtain from HUD USER website.
#' @returns A dataframe of all the response bodies.
#' @noRd
chas_do_query_calls <- function(allqueries, key) {
  list_res <- c()
  for(i in 1:nrow(allqueries)) {
    # Build the URL for querying the data.
    call<-try(GET(allqueries$url[i], add_headers(Authorization=paste("Bearer ",
                                                                     as.character(key))), timeout(30)),
              silent = TRUE) #try to make call

    cont<-try(content(call), silent = TRUE) #parse returned data

    if('error' %in% names(cont)) {
      warning(paste("Could not find data for query:", allqueries[i],
                    ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
    } else {
      list_res[[i]] <- unlist(cont[[1]])
    }
  }

  if(length(list_res) != 0) {
    res <- as.data.frame(do.call(rbind, list_res))
    return(res)
  }
  return(NULL)
}

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
hud_chas_nation <- function(year = c("2014-2018"), key = Sys.getenv("HUD_KEY")) {

  args <- chas_input_check_cleansing(year = year, key = key)
  year <- args[[1]]
  key <- args[[2]]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "1",
               "&year=", year,  sep="") #build URL

  allqueries <- data.frame(url = URL)
  return(chas_do_query_calls(allqueries, key))
}

#' @name hud_chas_state
#' @title hud_chas_state
#' @description Returns CHAS data for a state.
#' @param state The state to query for. Can supply as abbreviation, whole name, or as geoid.
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
hud_chas_state <- function(state, year = c("2014-2018"), key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(state, year, key)
  state <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  # Assume abbreviation or fips code if length of 2. Captitalize does not affect numbers.
  # Assume full state name if length more than 2
  if(nchar(state) == 2) state = toupper(state)
  if(nchar(state) > 2) state = capitalize(state)

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name == as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code == as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num == as.character(state),][3]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", "2",
               "&stateId=", fip_code, "&year=", year,  sep="") #build URL

  allqueries <- data.frame(url = URL, key)
  return(chas_do_query_calls(allqueries, key))
}

#' @name hud_chas_county
#' @title hud_chas_county
#' @description Returns CHAS data for a county.
#' @param county The county to query for. Must supply a geoid. 2 digit state fip + 3 digit county fip. hud_counties() will show an extra 99999 at the end. Just remove that.
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
hud_chas_county <- function(county, year = c("2014-2018"), key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(county, year, key)
  county <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  if(nchar(county) == 5) {
    state_fip <- substr(county, 1,2)
    county_fip <- substr(county, 3,5)
  } else {
    state_fip <- substr(county, 1,1)
    county_fip <- substr(county, 2,4)
    county = paste("0", county, sep = "")
  }

  # Don't know what the 99999 means, but it seems like every fips code has this tacked onto the end within
  # hud_counties() function call.
  check_county <- paste(county, "99999", sep="")
  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  # Check if first two numbers of county code inputted is a valid state.

  if(!any(as.character(state_fip) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")
  if(!any(as.character(check_county) == hud_counties(state_fip))) stop(paste("There is no matching FIPs code for ", county, sep = ""))

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "3", "&stateId=", state_fip, "&entityId=", county_fip,
               "&year=", year,  sep="") #build URL
  allqueries <- data.frame(url = URL)
  return(chas_do_query_calls(allqueries, key))
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
#' @returns Returns a dataframe with CHAS data for a particular minor civil division(mcd).
hud_chas_mcd <- function(state, mcd, year = c("2014-2018"), key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(mcd, year, key)
  mcd <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  if(nchar(state) == 2) state = toupper(state)
  if(nchar(state) > 2) state = capitalize(state)

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")
  if(!any(as.character(mcd) == hud_minor_civil_divisions(state))) stop("There is no matching FIPs code for this inputted state.")

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name == as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code == as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num == as.character(state),][3]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "4", "&stateId=", fip_code, "&entityId=", mcd,
               "&year=", year,  sep="") #build URL
  allqueries <- data.frame(url = URL)
  return(chas_do_query_calls(allqueries, key))
}

#' @name hud_chas_place
#' @title hud_chas_place
#' @description Returns CHAS data for place.
#' @param state The state name, abbreviation, or fips code. Make sure if state fips is 1 digit number, do not include leading 0.
#' @param place The place(Usually just cities) to query for. Must supply as a 5 digit geoid.
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
#' @returns Returns a dataframe with CHAS data for a particular place (Usually just cities).
hud_chas_place <- function(state, place, year = c("2014-2018"), key = Sys.getenv("HUD_KEY")) {
  args <- chas_input_check_cleansing(place, year, key)
  place <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  # Check if first two numbers of county code inputted is a valid state.
  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")

  if(nchar(state) == 2) state = toupper(state)
  if(nchar(state) > 2) state = capitalize(state)

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")
  if(!any(as.character(place) == hud_places(state))) stop("There is no matching FIPs code for this inputted state.")

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name == as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code == as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num == as.character(state),][3]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=",
               "5", "&stateId=", fip_code, "&entityId=", place,
               "&year=", year,  sep="") #build URL

  allqueries <- data.frame(url = URL)
  return(chas_do_query_calls(allqueries, key))
}
