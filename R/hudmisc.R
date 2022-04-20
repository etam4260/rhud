#' @import httr

# Misc APIs provided by the HUD provide:
# List States
# List Small Areas
# List Counties in State
# List MCDs in State
# List All Cities in State


# Need to allow filtering by state. For example, if given
# state = NA, will query for everything
# state = c(VA, MD) will query for VA or MD
# state = c(51,52)


#' @title hud_states_territories
#' @description Get a list of state and US territories
#'   along with the corresponding FIPs code and
#'   abbreviation.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords States
#' @export
#' @returns A dataframe containing details of all the states and territories in the US.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_states_territories()
#' }
hud_states_territories <- function(key = Sys.getenv("HUD_KEY")) {
  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""))
  }

  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listStates")

  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ",
                                                     as.character(key))),
                user_agent("https://github.com/etam4260/hudr"),
                timeout(30)),silent = TRUE) #try to make call

  cont<-try(content(call), silent = TRUE) #parse returned data

  states <- as.data.frame(do.call("rbind", cont))
  states$state_num <- as.character(as.integer(states$state_num))

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if(nrow(states) > 1) {
    states$state_name <- unlist(states$state_name)
    states$state_code <- unlist(states$state_code)
    states$state_num <- unlist(states$state_num)
    states$category <- unlist(states$category)
    return(states)
  }
  stop("The key used might be invalid.")
}

# Need to allow user to filter metropolitan areas similar to place, county,
# mcds.

#' @name hud_metropolitan
#' @title hud_metropolitan
#' @description Get a list of all metropolitan areas in the US along with its
#'   name and CBSA code.
#' @param state The state to get all the metropolitan areas.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of metropolitan areas in US.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_metropolitan()
#' }
hud_metropolitan <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""))
  }

  # The 'area_name' column gives information on the metropolitan place, the
  # abbreviation of state, and the stating that it is a MSA(metro stat areas)

  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listMetroAreas")
  call <- try(GET(URL, add_headers(Authorization=paste("Bearer ",
                                                     as.character(key))),
                user_agent("https://github.com/etam4260/hudr"),
                timeout(30)),silent = TRUE) #try to make call

  cont <- try(content(call), silent = TRUE) #parse returned data
  metro <- as.data.frame(do.call(rbind, cont))

  # Do a regular expression of the area name column to parse it into
  # the name, the state location, and whether is is a HUD small areas...
  reged <- regexec("^.*,\\s([^ ]*)\\s(.*)", metro$area_name)

  met_name <- c()
  parsed_states <- c()
  classifications <- c()
  for(i in seq_len(length(reged))) {
    met_name <- c(met_name, substr(metro$area_name[i], 1,
                                   reged[[i]][2] - 2))
    parsed_states <- c(parsed_states, substr(metro$area_name[i], reged[[i]][2],
                             reged[[i]][2] + 1))
    classifications <- c(classifications, substr(metro$area_name[i],
                                                 reged[[i]][3],
                                                 nchar(metro$area_name[i])))
  }

  # Remove the area_name column.
  metro <- metro[,c(1,3)]

  # Add new columns.
  metro$metro_name <- met_name
  metro$metro_state <- parsed_states
  metro$classifications <- classifications

  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states_territories(key = Sys.getenv("HUD_KEY"))
  }

  for(i in seq_len(length(state))) {
    if(!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
    }
  }

  # Filter now based on state input.
  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state),]) != 0) {
    state_abbr <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state),][2]
  }
  if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    state_abbr <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][2]
  }
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state),]) != 0) {
    state_abbr <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][2]
  }

  state_abbr <- unlist(state_abbr)

  metro <- metro[metro$metro_state == state_abbr, ]

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if(nrow(metro) > 1) {
    metro$cbsa_code <- unlist(metro$cbsa_code)
    metro$area_name <- unlist(metro$area_name)
    metro$category <- unlist(metro$category)
    return(metro)
  }

  stop("The key used might be invalid.")
}


#' @name hud_counties
#' @title hud_counties
#' @description Get a list of all counties within a state.
#' @param state The state to get all counties.
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords Counties
#' @export
#' @returns A dataframe containing all counties within a state
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_places("CA")
#' hud_places("Virginia")
#' hud_places("51")
#' }
hud_counties <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(state) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""))
  }

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""))
  }

  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states_territories(key = Sys.getenv("HUD_KEY"))
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
                                as.character(state),][2]
  }
  if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][2]
  }
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][2]
  }

  # Allow the user to specify multiple years states to query for... Just stack
  # them.

  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listCounties/",
               unlist(fip_code), sep = "")
  counties <- misc_do_query_call(URL, key)

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if(nrow(counties) > 1) {
    counties$state_code <- unlist(counties$state_code)
    counties$fips_code <- unlist(counties$fips_code)
    counties$county_name <- unlist(counties$county_name)
    counties$town_name <- unlist(counties$town_name)
    counties$category <- unlist(counties$category)
    return(counties)
  }

  stop("The key used might be invalid or could not find counties for this state.")
}

#' @name hud_places
#' @title hud_places
#' @description Get a list of all places in a state.
#' @param state The state to get all places.
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords places.
#' @export
#' @returns A dataframe containing details of places in a state.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_places("CA")
#' hud_places("Virginia")
#' hud_places("51")
#' }
hud_places <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(state) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""))
  }

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""))
  }

  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states_territories(key = Sys.getenv("HUD_KEY"))
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
  }
  if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][3]
  }
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][3]
  }

  URL <- paste("https://www.huduser.gov/hudapi/public/chas/listCities/",
               unlist(fip_code),
               sep = "")

  places <- misc_do_query_call(URL, key)

  if(nrow(places) > 1) {
    places$statecode <- unlist(places$statecode)
    places$entityId <- unlist(places$entityId)
    places$placename <- unlist(places$placename)
    return(places)
  }
  stop("The key used might be invalid or could not find places for this state.")
}

#' @name hud_minor_civil_divisions
#' @title hud_minor_civil_divisions
#' @description Get a list of all minor civil divisions in a state
#' @param state The state to get all MCD.
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of minor civil divisions in a state.
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_minor_civil_divisions("CA")
#' hud_minor_civil_divisions("Virginia")
#' hud_minor_civil_divisions("51")
#' }
hud_minor_civil_divisions <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(state) || !is.vector(key)) {
    stop(paste("Make sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""))
  }

  if(key == "") {
    stop(paste("Did you forget to set the key?",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 to",
               "and sign up and get a token. Then save",
               "this to your environment using",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""))
  }

  if(all(nchar(state) == 2)) state <- toupper(state)
  if(all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if(is.null(pkg.env$state)) {
    pkg.env$state <- hud_states_territories(key = Sys.getenv("HUD_KEY"))
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
  }
  if(nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state),][3]
  }
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state),]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state),][3]
  }

  URL <- paste("https://www.huduser.gov/hudapi/public/chas/listMCDs/",
               unlist(fip_code),
               sep = "")

  mcd <- misc_do_query_call(URL, key)

  if(nrow(mcd) > 1) {
    mcd$statecode <- unlist(mcd$statecode)
    mcd$entityId <- unlist(mcd$entityId)
    mcd$mcdname <- unlist(mcd$mcdname)
    return(mcd)
  }
  stop("The key used might be invalid or could not find mcds for this state")
}
