#' @import httr

# Misc APIs provided by the HUD to gain insights into what the user
# can query for in the main APIs
# List States
# List Small Areas
# List Counties in State
# List MCDs in State
# List All Cities in State

#' @name hud_states
#' @title hud_states
#' @description Get a list of state along with the corresponding FIPs code and abbreviation
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords States
#' @export
#' @returns A dataframe containing details of all the states in the US
hud_states <- function(key = Sys.getenv("HUD_KEY")) {
  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listStates") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  states <- as.data.frame(do.call(rbind, cont))
  states$state_num <- as.character(as.integer(states$state_num))
  return(states)
}

pkg.env <- new.env(parent = emptyenv())
pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))

#' @name hud_metropolitan
#' @title hud_metropolitan
#' @description Get a list of all metropolitan areas in the US along with its name and CBSA code
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of metropolitan areas in US
hud_metropolitan <- function(key = Sys.getenv("HUD_KEY")) {
  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listMetroAreas") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  return(as.data.frame(do.call(rbind, cont)))
}


#' @name hud_counties
#' @title hud_counties
#' @description Get a list of all counties within a state
#' @param state The state to get all counties
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords Counties
#' @export
#' @returns A dataframe containing all counties within a state
hud_counties <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name == as.character(state),][2]
  if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code == as.character(state),][2]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num == as.character(state),][2]

  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listCounties/", unlist(fip_code), sep = "") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  return(as.data.frame(do.call(rbind, cont)))
}

#' @name hud_cities
#' @title hud_cities
#' @description Get a list of all cities in a state
#' @param state The state to get all cities
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords Cities
#' @export
#' @returns A dataframe containing details of cities in a state
hud_cities <- function(state, key = Sys.getenv("HUD_KEY")) {

  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")
  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name == as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code == as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num == as.character(state),][3]
  URL <- paste("https://www.huduser.gov/hudapi/public/chas/listCities/", unlist(fip_code), sep = "") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  View(cont)
  return(as.data.frame(do.call(rbind, cont)))
}

#' @name hud_minor_civil_divisions
#' @title hud_minor_civil_divisions
#' @description Get a list of all minor civil divisions in a state
#' @param state The state to get all MCD
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of minor civil divisions in a state
hud_minor_civil_divisions <- function(state, key = Sys.getenv("HUD_KEY")) {

  if(!any(as.character(state) == pkg.env$state)) stop("There is no matching FIPs code for this inputted state.")

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name == as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code == as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) == as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num == as.character(state),][3]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas/listMCDs/", unlist(fip_code), sep = "") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  return(as.data.frame(do.call(rbind, cont)))
}
