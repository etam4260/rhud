#' @import httr


# Misc APIs provided by the HUD to gain insights into what the user
# can query for in the main APIs
# List States
# List Small Areas
# List Counties in State
# List MCDs in State
# List All Cities in State

# Create a package environment for storing state level data.
pkg.env <- new.env(parent = emptyenv())

#' @name misc_do_query_call
#' @title misc_do_query_call
#' @description Make queries calls given a list of URLs
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @returns A dataframe containing all queried rows.
misc_do_query_call <- function(URL, key) {
  list_res <- c()
  for(i in 1:length(URL)) {
    # Build the URL for querying the data.
    url <- URL[i]

    call<-try(GET(url, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))), user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE) #try to make call

    cont<-try(content(call), silent = TRUE) #parse returned data

    if('error' %in% names(cont) || length(cont) == 0) {
      warning(paste("Could not find data for query:", allqueries[i],
                    ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute. If you think this is wrong please report it at https://github.com/etam4260/hudr/issues.", sep = ""))
    } else {
      list_res[[i]] <- as.data.frame(do.call(rbind, cont))
    }
  }

  if(length(list_res) != 0) {
    res <- as.data.frame(do.call(rbind, list_res))
    return(res)
  }
  return(NULL)
}

#' @name hud_states
#' @title hud_states
#' @description Get a list of state along with the corresponding FIPs code and abbreviation.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords States
#' @export
#' @returns A dataframe containing details of all the states in the US.
hud_states <- function(key = Sys.getenv("HUD_KEY")) {
  if(key == "") stop("Did you forget to set the key? Please go to https://www.huduser.gov/hudapi/public/register?comingfrom=1 to and sign up and get a token. Then save this to your environment using Sys.setenv('HUD_KEY' = YOUR_KEY)")

  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listStates") #build URL

  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key))), user_agent("https://github.com/etam4260/hudr"), timeout(30)),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  states <- as.data.frame(do.call(rbind, cont))
  states$state_num <- as.character(as.integer(states$state_num))

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if(nrow(states) > 1) {
    states$state_code <- unlist(states$state_code)
    states$state_num <- unlist(states$state_num)
    states$category <- unlist(states$category)
    return(states)
  }
  stop("The key used might be invalid.")
}

#' @name hud_metropolitan
#' @title hud_metropolitan
#' @description Get a list of all metropolitan areas in the US along with its name and CBSA code.
#' @param key The API key for this user. You must go to HUD and sign up for
#'  an account and request for an API key.
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of metropolitan areas in US.
hud_metropolitan <- function(key = Sys.getenv("HUD_KEY")) {
  if(key == "") stop("Did you forget to set the key? Please go to https://www.huduser.gov/hudapi/public/register?comingfrom=1 to and sign up and get a token. Then save this to your environment using Sys.setenv('HUD_KEY' = YOUR_KEY)")


  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listMetroAreas") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key))), user_agent("https://github.com/etam4260/hudr"), timeout(30)),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  metro<-as.data.frame(do.call(rbind, cont))

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
hud_counties <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(state) || !is.vector(key)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")
  if(key == "") stop("Did you forget to set the key? Please go to https://www.huduser.gov/hudapi/public/register?comingfrom=1 to and sign up and get a token. Then save this to your environment using Sys.setenv('HUD_KEY' = YOUR_KEY)")

  if(all(nchar(state) == 2)) state = toupper(state)
  if(all(nchar(state) > 2)) state = capitalize(tolower(state))

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  for(i in 1:length(state)) {
    if(!any(as.character(state[i]) == pkg.env$state)) stop("There is no matching fips code for one of the inputted states.")
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name %in% as.character(state),][2]
  if(nrow(pkg.env$state[pkg.env$state$state_code %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code %in% as.character(state),][2]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num %in% as.character(state),][2]

  # Allow the user to specify multiple years states to query for... Just stack them.

  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/listCounties/", unlist(fip_code), sep = "") #build URL
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

  stop("The key used might be invalid or could not find this county")
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
hud_places <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(state) || !is.vector(key)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")
  if(key == "") stop("Did you forget to set the key? Please go to https://www.huduser.gov/hudapi/public/register?comingfrom=1 to and sign up and get a token. Then save this to your environment using Sys.setenv('HUD_KEY' = YOUR_KEY)")

  if(all(nchar(state) == 2)) state = toupper(state)
  if(all(nchar(state) > 2)) state = capitalize(tolower(state))

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  for(i in 1:length(state)) {
    if(!any(as.character(state[i]) == pkg.env$state)) stop("There is no matching fips code for one of the inputted states.")
  }


  # Allow user to supply state name or state abbr or state fips as inputs.

  if(nrow(pkg.env$state[pkg.env$state$state_name %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name %in% as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code %in% as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num %in% as.character(state),][3]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas/listCities/", unlist(fip_code), sep = "") #build URL

  places <- misc_do_query_call(URL, key)

  if(nrow(places) > 1) {
    places$statecode <- unlist(places$statecode)
    places$entityId <- unlist(places$entityId)
    places$placename <- unlist(places$placename)
    return(places)
  }
  stop("The key used might be invalid.")
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
hud_minor_civil_divisions <- function(state, key = Sys.getenv("HUD_KEY")) {
  if(!is.vector(state) || !is.vector(key)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")
  if(key == "") stop("Did you forget to set the key? Please go to https://www.huduser.gov/hudapi/public/register?comingfrom=1 to and sign up and get a token. Then save this to your environment using Sys.setenv('HUD_KEY' = YOUR_KEY)")

  if(all(nchar(state) == 2)) state = toupper(state)
  if(all(nchar(state) > 2)) state = capitalize(tolower(state))

  if(is.null(pkg.env$state)) pkg.env$state <- hud_states(key = Sys.getenv("HUD_KEY"))
  for(i in 1:length(state)) {
    if(!any(as.character(state[i]) == pkg.env$state)) stop("There is no matching fips code for one of the inputted states.")
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if(nrow(pkg.env$state[pkg.env$state$state_name %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_name %in% as.character(state),][3]
  if(nrow(pkg.env$state[pkg.env$state$state_code %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_code %in% as.character(state),][3]
  if(nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in% as.character(state),]) != 0) fip_code <- pkg.env$state[pkg.env$state$state_num %in% as.character(state),][3]

  URL <- paste("https://www.huduser.gov/hudapi/public/chas/listMCDs/", unlist(fip_code), sep = "") #build URL

  mcd <- misc_do_query_call(URL, key)

  if(nrow(mcd) > 1) {
    mcd$statecode <- unlist(mcd$statecode)
    mcd$entityId <- unlist(mcd$entityId)
    mcd$mcdname <- unlist(mcd$mcdname)
    return(mcd)
  }
  stop("The key used might be invalid.")
}
