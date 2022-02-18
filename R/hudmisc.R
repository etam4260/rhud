# List Counties
# List States
# List Small Areas

# List Counties in State
# List MCDs in State
# List All Cities in State


# Misc APIs provided by the HUD to gain geographic insights.


#' @name hud_states
#' @title hud_states
#' @description Get a list of state along with the corresponding FIPs code and abbreviation
#' @keywords States
#' @export
#' @returns A dataframe containing details of all the states in the US
hud_states <- function() {
  
}

#' @name hud_metropolitan
#' @title hud_metropolitan
#' @description Get a list of all metropolitan areas in the US along with its name and CBSA code
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of metropolitan areas in US
hud_metropolitan <- function(){
  
}


#' @name hud_counties
#' @title hud_counties
#' @description Get a list of all counties within a state
#' @keywords Counties
#' @export
#' @returns A dataframe containing all counties within a state
hud_counties <- function(state) {
  
}

#' @name hud_cities
#' @title hud_cities
#' @description Get a list of all cities in a state
#' @keywords Cities
#' @export
#' @returns A dataframe containing details of cities in a state
hud_cities <- function(state) {
  
}

#' @name hud_minor_civil_divisions
#' @title hud_minor_civil_divisions
#' @description Get a list of all minor civil divisions in a state
#' @keywords CBSA
#' @export
#' @returns A dataframe containing details of minor civil divisions in a state
hud_minor_civil_divisions <- function(state) {
  
}