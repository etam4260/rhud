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
  if(!is.vector(query) || !is.vector(year) || !is.vector(key)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")
  if(key == "") stop("Did you forget to set the key?")

  query <- paste(str_trim(as.character(query), side = "both"))
  year <- unique(paste(str_trim(as.character(year), side = "both")))
  key <- paste(str_trim(as.character(key), side = "both"))
  if(!all(year %in% c("2014-2018","2013-2017","2012-2016","2011-2015","2010-2014",
                      "2009-2013","2008-2012","2007-2011","2006-2010"))) stop("Years specified are not allowed. Check the documentation.")

  return(c(query, year, key))
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
hud_chas_nation <- function(year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {

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
hud_chas_state <- function(state, year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {

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
hud_chas_county <- function(county, year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {

}

#' @name hud_chas_mcd
#' @title hud_chas_mcd
#' @description Returns CHAS data for an mcd.
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
hud_chas_mcd <- function(mcd, year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {

}

#' @name hud_chas_place
#' @title hud_chas_place
#' @description Returns CHAS data for place.
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
hud_chas_place <- function(place, year = format(Sys.Date(), "%Y"), key = Sys.getenv("HUD_KEY")) {

}
