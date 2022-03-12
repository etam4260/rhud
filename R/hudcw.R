#' @import httr

#' @name cw_input_check_cleansing
#' @title cw_input_check_cleansing
#' @description Helper function used to clean user inputted variables for all
#' Crosswalk functions.
#' @param query
#'   The inputted GEOID
#' @param year The years to query for.
#' @param quarter The quarters to query for.
#' @param key The key obtain from HUD USER website.
#' @returns The cleansed input arguments.
#' @noRd
cw_input_check_cleansing <- function(query, year, quarter, key) {
  if(!is.vector(query) || !is.vector(year) || !is.vector(quarter) || !is.vector(key)) stop("Make sure all inputs are of type vector. Check types with typeof([variable]). If list try unlist([variable]); if matrix try as.vector([variable])")
  if(key == "") stop("Did you forget to set the key?")

  query <- paste(trimws(as.character(query), which = "both"))
  year <- unique(paste(trimws(as.character(year), which = "both")))
  quarter <- unique(paste(trimws(as.character(quarter), which = "both")))
  key <- paste(trimws(as.character(key), which = "both"))

  numbers_only <- function(x) !grepl("\\D", x)
  if(FALSE %in% numbers_only(query)) stop("Query input must only be numbers.")
  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  if(FALSE %in% numbers_only(quarter)) stop("Quarter input must only be numbers.")

  if(!all(as.character(quarter) %in% c("1","2","3","4"))) stop("Quarters must be from 1 to 4.")

  ifelse(any(as.integer(year) > as.integer(strsplit(as.character(Sys.Date()), "-")[[1]][1])),
         stop("A year specified seems to be in the future?"), "")

  return(list(query, year, quarter, key))
}

#' @name create_queries
#' @title create_queries
#' @description Helper function for creating a dataframe of all the parameters needed
#'   for querying.
#' @param query
#'   The inputted GEOID
#' @param year The years to query for.
#' @param quarter The quarters to query for.
#' @returns A dataframe consisting of all combinations of year and quarter as well as
#'   the associated GEOID.
#' @noRd
create_queries <- function(query, year, quarter) {
  allqueries <- expand.grid(year = year, quarter = quarter)
  allqueries$query <- query
  return(allqueries)
}

#' @name cw_do_query_calls
#' @title cw_do_query_calls
#' @description Helper function for making all the queries provided by the allqueries input.
#' @param allqueries
#'   Dataframe consisting of the queried GEOID, years, and quarters.
#' @param type HUD defines different crosswalk files into types. It goes from 1-12,
#'   the function calls in this file should follow that order.
#' @param primary_geoid The first geoid part of a function call. For example, hud_cw_zip_tract() has zip as first GEOID and tract as second GEOID.
#' @param secondary_geoid The second geoid part of a function call.
#' @param key The key needed to query the HUD API
#' @returns A data frame of all the results made from the query.
#' @noRd
cw_do_query_calls <- function(allqueries, type, primary_geoid, secondary_geoid, key) {
  list_res <- c()

  for(i in 1:nrow(allqueries)) {
    URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", type, "&query=", allqueries$query[i], "&year=", allqueries$year[i], "&quarter=", allqueries$quarter[i], sep="") #build URL

    call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key))), timeout(30)), silent = TRUE) #try to make call
    cont<-try(content(call), silent = TRUE) #parse returned data

    if('error' %in% names(cont[[1]])) {
      warning(paste("Could not find data for inputted query, year, or quarter where query equals ", allqueries$query[i], ", year equals ",allqueries$year[i], ", and quarter equals ", allqueries$quarter[i], ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
    } else {
      res <- as.data.frame(do.call(rbind, cont$data$results))
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      res$quarter <- allqueries$quarter[i]
      res[1] <- unlist(res[1])
      res[2] <- unlist(res[2])
      res[3] <- unlist(res[3])
      res[4] <- unlist(res[4])
      res[5] <- unlist(res[5])
      list_res[[i]] <- res
    }
  }

  allres <- NULL
  if(length(list_res) != 0) {
    allres <- do.call(rbind, list_res)
    colnames(allres)[6] <- primary_geoid
    colnames(allres)[1] <- secondary_geoid
  }
  return(as.data.frame(allres))
}


#' @name hud_cw_zip_tract
#' @title hud_cw_zip_tract
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to tract.
#' @param zip
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_zip_tract <- function(zip, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "tract"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(zip) != 5) stop("Query input is not of length 5")

  # Create dataframe with all queries needed.
  allqueries <- create_queries(zip, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "1", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "1", primary_geoid, secondary_geoid, key)$tract)
}


#' @name hud_cw_zip_county
#' @title hud_cw_zip_county
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to county.
#' @param zip
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_zip_county <- function(zip, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "county"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(zip) != 5) stop("Query input is not of length 5")

  # Create dataframe with all queries needed.
  allqueries <- create_queries(zip, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "2", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "2", primary_geoid, secondary_geoid, key)$county)
}

#' @name hud_cw_zip_cbsa
#' @title hud_cw_zip_cbsa
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to cbsa.
#' @param zip
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_zip_cbsa <- function(zip, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "cbsa"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(zip) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(zip, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "3", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "3", primary_geoid, secondary_geoid, key)$cbsa)
}

#' @name hud_cw_zip_cbsadiv
#' @title hud_cw_zip_cbsadiv
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to cbsadiv.
#' @param zip
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_zip_cbsadiv <- function(zip, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "cbsadiv"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(zip) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(zip, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "4", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "4", primary_geoid, secondary_geoid, key)$cbsadiv)
}

#' @name hud_cw_zip_cd
#' @title hud_cw_zip_cd
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to congressional district.
#' @param zip
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_zip_cd <- function(zip, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "cd"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(zip) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(zip, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "5", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "5", primary_geoid, secondary_geoid, key)$cd)
}

#' @name hud_cw_tract_zip
#' @title hud_cw_tract_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for tract to zip.
#' @param tract
#'   11 digit unique 2000 or 2010 Census tract GEOID consisting of
#'   state FIPS + county FIPS + tract code. Eg: 51059461700  for type 6
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_tract_zip <- function(tract, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "tract"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(tract, year, quarter, key)
  tract <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(tract) != 11) stop("Query input is not of length 11")

  allqueries <- create_queries(tract, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "6", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "6", primary_geoid, secondary_geoid, key)$zip)
}


#' @name hud_cw_county_zip
#' @title hud_cw_county_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for county to zip.
#' @param county
#'   5 digit unique 2000 or 2010 Census county GEOID consisting of
#'   state FIPS + county FIPS. Eg: 51600 for type 7
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_county_zip <- function(county, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "county"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(county, year, quarter, key)
  county <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(county) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(county, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "7", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "7", primary_geoid, secondary_geoid, key)$zip)
}



#' @name hud_cw_cbsa_zip
#' @title hud_cw_cbsa_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for cbsa to zip.
#' @param cbsa
#'   5 digit CBSA code for Micropolitan and Metropolitan Areas Eg: 10380 for type 8
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_cbsa_zip <- function(cbsa, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "cbsa"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(cbsa, year, quarter, key)
  cbsa <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(cbsa) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(cbsa, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "8", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "8", primary_geoid, secondary_geoid, key)$zip)
}

#' @name hud_cw_cbsadiv_zip
#' @title hud_cw_cbsadiv_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for cbsadiv to zip.
#' @param cbsadiv
#'   5-digit CBSA Division code which only applies to Metropolitan Areas.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_cbsadiv_zip <- function(cbsadiv, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "cbsadiv"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(cbsadiv, year, quarter, key)
  cbsadiv <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(cbsadiv) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(cbsadiv, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "9", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "9", primary_geoid, secondary_geoid, key)$zip)
}

#' @name hud_cw_cd_zip
#' @title hud_cw_cd_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for cbsadiv to zip.
#' @param cd
#'  4-digit GEOID for the Congressional District which consists of
#'  state FIPS + Congressional District code. Eg: 7200 for type 10
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_cd_zip <- function(cd, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "cd"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(cd, year, quarter, key)
  cd <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(cd) != 4) stop("Query input is not of length 4")

  allqueries <- create_queries(cd, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "10", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "10", primary_geoid, secondary_geoid, key)$zip)
}

#' @name hud_cw_zip_countysub
#' @title hud_cw_zip_countysub
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for zip to countysub.
#' @param zip
#'   5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_zip_countysub <- function(zip, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "zip"
  secondary_geoid <- "countysub"

  args <- cw_input_check_cleansing(zip, year, quarter, key)
  zip <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(zip) != 5) stop("Query input is not of length 5")

  allqueries <- create_queries(zip, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "11", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "11", primary_geoid, secondary_geoid, key)$countysub)
}

#' @name hud_cw_countysub_zip
#' @title hud_cw_countysub_zip
#' @description This function queries the Crosswalks API provided by
#'   US Department of Housing and Urban Development. This
#'   returns the crosswalk for countysub to zip.
#' @param countysub
#'   10-digit GEOID for the County sub Eg: 4606720300 for type 12
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   current year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param minimal Return just the crosswalked GEOIDs if true. Otherwise, return all fields.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for
#'   a particular GEOID. These measurements include res-ratio, bus-ratio,
#'   oth-ratio, tot-ratio. For more details on these measurements, visit
#'   https://www.huduser.gov/portal/dataset/uspszip-api.html
hud_cw_countysub_zip <- function(countysub, year = format(Sys.Date(), "%Y"), quarter = 1, minimal = FALSE, key = Sys.getenv("HUD_KEY")) {
  primary_geoid <- "countysub"
  secondary_geoid <- "zip"

  args <- cw_input_check_cleansing(countysub, year, quarter, key)
  countysub <- args[[1]]
  year <- args[[2]]
  quarter <- args[[3]]
  key <- args[[4]]

  if(nchar(countysub) != 10) stop("Query input is not of length 10")

  allqueries <- create_queries(countysub, year, quarter)
  # HUD has a list of types. 1 corresponds to zip-tract, 2 corresponds to zip_county...
  # The functions in this file should follow that order.
  if(!minimal) return(cw_do_query_calls(allqueries, "12", primary_geoid, secondary_geoid, key))
  return(cw_do_query_calls(allqueries, "12", primary_geoid, secondary_geoid, key)$zip)
}





