#library for http requests
#library to sanitize strings
#' @import httr
#' @import stringr

# Implementation thought process was adapted from
# https://github.com/dteck/HUD
# Inspiration to build an API
# came from
# https://github.com/ropensci/rnoaa

# An R interface for accessing HUD USER (US Department of Housing and Urban Development)
# API.
# The HUD USER has four main datasets:
# Crosswalk
# Fair Markets Rent
# Income Limits
# Comprehensive and Housing Affordability Strategy


# Creating global environment to set default values to some
# of parameters in the web queries.
pkg.env <- new.env(parent = emptyenv())
pkg.env$curr.year <- c(strsplit(toString(Sys.Date()), "-")[[1]][1])
pkg.env$curr.key <- NULL
pkg.env$curr.quarter <- c("1","2","3","4")


#' hudsetkey
#' @name hudsetkey
#' @title hudsetkey
#' @description The function will save the key into your package download. You will need to
#' update this for new downloads. You can also set a new key by using this function with a new one.
#' @param key The token given by USPS
#' @export
hudsetkey <- function(key) {
  pkg.env$curr.key <- key
}

#' hudcw
#' @name hudcw
#' @title hudcw
#' @description This function queries the Crosswalks API provided by US Department of Housing and Urban Development
#' @param type Must be a number between 1 and 12 depending on the Crosswalk type.
#' 1) zip-tract
#' 2) zip-county
#' 3) zip-cbsa
#' 4) zip-cbsadiv (Available 4th Quarter 2017 onwards)
#' 5) zip-cd
#' 6) tract-zip
#' 7) county-zip
#' 8) cbsa-zip
#' 9) cbsadiv-zip (Available 4th Quarter 2017 onwards)
#' 10) cd-zip
#' 11) zip-countysub (Available 2nd Quarter 2018 onwards)
#' 12) countysub-zip (Available 2nd Quarter 2018 onwards)
#' @param query
#' 5 digit USPS ZIP code of the data to retrieve. E.g. 22031 for type 1 to 5 and 11 .
#' or
#' 11 digit unique 2000 or 2010 Census tract GEOID consisting of state FIPS + county FIPS + tract code. Eg: 51059461700  for type 6
#' or
#' 5 digit unique 2000 or 2010 Census county GEOID consisting of state FIPS + county FIPS. Eg: 51600 for type 7
#' or
#' 5 digit CBSA code for Micropolitan and Metropolitan Areas Eg: 10380 for type 8
#' or
#' 5-digit CBSA Division code which only applies to Metropolitan Areas. Eg: 35614 for type 9
#' or
#' 4-digit GEOID for the Congressional District which consists of state FIPS + Congressional District code. Eg: 7200 for type 10
#' or
#' 10-digit GEOID for the County sub Eg: 4606720300 for type 12
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param quarter Gets the quarter of the year that this data was recorded. Defaults to the latest quarter.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing CROSSWALK data for a particular GEOID.
#' These measurements include res-ratio, bus-ratio, oth-ratio, tot-ratio. For more details on these measurements, visit https://www.huduser.gov/portal/dataset/uspszip-api.html
hudcw <- function(type, query, year = pkg.env$curr.year, quarter = pkg.env$curr.quarter, key = pkg.env$curr.key) {
  alltypes <- c("zip-tract","zip-county","zip-cbsa",
                "zip-cbsadiv","zip-cd","tract-zip",
                "county-zip","cbsa-zip","cbsadiv-zip",
                "cd-zip","zip-countysub","countysub-zip")
  lhgeoid <- NULL
  rhgeoid <- NULL
  URL <- NULL
  call <- NULL
  cont <- NULL
  res <- NULL
  thisyear <- NULL

  # Removing leading and ending spaces and converting all integer inputs to characters
  type <- paste(str_trim(as.character(type), side = "both"))
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- paste(str_trim(as.character(year), side = "both"))
  quarter <- paste(str_trim(as.character(quarter), side = "both"))
  key <- paste(str_trim(as.character(key), side = "both"))

  lhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][1]
  rhgeoid <- strsplit(alltypes[as.integer(type)], "-")[[1]][2]

  numbers_only <- function(x) !grepl("\\D", x)
  if(FALSE %in% numbers_only(type)) stop("Type input must only be numbers.")
  if(FALSE %in% numbers_only(query)) stop("Query input must only be numbers.")
  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  if(FALSE %in% numbers_only(quarter)) stop("Quarter input must only be numbers.")

  ifelse(TRUE %in% as.integer(year) > as.integer(pkg.env$curr.year), stop("The year specified seems to be in the future?"), "")
  ifelse(as.integer(type) < 1 || as.integer(type) >= 12, stop("The type input is not in the range of 1-12"), "")

  # Need to make sure query is a zip code of 5 digits.
  if(as.integer(type) >= 1 && as.integer(type) <= 5 || as.integer(type) == 11){
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is a fips code of 5 digits.
  } else if(as.integer(type) == 7) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is a fips code with census tract attached onto it 11 digits.
  } else if(as.integer(type) == 6) {
    if(nchar(query) != 11) stop("Query input is not of length 11")
  # Need to make sure query is CBSA code for micropolitan/metropolitan areas: 5 digits.
  } else if(as.integer(type) == 8) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is CBSA division code for metropolitan areas: 5 digits.
  } else if(as.integer(type) == 9) {
    if(nchar(query) != 5) stop("Query input is not of length 5")
  # Need to make sure query is 4 digit GEOID for congressional districts: 4 digits.
  } else if(as.integer(type) == 10) {
    if(nchar(query) != 4) stop("Query input is not of length 4")
  # Need to make sure query is 10 digits for county subdistrict
  } else if(as.integer(type) == 12) {
    if(nchar(query) != 10) stop("Query input is not of length 10")
  }

  final <- data.frame(lhgeoid=integer(0),
                      year=integer(0),
                      quarter=integer(0),
                      rhgeoid=integer(0),
                      res_ratio=integer(0),
                      bus_ratio=integer(0),
                      oth_ratio=integer(0),
                      tot_ratio=integer(0)
                      )
  colnames(final)[1] <- lhgeoid
  colnames(final)[4] <- rhgeoid

  for(indyear in seq(1, length(year), 1)) {

    thisyear <- year[indyear]

    for(indquarter in seq(1, length(quarter), 1)) {

      thisquarter <- quarter[indquarter]

      URL <- paste("https://www.huduser.gov/hudapi/public/usps?type=", type, "&query=", query, "&year=", thisyear, "&quarter=", thisquarter, sep="") #build URL
      call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
      cont<-try(content(call), silent = TRUE) #parse returned data

      for(i in seq(1,length(cont$data$results),1)){ #iterate over results and append to df

        final[nrow(final)+1, ] <- c(query,
                                    thisyear,
                                    thisquarter,
                                    cont$data$results[[i]][["geoid"]],
                                    cont$data$results[[i]][["res_ratio"]],
                                    cont$data$results[[i]][["bus_ratio"]],
                                    cont$data$results[[i]][["oth_ratio"]],
                                    cont$data$results[[i]][["tot_ratio"]]
                                  )
      }

    }
  }
  return(final) #returns df as output of function
}


#' hudfmr
#' @name hudfmr
#' @title hudfmr
#' @description This function queries the Fair Markets Rent API provided by US Department of Housing and Urban Development
#' @param query Can provide either a 10 digit FIPS code including county subdivision, or state abbreviation.
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Crosswalks API
#' @export
#' @returns This function returns a dataframe containing FAIR MARKETS RENT data for a particular county or state.
#' For county level data, these measurements include the county_name, counties_msa, town_name, metro_status, metro_name,
#' smallarea_status, basicdata, Efficiency, One-Bedroom, Two-Bedroom, Three-Bedroom, Four-Bedroom, and year
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
#' For state level data, these measurements will be the same as county level data, but will return a dataframe with
#' the individual measurements for each individual county within the state.
hudfmr <- function(query, year = pkg.env$curr.year, key = pkg.env$curr.key) {
  geoid <- NULL
  URL <- NULL
  call <- NULL
  cont <- NULL

  # Removing leading and ending spaces and converting all integer inputs to characters
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- paste(str_trim(as.character(year), side = "both"))
  key <- paste(str_trim(as.character(key), side = "both"))
  numbers_only <- function(x) !grepl("\\D", x)

  ifelse(as.integer(year) > as.integer(pkg.env$curr.year), stop("The year specified seems to be in the future?"), "")
  ifelse(!numbers_only(query) && nchar(query) != 2, stop("The inputted query for state abbreviation is not right."), "")
  ifelse(numbers_only(query) && nchar(query) != 10, stop("The inputted query input is not a 10 digit fips."), "")
  if(!numbers_only(query)) geoid <- "state"
  if(numbers_only(query)) geoid <- "county or CBSA"

  # Build the URL for querying the data.
  URL <- paste("https://www.huduser.gov/hudapi/public/fmr/", if(geoid == "state") "statedata/" else "data/", query, "?year=", year, sep="") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data

  if (length(cont$data) == 0){ #check to see if results exist - if not warn use of errors in their input values
    stop("The query did not return any results. Please check if these input values follow the rules stated for each parameter. Maybe your authorization key is wrong? Maybe
         there hasn't been data recorded for this particular time period.")
  } else {
    if(geoid == "state") {
      res<-data.frame(geoid=character(length(cont$data$counties)),
                      year =integer(length(cont$data$counties)),
                      town=character(length(cont$data$counties)),
                      county=character(length(cont$data$counties)),
                      metro=character(length(cont$data$counties)),
                      fips=character(length(cont$data$counties)),
                      efficiency=integer(length(cont$data$counties)),
                      onebedroom=integer(length(cont$data$counties)),
                      twobedroom=integer(length(cont$data$counties)),
                      threebedroom=integer(length(cont$data$counties)),
                      fourbedroom=integer(length(cont$data$counties)),
                      fmrpercentile=integer(length(cont$data$counties)),
                      statename=character(length(cont$data$counties)),
                      smallareastatus=integer(length(cont$data$counties))) #build df

      for(i in seq(1,length(cont$data$counties),1)){ #iterate over results and append to d
        res$geoid[i] <- query
        res$year[i] <- year
        res$town[i] <- if(is.null(cont$data$counties[[i]][["town_name"]]) || cont$data$counties[[i]][["town_name"]] == "") "NA" else cont$data$counties[[i]][["town_name"]]
        res$county[i] <- cont$data$counties[[i]][["county_name"]]
        res$metro[i] <- cont$data$counties[[i]][["metro_name"]]
        res$fips[i] <- cont$data$counties[[i]][["fips_code"]]
        res$efficiency[i] <- cont$data$counties[[i]][["Efficiency"]]
        res$onebedroom[i] <- cont$data$counties[[i]]$`One-Bedroom`
        res$twobedroom[i] <- cont$data$counties[[i]]$`Two-Bedroom`
        res$threebedroom[i] <- cont$data$counties[[i]]$`Three-Bedroom`
        res$fourbedroom[i] <- cont$data$counties[[i]]$`Four-Bedroom`
        res$fmrpercentile[i] <- cont$data$counties[[i]]$`FMR Percentile`
        res$statename[i] <- cont$data$counties[[i]][["statename"]]
        res$smallareastatus[i] <- cont$data$counties[[i]][["smallarea_status"]]

      }
    } else {
      res<-data.frame(geoid=integer(1),
                      year = integer(1),
                      town=character(1),
                      county=character(1),
                      metro=character(1),
                      metrostatus=integer(1),
                      efficiency=integer(1),
                      onebedroom=integer(1),
                      twobedroom=integer(1),
                      threebedroom=integer(1),
                      fourbedroom=integer(1),
                      smallareastatus=integer(1)) #build df

      res$geoid[1] <- query
      res$year[1] <- year
      res$town[1] <- if(is.null(cont$data[["town_name"]]) || cont$data[["town_name"]] == "") "NA" else cont$data[["town_name"]]
      res$county[1] <- cont$data[["county_name"]]
      res$metro[1] <- cont$data[["metro_name"]]
      res$metrostatus[1] <- cont$data[["metro_status"]]
      res$efficiency[1] <- cont$data$basicdata[["Efficiency"]]
      res$onebedroom[1] <- cont$data$basicdata$`One-Bedroom`
      res$twobedroom[1] <- cont$data$basicdata$`Two-Bedroom`
      res$threebedroom[1] <- cont$data$basicdata$`Three-Bedroom`
      res$fourbedroom[1] <- cont$data$basicdata$`Four-Bedroom`
      res$smallareastatus[1] <- cont$data[["smallarea_status"]]
    }
    colnames(res)[1] <- geoid
    return(res) #returns df as output of function
  }
}




#' hudil
#' @name hudil
#' @title hudil
#' @description This function queries the Income Limits API provided by US Department of Housing and Urban Development
#' @param query Can provide either a 10 digit FIPS code including county subdivision, or state abbreviation.
#' @param year Gets the year that this data was recorded. Default is the latest year.
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Income Limits API
#' @export
#' @returns This function returns a dataframe containing INCOME LIMITS data for a particular county or state.
#' For county level data, these measurements include the county_name, counties_msa, town_name, metro_status, metro_name,
#' year, median_income, very_low+, extremely_low+, and low+.
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
hudil <- function(query, year = pkg.env$curr.year, key = pkg.env$curr.key) {

  geoid <- NULL
  URL <- NULL
  call <- NULL
  cont <- NULL

  # Removing leading and ending spaces and converting all integer inputs to characters
  query <- paste(str_trim(as.character(query), side = "both"))
  year <- paste(str_trim(as.character(year), side = "both"))
  key <- paste(str_trim(as.character(key), side = "both"))
  numbers_only <- function(x) !grepl("\\D", x)

  if(as.integer(year) > as.integer(pkg.env$curr.year)) stop("The year specified seems to be in the future?")
  if(!numbers_only(query) && nchar(query) != 2) stop("The inputted query for state abbreviation is not right.")
  if(numbers_only(query) && nchar(query) != 10) stop("The inputted query input is not a 10 digit fips.")
  if(!numbers_only(query)) geoid <- "state"
  if(numbers_only(query)) geoid <- "county or CBSA"

  # Build the URL for querying the data.
  URL <- paste("https://www.huduser.gov/hudapi/public/il/", if(geoid == "state") "statedata/" else "data/", query, "?year=", year, sep="") #build URL
  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data

  if (length(cont$data) == 0){ #check to see if results exist - if not warn use of errors in their input values
    stop("The query did not return any results. Please check if these input values follow the rules stated for each parameter. Maybe your authorization key is wrong? Maybe
         there hasn't been data recorded for this particular time period.")
  } else {
    if(geoid == "state") {
      res<-data.frame(geoid=character(1),
                      year=integer(1),
                      median_income=integer(1),
                      verylowil50p1=integer(1),
                      verylowil50p2=integer(1),
                      verylowil50p3=integer(1),
                      verylowil50p4=integer(1),
                      verylowil50p5=integer(1),
                      verylowil50p6=integer(1),
                      verylowil50p7=integer(1),
                      verylowil50p8=integer(1),
                      extremelowil30p1=integer(1),
                      extremelowil30p2=integer(1),
                      extremelowil30p3=integer(1),
                      extremelowil30p4=integer(1),
                      extremelowil30p5=integer(1),
                      extremelowil30p6=integer(1),
                      extremelowil30p7=integer(1),
                      extremelowil30p8=integer(1),
                      lowil80p1=integer(1),
                      lowil80p2=integer(1),
                      lowil80p3=integer(1),
                      lowil80p4=integer(1),
                      lowil80p5=integer(1),
                      lowil80p6=integer(1),
                      lowil80p7=integer(1),
                      lowil80p8=integer(1)
                      ) #build df


      res$geoid[1]=query
      res$year[1]=year
      res$median_income[1]=cont$data$median_income
      res$verylowil50p1[1]=cont$data$very_low$il50_p1
      res$verylowil50p2[1]=cont$data$very_low$il50_p2
      res$verylowil50p3[1]=cont$data$very_low$il50_p3
      res$verylowil50p4[1]=cont$data$very_low$il50_p4
      res$verylowil50p5[1]=cont$data$very_low$il50_p5
      res$verylowil50p6[1]=cont$data$very_low$il50_p6
      res$verylowil50p7[1]=cont$data$very_low$il50_p7
      res$verylowil50p8[1]=cont$data$very_low$il50_p8
      res$extremelowil30p1[1]=cont$data$extremely_low$il30_p1
      res$extremelowil30p2[1]=cont$data$extremely_low$il30_p2
      res$extremelowil30p3[1]=cont$data$extremely_low$il30_p3
      res$extremelowil30p4[1]=cont$data$extremely_low$il30_p4
      res$extremelowil30p5[1]=cont$data$extremely_low$il30_p5
      res$extremelowil30p6[1]=cont$data$extremely_low$il30_p6
      res$extremelowil30p7[1]=cont$data$extremely_low$il30_p7
      res$extremelowil30p8[1]=cont$data$extremely_low$il30_p8
      res$lowil80p1[1]=cont$data$low$il80_p1
      res$lowil80p2[1]=cont$data$low$il80_p2
      res$lowil80p3[1]=cont$data$low$il80_p3
      res$lowil80p4[1]=cont$data$low$il80_p4
      res$lowil80p5[1]=cont$data$low$il80_p5
      res$lowil80p6[1]=cont$data$low$il80_p6
      res$lowil80p7[1]=cont$data$low$il80_p7
      res$lowil80p8[1]=cont$data$low$il80_p8

    } else {
      res<-data.frame(geoid=integer(1),
                      town = character(1),
                      metrostatus = integer(1),
                      areaname = character(1),
                      year=integer(1),
                      median_income=integer(1),
                      verylowil50p1=integer(1),
                      verylowil50p2=integer(1),
                      verylowil50p3=integer(1),
                      verylowil50p4=integer(1),
                      verylowil50p5=integer(1),
                      verylowil50p6=integer(1),
                      verylowil50p7=integer(1),
                      verylowil50p8=integer(1),
                      extremelowil30p1=integer(1),
                      extremelowil30p2=integer(1),
                      extremelowil30p3=integer(1),
                      extremelowil30p4=integer(1),
                      extremelowil30p5=integer(1),
                      extremelowil30p6=integer(1),
                      extremelowil30p7=integer(1),
                      extremelowil30p8=integer(1),
                      lowil80p1=integer(1),
                      lowil80p2=integer(1),
                      lowil80p3=integer(1),
                      lowil80p4=integer(1),
                      lowil80p5=integer(1),
                      lowil80p6=integer(1),
                      lowil80p7=integer(1),
                      lowil80p8=integer(1)
      ) #build df

      res$geoid[1]=query
      res$year[1]=year
      res$town[1]=cont$data$town_name
      res$metrostatus[1]=cont$data$metro_status
      res$areaname[1]=cont$data$area_name
      res$median_income[1]=cont$data$median_income
      res$verylowil50p1[1]=cont$data$very_low$il50_p1
      res$verylowil50p2[1]=cont$data$very_low$il50_p2
      res$verylowil50p3[1]=cont$data$very_low$il50_p3
      res$verylowil50p4[1]=cont$data$very_low$il50_p4
      res$verylowil50p5[1]=cont$data$very_low$il50_p5
      res$verylowil50p6[1]=cont$data$very_low$il50_p6
      res$verylowil50p7[1]=cont$data$very_low$il50_p7
      res$verylowil50p8[1]=cont$data$very_low$il50_p8
      res$extremelowil30p1[1]=cont$data$extremely_low$il30_p1
      res$extremelowil30p2[1]=cont$data$extremely_low$il30_p2
      res$extremelowil30p3[1]=cont$data$extremely_low$il30_p3
      res$extremelowil30p4[1]=cont$data$extremely_low$il30_p4
      res$extremelowil30p5[1]=cont$data$extremely_low$il30_p5
      res$extremelowil30p6[1]=cont$data$extremely_low$il30_p6
      res$extremelowil30p7[1]=cont$data$extremely_low$il30_p7
      res$extremelowil30p8[1]=cont$data$extremely_low$il30_p8
      res$lowil80p1[1]=cont$data$low$il80_p1
      res$lowil80p2[1]=cont$data$low$il80_p2
      res$lowil80p3[1]=cont$data$low$il80_p3
      res$lowil80p4[1]=cont$data$low$il80_p4
      res$lowil80p5[1]=cont$data$low$il80_p5
      res$lowil80p6[1]=cont$data$low$il80_p6
      res$lowil80p7[1]=cont$data$low$il80_p7
      res$lowil80p8[1]=cont$data$low$il80_p8

    }
    colnames(res)[1] <- geoid
    return(res) #returns df as output of function
  }
}



#' hudchas
#' @name hudchas
#' @title hudchas
#' @description This function queries the CHAS API provided by US Department of Housing and Urban Development
#' @param type Queries the data based off
#' 1 - Nation
#' 2 - State
#' 3 - County
#' 4 - MCD
#' 5 - Place
#' @param stateId For types 2,3,4,5, you must provide a stateId. For 3,4,5
#' @param entityId For types 3,4,5, you must provide a fips code
#' @param year Gets the year that this data was recorded. Defaults to 2014-2018. There are specific year ranges
#' that are only accepted.
#' 2014-2018
#' 2013-2017
#' 2012-2016
#' 2011-2015
#' 2010-2014
#' 2009-2013
#' 2008-2012
#' 2007-2011
#' 2006-2010
#' @param key The API key for this user. You must go to HUD and sign up for an account and request for an API key.
#' @keywords Comprehensive Housing Affordability Strategy (CHAS) API
#' @export
#' @returns This function returns a dataframe containing CHAS data for a particular state.
#' For more details about these measurements, go to https://www.huduser.gov/portal/dataset/fmr-api.html
hudchas <- function(type, stateId = "", entityId = "", year = "2014-2018", key =  pkg.env$curr.key) {
  URL <- NULL
  call <- NULL
  cont <- NULL

  # Removing leading and ending spaces and converting all integer inputs to characters
  type <- paste(str_trim(as.character(type), side = "both"))
  stateId <- paste(str_trim(as.character(stateId), side = "both"))
  entityId <- paste(str_trim(as.character(entityId), side = "both"))
  year <- paste(str_trim(as.character(year), side = "both"))
  key <- paste(str_trim(as.character(key), side = "both"))

  # Build the URL for querying the data.
  if(type == "1") {
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type, "&year=", year,  sep="") #build URL
  }
  if(type == "2") {
    if(is.null(stateId)) stop("You need to specify a stateId for this type.")
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type, "&stateId=", stateId, "&year=", year,  sep="") #build URL
  }
  if(type == "3" || type == "4" || type == "5") {
    if(is.null(stateId) || is.null(entityId)) stop("You need to specify a stateId and entityId for this type.")
    URL <- paste("https://www.huduser.gov/hudapi/public/chas?type=", type, "&stateId=", stateId, "&entityId", entityId, "&year=", year,  sep="") #build URL
  }

  call<-try(GET(URL, add_headers(Authorization=paste("Bearer ", as.character(key)))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data

  if(length(cont) == 0) {
    stop("The query did not return any results. Please check if these input values follow the rules stated for each parameter. Maybe your authorization key is wrong? Maybe
         there hasn't been data recorded for this particular time period.")
  } else {
    # Parse for state type
    res<-data.frame(
      geoname = character(1),
      sumlevel= character(1),
      year= character(1),
      A1= numeric(1),
      A2= numeric(1),
      A3= numeric(1),
      A4= numeric(1),
      A5= numeric(1),
      A6= numeric(1),
      A7= numeric(1),
      A8= numeric(1),
      A9= numeric(1),
      A10= numeric(1),
      A11= numeric(1),
      A12= numeric(1),
      A13= numeric(1),
      A14= numeric(1),
      A15= numeric(1),
      A16= numeric(1),
      A17= numeric(1),
      A18= numeric(1),
      B1= numeric(1),
      B2= numeric(1),
      B3= numeric(1),
      B4= numeric(1),
      B5= numeric(1),
      B6= numeric(1),
      B7= numeric(1),
      B8= numeric(1),
      B9= numeric(1),
      C1= numeric(1),
      C2= numeric(1),
      C3= numeric(1),
      C4= numeric(1),
      C5= numeric(1),
      C6= numeric(1),
      D1= numeric(1),
      D2= numeric(1),
      D3= numeric(1),
      D4= numeric(1),
      D5= numeric(1),
      D6= numeric(1),
      D7= numeric(1),
      D8= numeric(1),
      D9= numeric(1),
      D10= numeric(1),
      D11= numeric(1),
      D12= numeric(1),
      E1= numeric(1),
      E2= numeric(1),
      E3= numeric(1),
      E5= numeric(1),
      E6= numeric(1),
      E7= numeric(1),
      E9= numeric(1),
      E10= numeric(1),
      E11= numeric(1),
      E13= numeric(1),
      E14= numeric(1),
      E15= numeric(1),
      E17= numeric(1),
      E18= numeric(1),
      E19= numeric(1),
      E21= numeric(1),
      E22= numeric(1),
      E23= numeric(1),
      F1= numeric(1),
      F2= numeric(1),
      F3= numeric(1),
      F5= numeric(1),
      F6= numeric(1),
      F7= numeric(1),
      F9= numeric(1),
      F10= numeric(1),
      F11= numeric(1),
      F13= numeric(1),
      F14= numeric(1),
      F15= numeric(1),
      F17= numeric(1),
      F18= numeric(1),
      F19= numeric(1),
      F21= numeric(1),
      F22= numeric(1),
      F23= numeric(1),
      G1= numeric(1),
      G2= numeric(1),
      G3= numeric(1),
      G5= numeric(1),
      G6= numeric(1),
      G7= numeric(1),
      G9= numeric(1),
      G10= numeric(1),
      G11= numeric(1),
      G13= numeric(1),
      G14= numeric(1),
      G15= numeric(1),
      G17= numeric(1),
      G18= numeric(1),
      G19= numeric(1),
      H1= numeric(1),
      H2= numeric(1),
      H4= numeric(1),
      H5= numeric(1),
      H7= numeric(1),
      H8= numeric(1),
      H10= numeric(1),
      H11= numeric(1),
      H13= numeric(1),
      H14= numeric(1),
      H16= numeric(1),
      I1= numeric(1),
      I2= numeric(1),
      I4= numeric(1),
      I5= numeric(1),
      I7= numeric(1),
      I8= numeric(1),
      I10= numeric(1),
      I11= numeric(1),
      I13= numeric(1),
      I14= numeric(1),
      I16= numeric(1),
      J1= numeric(1),
      J2= numeric(1),
      J4= numeric(1),
      J5= numeric(1),
      J7= numeric(1),
      J8= numeric(1),
      J10= numeric(1),
      J11= numeric(1),
      J13= numeric(1),
      J14= numeric(1),
      J16= numeric(1)
    ) #build df

    res$geoname <- if(is.null(cont[[1]]$geoname)) "NA" else cont[[1]]$geoname
    res$sumlevel <- if(is.null(cont[[1]]$sumlevel)) "NA" else cont[[1]]$sumlevel
    res$year <- if(is.null(cont[[1]]$year )) "NA" else cont[[1]]$year
    res$A1 <- if(is.null(cont[[1]]$A1 )) "NA" else cont[[1]]$A1
    res$A2 <- if(is.null(cont[[1]]$A2 )) "NA" else cont[[1]]$A2
    res$A3 <- if(is.null(cont[[1]]$A3 )) "NA" else cont[[1]]$A3
    res$A4 <- if(is.null(cont[[1]]$A4 )) "NA" else cont[[1]]$A4
    res$A5 <- if(is.null(cont[[1]]$A5 )) "NA" else cont[[1]]$A5
    res$A6 <- if(is.null(cont[[1]]$A6 )) "NA" else cont[[1]]$A6
    res$A7 <- if(is.null(cont[[1]]$A7 )) "NA" else cont[[1]]$A7
    res$A8 <- if(is.null(cont[[1]]$A8 )) "NA" else cont[[1]]$A8
    res$A9 <- if(is.null(cont[[1]]$A9 )) "NA" else cont[[1]]$A9
    res$A10 <- if(is.null(cont[[1]]$A10 )) "NA" else cont[[1]]$A10
    res$A11 <- if(is.null(cont[[1]]$A11 )) "NA" else cont[[1]]$A11
    res$A12 <- if(is.null(cont[[1]]$A12 )) "NA" else cont[[1]]$A12
    res$A13 <- if(is.null(cont[[1]]$A13 )) "NA" else cont[[1]]$A13
    res$A14 <- if(is.null(cont[[1]]$A14 )) "NA" else cont[[1]]$A14
    res$A15 <- if(is.null(cont[[1]]$A15 )) "NA" else cont[[1]]$A15
    res$A16 <- if(is.null(cont[[1]]$A16 )) "NA" else cont[[1]]$A16
    res$A17 <- if(is.null(cont[[1]]$A17 )) "NA" else cont[[1]]$A17
    res$A18 <- if(is.null(cont[[1]]$A18 )) "NA" else cont[[1]]$A18
    res$B1 <- if(is.null(cont[[1]]$B1 )) "NA" else cont[[1]]$B1
    res$B2 <- if(is.null(cont[[1]]$B2 )) "NA" else cont[[1]]$B2
    res$B3 <- if(is.null(cont[[1]]$B3 )) "NA" else cont[[1]]$B3
    res$B4 <- if(is.null(cont[[1]]$B4 )) "NA" else cont[[1]]$B4
    res$B5 <- if(is.null(cont[[1]]$B5 )) "NA" else cont[[1]]$B5
    res$B6 <- if(is.null(cont[[1]]$B6 )) "NA" else cont[[1]]$B6
    res$B7 <- if(is.null(cont[[1]]$B7 )) "NA" else cont[[1]]$B7
    res$B8 <- if(is.null(cont[[1]]$B8 )) "NA" else cont[[1]]$B8
    res$B9 <- if(is.null(cont[[1]]$B9 )) "NA" else cont[[1]]$B9
    res$C1 <- if(is.null(cont[[1]]$C1 )) "NA" else cont[[1]]$C1
    res$C2 <- if(is.null(cont[[1]]$C2 )) "NA" else cont[[1]]$C2
    res$C3 <- if(is.null(cont[[1]]$C3 )) "NA" else cont[[1]]$C3
    res$C4 <- if(is.null(cont[[1]]$C4 )) "NA" else cont[[1]]$C4
    res$C5 <- if(is.null(cont[[1]]$C5 )) "NA" else cont[[1]]$C5
    res$C6 <- if(is.null(cont[[1]]$C6 )) "NA" else cont[[1]]$C6
    res$D1 <- if(is.null(cont[[1]]$D1 )) "NA" else cont[[1]]$D1
    res$D2 <- if(is.null(cont[[1]]$D2 )) "NA" else cont[[1]]$D2
    res$D3 <- if(is.null(cont[[1]]$D3 )) "NA" else cont[[1]]$D3
    res$D4 <- if(is.null(cont[[1]]$D4 )) "NA" else cont[[1]]$D4
    res$D5 <- if(is.null(cont[[1]]$D5 )) "NA" else cont[[1]]$D5
    res$D6 <- if(is.null(cont[[1]]$D6 )) "NA" else cont[[1]]$D6
    res$D7 <- if(is.null(cont[[1]]$D7 )) "NA" else cont[[1]]$D7
    res$D8 <- if(is.null(cont[[1]]$D8 )) "NA" else cont[[1]]$D8
    res$D9 <- if(is.null(cont[[1]]$D9 )) "NA" else cont[[1]]$D9
    res$D10 <- if(is.null(cont[[1]]$D10 )) "NA" else cont[[1]]$D10
    res$D11 <- if(is.null(cont[[1]]$D11 )) "NA" else cont[[1]]$D11
    res$D12 <- if(is.null(cont[[1]]$D12 )) "NA" else cont[[1]]$D12
    res$E1 <- if(is.null(cont[[1]]$E1 )) "NA" else cont[[1]]$E1
    res$E2 <- if(is.null(cont[[1]]$E2 )) "NA" else cont[[1]]$E2
    res$E3 <- if(is.null(cont[[1]]$E3 )) "NA" else cont[[1]]$E3
    res$E5 <- if(is.null(cont[[1]]$E5 )) "NA" else cont[[1]]$E5
    res$E6 <- if(is.null(cont[[1]]$E6 )) "NA" else cont[[1]]$E6
    res$E7 <- if(is.null(cont[[1]]$E7 )) "NA" else cont[[1]]$E7
    res$E9 <- if(is.null(cont[[1]]$E9 )) "NA" else cont[[1]]$E9
    res$E10 <- if(is.null(cont[[1]]$E10 )) "NA" else cont[[1]]$E10
    res$E11 <- if(is.null(cont[[1]]$E11 )) "NA" else cont[[1]]$E11
    res$E13 <- if(is.null(cont[[1]]$E13 )) "NA" else cont[[1]]$E13
    res$E14 <- if(is.null(cont[[1]]$E14 )) "NA" else cont[[1]]$E14
    res$E15 <- if(is.null(cont[[1]]$E15 )) "NA" else cont[[1]]$E15
    res$E17 <- if(is.null(cont[[1]]$E17 )) "NA" else cont[[1]]$E17
    res$E18 <- if(is.null(cont[[1]]$E18 )) "NA" else cont[[1]]$E18
    res$E19 <- if(is.null(cont[[1]]$E19 )) "NA" else cont[[1]]$E19
    res$E21 <- if(is.null(cont[[1]]$E21 )) "NA" else cont[[1]]$E21
    res$E22 <- if(is.null(cont[[1]]$E22 )) "NA" else cont[[1]]$E22
    res$E23 <- if(is.null(cont[[1]]$E23 )) "NA" else cont[[1]]$E23
    res$F1 <- if(is.null(cont[[1]]$F1 )) "NA" else cont[[1]]$F1
    res$F2 <- if(is.null(cont[[1]]$F2 )) "NA" else cont[[1]]$F2
    res$F3 <- if(is.null(cont[[1]]$F3 )) "NA" else cont[[1]]$F3
    res$F5 <- if(is.null(cont[[1]]$F5 )) "NA" else cont[[1]]$F5
    res$F6 <- if(is.null(cont[[1]]$F6 )) "NA" else cont[[1]]$F6
    res$F7 <- if(is.null(cont[[1]]$F7 )) "NA" else cont[[1]]$F7
    res$F9 <- if(is.null(cont[[1]]$F9 )) "NA" else cont[[1]]$F9
    res$F10 <- if(is.null(cont[[1]]$F10 )) "NA" else cont[[1]]$F10
    res$F11 <- if(is.null(cont[[1]]$F11 )) "NA" else cont[[1]]$F11
    res$F13 <- if(is.null(cont[[1]]$F13 )) "NA" else cont[[1]]$F13
    res$F14 <- if(is.null(cont[[1]]$F14 )) "NA" else cont[[1]]$F14
    res$F15 <- if(is.null(cont[[1]]$F15 )) "NA" else cont[[1]]$F15
    res$F17 <- if(is.null(cont[[1]]$F17 )) "NA" else cont[[1]]$F17
    res$F18 <- if(is.null(cont[[1]]$F18 )) "NA" else cont[[1]]$F18
    res$F19 <- if(is.null(cont[[1]]$F19 )) "NA" else cont[[1]]$F19
    res$F21 <- if(is.null(cont[[1]]$F21 )) "NA" else cont[[1]]$F21
    res$F22 <- if(is.null(cont[[1]]$F22 )) "NA" else cont[[1]]$F22
    res$F23 <- if(is.null(cont[[1]]$F23 )) "NA" else cont[[1]]$F23
    res$G1 <- if(is.null(cont[[1]]$G1 )) "NA" else cont[[1]]$G1
    res$G2 <- if(is.null(cont[[1]]$G2 )) "NA" else cont[[1]]$G2
    res$G3 <- if(is.null(cont[[1]]$G3 )) "NA" else cont[[1]]$G3
    res$G5 <- if(is.null(cont[[1]]$G5 )) "NA" else cont[[1]]$G5
    res$G6 <- if(is.null(cont[[1]]$G6 )) "NA" else cont[[1]]$G6
    res$G7 <- if(is.null(cont[[1]]$G7 )) "NA" else cont[[1]]$G7
    res$G9 <- if(is.null(cont[[1]]$G9 )) "NA" else cont[[1]]$G9
    res$G10 <- if(is.null(cont[[1]]$G10 )) "NA" else cont[[1]]$G10
    res$G11 <- if(is.null(cont[[1]]$G11 )) "NA" else cont[[1]]$G11
    res$G13 <- if(is.null(cont[[1]]$G13 )) "NA" else cont[[1]]$G13
    res$G14 <- if(is.null(cont[[1]]$G14 )) "NA" else cont[[1]]$G14
    res$G15 <- if(is.null(cont[[1]]$G15 )) "NA" else cont[[1]]$G15
    res$G17 <- if(is.null(cont[[1]]$G17 )) "NA" else cont[[1]]$G17
    res$G18 <- if(is.null(cont[[1]]$G18 )) "NA" else cont[[1]]$G18
    res$G19 <- if(is.null(cont[[1]]$G19 )) "NA" else cont[[1]]$G19
    res$H1 <- if(is.null(cont[[1]]$H1 )) "NA" else cont[[1]]$H1
    res$H2 <- if(is.null(cont[[1]]$H2 )) "NA" else cont[[1]]$H2
    res$H4 <- if(is.null(cont[[1]]$H4 )) "NA" else cont[[1]]$H4
    res$H5 <- if(is.null(cont[[1]]$H5 )) "NA" else cont[[1]]$H5
    res$H7 <- if(is.null(cont[[1]]$H7 )) "NA" else cont[[1]]$H7
    res$H8 <- if(is.null(cont[[1]]$H8 )) "NA" else cont[[1]]$H8
    res$H10 <- if(is.null(cont[[1]]$H10 )) "NA" else cont[[1]]$H10
    res$H11 <- if(is.null(cont[[1]]$H11 )) "NA" else cont[[1]]$H11
    res$H13 <- if(is.null(cont[[1]]$H13 )) "NA" else cont[[1]]$H13
    res$H14 <- if(is.null(cont[[1]]$H14 )) "NA" else cont[[1]]$H14
    res$H16 <- if(is.null(cont[[1]]$H16 )) "NA" else cont[[1]]$H16
    res$I1 <- if(is.null(cont[[1]]$I1 )) "NA" else cont[[1]]$I1
    res$I2 <- if(is.null(cont[[1]]$I2 )) "NA" else cont[[1]]$I2
    res$I4 <- if(is.null(cont[[1]]$I4 )) "NA" else cont[[1]]$I4
    res$I5 <- if(is.null(cont[[1]]$I5 )) "NA" else cont[[1]]$I5
    res$I7 <- if(is.null(cont[[1]]$I7 )) "NA" else cont[[1]]$I7
    res$I8 <- if(is.null(cont[[1]]$I8 )) "NA" else cont[[1]]$I8
    res$I10 <- if(is.null(cont[[1]]$I10 )) "NA" else cont[[1]]$I10
    res$I11 <- if(is.null(cont[[1]]$I11 )) "NA" else cont[[1]]$I11
    res$I13 <- if(is.null(cont[[1]]$I13 )) "NA" else cont[[1]]$I13
    res$I14 <- if(is.null(cont[[1]]$I14 )) "NA" else cont[[1]]$I14
    res$I16 <- if(is.null(cont[[1]]$I16 )) "NA" else cont[[1]]$I16
    res$J1 <- if(is.null(cont[[1]]$J1 )) "NA" else cont[[1]]$J1
    res$J2 <- if(is.null(cont[[1]]$J2 )) "NA" else cont[[1]]$J2
    res$J4 <- if(is.null(cont[[1]]$J4 )) "NA" else cont[[1]]$J4
    res$J5 <- if(is.null(cont[[1]]$J5 )) "NA" else cont[[1]]$J5
    res$J7 <- if(is.null(cont[[1]]$J7 )) "NA" else cont[[1]]$J7
    res$J8 <- if(is.null(cont[[1]]$J8 )) "NA" else cont[[1]]$J8
    res$J10 <- if(is.null(cont[[1]]$J10 )) "NA" else cont[[1]]$J10
    res$J11 <- if(is.null(cont[[1]]$J11 )) "NA" else cont[[1]]$J11
    res$J13 <- if(is.null(cont[[1]]$J13 )) "NA" else cont[[1]]$J13
    res$J14 <- if(is.null(cont[[1]]$J14 )) "NA" else cont[[1]]$J14
    res$J16 <- if(is.null(cont[[1]]$J16 )) "NA" else cont[[1]]$J16

  }
  return(res)
}
