#' @import httr
#' @import tibble
#' @import R.cache

# Misc APIs provided by the HUD provide:

#' @name hud_nation_states_territories
#' @title US States and Territories
#' @description Get a list of state and US territories
#'   along with the corresponding fips code and
#'   abbreviation.
#' @param key The API key for this user. You must go to
#'   (US Department of Housing and Urban Development) HUD USER and sign up
#'   for an account and request for an API key.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords states territories
#' @export
#' @returns A dataframe containing details of all the states and territories
#'   in the US.
#' @seealso
#' * [rhud::hud_nation_states_territories()]
#' * [rhud::hud_state_metropolitan()]
#' * [rhud::hud_state_counties()]
#' * [rhud::hud_state_places()]
#' * [rhud::hud_state_minor_civil_divisions()]
#' @examples
#' \dontrun{
#' hud_nation_states_territories()
#' }
hud_nation_states_territories <- function(key = Sys.getenv("HUD_KEY"),
                                          to_tibble) {

  if (!curl::has_internet()) {
    stop("\nYou currently do not have internet access.", call. = FALSE)
  }

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble <- getOption("rhud_use_tibble")
    message(paste(
      "Outputted in tibble format",
      "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble <- FALSE
  }

  if (!is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)
  }

  urls <- "https://www.huduser.gov/hudapi/public/fmr/listStates"

  call <- memoizedCall(make_query_calls, urls, key)

  cont <- try(content(call), silent = TRUE) #parse returned data

  # A lot of the functions in this package use the states in this function call
  # to validate the input variables from user. Do not show this download bar
  # when validating.
  download_bar(done = 1, total = 1,
                 current = urls, error = 0)

  states <- as.data.frame(do.call("rbind", cont))
  states$state_num <- as.character(as.integer(states$state_num))

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if (!is.null(states) && nrow(states) > 1) {

    states$state_name <- unlist(states$state_name)
    states$state_code <- unlist(states$state_code)
    states$state_num <- unlist(states$state_num)
    states$category <- unlist(states$category)

    if (to_tibble == FALSE) {
      return(states)
    } else {
      return(as_tibble(states))
    }
  }
  return(NULL)
}

# Need to allow user to filter metropolitan areas similar to place, county,
# mcds.

#' @name hud_state_metropolitan
#' @title US Metropolitan Areas
#' @description Get details for all metropolitan areas for queried states with
#'   their name and (core based statistical area) cbsa code.
#' @param state The state to get all the metropolitan areas. Can be provided as
#'   the full name, fip code or abbreviation.
#' @param key The API key for this user. You must go to HUD and sign up for an
#'   account and request for an API key.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords cbsa
#' @export
#' @returns A dataframe containing details of metropolitan areas in state(s)
#' @seealso
#' * [rhud::hud_nation_states_territories()]
#' * [rhud::hud_state_metropolitan()]
#' * [rhud::hud_state_counties()]
#' * [rhud::hud_state_places()]
#' * [rhud::hud_state_minor_civil_divisions()]
#' @examples
#' \dontrun{
#'
#' hud_state_metropolitan("VA")
#'
#' }
hud_state_metropolitan <- function(state, key = Sys.getenv("HUD_KEY"),
                                   to_tibble) {

  if (!curl::has_internet()) {
    stop("\nYou currently do not have internet access.", call. = FALSE)
  }

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble <- getOption("rhud_use_tibble")
    message(paste(
      "Outputted in tibble format",
      "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble <- FALSE
  }


  if (!is.vector(state) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)
  }

  # The 'area_name' column gives information on the metropolitan place, the
  # abbreviation of state, and the stating that it is a MSA(metro stat areas)

  urls <- "https://www.huduser.gov/hudapi/public/fmr/listMetroAreas"

  call <- memoizedCall(make_query_calls, urls, key)

  cont <- try(content(call), silent = TRUE) #parse returned data

  download_bar(done = 1, total = 1,
               current = urls, error = 0)

  message("\n")

  metro <- as.data.frame(do.call(rbind, cont))

  # Do a regular expression of the area name column to parse it into
  # the name, the state location, and whether is is a HUD small areas...
  reged <- regexec("^.*,\\s([^ ]*)\\s(.*)", metro$area_name)

  met_name <- c()
  parsed_states <- c()
  classifications <- c()
  for (i in seq_len(length(reged))) {
    met_name <- c(met_name, substr(metro$area_name[i], 1,
                                   reged[[i]][2] - 2))
    parsed_states <- c(parsed_states, substr(metro$area_name[i], reged[[i]][2],
                             reged[[i]][2] + 1))
    classifications <- c(classifications, substr(metro$area_name[i],
                                                 reged[[i]][3],
                                                 nchar(metro$area_name[i])))
  }

  # Remove the area_name column.
  metro <- metro[, c(1, 3)]

  # Add new columns.
  metro$metro_name <- met_name
  metro$metro_state <- parsed_states
  metro$classifications <- classifications

  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(hud_nation_states_territories(
      key = Sys.getenv("HUD_KEY")))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("There is no matching fips code for one of the inputted states.")
    }
  }

  # Filter now based on state input.
  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    state_abbr <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][2]
  }
  if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    state_abbr <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][2]
  }
  if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state), ]) != 0) {
    state_abbr <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][2]
  }

  state_abbr <- unlist(state_abbr)

  metro <- metro[metro$metro_state == state_abbr, ]

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if (!is.null(metro) && nrow(metro) > 1) {
    metro$cbsa_code <- unlist(metro$cbsa_code)
    metro$area_name <- unlist(metro$area_name)
    metro$category <- unlist(metro$category)
    if (to_tibble == FALSE) {
      return(metro)
    } else {
      return(as_tibble(metro))
    }

  }

  return(NULL)
}


#' @name hud_state_counties
#' @title US Counties
#' @description Get a dataframe with details describing the county(s) located
#'   within the queried state(s).
#' @param state The state(s) to get all county(s).
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords counties
#' @seealso
#' * [rhud::hud_nation_states_territories()]
#' * [rhud::hud_state_metropolitan()]
#' * [rhud::hud_state_counties()]
#' * [rhud::hud_state_places()]
#' * [rhud::hud_state_minor_civil_divisions()]
#' @export
#' @returns A dataframe containing all counties within state(s).
#' @examples
#' \dontrun{
#' hud_state_counties("CA")
#' hud_state_counties("Virginia")
#' hud_state_counties("51")
#' }
hud_state_counties <- function(state, key = Sys.getenv("HUD_KEY"),
                               to_tibble) {

  if (!curl::has_internet()) {
    stop("\nYou currently do not have internet access.", call. = FALSE)
  }

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble <- getOption("rhud_use_tibble")
    message(paste(
      "Outputted in tibble format",
      "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble <- FALSE
  }

  if (!is.vector(state) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)
  }

  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(hud_nation_states_territories(
      key = Sys.getenv("HUD_KEY")))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("\nThere is no matching fips code for one of the inputted states.",
           call. = FALSE)
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][2]
  }
  if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][2]
  }
  if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][2]
  }

  # Allow the user to specify multiple years states to query for... Just stack
  # them.

  urls <- paste("https://www.huduser.gov/hudapi/public/fmr/listCounties/",
               unlist(fip_code), sep = "")
  counties <- misc_do_query_call(urls, key, to_tibble)

  # A very ambiguous check. Assume that error and only errors return 1 row of
  # text explaining so error.
  if (!is.null(counties) && nrow(counties) > 1) {
    counties$state_code <- unlist(counties$state_code)
    counties$fips_code <- unlist(counties$fips_code)
    counties$county_name <- unlist(counties$county_name)
    counties$town_name <- unlist(counties$town_name)
    counties$category <- unlist(counties$category)

    if (to_tibble == FALSE) {
      return(counties)
    } else {
      return(as_tibble(counties))
    }
  }

  return(NULL)
}

#' @name hud_state_places
#' @title US Places
#' @description Get a list of all places in state(s).
#' @param state The state(s) to get all places.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords places
#' @export
#' @seealso
#' * [rhud::hud_nation_states_territories()]
#' * [rhud::hud_state_metropolitan()]
#' * [rhud::hud_state_counties()]
#' * [rhud::hud_state_places()]
#' * [rhud::hud_state_minor_civil_divisions()]
#' @returns A dataframe containing details of places in state(s).
#' @examples
#' \dontrun{
#' hud_state_places("CA")
#' hud_state_places("Virginia")
#' hud_state_places("51")
#' }
hud_state_places <- function(state, key = Sys.getenv("HUD_KEY"),
                             to_tibble) {

  if (!curl::has_internet()) {
    stop("\nYou currently do not have internet access.", call. = FALSE)
  }

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble <- getOption("rhud_use_tibble")
    message(paste(
      "Outputted in tibble format",
      "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble <- FALSE
  }


  if (!is.vector(state) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)
  }

  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(hud_nation_states_territories(
      key = Sys.getenv("HUD_KEY")))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("\nThere is no matching fips code for one of the inputted states.",
           call. = FALSE)
    }
  }


  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][3]
  }
  if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][3]
  }
  if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][3]
  }

  urls <- paste("https://www.huduser.gov/hudapi/public/chas/listCities/",
               unlist(fip_code),
               sep = "")

  places <- misc_do_query_call(urls, key, to_tibble)

  if (!is.null(places) && nrow(places) > 1) {
    places$statecode <- unlist(places$statecode)
    places$entityId <- unlist(places$entityId)
    places$placename <- unlist(places$placename)
    if (to_tibble == FALSE) {
      return(places)
    } else {
      return(as_tibble(places))
    }
  }

  return(NULL)
}

#' @name hud_state_minor_civil_divisions
#' @title US Minor Civil Divisions
#' @description Get a list of all minor civil divisions in a state
#' @param state The state to get all mcd(s).
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble If TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords mcd
#' @export
#' @seealso
#' * [rhud::hud_nation_states_territories()]
#' * [rhud::hud_state_metropolitan()]
#' * [rhud::hud_state_counties()]
#' * [rhud::hud_state_places()]
#' * [rhud::hud_state_minor_civil_divisions()]
#' @returns A dataframe containing details of minor civil divisions in state(s).
#' @examples
#' \dontrun{
#' hud_state_minor_civil_divisions("CA")
#' hud_state_minor_civil_divisions("Virginia")
#' hud_state_minor_civil_divisions("51")
#' }
hud_state_minor_civil_divisions <- function(state,
                                            key = Sys.getenv("HUD_KEY"),
                                            to_tibble) {

  if (!curl::has_internet()) {
    stop("\nYou currently do not have internet access.", call. = FALSE)
  }

  if (!is.null(getOption("rhud_use_tibble")) && missing(to_tibble)) {
    to_tibble <- getOption("rhud_use_tibble")
    message(paste(
      "Outputted in tibble format",
      "because it was set using `options(rhud_use_tibble = TRUE)`\n"))
  } else if (missing(to_tibble)) {
    to_tibble <- FALSE
  }


  if (!is.vector(state) || !is.vector(key)) {
    stop(paste("\nMake sure all inputs are of type vector. ",
               "Check types with typeof([variable]). ",
               "If list try unlist([variable]); ",
               "if matrix try as.vector([variable])", sep = ""), call. = FALSE)
  }

  if (key == "") {
    stop(paste("\nDid you forget to set the key? ",
               "Please go to https://www.huduser.gov/",
               "hudapi/public/register?comingfrom=1 ",
               "to sign up and get a token. Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)", sep = ""), call. = FALSE)
  }

  if (all(nchar(state) == 2)) state <- toupper(state)
  if (all(nchar(state) > 2)) state <- capitalize(tolower(state))

  if (is.null(pkg.env$state)) {
    pkg.env$state <- suppressMessages(hud_nation_states_territories(
      key = Sys.getenv("HUD_KEY")))
  }

  for (i in seq_len(length(state))) {
    if (!any(as.character(state[i]) == pkg.env$state)) {
      stop("\nThere is no matching fips code for one of the inputted states.",
           call. = FALSE)
    }
  }

  # Allow user to supply state name or state abbr or state fips as inputs.
  if (nrow(pkg.env$state[pkg.env$state$state_name %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_name %in%
                                as.character(state), ][3]
  }
  if (nrow(pkg.env$state[pkg.env$state$state_code %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_code %in%
                                as.character(state), ][3]
  }
  if (nrow(pkg.env$state[as.character(pkg.env$state$state_num) %in%
                        as.character(state), ]) != 0) {
    fip_code <- pkg.env$state[pkg.env$state$state_num %in%
                                as.character(state), ][3]
  }

  urls <- paste("https://www.huduser.gov/hudapi/public/chas/listMCDs/",
               unlist(fip_code),
               sep = "")

  mcd <- misc_do_query_call(urls, key, to_tibble)

  if (!is.null(mcd) && nrow(mcd) > 1) {
    mcd$statecode <- unlist(mcd$statecode)
    mcd$entityId <- unlist(mcd$entityId)
    mcd$mcdname <- unlist(mcd$mcdname)
    if (to_tibble == FALSE) {
      return(mcd)
    } else {
      return(to_tibble(mcd))
    }
  }

  return(NULL)
}
