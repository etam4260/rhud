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
                                    to_tibble = getOption("rhud_use_tibble",
                                                          FALSE)) {
  res <- NULL
  is_internet_available()

  misc_input_check_cleansing(NULL, key, "nation-state")


  urls <- "https://www.huduser.gov/hudapi/public/fmr/listStates"
  call <- memoizedCall(make_query_calls, urls, key)

  states <- misc_do_query_call(urls, key, to_tibble = FALSE)


  # cont <- try(content(call), silent = TRUE) #parse returned data
  # states <- as.data.frame(do.call("rbind", cont))

  states$state_num <- as.character(as.integer(states$state_num))

  states
}


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
                                  to_tibble = getOption("rhud_use_tibble",
                                                        FALSE)) {

  res <- NULL
  is_internet_available()

  cleaned <- misc_input_check_cleansing(state, key, "state-metro")

  state_abbr <- cleaned[1]
  key <- cleaned[2]

  # The 'area_name' column gives information on the metropolitan place, the
  # abbreviation of state, and the stating that it is a MSA(metro state areas)

  urls <- "https://www.huduser.gov/hudapi/public/fmr/listMetroAreas"
  metro <- misc_do_query_call(urls, key, to_tibble = FALSE)

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

  state_abbr <- unlist(state_abbr)

  metro <- metro[metro$metro_state == state_abbr, ]

  metro
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
                               to_tibble = getOption("rhud_use_tibble",
                                                     FALSE)) {
  res <- NULL
  is_internet_available()

  cleaned <- misc_input_check_cleansing(state, key, "state-county")
  state_abbr <- cleaned$fip_code
  key <- cleaned$key

  # Allow the user to specify multiple years states to query for... Just stack
  # them.

  urls <- paste("https://www.huduser.gov/hudapi/public/fmr/listCounties/",
               unlist(state_abbr), sep = "")
  counties <- misc_do_query_call(urls, key, to_tibble)


  counties
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
                             to_tibble = getOption("rhud_use_tibble", FALSE)) {

  res <- NULL
  is_internet_available()

  cleaned <- misc_input_check_cleansing(state, key, "state-place")
  fip_code <- cleaned$fip_code
  key <- cleaned$key


  urls <- paste("https://www.huduser.gov/hudapi/public/chas/listCities/",
               unlist(fip_code),
               sep = "")

  places <- misc_do_query_call(urls, key, to_tibble)

  places
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
                                    to_tibble = getOption("rhud_use_tibble",
                                                          FALSE)) {
  res <- NULL
  is_internet_available()

  cleaned <- misc_input_check_cleansing(state, key, "state-mcd")
  fip_code <- cleaned$fip_code
  key <- cleaned$key

  urls <- paste("https://www.huduser.gov/hudapi/public/chas/listMCDs/",
               unlist(fip_code),
               sep = "")

  mcd <- misc_do_query_call(urls, key, to_tibble)

  mcd
}
