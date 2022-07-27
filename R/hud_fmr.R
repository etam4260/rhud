#' @import tibble
#' @import R.cache

#' @name hud_fmr_state_metroareas
#' @title Fair Markets Rent State Queries for Metroareas
#' @description This function queries for state(s) and returns the
#'   (Fair Markets Rent) FMR calculation
#'   at a metroarea resolution for all metroareas in the state query.
#' @param state A character or numeric vector: the state(s) to query for.
#'   Can be abbreviation(s),
#'   fip code(s), or full name(s).
#' @param year A character of numeric vector: gets the year
#'   that this data was recorded.
#'   Can specify multiple year(s). Default is the
#'   previous year.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Fair Markets Rent API
#' @seealso
#' * [rhud::hud_fmr_state_metroareas()]
#' * [rhud::hud_fmr_state_counties()]
#' * [rhud::hud_fmr_metroarea_zip()]
#' * [rhud::hud_fmr_county_zip()]
#' * [rhud::hud_fmr()]
#' @export
#' @returns A data frame with Fair Markets Rent for metro areas in state(s) for
#'   all combinations of "state" and "year" inputs.
#' @examples
#' \dontrun{
#'
#' hud_fmr_state_metroareas("VA", year = c(2021))
#'
#' hud_fmr_state_metroareas("Alabama", year = c(2021))
#'
#' hud_fmr_state_metroareas("24", year = c(2021))
#'
#' }
hud_fmr_state_metroareas <- function(state,
                                     year = format(Sys.Date() - 365, "%Y"),
                                     key = Sys.getenv("HUD_KEY"),
                                     to_tibble = getOption("rhud_use_tibble",
                                                           FALSE)) {
  is_internet_available()

  args <- fmr_il_input_check_cleansing(state, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  error_urls <- c()

  # Create all combinations of query and year...
  allqueries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  list_res <- c()
  for (i in seq_len(nrow(allqueries))) {
    # Build the urls for querying the data.

    urls <- paste(get_hud_host_name(),
                 "fmr/",
                 "statedata/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- R.cache::memoizedCall(make_query_calls, urls, key)

    cont <- try(content(call), silent = TRUE)

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {
      res <- as.data.frame(do.call(rbind, cont$data$metroareas))
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      list_res[[i]] <- res
    }

    download_bar(done = i, total = nrow(allqueries),
                 current = urls, error = length(error_urls))
  }


  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries: \n\n",
                  paste(paste("*", error_urls, sep = " "), collapse = "\n"),
                  "\n\nIt is possible that your key maybe invalid or ",
                  "there isn't any data for these parameters, ",
                  "If you think this is wrong please ",
                  "report it at https://github.com/etam4260/rhud/issues.",
                  sep = ""), call. = FALSE)
  }


  if (length(list_res) != 0) {

    res <- as.data.frame(do.call(rbind, list_res))
    res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

    if (to_tibble) {
      res <- tibble(res)
    }

  }

  res
}


#' @name hud_fmr_state_counties
#' @title Fair Markets Rent State Queries for Counties
#' @description This function queries for state(s) and returns the
#'   (Fair Markets Rent) FMR calculation
#'   at a county resolution for all counties in state input.
#' @param state A character or numeric vector: the state(s) to query for.
#'   Can be abbreviation(s),
#'   fip code(s), or full name(s).
#' @param year A character of numeric vector: gets the year
#'   that this data was recorded.
#'   Can specify multiple year(s). Default is the
#'   previous year.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Fair Markets Rent API
#' @seealso
#' * [rhud::hud_fmr_state_metroareas()]
#' * [rhud::hud_fmr_state_counties()]
#' * [rhud::hud_fmr_metroarea_zip()]
#' * [rhud::hud_fmr_county_zip()]
#' * [rhud::hud_fmr()]
#' @export
#' @returns A data frame with fair markets rent for counties in states for
#'   all combinations of "state" and "year" inputs.
#' @examples
#' \dontrun{
#' hud_fmr_state_counties("VA", year = c(2021))
#'
#' hud_fmr_state_counties("Alabama", year = c(2021))
#'
#' hud_fmr_state_counties("24", year = c(2021))
#' }
hud_fmr_state_counties <- function(state, year = format(Sys.Date() - 365, "%Y"),
                                   key = Sys.getenv("HUD_KEY"),
                                   to_tibble = getOption("rhud_use_tibble",
                                                         FALSE)) {
  is_internet_available()

  args <- fmr_il_input_check_cleansing(state, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  error_urls <- c()

  # Create all combinations of query and year...
  allqueries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  list_res <- c()
  for (i in seq_len(nrow(allqueries))) {
    # Build the urls for querying the data.
    urls <- paste(get_hud_host_name(),
                 "fmr/",
                 "statedata/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- R.cache::memoizedCall(make_query_calls, urls, key)

    cont <- try(content(call), silent = TRUE)

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {
      res <- as.data.frame(do.call(rbind, cont$data$counties))
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      list_res[[i]] <- res
    }

    download_bar(done = i, total = nrow(allqueries),
                 current = urls, error = length(error_urls))

  }



  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries: \n\n",
                  paste(paste("*", error_urls, sep = " "), collapse = "\n"),
                  "\n\nIt is possible that your key maybe invalid or ",
                  "there isn't any data for these parameters, ",
                  "If you think this is wrong please ",
                  "report it at https://github.com/etam4260/rhud/issues.",
                  sep = ""), call. = FALSE)
  }


  if (length(list_res) != 0) {

    res <- as.data.frame(do.call(rbind, list_res))
    res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

    if (to_tibble) {
      res <- as_tibble(res)
    }

  }

  res
}

#' @name hud_fmr_county_zip
#' @title Fair Markets Rent County Queries for Zip
#' @description This function queries for county(s) and returns FMR calculation.
#'    If the county is not
#'    a small area, it will return only single
#'    measurement for that county. If the county is considered a small area,
#'    it will return data at a zip code level.
#' @param county A character or numeric vector: a county to query for.
#'   Must be provided as a 5 digit fipcode.
#' @param year A character of numeric vector: gets the year
#'   that this data was recorded.
#'   Can specify multiple year(s). Default is the
#'   previous year.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Fair Markets Rent API
#' @seealso
#' * [rhud::hud_fmr_state_metroareas()]
#' * [rhud::hud_fmr_state_counties()]
#' * [rhud::hud_fmr_metroarea_zip()]
#' * [rhud::hud_fmr_county_zip()]
#' * [rhud::hud_fmr()]
#' @export
#' @returns A data frame with fair markets rent for zip codes in counties for
#'   all combinations of "county" and "year" inputs.
#' @examples
#' \dontrun{
#' hud_fmr_county_zip("5100199999", year = c(2021))
#'
#' hud_fmr_county_zip("5100199999", year = c("2021"))
#'
#' hud_fmr_county_zip(5151099999, year = c(2021))
#' }
hud_fmr_county_zip <- function(county, year = format(Sys.Date() - 365, "%Y"),
                               key = Sys.getenv("HUD_KEY"),
                               to_tibble = getOption("rhud_use_tibble",
                                                     FALSE)) {
  is_internet_available()

  args <- fmr_il_input_check_cleansing(county, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  error_urls <- c()

  allqueries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  list_res <- c()
  for (i in seq_len(nrow(allqueries))) {
    # Build the urls for querying the data.
    urls <- paste(get_hud_host_name(),
                 "fmr/",
                 "data/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- R.cache::memoizedCall(make_query_calls, urls, key)

    cont <- try(content(call), silent = TRUE)

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {
      if (cont$data$smallarea_status == "0") {
        # Returns just county
        res <- as.data.frame(do.call(cbind, cont$data$basicdata))

        res$query <- allqueries$query[i]
        res$year <- allqueries$year[i]
        res$zip_code <- ""

        res$county_name <- cont$data$county_name
        res$counties_msa <- cont$data$counties_msa
        res$town_name <- cont$data$town_name
        res$metro_status <- cont$data$metro_status
        res$metro_name <- cont$data$metro_name
        res$area_name <- cont$data$area_name
        res$smallarea_status <- cont$data$smallarea_status

        list_res[[i]] <- res

      } else {
        # Returns zip code level data
        res <- as.data.frame(do.call(rbind, cont$data$basicdata))
        res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

        res$query <- allqueries$query[i]
        res$year <- allqueries$year[i]

        res$county_name <- cont$data$county_name
        res$counties_msa <- cont$data$counties_msa
        res$town_name <- cont$data$town_name
        res$metro_status <- cont$data$metro_status
        res$metro_name <- cont$data$metro_name
        res$area_name <- cont$data$area_name
        res$smallarea_status <- cont$data$smallarea_status

        list_res[[i]] <- res
      }
    }

    download_bar(done = i, total = nrow(allqueries),
                 current = urls, error = length(error_urls))

  }



  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries: \n\n",
                  paste(paste("*", error_urls, sep = " "), collapse = "\n"),
                  "\n\nIt is possible that your key maybe invalid or ",
                  "there isn't any data for these parameters, ",
                  "If you think this is wrong please ",
                  "report it at https://github.com/etam4260/rhud/issues.",
                  sep = ""), call. = FALSE)
  }


  if (length(list_res) != 0) {

    if (length(list_res) != 1) {

      res <- as.data.frame(do.call(rbind, list_res))
      res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

      if (to_tibble) {
        res <- as_tibble(res)
      }
    }

  }

  res
}



#' @name hud_fmr_metroarea_zip
#' @title Fair Markets Rent Metroarea Queries for Zip
#' @description This function queries for metroarea(s) and returns
#'    FMR calculation. If the metroarea is not
#'    a small area, it will return only single
#'    measurement for that metroarea. If the metrarea is considered a
#'    small area, it will return data at a zip code level.
#' @param metroarea A character vector: metroarea(s) to query for.
#' @param year A character of numeric vector: gets the year
#'   that this data was recorded.
#'   Can specify multiple year(s). Default is the
#'   previous year.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @keywords Fair Markets Rent API
#' @seealso
#' * [rhud::hud_fmr_state_metroareas()]
#' * [rhud::hud_fmr_state_counties()]
#' * [rhud::hud_fmr_metroarea_zip()]
#' * [rhud::hud_fmr_county_zip()]
#' * [rhud::hud_fmr()]
#' @export
#' @returns A data frame with fair markets rent for zip codes in metro areas for
#'   all combinations of "metroarea" and "year" inputs.
#' @examples
#' \dontrun{
#' hud_fmr_metroarea_zip("METRO47900M47900", year = c(2018))
#'
#' hud_fmr_metroarea_zip("METRO29180N22001", year = c(2019))
#'
#' hud_fmr_metroarea_zip("METRO10380M10380", year = c(2020))
#' }
hud_fmr_metroarea_zip <- function(metroarea,
                                  year = format(Sys.Date() - 365, "%Y"),
                                  key = Sys.getenv("HUD_KEY"),
                                  to_tibble = getOption("rhud_use_tibble",
                                                        FALSE)) {
  is_internet_available()

  args <- fmr_il_input_check_cleansing(metroarea, year, key)
  query <- args[[1]]
  year <- args[[2]]
  key <- args[[3]]

  error_urls <- c()

  allqueries <- expand.grid(query = query, year = year,
                            stringsAsFactors = FALSE)

  list_res <- c()
  for (i in seq_len(nrow(allqueries))) {
    # Build the urls for querying the data.
    urls <- paste(get_hud_host_name(),
                 "fmr/",
                 "data/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- R.cache::memoizedCall(make_query_calls, urls, key)

    cont <- try(content(call), silent = TRUE)

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {

      if (cont$data$smallarea_status == "0") {
        # Returns just county
        res <- as.data.frame(do.call(cbind, cont$data$basicdata))

        res$query <- allqueries$query[i]
        res$year <- allqueries$year[i]
        res$zip_code <- ""

        res$county_name <- cont$data$county_name
        res$counties_msa <- cont$data$counties_msa
        res$town_name <- cont$data$town_name
        res$metro_status <- cont$data$metro_status
        res$metro_name <- cont$data$metro_name
        res$area_name <- cont$data$area_name
        res$smallarea_status <- cont$data$smallarea_status

        list_res[[i]] <- res

      } else {
        # Returns zip code level data
        res <- as.data.frame(do.call(rbind, cont$data$basicdata))
        res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

        res$query <- allqueries$query[i]
        res$year <- allqueries$year[i]

        res$county_name <- cont$data$county_name
        res$counties_msa <- cont$data$counties_msa
        res$town_name <- cont$data$town_name
        res$metro_status <- cont$data$metro_status
        res$metro_name <- cont$data$metro_name
        res$area_name <- cont$data$area_name
        res$smallarea_status <- cont$data$smallarea_status

        list_res[[i]] <- res
      }
    }

    download_bar(done = i, total = nrow(allqueries),
                 current = urls, error = length(error_urls))

  }



  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries: \n\n",
                  paste(paste("*", error_urls, sep = " "), collapse = "\n"),
                  "\n\nIt is possible that your key maybe invalid or ",
                  "there isn't any data for these parameters, ",
                  "If you think this is wrong please ",
                  "report it at https://github.com/etam4260/rhud/issues.",
                  sep = ""), call. = FALSE)
  }

  if (length(list_res) != 0) {

    if (length(list_res) != 1) {

      res <- as.data.frame(do.call(rbind, list_res))
      res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

      if (to_tibble) {
        res <- as_tibble(res)
      }

    }

    # return(list_res[[1]])

  }

  res
}
