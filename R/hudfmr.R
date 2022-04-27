#' @name hud_fmr_state_metroareas
#' @title hud_fmr_state_metroareas
#' @description This function queries for a state and returns the
#'   FMR calculation
#'   at a metroarea resolution for all metroareas in this state.
#' @param state The state to query for.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @keywords Fair Markets Rent API
#' @export
#' @returns Fair markets rent for metroareas.
hud_fmr_state_metroareas <- function(state,
                                     year = format(Sys.Date() - 365, "%Y"),
                                     key = Sys.getenv("HUD_KEY")) {
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
    urls <- paste("https://www.huduser.gov/hudapi/public/fmr/",
                 "statedata/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- try(GET(urls, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, nrow(allqueries))

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {
      res <- as.data.frame(do.call(rbind, cont$data$metroareas))
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      list_res[[i]] <- res
    }
  }
  message("\n")

  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries:", paste(error_urls,
                                                            collapse = "\n"),
                  "It is possible that your key maybe invalid,",
                  "there isn't any data for these parameters,",
                  "or you have reached the maximum number of API",
                  "calls per minute. If you think this is wrong please",
                  "report it at https://github.com/etam4260/hudr/issues.",
                  sep = " "))
  }


  if (length(list_res) != 0) {
    res <- as.data.frame(do.call(rbind, list_res))
    res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

    return(res)
  }
  return(NULL)
}


#' @name hud_fmr_state_counties
#' @title hud_fmr_state_counties
#' @description This function queries for a state and returns the
#'   FMR calculation
#'   at a county resolution for all counties in this state.
#' @param state The state to query for.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @keywords Fair Markets Rent API
#' @export
#' @returns Fair markets rent for counties
hud_fmr_state_counties <- function(state, year = format(Sys.Date() - 365, "%Y"),
                                   key = Sys.getenv("HUD_KEY")) {
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
    urls <- paste("https://www.huduser.gov/hudapi/public/fmr/",
                 "statedata/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- try(GET(urls, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, nrow(allqueries))

    if ("error" %in% names(cont)) {
      error_urls <- c(error_urls, urls)
    } else {
      res <- as.data.frame(do.call(rbind, cont$data$counties))
      res$query <- allqueries$query[i]
      res$year <- allqueries$year[i]
      list_res[[i]] <- res
    }
  }
  message("\n")


  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries:", paste(error_urls,
                                                            collapse = "\n"),
                  "It is possible that your key maybe invalid,",
                  "there isn't any data for these parameters,",
                  "or you have reached the maximum number of API",
                  "calls per minute. If you think this is wrong please",
                  "report it at https://github.com/etam4260/hudr/issues.",
                  sep = " "))
  }


  if (length(list_res) != 0) {
    res <- as.data.frame(do.call(rbind, list_res))
    res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

    return(res)
  }
  return(NULL)

}

#' @name hud_fmr_county_zip
#' @title hud_fmr_county_zip
#' @description This function queries for a county and returns FMR calculation.
#'    If the county is not
#'    a small area, it will return only single
#'    measurement for that county. If the county is considered a small area,
#'    it will return data at a zip code level.
#' @param county A county to query for.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @keywords Fair Markets Rent API
#' @export
#' @returns
hud_fmr_county_zip <- function(county, year = format(Sys.Date() - 365, "%Y"),
                               key = Sys.getenv("HUD_KEY")) {

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
    urls <- paste("https://www.huduser.gov/hudapi/public/fmr/",
                 "data/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- try(GET(urls, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, nrow(allqueries))

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

      # Now merge them both
    }
  }
  message("\n")


  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries:", paste(error_urls,
                                                            collapse = "\n"),
                  "It is possible that your key maybe invalid,",
                  "there isn't any data for these parameters,",
                  "or you have reached the maximum number of API",
                  "calls per minute. If you think this is wrong please",
                  "report it at https://github.com/etam4260/hudr/issues.",
                  sep = " "))
  }


  if (length(list_res) != 0) {
    if (length(list_res) != 1) {
      res <- as.data.frame(do.call(rbind, list_res))
      res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

      return(res)
    }
    return(list_res[[1]])
  }
  return(NULL)
}



#' @name hud_fmr_metroarea_zip
#' @title hud_fmr_metroarea_zip
#' @description This function queries for a metroarea and returns
#'    FMR calculation. If the metroarea is not
#'    a small area, it will return only single
#'    measurement for that metroarea. If the metrarea is considered a
#'    small area, it will return data at a zip code level.
#' @param metroarea A metroarea to query for.
#' @param year Gets the year that this data was recorded.
#'   Can specify multiple years. Default is the
#'   previous year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @keywords Fair Markets Rent API
#' @export
#' @returns
hud_fmr_metroarea_zip <- function(metroarea,
                                  year = format(Sys.Date() - 365, "%Y"),
                                  key = Sys.getenv("HUD_KEY")) {
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
    urls <- paste("https://www.huduser.gov/hudapi/public/fmr/",
                 "data/",
                 allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- try(GET(urls, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, nrow(allqueries))

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
  }
  message("\n")


  if (length(error_urls) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries:", paste(error_urls,
                                                            collapse = "\n"),
                  "It is possible that your key maybe invalid,",
                  "there isn't any data for these parameters,",
                  "or you have reached the maximum number of API",
                  "calls per minute. If you think this is wrong please",
                  "report it at https://github.com/etam4260/hudr/issues.",
                  sep = " "))
  }

  if (length(list_res) != 0) {

    if (length(list_res) != 1) {
      res <- as.data.frame(do.call(rbind, list_res))
      res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

      return(res)
    }
    return(list_res[[1]])
  }
  return(NULL)
}
