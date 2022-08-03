#' @import httr
#' @import tibble
#' @import R.cache


#' @name chas_do_query_calls
#' @title API Calls for Comprehensive Housing and Affordability Helper
#' @description Helper function for making the query calls to
#'   Comprehensive Housing and Affordability Strategy (CHAS)
#'   API endpoint as well as concatenating all response objects to
#'   be returned to the user. Collect error urls and warn users and cache the
#'   queries and show the download bar.
#' @param urls A character vector: the urls to query for.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return as tibble else dataframe.
#' @returns A tibble or dataframe of all response bodies.
#' @noRd
#' @noMd
chas_do_query_calls <- function(urls, key, to_tibble) {
  # Form all query calls...
  list_res <- c()
  error_urls <- character(0)

  # These measurements are hardcoded in, but a more effective method might
  # be systematic checks to find all unique columns names from all CHAS
  # datasets.

  all_measurements <- hud_chas_col_names()

  for (i in seq_len(length(urls))) {

    url <- urls[i]

    call <- R.cache::memoizedCall(make_query_calls, url, key)
    processed_code <- process_status_codes(call)

    if (!is.null(processed_code)) {

      error_urls <- list(c(processed_code, url))

    } else {
      cont <- try(content(call), silent = TRUE)

      not_measured <- all_measurements[!all_measurements %in%
                                         names(unlist(cont[[1]]))]
      # Check this CHAS data does not have data defined for
      # all expected fields. If so fill them in with NA's.
      if (length(not_measured) >= 1) {

        extra_mes <- rep(NA, length(not_measured))
        names(extra_mes) <- not_measured

        list_res[[i]] <- c(unlist(cont[[1]]), extra_mes)

      } else {

        list_res[[i]] <- unlist(cont[[1]])

      }
    }

    download_bar(done = i, total = length(urls),
                 current = url, error = length(error_urls))

  }

  print_resp_warning_messages(error_urls)
  if_tibble_return(list_res = list_res,
                   to_tibble = to_tibble,
                   api = "chas")
}


#' @name cw_do_query_calls
#' @title API Calls for USPS Crosswalk Helper
#' @description Helper function for making the query calls to
#'   USPS Crosswalk API endpoint as well as concatenating all response objects
#'   to be returned to the user.  Collect error urls and warn users and cache
#'   the
#'   queries and show the download bar.
#' @param urls A character vector : the urls to query for.
#' @param query A character vector : the geoids to query for.
#' @param year A character or numeric vector : the years to query for.
#' @param quarter A character or numeric vector: the quarters in the year
#'   to query for.
#' @param primary_geoid A character vector: the first geoid part of a
#'   function call. For example,
#'   hud_cw_zip_tract() has zip as first geoid and tract as second geoid.
#' @param secondary_geoid A character vector: the second geoid part of
#'   function call.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return as tibble else dataframe.
#' @returns A tibble or dataframe of all response bodies.
#' @noRd
#' @noMd
cw_do_query_calls <- function(urls, query, year, quarter, primary_geoid,
                              secondary_geoid, key, to_tibble) {
  list_res <- c()
  error_urls <- character(0)

  for (i in seq_len(length(urls))) {
    url <- urls[i]

    call <- R.cache::memoizedCall(make_query_calls, url, key)
    processed_code <- process_status_codes(call)

    if (!is.null(processed_code)) {
      # Need to output a single error message instead of a bunch when
      # something bad occurs. Append to list of errored urls.
      error_urls <- list(c(processed_code, url))

    } else {

      cont <- try(content(call), silent = TRUE)

      res <- as.data.frame(do.call(rbind, cont$data$results))

      res$query <- query[i]
      res$year <- year[i]
      res$quarter <- quarter[i]

      res[1] <- unlist(res[1])
      res[2] <- unlist(res[2])
      res[3] <- unlist(res[3])
      res[4] <- unlist(res[4])
      res[5] <- unlist(res[5])

      list_res[[i]] <- res

    }

    download_bar(done = i, total = length(urls),
                 current = url, error = length(error_urls))

  }

  print_resp_warning_messages(error_urls)

  if_tibble_return(list_res = list_res,
                   to_tibble = to_tibble,
                   api = "cw",
                   primary_geoid = primary_geoid,
                   secondary_geoid = secondary_geoid)
}


#' @name misc_do_query_call
#' @title API Calls for Misc Helper
#' @description Helper function for making the query calls to
#'   misc endpoints as well as concatenating all response objects
#'   to be returned to the user. The misc APIs are located in hud_misc.R.
#'   Collect error urls and warn users and cache the
#'   queries and show the download bar.
#' @param urls A character vector: the urls to query for.
#' @param key A character vector of length one: the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return as tibble else dataframe.
#' @returns A tibble or dataframe of all response bodies.
#' @noRd
#' @noMd
misc_do_query_call <- function(urls, key, to_tibble) {
  list_res <- c()
  error_urls <- character(0)

  for (i in seq_len(length(urls))) {

    url <- urls[i]

    call <- R.cache::memoizedCall(make_query_calls, url, key)
    processed_code <- process_status_codes(call)

    if (!is.null(processed_code)) {
      # Need to output a single error message instead of a bunch when
      # something bad occurs. Append to list of errored urls.
      error_urls <- list(c(processed_code, url))

    } else {

      cont <- try(content(call), silent = TRUE)
      list_res[[i]] <- as.data.frame(do.call(rbind, cont))

    }

    download_bar(done = i, total = length(urls),
                 current = url, error = length(error_urls))

  }

  print_resp_warning_messages(error_urls)
  if_tibble_return(list_res = list_res,
                   to_tibble = to_tibble,
                   api = "misc")
}





#' @name il_do_query_call
#' @title API Calls for Fair Markets Rent
#' @description Helper function for making the query calls to
#'   FMR (Fair Markets Rent) endpoints as well as concatenating all
#'   response objects
#'   to be returned to the user.
#' @param all_queries The components of an IL query call, including the
#'   geoid and year.
#' @param key A character vector of length one: the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return as tibble else data frame.
#' @param query_type The geoid type being queried for: state, county, or cbsa.
#' @returns A tibble or dataframe of all response bodies.
#' @noRd
#' @noMd
il_do_query_call <- function(all_queries, key, to_tibble, query_type) {

  list_res <- c()
  error_urls <- c()

  for (i in seq_len(nrow(all_queries))) {

    # Build the urls for querying the data.

    urls <- paste(get_hud_host_name(),
                  "il/",
                  if (querytype == "state") "statedata/" else "data/",
                  all_queries$query[i], "?year=", all_queries$year[i], sep = "")

    call <- R.cache::memoizedCall(make_query_calls, urls, key)
    processed_code <- process_status_codes(call)

    if (!is.null(processed_code)) {

      error_urls <- list(c(processed_code, url))

    } else {

      cont <- try(content(call), silent = TRUE)

      if (querytype == "state") {

        res <- as.data.frame(cont$data)
        res$statecode <- cont$data$statecode

        oth <- data.frame(query = all_queries$query[i],
                          year = all_queries$year[i],
                          median_income = cont$data$median_income,
                          stringsAsFactors = FALSE)

        res <- cbind(oth, res)

      } else if (querytype == "county" || query_type == "cbsa") {

        res <- as.data.frame(cont$data)

        oth <- data.frame(query = all_queries$query[i],
                          stringsAsFactors = FALSE)

        res <- cbind(oth, res)

      }

      list_res[[i]] <- res
    }

    download_bar(done = i, total = nrow(all_queries),
                 current = urls, error = length(error_urls))
  }

  print_resp_warning_messages(error_urls)

  if_tibble_return(list_res = list_res,
                   to_tibble = to_tibble,
                   api = "il",
                   query_type = query_type)

}

#' @name fmr_do_query_call
#' @title API Calls for Income Limits
#' @description Helper function for making the query calls to
#'   IL (Income Limits) endpoints as well as concatenating all response objects
#'   to be returned to the user.
#' @param all_queries The components of an IL query call, including the
#'   geoid and year.
#' @param key A character vector of length one: the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return as tibble else dataframe.
#' @param query_type The geoid type being queried for: state, county, or cbsa.
#' @returns A tibble or dataframe of all response bodies.
#' @noRd
#' @noMd
fmr_do_query_call <- function(all_queries, key, to_tibble, query_type) {

  res <- NULL

  error_urls <- c()
  list_county_res <- c()
  list_metroarea_res <- c()
  list_res <- c()


  for (i in seq_len(nrow(allqueries))) {
    # Build the urls for querying the data.

    urls <- paste(get_hud_host_name(),
                  "fmr/",
                  if (query_type == "state") "statedata/" else "data/",
                  allqueries$query[i], "?year=", allqueries$year[i], sep = "")

    call <- R.cache::memoizedCall(make_query_calls, urls, key)
    processed_code <- process_status_codes(call)

    if (!is.null(processed_code)) {

      error_urls <- list(c(processed_code, url))

    } else {

      cont <- try(content(call), silent = TRUE)

      if (query_type == "state") {

        res_county <- as.data.frame(do.call(rbind, cont$data$counties))
        res_metroareas <- as.data.frame(do.call(rbind, cont$data$metroareas))

        res_county$query <- allqueries$query[i]
        res_county$year <- allqueries$year[i]

        res_metroareas$query <- allqueries$query[i]
        res_metroareas$year <- allqueries$year[i]

        list_county_res[[i]] <- res_county
        list_metroarea_res[[i]] <- res_metroareas

      } else if (query_type == "county" || query_type == "cbsa") {

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

    download_bar(done = i, total = nrow(allqueries),
                 current = urls, error = length(error_urls))

  }

  print_resp_warning_messages(error_urls)

  if (query_type == "state") {

    res <- if_tibble_return(list_res = list_county_res,
                            list_res_two = list_metroarea_res,
                            to_tibble = to_tibble,
                            api = "fmr",
                            resolution = query_type)

  } else if (query_type == "county" || query_type == "cbsa") {

    res <- if_tibble_return(list_res = list_res,
                            to_tibble = to_tibble,
                            api = "fmr",
                            resolution = query_type)

  }

  res
}


#' @name if_tibble_return
#' @title Convert Final Object to Tibble
#' @description Convert Final Object to Tibble if TRUE, else return
#'   as data frame.
#' @param list_res The list of data frame responses to concatenate together.
#' @param list_res_two Secondary list of response objects for separating
#'  metro and county level data if user
#'  specifies states as the query using fmr api.
#' @param to_tibble If TRUE convert to tibble: If FALSE keep as data frame.
#' @param api The API that is queried for:
#' @param resolution If FMR or IL, specify if state, county, or cbsa.
#'   1) chas
#'   2) cw
#'   3) fmr
#'   4) il
#'   5) misc
#' @returns The final response object.
#' @noRd
#' @noMd
if_tibble_return <- function(list_res,
                             list_res_two = NULL,
                             to_tibble,
                             api,
                             primary_geoid = NULL,
                             secondary_geoid = NULL,
                             resolution =  NULL) {

  res <- NULL

  if (api == "fmr") {

      if (resolution == "state") {

        if (length(list_res) != 0) {

          res_county <- as.data.frame(do.call(rbind, list_res))
          res_county <- as.data.frame(sapply(res_county,
                                             function(x) unlist(as.character(x))))

          res_metroareas <- as.data.frame(do.call(rbind, list_res_two))
          res_metroareas <- as.data.frame(sapply(
            res_metroareas,
            function(x) unlist(as.character(x))))

          if (to_tibble) {
            res_county <- tibble(res_county)
            res_metroareas <- tibble(res_metroareas)
          }

          res <- list(counties = res_county, metroareas = res_metroareas)
        }

      } else if (resolution == "county" || resolution == "cbsa") {

        if (length(list_res) > 1) {

          res <- as.data.frame(do.call(rbind, list_res))
          res <- as.data.frame(sapply(res, function(x) unlist(as.character(x))))

        }

      }

  } else {

    if (length(list_res) != 0) {

      res <- do.call(rbind, list_res)

      if (api == "cw") {

        colnames(allres)[6] <- primary_geoid
        colnames(allres)[1] <- secondary_geoid

      }

    }

  }

  if (to_tibble) {
    res <- as_tibble(res)
  }

  res
}



#' @name make_query_calls
#' @title Make Query Calls to HUD USER
#' @description Centralized atomic function for querying API calls
#'   as to make R.cache memoizedCall work at a singular API call resolution.
#' @param urls A character vector: the urls to query for.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @returns The response object.
#' @noRd
#' @noMd
make_query_calls <- function(url, key, path, query) {

  # Check if Sys.getenv("HUD_USER_AGENT") has been set
  # and is not empty string. If so, then allow user agent
  # will be set to this. Otherwise, just point back to the
  # url of the package.

  the_user_agent <- if (Sys.getenv("HUD_USER_AGENT") != "") {
                      Sys.getenv("HUD_USER_AGENT")
                    } else {
                      "https://github.com/etam4260/rhud"
                    }

  config <- add_headers(Authorization = paste("Bearer ", as.character(key)))

  # For retries, we might want to allow user to specify this
  # parameter... giving them the choice between speed vs accuracy...
  request <- RETRY("GET",
                   url,
                   config,
                   pause_cap = 1,
                   times = 2,
                   user_agent(the_user_agent),
                   quiet = TRUE)


  request
}




#' @name process_status_codes
#' @title Handle Status Codes Returned By Query
#' @description Given a response object from a query, handle it differently
#'   based on the status codes returned by HUD USER API server.
#' @param call The response object
#' @returns NULL if the response is 200 else return the status code and
#'   the associated error.
#' @noRd
#' @noMd
process_status_codes <- function(call) {
  error <- NULL

  if (status_code(call) == 400) {

    error <- c(400, paste("An invalid value was specified for one of ",
                  "the query parameters in the request URI."))

  } else if (status_code(call) == 401) {

    error <- c(401, paste("Authentication failure"))

  } else if (status_code(call) == 403) {

    error <- paste(403, "Not allowed to access this dataset API, ",
                  "because you have not registered for it.")

  } else if (status_code(call) == 404) {

    error <- paste(404, "No data found using '(value you entered)'")

  } else if (status_code(call) == 405) {

    error <- paste(405, "Unsupported method, only GET is supported")

  } else if (status_code(call) == 406) {

    error <- paste(406, "Unsupported Accept Header value, ",
                  "must be application/json")

  } else if (status_code(call) == 500) {

    error <- paste(500, "Internal server error occurred")

  }

  error

}




#' @name print_resp_warning_messages
#' @title Print Response Warning Messages
#' @description Print warning messages associated with query calls.
#' @param errors The c(response code, response code description, url)
#' @noRd
#' @noMd
print_resp_warning_messages <- function(errors) {
  # Spit out error messages to user after all
  # queries are done.
  if (length(errors) != 0) {
    # Spit out error messages to user after all
    # queries are done.
    warning(paste("Could not find data for queries: \n\n",
                  paste(paste("*", errors, sep = " "), collapse = "\n"),
                  "\n\nIt is possible that your key maybe invalid or ",
                  "there isn't any data for these parameters, ",
                  "If you think this is wrong please ",
                  "report it at https://github.com/etam4260/rhud/issues.",
                  sep = ""), call. = FALSE)

  }
}
