#' @import httr
#' @import tibble

#' @name chas_do_query_calls
#' @title chas_do_query_calls
#' @description Helper function for making the query calls to CHAS
#' API endpoint.
#' @param urls The urls to query for.
#' @param key The key obtain from HUD USER website.
#' @returns A dataframe of all the response bodies.
#' @noRd
#' @noMd
chas_do_query_calls <- function(urls, key, to_tibble) {
  # Form all query calls...
  list_res <- c()
  error_urls <- c()

  `%notin%` <- Negate(`%in%`)

  # These measurements are hardcoded in, but a more effective method might
  # be to systematic checks to find all unique columns names from all CHAS
  # datasets.
  all_measurements <- c("geoname", "sumlevel", "year",
                        "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",
                        "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17",
                        "A18",
                        "B1", "B2", "B3", "B4", "B5", "B6", "B7",
                        "B8", "B9",
                        "C1", "C2", "C3", "C4", "C5", "C6",
                        "D1", "D2", "D3",
                        "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                        "E1", "E2", "E3", "E5", "E6", "E7", "E9", "E10", "E11",
                        "E13", "E14", "E15", "E17", "E18", "E19", "E21", "E22",
                        "E23",
                        "F1", "F2", "F3", "F5", "F6", "F7", "F9", "F10", "F11",
                        "F13", "F14", "F15", "F17", "F18", "F19", "F21", "F22",
                        "F23",
                        "G1", "G2", "G3", "G5", "G6", "G7", "G9", "G10", "G11",
                        "G13", "G14", "G15", "G17", "G18", "G19",
                        "H1", "H2", "H4", "H5", "H7", "H8", "H10", "H11", "H13",
                        "H14", "H16",
                        "I1", "I2", "I4", "I5", "I7", "I8", "I10", "I11",
                        "I13", "I14", "I16",
                        "J1", "J2", "J4", "J5", "J7", "J8", "J10",
                        "J11", "J13", "J14", "J16")


  for (i in seq_len(length(urls))) {

    url <- urls[i]

    call <- try(GET(url, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/rhud"), timeout(30)),
              silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, length(urls))

    if ("error" %in% names(cont) || length(cont) == 0) {
      # Need to output a single error message instead of a bunch when
      # something bad occurs. Append to list of errored urlss.
      error_urls <- c(error_urls, url)
    } else {
      not_measured <- all_measurements[all_measurements %notin%
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
  }
  message("\n")

  # Spit out error messages to user after all
  # queries are done.
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
    res <- as.data.frame(do.call("rbind", list_res))
    if (to_tibble == FALSE) {
      return(res)
    } else {
      return(as_tibble(res))
    }
  }

  return(NULL)
}


#' @name cw_do_query_calls
#' @title cw_do_query_calls
#' @description Helper function for queries to the crosswalk API.
#' @param urls The url endpoints to query for.
#' @param query The geoids to query for.
#' @param year The years to query for.
#' @param quarter The quarters in the year to query for.
#' @param primary_geoid The first geoid part of a function call. For example,
#'   hud_cw_zip_tract() has zip as first GEOID and tract as second GEOID.
#' @param secondary_geoid The second geoid part of a function call.
#' @param key The key needed to query the HUD API
#' @returns A data frame of all the results made from the query.
#' @noRd
#' @noMd
cw_do_query_calls <- function(urls, query, year, quarter, primary_geoid,
                              secondary_geoid, key, to_tibble) {
  list_res <- c()
  error_urls <- c()

  for (i in seq_len(length(urls))) {
    url <- urls[i]

    call <- try(GET(url, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/rhud"),
                  timeout(30)), silent = TRUE)

    cont <- try(content(call), silent = TRUE)

    download_bar(i, length(urls))

    if ("error" %in% names(cont[[1]])) {
      # Need to output a single error message instead of a bunch when
      # something bad occurs. Append to list of errored urlss.
      error_urls <- c(error_urls, url)
    } else {
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
  }
  message("\n")

  # Spit out error messages to user after all
  # queries are done.
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



  allres <- NULL
  if (length(list_res) != 0) {
    # Some years may not contain all the measurements, fill those in with NA
    # first before combining.


    allres <- do.call(rbind, list_res)
    colnames(allres)[6] <- primary_geoid
    colnames(allres)[1] <- secondary_geoid
  }

  if (to_tibble == FALSE) {
    return(as.data.frame(allres))
  } else {
    return(as_tibble(allres))
  }
}


#' @name misc_do_query_call
#' @title misc_do_query_call
#' @description Make queries calls given a list of urlss
#' @param urls The urlss to query for.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @returns A dataframe containing all queried rows.
#' @noRd
#' @noMd
misc_do_query_call <- function(urls, key, to_tibble) {
  list_res <- c()
  error_urls <- c()

  for (i in seq_len(length(urls))) {

    url <- urls[i]

    call <- try(GET(url, add_headers(Authorization = paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/rhud"), timeout(30)),
              silent = TRUE)

    download_bar(i, length(urls))

    cont <- try(content(call), silent = TRUE)

    if ("error" %in% names(cont) || length(cont) == 0) {
      # Need to output a single error message instead of a bunch when
      # something bad occurs. Append to list of errored urlss.
      error_urls <- c(error_urls, url)

    } else {
      list_res[[i]] <- as.data.frame(do.call(rbind, cont))
    }
  }
  message("\n")

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
    if (to_tibble == FALSE) {
      return(res)
    } else {
      return(as_tibble(res))
    }
  }

  return(NULL)
}
