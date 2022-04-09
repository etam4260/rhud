#' @import httr

#' @name chas_do_query_calls
#' @title chas_do_query_calls
#' @description Helper function for making the query calls to CHAS
#' API endpoint.
#' @param URL The URLs to query for.
#' @param key The key obtain from HUD USER website.
#' @returns A dataframe of all the response bodies.
#' @noRd
#' @noMd
chas_do_query_calls <- function(URL, key) {
  # Form all query calls...
  list_res <- c()
  for(i in seq_len(length(URL))) {

    url <- URL[i]

    call<-try(GET(url, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont<-try(content(call), silent = TRUE)

    if('error' %in% names(cont) || length(cont) == 0) {
      warning(paste("Could not find data for query:", url,
                    "  It is possible that your key maybe invalid,",
                    "there isn't any data for these parameters,",
                    "or you have reached the maximum number of API",
                    "calls per minute. If you think this is wrong please",
                    "report it at https://github.com/etam4260/hudr/issues.",
                    sep = " "))
    } else {

      list_res[[i]] <- unlist(cont[[1]])
    }
  }


  if(length(list_res) != 0) {

    res <- as.data.frame(do.call("rbind", list_res))

    return(res)
  }
  return(NULL)
}


#' @name cw_do_query_calls
#' @title cw_do_query_calls
#' @description Helper function for queries to the crosswalk API.
#' @param URL The url endpoints to query for.
#' @param primary_geoid The first geoid part of a function call. For example,
#'   hud_cw_zip_tract() has zip as first GEOID and tract as second GEOID.
#' @param secondary_geoid The second geoid part of a function call.
#' @param key The key needed to query the HUD API
#' @returns A data frame of all the results made from the query.
#' @noRd
#' @noMd
cw_do_query_calls <- function(URL, query, year, quarter, primary_geoid,
                              secondary_geoid, key) {
  list_res <- c()

  for(i in seq_len(length(URL))) {
    url <- URL[i]

    call<-try(GET(url, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"),
                  timeout(30)), silent = TRUE)

    cont<-try(content(call), silent = TRUE)

    if('error' %in% names(cont[[1]])) {
      warning(paste("Could not find data for inputted query, year,
                    or quarter where query equals ", query,
                    ", year equals ", year,
                    ", and quarter equals ", quarter,
                    ". It is possible that your key maybe invalid, ",
                    "there isn't any data for these parameters, ",
                    "or you have reached the maximum number of API calls ",
                    "per minute. If you think this is wrong please report ",
                    "it at https://github.com/etam4260/hudr/issues.",
                    sep = ""))
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

  allres <- NULL
  if(length(list_res) != 0) {
    allres <- do.call(rbind, list_res)
    colnames(allres)[6] <- primary_geoid
    colnames(allres)[1] <- secondary_geoid
  }

  return(as.data.frame(allres))
}


#' @name misc_do_query_call
#' @title misc_do_query_call
#' @description Make queries calls given a list of URLs
#' @param URL The URLs to query for.
#' @param key The API key for this user. You must go to HUD and sign up for
#'   an account and request for an API key.
#' @returns A dataframe containing all queried rows.
#' @noRd
#' @noMd
misc_do_query_call <- function(URL, key) {
  list_res <- c()
  for(i in seq_len(length(URL))) {

    url <- URL[i]

    call<-try(GET(url, add_headers(Authorization=paste("Bearer ",
                                                       as.character(key))),
                  user_agent("https://github.com/etam4260/hudr"), timeout(30)),
              silent = TRUE)

    cont<-try(content(call), silent = TRUE)

    if('error' %in% names(cont) || length(cont) == 0) {
      warning(paste("Could not find data for query:", url,
                    ". It is possible that your key maybe invalid, ",
                    "there isn't any data for these parameters, ",
                    "or you have reached the maximum number of API calls per ",
                    "minute. If you think this is wrong please report",
                    "it at https://github.com/etam4260/hudr/issues.", sep = ""))
    } else {
      list_res[[i]] <- as.data.frame(do.call(rbind, cont))
    }
  }

  if(length(list_res) != 0) {
    res <- as.data.frame(do.call(rbind, list_res))
    return(res)
  }
  return(NULL)
}
