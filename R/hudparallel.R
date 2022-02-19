#' @import furrr
#' @import httr

# Modern computers contain multiple cores which can be utilized for parallel
# computing. Making multiple API calls is a good example of where
# parallel computing can be exploited or else we would be wasting
# compute power. Even Raspberry Pis have 4 cores.

# One of the many problems with parallel computing is determining the optimal
# number of cores for a particular task. Where does the overhead of adding more
# cores begin to deteriorate overall performance time pertaining to API calls?

#' @name parallelize_api_calls
#' @title parallelize_api_calls
#' @description Make API calls using multiple cores.
#' @param allqueries All queries
#' @param cores The number of CPU cores to use when processing the requests.
#' @keywords parallel
#' @returns The dataframe collection of all the response bodies.
parallelize_api_calls <- function(allqueries, cores = 1) {
  plan(multicore, workers = cores)
  comb = future_map(split(allqueries, seq(nrow(allqueries))), api_call)
  return(do.call(rbind, comb))
}

#' @name api_call
#' @title api_call
#' @description Make API calls
#' @param listed Each individual entry url to query for as well as index an meta data.
#' @keywords call
#' @returns The dataframe collection of all the response bodies.
api_call <- function(listed) {
  call<-try(GET(as.character(listed['url']), add_headers(Authorization=paste("Bearer ", listed['key']))),silent = TRUE) #try to make call
  cont<-try(content(call), silent = TRUE) #parse returned data
  if(cont[[1]]["error"] != "NULL") {
    warning(paste("Could not find data for inputted query, year, or quarter where query equals ", listed['query'], " ,year equals ",listed['year'], " ,and quarter equals ", listed['quarter'], ". It is possible that your key maybe invalid, there isn't any data for these parameters, or you have reached the maximum number of API calls per minute.", sep = ""))
  } else {
    res <- as.data.frame(do.call(rbind, cont$data$results))
    res$type <- as.character(listed['type'])
    res$query <- as.character(listed['query'])
    res$year <- as.character(listed['year'])
    res$quarter <- as.character(listed['quarter'])
    return(res)
  }
}
