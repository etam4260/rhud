#' @import future
#' @import furrr
 
# Modern computers contain multiple cores which can be utilized for parallel 
# computing. Making multiple API calls is a good example of where
# parallel computing can be exploited or else we would be wasting 
# compute power. Even Raspberry Pis have 4 cores.

# One of the many problems with parallel computing is determining the optimal
# number of cores for a particular task. Where does the overhead of adding more
# cores begin to deteriorate overall performance time? Instead of doing
# benchmark testing during installation/compilation time, the hope is to 
# dynamically determine optimal core count as the user makes API calls.

# Will attempt to use the kneedle algorithm developed at
# https://github.com/etam4260/kneedle as a tool for detecting optimal core count.
# However, there are likely more sophisticated tools such as the inflection
# package for such a task.

# Need to save every time user makes call to function
pkg.env$metrics <- data.frame(cores = c(), function_used = c(), num_queries = c(), response_time = c())
pkg.env$recommendcores <- 1

#' @name determine_optimal_core_count
#' @title determine_optimal_core_count
#' @description Determines the optimal number of cores to use when doing parallel API calls.
#' @keywords cores
#' @export
#' @returns The number of optimal cores for this user
determine_optimal_core_count <- function() {
  # Deal with loading initial package
  if(nrow(pkg.env$metric) == 0) {
      sys.time
  } else {
    
  }
}

#' @name parallelize_api_calls
#' @title parallelize_api_calls
#' @description Make API calls using multiple cores.
#' @param urls A list of urls to query for.
#' @param cores The number of CPU cores to use when processing the requests.
#' @keywords parallel
#' @export
#' @returns The response body provided by the API call.
parallelize_api_calls <- function(urls, cores = 1) {
  
}