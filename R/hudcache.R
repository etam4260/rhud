# File adapted from the tigris package:
# https://github.com/walkerke/tigris/blob/master/R/helpers.R

#' @name rhud_cache_dir
#' @title rhud_cache_dir
#' @description Set the caching directory to store data retrieved using the
#' rhud API calls. By default, rhud uses a non-persistent temporary directory given
#' for an R session. However, it is possible that a user might want
#' to save their queried data in a custom location. This function allows users
#' to set the cache directory. The user can also store the result in the
#' .RProfile so that rhud will remember the set preference directory throughout
#' sessions.
#'
#' Windows users: please note that you'll need to use double-backslashes or
#' forward slashes when specifying your cache directory's path in R.
#'
#' @param path The full path to the desired cache directory
#' @param in_wkdir Set the .Rprofile in current working directly to have
#'   have rhud cache to path.
#' @param in_home Set the .Rprofile in the HOME directory to have
#'   have rhud cache to path.
#' @export
#' @examples
#' \dontrun{
#' rhud_cache_dir("./an/example/path", in_wkdir = TRUE, in_home = TRUE)
#'
#' }
rhud_cache_dir <- function(path, in_wkdir = FALSE, in_home = FALSE) {

}
