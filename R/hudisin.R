# General utility functions to determine whether one geoid is in another
# geoid using crosswalk functions

#' @name %cw_in%
#' @title %cw_in%
#' @description Given two geoids, determine if they overlap using the
#'   crosswalk functions. If two geoids overlap, return TRUE. These geoids must
#'   correspond to those present in the crosswalk functions. This will use
#'   the most recent crosswalk file available. For example, does the zip code
#'   37232 exists inside the congressional district 24?
#' @param geoid_one The first geoid
#' @param geoid_two The second geoid
#'
#' @export
`%cw_in%` <- function(geoid_one, geoid_two) {

}
