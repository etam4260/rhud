#' @name hud_set_user_agent
#' @title hud_set_user_agent
#' @description Set a user agent when querying the HUD User APIs.
#' @param user_agent A character vector with the user agent.
#' @param in_wkdir set the key in the user's .RProfile in this directory.
#'   Is defaulted to false.
#' @param in_home set the key in the user's HOME directory.
#'   Is defaulted to false.
#' @export
#' @seealso
#' * [rhud::hud_get_user_agent()]
#' * [rhud::hud_set_user_agent()]
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_set_user_agent("im-the-user")
#'
#' }
hud_set_user_agent <- function(user_agent, in_wkdir, in_home) {
  Sys.setenv("HUD_USER_AGENT" = user_agent)



}



#' @name hud_get_user_agent
#' @title hud_get_user_agent
#' @description Get the most recent user agent set.
#' @returns A character vector with the user agent used for querying HUD User
#'   APIs.
#' @export
#' @seealso
#' * [rhud::hud_get_user_agent()]
#' * [rhud::hud_set_user_agent()]
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' hud_get_user_agent()
#'
#' }
hud_get_user_agent <- function() {
  return(Sys.getenv("HUD_USER_AGENT"))
}
