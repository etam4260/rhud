#' @name hud_get_key
#' @title hud_get_key
#' @description  Return most recent key set in the HUD_KEY environment variable.
#'   If no key is set, return "".
#' @returns Returns a string.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#'
#' hud_get_key()
#' }
hud_get_key <- function() {
  return(Sys.getenv("HUD_KEY"))
}

#' @name hud_set_key
#' @title hud_set_key
#' @description A wrapper around Sys.getenv() to set HUD_KEY environment variable.
#' @param key key obtained at https://www.huduser.gov/hudapi/public/register?comingfrom=1
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#' hud_get_key()
#'
#' hud_set_key("dwqkqodkqodqkqoqdq")
#' hud_get_key()
#' }
hud_set_key <- function(key) {
  Sys.setenv("HUD_KEY" = key)
  message("Setting the HUD_KEY variable!")
}
