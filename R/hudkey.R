
#' @name hud_get_key
#' @title hud_get_key
#' @description  Return most recent key set in the HUD_KEY environment variable.
#'   If no key is set, return "".
#' @returns Returns a string.
#' @export
hud_get_key <- function() {
  return(Sys.getenv("HUD_KEY"))
}
