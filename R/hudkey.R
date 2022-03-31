
#' @name hud_get_key
#' @title hud_get_key
#' @description  Return most recent key set in the HUD_KEY environment variable.
#'   If no key is set, return "".
#' @returns Returns a string.
#' @export
hud_get_key <- function() {
  return(Sys.getenv("HUD_KEY"))
}

#' @name hud_set_key
#' @title hud_set_key
#' @description A wrapper around Sys.getenv() to set HUD_KEY environment variable.
#' @export
hud_set_key <- function(key) {
  Sys.getenv("HUD_KEY" = key)
  message("... setting HUD_KEY")
}
