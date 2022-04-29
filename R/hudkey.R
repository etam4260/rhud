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
#' @description A wrapper around Sys.getenv() to set HUD_KEY environment
#'   variable. Allows the user to also set the key to their
#'
#' @param key key obtained at
#'   https://www.huduser.gov/hudapi/public/register?comingfrom=1
#' @param set_key_wkdir set the key in the user's .RProfile in this directory.
#' @param set_key_home set the key in the user's HOME directory.
#' @param set_key_rhome set the key in the user's R HOME directory.
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
hud_set_key <- function(key,
                        set_key_wkdir = FALSE,
                        set_key_home = FALSE,
                        set_key_rhome = FALSE) {

  # Set the key in working R session.
  Sys.setenv("HUD_KEY" = key)
  message("Setting the HUD_KEY variable!")

  if(set_key_wkdir) {
    # Set the key in the Rprofile working direct. If not made, make one and
    # set key.
    if(!any(list.files() == ".Rprofile")) {
      file.create(".Rprofile")

    }
    message("Setting the HUD_KEY in working directory .Rprofile!")
  }

  if(set_key_home) {
    # Set the key in the Rprofile home direct
    message("Setting the HUD_KEY in HOME directory .Rprofile!")

    # Make system call to get home directory for this user.

    # Make the file here.
  }

  if(set_key_rhome) {
    message("Setting the HUD_KEY in R HOME directory .Rprofile !")

    # Make system call to get the R HOME directory for this user.

    # Make the file here.

  }
}
