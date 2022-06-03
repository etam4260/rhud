#' @name hud_get_key
#' @title hud_get_key
#' @description  Return most recent key set in the HUD_KEY environment variable.
#'   If no key is set, return "".
#' @returns Returns a string.
#' @export
#' @seealso
#' * [rhud::hud_get_key()]
#' * [rhud::hud_set_key()]
#' @examples
#' \dontrun{
#' library(rhud)
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
#'   variable. Allows the user to also set the key to their .RProfile
#'   in working or HOME.
#' @param key key obtained at
#'   https://www.huduser.gov/hudapi/public/register?comingfrom=1
#' @param in_wkdir set the key in the user's .RProfile in this directory.
#'   Is defaulted to false.
#' @param in_home set the key in the user's HOME directory.
#'   Is defaulted to false.
#' @export
#' @seealso
#' * [rhud::hud_get_key()]
#' * [rhud::hud_set_key()]
#' @examples
#' \dontrun{
#' library(rhud)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#' hud_get_key()
#'
#' hud_set_key("dwqkqodkqodqkqoqdq")
#' hud_get_key()
#' }
hud_set_key <- function(key,
                        in_wkdir = FALSE,
                        in_home = FALSE) {

  # Validate types of key, inwkdir, in_home
  if (!is.character(key) || length(key) != 1) {
    stop(paste("Make sure argument key is of type",
               "character and is of vector length 1",
               sep = ""))
  }

  if(!is.logical(in_wkdir) ||
     !is.logical(in_home) ||
     length(in_wkdir) != 1  ||
     length(in_home) != 1) {
    stop("Make sure argument in_wkdir and in_home are of type logical.")
  }

  # Set the key in working R session.
  Sys.setenv("HUD_KEY" = key)
  message("* Setting the HUD_KEY variable for the working session.")

  if (in_wkdir) {
    # Set the key in the Rprofile working direct. If not made, make one and set.
    if (any(list.files(all.files = TRUE) == ".Rprofile")) {
      # Check the file if it contains a call to set hud key, regex for it.
      rprof <- readLines("./.Rprofile")
      all_occur <- grep("^Sys\\.setenv\\(\"HUD_KEY\" = \".*\"\\)", rprof)

      if (any(all_occur)) {
        message(paste("* It looks like your .RProfile contains multiple ",
                      "definitions of the HUD_KEY. Do file.edit(\".Rprofile\") ",
                      "to take a look at it.",
                      sep = ""))
      }

      message("* Writing the HUD_KEY in working directory .Rprofile!")
      writeLines(paste(paste(rprof, collapse = "\n"),
                       "\nSys.setenv(\"HUD_KEY\" = \"", key,"\")\n", sep = ""),
                 ".Rprofile")
    } else {
      file.create(".Rprofile")
      writeLines(paste("\nSys.setenv(\"HUD_KEY\" = \"", key,"\")\n", sep = ""),
                 ".Rprofile")
      message("* Writing the HUD_KEY in working directory .Rprofile!")
    }

  }


  if (in_home) {
    # Set the key in the HOME direct
    # Make system call to get home directory for this user.
    # Make the file here.
    if (any(list.files(file.path(Sys.getenv("HOME")),
                       all.files = TRUE) == ".Rprofile")) {
      # Check the file if it contains a call to set hud key, regex for it.

      rprof = readLines("~/.Rprofile")
      all_occur <- grep("^Sys\\.setenv\\(\"HUD_KEY\" = \".*\"\\)", rprof)

      if (any(all_occur)) {
        message(paste("* It looks like your HOME .RProfile contains multiple ",
                      "definitions of the HUD_KEY. Do file.edit(\"~/.Rprofile\") ",
                      "to take a look at it.",
                      sep = ""))
      }

      message("* Writing the HUD_KEY in HOME directory .Rprofile!")

      writeLines(paste(paste(rprof, collapse = "\n"),
                       "\nSys.setenv(\"HUD_KEY\" = \"", key,"\")\n", sep = ""),
                 "~/.Rprofile")
    } else {
      file.create("~/.Rprofile")
      writeLines(paste("\nSys.setenv(\"HUD_KEY\" = \"", key,"\")\n", sep = ""),
                 "~/.Rprofile")
      message("* Writing the HUD_KEY in HOME directory .Rprofile!")
    }
  }
}
