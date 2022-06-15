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
#'
#' hud_set_user_agent("im-the-user")
#'
#' }
hud_set_user_agent <- function(user_agent, in_wkdir = FALSE, in_home = FALSE) {

  if (!is.character(user_agent) || length(user_agent) != 1) {
    stop(paste("Make sure argument key is of type",
               "character and is of vector length 1",
               sep = ""))
  }


  if (!is.logical(in_wkdir) ||
     !is.logical(in_home) ||
     length(in_wkdir) != 1  ||
     length(in_home) != 1) {

    stop("Make sure argument in_wkdir and in_home are of type logical.")
  }

  Sys.setenv("HUD_USER_AGENT" = user_agent)
  message("* Setting the HUD_USER_AGENT variable for the working session.")


  if (in_wkdir) {
    # Set the user agent in the Rprofile working direct. If not made,
    # make one and set.
    if (any(list.files(all.files = TRUE) == ".Rprofile")) {
      # Check the file if it contains a call to set user agent, regex for it.
      rprof <- readLines("./.Rprofile")
      all_occur <- grep("^Sys\\.setenv\\(\"HUD_USER_AGENT\" = \".*\"\\)", rprof)

      if (any(all_occur)) {
        message(paste("* It looks like your .RProfile contains multiple ",
                      "definitions of the HUD_USER_AGENT.",
                      " Do file.edit(\".Rprofile\") ",
                      "to take a look at it.",
                      sep = ""))
      }

      message("* Writing the HUD_USER_AGENT in working directory .Rprofile!")
      writeLines(paste(paste(rprof, collapse = "\n"),
                       "\nSys.setenv(\"HUD_USER_AGENT\" = \"",
                       user_agent, "\")\n", sep = ""),
                 ".Rprofile")
    } else {
      file.create(".Rprofile")
      writeLines(paste("\nSys.setenv(\"HUD_USER_AGENT\" = \"",
                       user_agent, "\")\n", sep = ""),
                 ".Rprofile")
      message("* Writing the HUD_USER_AGENT in working directory .Rprofile!")
    }

  }


  if (in_home) {
    # Set the key in the HOME direct
    # Make system call to get home directory for this user.
    # Make the file here.
    if (any(list.files(file.path(Sys.getenv("HOME")),
                       all.files = TRUE) == ".Rprofile")) {
      # Check the file if it contains a call to set user agent, regex for it.

      rprof <- readLines("~/.Rprofile")
      all_occur <- grep("^Sys\\.setenv\\(\"HUD_USER_AGENT\" = \".*\"\\)", rprof)

      if (any(all_occur)) {
        message(paste("* It looks like your HOME .RProfile contains multiple ",
                      "definitions of the HUD_USER_AGENT. ",
                      "Do file.edit(\"~/.Rprofile\") ",
                      "to take a look at it.",
                      sep = ""))
      }

      message("* Writing the HUD_USER_AGENT in HOME directory .Rprofile!")
      writeLines(paste(paste(rprof, collapse = "\n"),
                       "\nSys.setenv(\"HUD_KEY\" = \"", user_agent,
                       "\")\n", sep = ""),
                 "~./Rprofile")
    } else {
      file.create("~/.Rprofile")
      writeLines(paste("\nSys.setenv(\"HUD_USER_AGENT\" = \"", user_agent,
                       "\")\n", sep = ""),
                 "~/.Rprofile")
      message("* Writing the HUD_USER_AGENT in HOME directory .Rprofile!")
    }
  }


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
#'
#' hud_get_user_agent()
#'
#' }
hud_get_user_agent <- function() {
  return(Sys.getenv("HUD_USER_AGENT"))
}
