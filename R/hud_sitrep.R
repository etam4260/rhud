#' @name rhud_sitrep
#' @title Package Status
#' @description Checks whether the rhud key has been set, if rhud can
#'    query some the APIs, and displays the current cache settings
#'    for the package.
#' @export
#' @examples
#' \dontrun{
#'
#' rhud_sitrep()
#'
#' }
rhud_sitrep <- function() {

  if (hud_get_key() == "") {
    # First check if hud_key is set, if not then return warning

    warning(paste("It seems like the HUD_KEY has not been set. ",
               "Please go to https://www.huduser.gov/hudapi/public/login,",
               "sign up, and request for an API key. ",
               "Then save ",
               "this to your environment using ",
               "Sys.setenv('HUD_KEY' = YOUR_KEY)",
               sep = ""), call. = FALSE)

  } else {

    message("The HUD_KEY is set to:\n",
            hud_get_key(),
            "\n",
            sep = "")

  }


  message("Checking if APIs can be reached...\n")


  # Check if API key is reachable by checking for
  # NA return value.
  if (is.null(hud_rec_cw_yr()[1])) {
    warning(paste("USPS Crosswalk API: Bad"), call. = FALSE)
  } else {
    message(paste("USPS Crosswalk API: Okay"))
  }

  if (is.null(hud_rec_fmr_yr()[1])) {
    warning(paste("Fair Markets Rent: Bad"), call. = FALSE)
  } else {
    message(paste("Fair Markets Rent: Okay"))
  }

  if (is.null(hud_rec_il_yr())) {
    warning(paste("Income Limits: Bad"), call. = FALSE)
  } else {
    message(paste("Income Limits: Okay"))
  }


  # Display the current cache settings
  cache_dir <- hud_get_cache_dir()

  message("\nThe current cache directory is: ",
          cache_dir,
          ".\nTo set a new cache directory, use: ",
          "hud_set_cache_dir()",
          sep = "")

}
