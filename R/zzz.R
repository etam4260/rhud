# File adapted from the tigris package:
# https://github.com/walkerke/tigris/blob/master/R/zzz.R


#' @docType package
#' @name rhud-package
#' @aliases rhud rhud-package
#' @title rhud: A R Interface to the US Department of Housing and Urban
#'   Development APIs
#' @description
#' rhud is a R library that allows users to easily download data from the
#' US Department of Housing and Urban Development APIs @@ HUD User.
#' The data is defaulted to return measurements for the prior year
#' (the year 365 days ago from the current system date) for most functions.
#' For details about this package as well as details about its python
#' counterpart, visit https://github.com/etam4260/rhud
#'
#' The default return type of data is a dataframe.
#' To get tibbles instead, use: options(rhud_use_tibble = TRUE)
#'
#' To quiet the download bar, use: options(rhud_quiet_loads = TRUE)
#'
#' This interface uses RETRY over GET requests. By default, rhud will ping a
#' URL 'twice' at most for a successful response. To set the number of retries
#' used, use options(rhud_num_retries = 1), where in this case the
#' number '1' is a placeholder.
#'
#' @author Emmet Tam
NULL


# When user attaches the library, make sure to ask them to use
# caching as an option. Give them options that they can use
# to set the default year, quarter, and whether they want dataframes or
# tibbles when querying data.
.onAttach <- function(libname, pkgname) {

  # If HUD_KEY is already set, then they likely don't need the start up message

  if (Sys.getenv("HUD_KEY") == "") {
    packageStartupMessage(paste("\nrhud:\n",
                                "---------------------------------------------",
                                "---------------------------------------------",
                                "\n* To begin, first obtain a key ",
                              "from HUD USER website at `https://www.huduser.",
                                "gov/hudapi/public/login`\n",
                                "* To set the key, use hud_set_key",
                                "(\"your-key\")\n",
                                "* To get tibbles instead of dataframes, ",
                                "use `options(rhud_use_tibble = TRUE)`\n",
                                "* To silence messages from downloads, use",
                                " `options(rhud_quiet_loads = TRUE)`\n",
                                "* To set the number of retries ",
                                "used, use options(rhud_num_retries = 1)\n",
                                "* By default, rhud will ping a URL 'twice' at ",
                                "most for a successful response.\n",
                               "* Caching is enabled by default. To set a new ",
                                "cache ",
                                "directory use hud_set_cache_dir().\n",
                                "* To delete cached ",
                                "items, use hud_clear_cache()\n",
                                "* Set these in your R script for single ",
                                "session ",
                                "or in .Rprofile for persistence.\n",
                                "* For more information on how to use this ",
                                "package, ",
                                "visit `https://etam4260.github.io/rhud/",
                                "index.html`\n",
                                "--------------------------------------------",
                                "--------------------------------------------",
                                sep = ""))

    if (Sys.getenv("RHUD_CACHE_DIR") == "" ||
        Sys.getenv("RHUD_CACHE_DIR") == "NULL") {
      packageStartupMessage("\n* Setting rhud cache to R temp directory.")

    } else {
      packageStartupMessage(paste("\n* Setting rhud cache to",
                                  Sys.getenv("RHUD_CACHE_DIR"),
                                  "directory.\n"))
    }
  }
}


.onLoad <- function(libname, pkgname) {
  # Use evalWithMemoization to cache the data properly.

  if (Sys.getenv("RHUD_CACHE_DIR") == "" ||
     Sys.getenv("RHUD_CACHE_DIR") == "NULL") {

    Sys.setenv("RHUD_CACHE_DIR" = paste(tempdir(), "//", "rhud_cache",
                                        sep = ""))

    suppressWarnings(R.cache::setCacheRootPath(paste(tempdir(), "//",
                                                     "rhud_cache", sep = "")))
  } else {
    suppressWarnings(R.cache::setCacheRootPath(Sys.getenv("RHUD_CACHE_DIR")))
  }

}
