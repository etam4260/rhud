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
#' (the year 365 days ago from the current system date). For details about
#' this package as well as details about its python counterpart,
#' visit https://github.com/etam4260/rhud
#'
#' The default return type of data is a dataframe.
#' To get tibbles instead, use: options(rhud_use_tibble = TRUE)
#'
#' @author Emmet Tam
NULL


# When user attaches the library, make sure to ask them to use
# caching as an option. Give them options that they can use
# to set the default year, quarter, and whether they want dataframes or
# tibbles when querying data.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("\nrhud:\n",
                              "---------------------------------------------",
                              "---------------------------------------------",
                              "\n* To begin, first obtain a key ",
                              "from HUD User website at `https://www.huduser.",
                              "gov/hudapi/public/login`\n",
                              "* To set the key, use hud_set_key",
                              "(\"your-key\")\n",
                              "* To get tibbles instead of dataframes, ",
                              "use `options(rhud_use_tibble = TRUE)`\n",
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


.onLoad <- function(libname, pkgname) {
  # Use evalWithMemoization to cache the data properly.

  if (Sys.getenv("RHUD_CACHE_DIR") == "" ||
     Sys.getenv("RHUD_CACHE_DIR") == "NULL") {
    suppressWarnings(R.cache::setCacheRootPath(paste(tempdir(), "//",
                                                     "rhud_cache", sep = "")))
  } else {
    suppressWarnings(R.cache::setCacheRootPath(Sys.getenv("RHUD_CACHE_DIR")))
  }

}
