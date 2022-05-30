#' @import R.cache

# File adapted from the tigris package:
# https://github.com/walkerke/tigris/blob/master/R/zzz.R

# When user attaches the library, make sure to ask them to use
# caching as an option. Give them options that they can use
# to set the default year, quarter, and whether they want dataframes or
# tibbles when querying data.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("\nrhud:\n",
                              "------------------------------------------------",
                              "------------------------------------------------",
                              "\n* To begin, first obtain a key ",
                              "from HUD User website at `https://www.huduser.gov/hudapi/public/login`\n",
                              "* To get tibbles instead of dataframes, ",
                              "use `options(rhud_use_tibble = TRUE)`\n",
                              "* Caching is enabled by default. To set a new cache ",
                              "directory use rhud_cache_dir().\n",
                              "* To delete cached ",
                              "items, use rhud_clear_cache()\n",
                              "* Set these in your R script for single session ",
                              "or in .Rprofile for persistence.\n",
                              "* For more information on how to use this package, ",
                              "visit `https://etam4260.github.io/rhud/index.html`\n",
                              "------------------------------------------------",
                              "------------------------------------------------\n",
                              sep = ""))

}


# When package is loaded, if the user had declared rhud_cache_dir, then
# implicitly they would want caching removing the need to make use have to
# turn it on and off. That function saves the variables that the user wants
# as system variables then this uses it to initial the caching directory when
# user loads the package.
.onLoad <- function(libname, pkgname) {
  # Use evalWithMemoization to cache the data properly.

  if(Sys.getenv("RHUD_CACHE_DIR") == "" || Sys.getenv("RHUD_CACHE_DIR") == "NULL") {
    suppressWarnings(setCacheRootPath(paste(tempdir(),"//", "rhud_cache", sep ="")))
    packageStartupMessage("\n* Setting rhud cache to R temp directory.")
  } else {
    suppressWarnings(setCacheRootPath(Sys.getenv("RHUD_CACHE_DIR")))
    packageStartupMessage(paste("\n* Setting rhud cache to",
                                Sys.getenv("RHUD_CACHE_DIR"),
                                "directory."))
  }

}

