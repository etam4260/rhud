# File adapted from the tigris package:
# https://github.com/walkerke/tigris/blob/master/R/zzz.R


# When user attaches the library, make sure to ask them to use
# caching as an option. Give them options that they can use
# to set the default year, quarter, and whether they want dataframes or
# tibbles when querying data.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste("\nrhud:\n",
                              "------------------------------------------------",
                              "-------------------------------------",
                              "\n* To begin using this package, you must obtain a key ",
                              "from HUD User website.",
                              "\n* To enable caching of rhud data, ",
                              "use `options(rhud_use_cache = TRUE)`\n",
                              "* To get tibbles instead of dataframes, ",
                              "use `options(rhud_use_tibble = TRUE)`\n",
                              "* Set these in your R script for single session ",
                              "or in .Rprofile for persistence.\n",
                              "------------------------------------------------",
                              "-------------------------------------\n",
                              sep = ""))

}

