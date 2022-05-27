#' @name rhud
#' @title rhud
#' @description Access the US Department of Housing and Urban Development APIs
#' in R
#'
#' rhud is an R package that enables users to easily download data from the
#' US Department of Housing and Urban Development APIs. The data returns
#' measurements for the previous year (the year 365 days ago
#' from the current system date). For details about the package as well as
#' information about its sibling python counterpart,
#' visit https://github.com/etam4260/rhud
#'
#' Before using rhud, it is recommended to enable caching by calling
#' options(rhud_use_cache = TRUE)
#'
#' To set the default year for all function calls to another year use:
#' options(rhud_use_year = 2020)
#' options(rhud_use_year = c(2019, 2020))
#' This, however, does not affect the CHAS functions.
#'
#' To set the default quarter for crosswalk function calls use:
#' options(rhud_use_quarter = 1)
#' options(rhud_use_quarter = c(1, 2, 3))
#'
#' The default return type of data from rhud is a dataframe. The changing landscape
#' of R has led to the use of tibbles. To get tibbles instead, call:
#' options(rhud_use_tibble = TRUE)
#'
#' @docType package
#' @author Emmet Tam
NULL


#' rhud exported operators
#'
#'
#' @name rhud-exports
NULL
