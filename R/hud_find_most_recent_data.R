#' @name hud_rec_cw_yr
#' @title hud_rec_cw_yr
#' @description Pings the (United States Postal Service) USPS Crosswalk API
#'    provided by HUD USER to determine the most recently released files.
#'    This will only ping for the last two years for every quarter.
#'    This tests hud_cw_tract_zip(tract = 48201223100) as
#'    the endpoint query.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @seealso
#' * [rhud::hud_rec_cw_yr()]
#' * [rhud::hud_rec_fmr_yr()]
#' * [rhud::hud_rec_il_yr()]
#' @returns The most recent year and quarter of available
#'   (United States Postal Service)  USPS Crosswalk files.
#' @export
#' @examples
#' \dontrun{
#'
#' hud_rec_cw_yr()
#'
#' }
hud_rec_cw_yr <- function(key = Sys.getenv("HUD_KEY")) {

  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%m"))

  if (month >= 1 && month <= 3) {
    quarter <- 1
  } else if (month >= 4 && month <= 6) {
    quarter <- 2
  } else if (month >= 7 && month <= 9) {
    quarter <- 3
  } else if (month >= 10 && month <= 12) {
    quarter <- 4
  }

  # Ping HUD CW API. Set a limit for 8 pings or the two years from current one.
  i <- 8
  while (i > 0) {
    # It might be worthwhile to make this a little more robust by pinging
    # different CW files and making a percentage threshold on them.
    data <- suppressMessages(suppressWarnings(hud_cw_tract_zip(
      tract = 48201223100,
      year = year,
      quarter = quarter,
      key = key
    )))

    if (!is.null(data) && nrow(data) >= 1) {
      return(c(year = year, quarter = quarter))
    }

    if (quarter > 1) {
      quarter <- quarter - 1
    } else {
      year <- year - 1
      quarter <- 4
    }
    i <- i - 1
  }

  return(c(NA, NA))
}


#' @name hud_rec_fmr_yr
#' @title hud_rec_fmr_yr
#' @description  Pings the Fair Markets Rent API provided by
#'    (US Department of Housing and Urban Development) HUD User to
#'    determine the most recently released files. This will only ping
#'    for the last two years. Will return years for state, county, and metroarea
#'    resolutions.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @seealso
#' * [rhud::hud_rec_cw_yr()]
#' * [rhud::hud_rec_fmr_yr()]
#' * [rhud::hud_rec_il_yr()]
#' @returns The most recent year for the fmr files for state, county, and
#'   metroarea queries.
#' @export
#' @examples
#' \dontrun{
#'
#' hud_rec_fmr_yr()
#'
#' }
hud_rec_fmr_yr <- function(key = Sys.getenv("HUD_KEY")) {
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%m"))

  if (month >= 1 && month <= 3) {
    quarter <- 1
  } else if (month >= 4 && month <= 6) {
    quarter <- 2
  } else if (month >= 7 && month <= 9) {
    quarter <- 3
  } else if (month >= 10 && month <= 12) {
    quarter <- 4
  }

  year_state <- NULL
  year_county <- NULL
  year_metroarea <- NULL

  i <- 8
  while (i > 0) {

    if (is.null(year_state)) {
      data <- suppressMessages(suppressWarnings(hud_fmr("MD",
                                                        year = year,
                                                        key = key
                                                        )))

      if (!is.null(data) && length(data) >= 1) {
        year_state <- year
      }
    }


    if (is.null(year_county)) {
      data <- suppressMessages(suppressWarnings(hud_fmr_county_zip("5100199999",
                                                                   year = year,
                                                                   key = key
                                                                   )))

      if (!is.null(data) && nrow(data) >= 1) {
        year_county <- year
      }
    }


    if (is.null(year_metroarea)) {
      data <- suppressMessages(suppressWarnings(
        hud_fmr_metroarea_zip("METRO47900M47900",
                              year = year,
                              key = key
                              )))

      if (!is.null(data) && nrow(data) >= 1) {
        year_metroarea <- year
      }
    }


    if (quarter > 1) {
      quarter <- quarter - 1
    } else {
      year <- year - 1
      quarter <- 4
    }

    i <- i - 1

  }

  return(c(state = year_state,
           county = year_county,
           metroarea = year_metroarea))
}




#' @name hud_rec_il_yr()
#' @title hud_rec_il_yr()
#' @description Pings the Income Limits API provided by HUD User to
#'    determine the most recently released files. This will only ping
#'    for the last two years. Will return years for state, county, and metroarea
#'    resolution.
#' @param key The key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @seealso
#' * [rhud::hud_rec_cw_yr()]
#' * [rhud::hud_rec_fmr_yr()]
#' * [rhud::hud_rec_il_yr()]
#' @returns The most recent year for the Income Limits for state, county, and
#'   metroarea queries.
#' @export
#' @examples
#' \dontrun{
#'
#' hud_rec_il_yr()
#'
#' }
hud_rec_il_yr <- function(key = Sys.getenv("HUD_KEY")) {
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%m"))

  if (month >= 1 && month <= 3) {
    quarter <- 1
  } else if (month >= 4 && month <= 6) {
    quarter <- 2
  } else if (month >= 7 && month <= 9) {
    quarter <- 3
  } else if (month >= 10 && month <= 12) {
    quarter <- 4
  }

  year_state <- NULL
  year_county <- NULL
  year_metroarea <- NULL

  i <- 8
  while (i > 0) {

    if (is.null(year_state)) {
      data <- suppressMessages(suppressWarnings(hud_il("MD",
                                                        year = year,
                                                        key = key
                                                       )))

      if (!is.null(data) && nrow(data) >= 1) {
        year_state <- year
      }
    }


    if (is.null(year_county)) {
      data <- suppressMessages(suppressWarnings(hud_il("5100199999",
                                                       year = year,
                                                       key = key
                                                       )))

      if (!is.null(data) && nrow(data) >= 1) {
        year_county <- year
      }
    }


    if (is.null(year_metroarea)) {
      data <- suppressMessages(suppressWarnings(hud_il("METRO47900M47900",
                                                        year = year,
                                                        key = key
                                                       )))

      if (!is.null(data) && nrow(data) >= 1) {
        year_metroarea <- year
      }
    }


    if (quarter > 1) {
      quarter <- quarter - 1
    } else {
      year <- year - 1
      quarter <- 4
    }

    i <- i - 1

  }

  return(c(state = year_state,
           county = year_county,
           metroarea = year_metroarea))
}
