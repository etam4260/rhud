#' @name z_in_trt
#' @title Zip Addresses in Tract Addresses?
#' @description Given zip code(s) and tract(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip(s) to determine overlap with tract(s).
#' @param tract The tract(s) to determine overlap with zip(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @returns If zip(s) exist in the tract(s) specified, then TRUE is returned.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @export
#' @examples
#' \dontrun{
#'
#' z_in_trt(zip = 71052, tract = 22031950600, year = 2019, quarter = 2)
#'
#' }
z_in_trt <- function(zip, tract, year, quarter,
                     key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  # Need to validate tract..
  cleaned <- cw_input_check_cleansing(primary_geoid = "tract",
                                      secondary_geoid = "zip",
                                      query = tract, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  tract <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(tract) != 11)) {
    stop("\nInputted tract(s) are not all of length 11.", call. = FALSE)
  }

  res <- c()

  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_tract,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip",
                                                   key = key
                                                   )

    if (any(queried %in% as.character(tract))) {
      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}





#' @name z_in_cty
#' @title Zip Addresses in County Addresses?
#' @description Given zip code(s) and county(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip(s) to determine overlap with county(s).
#' @param county The county(s) to determine overlap with zip(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @returns If zip(s) exist in the county(s) specified, then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' z_in_cty(zip = 71052, county = 22031, year = 2019, quarter = 2)
#'
#' }
z_in_cty <- function(zip, county, year, quarter,
                     key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  cleaned <- cw_input_check_cleansing(primary_geoid = "county",
                                      secondary_geoid = "zip",
                                      query = county, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  county <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(county) != 5)) {
    stop("\nInputted county(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_county,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip",
                                                   key = key
                                                   )

    if (any(queried %in% as.character(county))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name z_in_cbsa
#' @title Zip Addresses in CBSA Addresses?
#' @description Given zip code(s) and cbsa(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip(s) to determine overlap with cbsa(s).
#' @param cbsa The cbsa(s) to determine overlap with zip(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @returns If zip(s) exist in the cbsa(s) specified, then TRUE is returned.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @export
#' @examples
#' \dontrun{
#'
#' z_in_cbsa(zip = 71052, cbsa = 43340, year = 2019, quarter = 2)
#'
#' }
z_in_cbsa <- function(zip, cbsa, year, quarter,
                      key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  cleaned <- cw_input_check_cleansing(primary_geoid = "cbsa",
                                      secondary_geoid = "zip",
                                      query = cbsa, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  cbsa <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(cbsa) != 5)) {
    stop("\nInputted cbsa(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_cbsa,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip",
                                                   key = key
                                                   )

    if (any(queried %in% as.character(cbsa))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name z_in_cbsadiv
#' @title Zip Addresses in CBSAdiv Addresses?
#' @description Given zip code(s) and cbsadiv(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip(s) to determine overlap with cbsadiv(s).
#' @param cbsadiv The cbsadiv(s) to determine overlap with zip(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @returns If zip(s) exist in the cbsadiv(s) specified, then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#'
#' z_in_cbsadiv(zip = 71052, cbsadiv = 43340, year = 2017, quarter = 4)
#'
#' }
z_in_cbsadiv <- function(zip, cbsadiv, year, quarter,
                         key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  cleaned <- cw_input_check_cleansing(primary_geoid = "cbsadiv",
                                      secondary_geoid = "zip",
                                      query = cbsadiv, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  cbsadiv <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(cbsadiv) != 5)) {
    stop("\nInputted cbsadiv(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_cbsadiv,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip",
                                                   key = key)

    if (any(queried %in% as.character(cbsadiv))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}





#' @name z_in_ctysb
#' @title Zip Addresses in Countysub Addresses?
#' @description Given zip code(s) and a countysub(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip(s) to determine overlap with countysub(s).
#' @param countysub The countysub(s) to determine overlap with zip(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @returns If zip(s) exist in the countysub(s) specified, then
#'  TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' z_in_ctysb(zip = 35213, countysub = "0107390324", year = 2019, quarter = 2)
#'
#' }
z_in_ctysb <- function(zip, countysub, year, quarter,
                       key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  cleaned <- cw_input_check_cleansing(primary_geoid = "countysub",
                                      secondary_geoid = "zip",
                                      query = countysub, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  countysub <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(countysub) != 10)) {
    stop("\nInputted countysub(s) are not all of length 10.",
         call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_countysub,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip",
                                                   key = key)

    if (any(queried %in% as.character(countysub))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}



#' @name z_in_cd
#' @title Zip Addresses in Congressional District Addresses?
#' @description Given zip code(s) and congressional district(s), determine
#'   if they overlap using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip(s) to determine overlap with cd(s).
#' @param cd The cd(s) to determine overlap with zip(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @returns If zip(s) exist in the cd(s) specified, then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' z_in_cd(zip = 35213, cd = 2204, year = 2019, quarter = 2)
#'
#' }
z_in_cd <- function(zip, cd, year, quarter, key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  cleaned <- cw_input_check_cleansing(primary_geoid = "cd",
                                      secondary_geoid = "zip",
                                      query = cd, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  cd <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(cd) != 4)) {
    stop("\nInputted cd(s) are not all of length 4.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_cd,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip",
                                                   key = key)

    if (any(queried %in% as.character(cd))) {

      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}








#' @name trt_in_z
#' @title Tract Addresses in Zip Addresses?
#' @description Given tract(s) and a zip code(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param tract The tract(s) to determine overlap with zip(s).
#' @param zip The zip(s) to determine overlap with tract(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @export
#' @returns If tract(s) exist in the zip(s) specified, then TRUE is returned.
#' @examples
#' \dontrun{
#'
#' trt_in_z(tract = 22031950600, zip = 71052, year = 2019, quarter = 2)
#'
#' }
trt_in_z <- function(tract, zip, year, quarter,
                     key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }

  cleaned <- cw_input_check_cleansing(primary_geoid = "zip",
                                      secondary_geoid = "tract",
                                      query = zip, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  zip <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(zip) != 5)) {
    stop("\nInputted zip(s) are not all of length 5.", call. = FALSE)
  }


  res <- c()
  for (i in seq_len(length(tract))) {

    queried <- geo_is_infix_query_and_get_warnings(query = tract[i],
                                                   f = hud_cw_tract_zip,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "tract",
                                                   key = key
                                                   )


    if (any(queried %in% as.character(zip))) {
      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}





#' @name cty_in_z
#' @title County Addresses in Zip Addresses?
#' @description Given county(s) and a zip(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param county The county(s) to determine overlap with zip(s).
#' @param zip The zip(s) to determine overlap with county(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @export
#' @returns If county(s) exist in the zip(s) specified, then TRUE is returned.
#' @examples
#' \dontrun{
#'
#' cty_in_z(county = 22031, zip = 71052, year = 2019, quarter = 2)
#'
#' }
cty_in_z <- function(county, zip, year, quarter,
                     key= Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }


  cleaned <- cw_input_check_cleansing(primary_geoid = "zip",
                                      secondary_geoid = "county",
                                      query = zip, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  zip <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(zip) != 5)) {
    stop("\nInputted zip(s) are not all of length 5.", call. = FALSE)
  }


  res <- c()
  for (i in seq_len(length(county))) {

    queried <- geo_is_infix_query_and_get_warnings(query = county[i],
                                                   f = hud_cw_county_zip,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "county",
                                                   key = key
                                                   )

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name cbsa_in_z
#' @title CBSA Addresses in Zip Addresses?
#' @description Given a cbsa(s) and a zip(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cbsa The cbsa(s) to determine overlap with zip(s).
#' @param zip The zip(s) to determine overlap with cbsa(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @export
#' @returns If cbsa(s) exist in the zip(s) specified, then TRUE is returned.
#' @examples
#' \dontrun{
#'
#' cbsa_in_z(cbsa = 43340, zip = 71052, year = 2019, quarter = 1)
#'
#' }
cbsa_in_z <- function(cbsa, zip, year, quarter,
                      key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }


  cleaned <- cw_input_check_cleansing(primary_geoid = "zip",
                                      secondary_geoid = "cbsa",
                                      query = zip, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  zip <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]


  if (any(nchar(zip) != 5)) {
    stop("\nInputted zip(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(cbsa))) {

    queried <- geo_is_infix_query_and_get_warnings(query = cbsa[i],
                                                   f = hud_cw_cbsa_zip,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "cbsa",
                                                   key = key)

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)

}



#' @name cbsadiv_in_z
#' @title CBSAdiv Addresses in Zip Addresses?
#' @description Given cbsadiv(s) and zip(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cbsadiv The cbsadiv(s) to determine overlap with zip(s).
#' @param zip The zip(s) to determine overlap with cbsadiv(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @export
#' @returns If cbsadiv(s) exist in the zip(s) specified, then TRUE is returned.
#' @examples
#' \dontrun{
#'
#' cbsadiv_in_z(cbsadiv = 43340, zip = 71052, year = 2017, quarter = 4)
#'
#' }
cbsadiv_in_z <- function(cbsadiv, zip, year, quarter,
                         key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }


  cleaned <- cw_input_check_cleansing(primary_geoid = "zip",
                                      secondary_geoid = "cbsadiv",
                                      query = zip, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  zip <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(zip) != 5)) {
    stop("\nInputted zip(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(cbsadiv))) {

    queried <- geo_is_infix_query_and_get_warnings(query = cbsadiv[i],
                                                   f = hud_cw_cbsadiv_zip,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "cbsadiv",
                                                   key = key)

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name cd_in_z
#' @title Congressional District Addresses in Zip Addresses?
#' @description Given congressional district(s) and zip(s), determine if they
#'   overlap using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cd The cd(s) to determine overlap with zip(s).
#' @param zip The zip(s) to determine overlap with cd(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @returns If cd(s) exist in the zip(s) specified, then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' cd_in_z(cd = 2204, zip = 71052, year = 2019, quarter = 4)
#'
#' }
cd_in_z <- function(cd, zip, year, quarter,
                    key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }


  cleaned <- cw_input_check_cleansing(primary_geoid = "zip",
                                      secondary_geoid = "cd",
                                      query = zip, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  zip <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(zip) != 5)) {
    stop("\nInputted zip(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(cd))) {

    queried <- geo_is_infix_query_and_get_warnings(query = cd[i],
                                                   f = hud_cw_cd_zip,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "cd",
                                                   key = key)

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name ctysb_in_z
#' @title Countysub Addresses in Zip Addresses?
#' @description Given countysub(s) and zip code(s), determine if they overlap
#'   using the (United States Postal Service)
#'   USPS Crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param countysub The countysub(s). to determine overlap with zip(s).
#' @param zip The zip(s). to determine overlap with countysub(s).
#' @param year Gets the year that this data was recorded. Can specify multiple
#'   years. Default is the previous year.
#' @param quarter Gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key The API key for this user. You must go to HUD and sign up
#'   for an account and request for an API key.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_cty()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_in_cd]
#' * [rhud::trt_in_z()]
#' * [rhud::cty_in_z()]
#' * [rhud::cbsa_in_z()]
#' * [rhud::cbsadiv_in_z()]
#' * [rhud::cd_in_z()]
#' * [rhud::ctysb_in_z()]
#' @returns If countysub(s) exist in the zip(s) specified,
#'   then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' ctysb_in_z(countysub = 2203194756, zip = 71052, year = 2019, quarter = 2)
#'
#' }
ctysb_in_z <- function(countysub, zip, year, quarter,
                       key = Sys.getenv("HUD_KEY")) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year <- args[1]
    }

    if (missing(quarter)) {
      quarter <- args[2]
    }
  }


  cleaned <- cw_input_check_cleansing(primary_geoid = "zip",
                                      secondary_geoid = "ctysb",
                                      query = zip, year = year,
                                      quarter = quarter,
                                      Sys.getenv("HUD_KEY"))


  zip <- cleaned[1]
  year <- cleaned[2]
  quarter <- cleaned[3]
  key <- cleaned[4]

  if (any(nchar(zip) != 5)) {
    stop("\nInputted zip(s) are not all of length 5.", call. = FALSE)
  }

  res <- c()
  for (i in seq_len(length(countysub))) {

    queried <- geo_is_infix_query_and_get_warnings(query = countysub[i],
                                                   f = hud_cw_countysub_zip,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "countysub",
                                                   key = key)

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}
