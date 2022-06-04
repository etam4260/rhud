#' @name z_in_trt
#' @title z_in_trt
#' @description Given a zip code and a tract, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with tract
#' @param tract The tract to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
z_in_trt <- function(zip, tract, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  args <- hud_rec_cw_yr()

  # TODO: We might want to allow using names also..
  # There is a bit of overhead cost for doing individual queries because each
  # zip will need individual calls to hud_cw_zip_tract... Could optimize by
  # using internal functions...

  # Need to validate tract..
  tract <- geo_is_infix_rhs_cleansing(query = tract, "tract")

  res <- c()

  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_tract,
                                                   year = year,
                                                   quarter = quarter,
                                                   querytype = "zip")

    if (any(queried %in% as.character(tract))) {
      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}





#' @name z_in_cty
#' @title z_in_cty
#' @description Given a zip code and a county, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with county
#' @param county The county to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
z_in_cty <- function(zip, county, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  county <- geo_is_infix_rhs_cleansing(query = county, "county")

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_county,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "zip")

    if (any(queried %in% as.character(county))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name z_in_cbsa
#' @title z_in_cbsa
#' @description Given a zip code and a cbsa, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with cbsa
#' @param cbsa The cbsa to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
z_in_cbsa <- function(zip, cbsa, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  args <- hud_rec_cw_yr()


  cbsa <- geo_is_infix_rhs_cleansing(query = cbsa, "cbsa")

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_cbsa,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "zip")

    if (any(queried %in% as.character(cbsa))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name z_in_cbsadiv
#' @title z_in_cbsadiv
#' @description Given a zip code and a cbsadiv, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with cbsadiv
#' @param cbsadiv The cbsadiv to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
#' z_in_cbsadiv(zip = 71052, cbsadiv = 43340, year = 2019, quarter = 2)
#'
#' }
z_in_cbsadiv <- function(zip, cbsadiv, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  cbsadiv <- geo_is_infix_rhs_cleansing(query = cbsadiv, "cbsadiv")

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_cbsadiv,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "zip")

    if (any(queried %in% as.character(cbsadiv))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}





#' @name z_in_ctysb
#' @title z_in_ctysb
#' @description Given a zip code and a countysub, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with countysub
#' @param countysub The countysub to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
#' @returns If zip(s) exist in the countysub(s) specified, then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' z_in_ctysb(zip = 35213, countysub = 0107390324, year = 2019, quarter = 2)
#'
#' }
z_in_ctysb <- function(zip, countysub, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  countysub <- geo_is_infix_rhs_cleansing(query = countysub, "countysub")

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_countysub,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "zip")

    if (any(queried %in% as.character(countysub))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}



#' @name z_in_cd
#' @title z_in_cd
#' @description Given a zip code and a congressional district, determine
#'   if they overlap using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with cd
#' @param cd The cd to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
z_in_cd <- function(zip, cd, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  cd <- geo_is_infix_rhs_cleansing(query = cd, "cd")

  res <- c()
  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_cd,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "zip")

    if (any(queried %in% as.character(cd))) {

      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}








#' @name trt_in_z
#' @title trt_in_z
#' @description Given a tract and a zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param tract The tract to determine overlap with zip
#' @param zip The zip to determine overlap with tract
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
trt_in_z <- function(tract, zip, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  zip <- geo_is_infix_rhs_cleansing(query = zip, "zip")

  res <- c()
  for (i in seq_len(length(tract))) {

    queried <- geo_is_infix_query_and_get_warnings(query = tract[i],
                                                   f = hud_cw_tract_zip,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "tract")


    if (any(queried %in% as.character(zip))) {
      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}





#' @name cty_in_z
#' @title cty_in_z
#' @description Given a county and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param county The county to determine overlap with zip.
#' @param zip The zip to determine overlap with county.
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
cty_in_z <- function(county, zip, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  zip <- geo_is_infix_rhs_cleansing(query = zip, "zip")

  res <- c()
  for (i in seq_len(length(county))) {

    queried <- geo_is_infix_query_and_get_warnings(query = county[i],
                                                   f = hud_cw_county_zip,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "county")

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name cbsa_in_z
#' @title cbsa_in_z
#' @description Given a cbsa and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cbsa The cbsa to determine overlap with zip
#' @param zip The zip to determine overlap with cbsa
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
cbsa_in_z <- function(cbsa, zip, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  zip <- geo_is_infix_rhs_cleansing(query = zip, "zip")

  res <- c()
  for (i in seq_len(length(cbsa))) {

    queried <- geo_is_infix_query_and_get_warnings(query = cbsa[i],
                                                   f = hud_cw_cbsa_zip,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "cbsa")

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)

}



#' @name cbsadiv_in_z
#' @title cbsadiv_in_z
#' @description Given a cbsadiv and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cbsadiv The cbsadiv to determine overlap with zip
#' @param zip The zip to determine overlap with cbsadiv
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
#' cbsadiv_in_zip(cbsadiv = 43340, zip = 71052, year = year, quarter = quarter)
#'
#' }
cbsadiv_in_z <- function(cbsadiv, zip, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  zip <- geo_is_infix_rhs_cleansing(query = zip, "zip")

  res <- c()
  for (i in seq_len(length(cbsadiv))) {

    queried <- geo_is_infix_query_and_get_warnings(query = cbsadiv[i],
                                                   f = hud_cw_cbsadiv_zip,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "cbsadiv")

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name cd_in_z
#' @title cd_in_z
#' @description Given a congressional district and a zip, determine if they
#'   overlap using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cd The cd to determine overlap with zip
#' @param zip The zip to determine overlap with cd
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
cd_in_z <- function(cd, zip, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  zip <- geo_is_infix_rhs_cleansing(query = zip, "zip")

  res <- c()
  for (i in seq_len(length(cd))) {

    queried <- geo_is_infix_query_and_get_warnings(query = cd[i],
                                                   f = hud_cw_cd_zip,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "cd")

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}




#' @name ctysb_in_z
#' @title ctysb_in_z
#' @description Given a countysub and zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param countysub The countysub to determine overlap with zip
#' @param zip The zip to determine overlap with countysub
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
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
#' @returns If countysub(s) exist in the zip(s) specified, then TRUE is returned.
#' @export
#' @examples
#' \dontrun{
#'
#' ctysb_in_z(countysub = 2203194756, zip = 71052, year = 2019, quarter = 2)
#'
#' }
ctysb_in_z <- function(countysub, zip, year, quarter) {

  if (missing(year) || missing(quarter)) {
    args <- hud_rec_cw_yr()
    if (missing(year)) {
      year = args[1]
    }

    if (missing(quarter)) {
      quarter = args[2]
    }
  }

  zip <- geo_is_infix_rhs_cleansing(query = zip, "zip")

  res <- c()
  for (i in seq_len(length(countysub))) {

    queried <- geo_is_infix_query_and_get_warnings(query = countysub[i],
                                                   f = hud_cw_countysub_zip,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "countysub")

    if (any(queried %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}
