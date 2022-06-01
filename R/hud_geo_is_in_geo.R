#' @name z_in_trt
#' @title z_in_trt
#' @description Given a zip code and a tract, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with tract
#' @param tract The tract to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(tract) %in%
      as.character(suppressMessages(hud_cw_zip_tract(zip,
                                                     minimal = TRUE,
                                                     year = year,
                                                     quarter = quarter)))) {
    return(TRUE)
  }

  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' z_in_cty(zip = 71052, county = 22031, year = 2019, quarter = 2)
#'
#' }
z_in_cty <- function(zip, county, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(county) %in%
      as.character(suppressMessages(hud_cw_zip_county(zip,
                                                      minimal = TRUE,
                                                      year = year,
                                                      quarter = quarter)))) {
    return(TRUE)
  }

  return(FALSE)
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
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(cbsa) %in%
      as.character(suppressMessages(hud_cw_zip_cbsa(zip,
                                                      minimal = TRUE,
                                                      year = year,
                                                      quarter = quarter)))) {
    return(TRUE)
  }

  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' z_in_cbsadiv(zip = 71052, cbsadiv = 43340, year = 2019, quarter = 2)
#'
#' }
z_in_cbsadiv <- function(zip, cbsadiv, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(cbsadiv) %in%
      as.character(suppressMessages(hud_cw_zip_cbsadiv(zip,
                                                       minimal = TRUE,
                                                       year = year,
                                                       quarter = quarter)))) {
    return(TRUE)
  }

  return(FALSE)
}





#' @name z_in_ctysub
#' @title z_in_ctysub
#' @description Given a zip code and a countysub, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with countysub
#' @param countysub The countysub to determine overlap with zip
#' @param year The year of the crosswalk files.
#' @param quarter The quarter of the crosswalk files.
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' z_in_ctysub(zip = 35213, countysub = 0107390324, year = 2019, quarter = 2)
#'
#' }
z_in_ctysb <- function(zip, countysub, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(countysub) %in%
      as.character(suppressMessages(hud_cw_zip_countysub(zip,
                                                       minimal = TRUE,
                                                       year = year,
                                                       quarter = quarter)))) {
    return(TRUE)
  }

  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' z_in_cd(zip = 35213, cd = 2204, year = 2019, quarter = 2)
#'
#' }
z_in_cd <- function(zip, cd, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(cd) %in%
      as.character(suppressMessages(hud_cw_zip_cd(zip,
                                                 minimal = TRUE,
                                                 year = year,
                                                 quarter = quarter)))) {
    return(TRUE)
  }

  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' trt_in_z(tract = 22031950600, zip = 71052, year = 2019, quarter = 2)
#'
#' }
trt_in_z <- function(tract, zip, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_tract_zip(tract,
                                                     minimal = TRUE,
                                                     year = year,
                                                     quarter = quarter
      )))) {
    return(TRUE)
  }
  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' cty_in_z(county = 22031, zip = 71052, year = 2019, quarter = 2)
#'
#' }
cty_in_z <- function(county, zip, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_county_zip(county,
                                                      minimal = TRUE,
                                                      year = year,
                                                      quarter = quarter
      )))) {
    return(TRUE)
  }
  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' cbsa_in_z(cbsa = 43340, zip = 71052, year = 2019, quarter = 1)
#'
#' }
cbsa_in_z <- function(cbsa, zip, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }


  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_cbsa_zip(cbsa,
                                                    minimal = TRUE,
                                                    year = year,
                                                    quarter = quarter
      )))) {
    return(TRUE)
  }
  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' z_in_cbsadiv(cbsadiv = 43340, zip = 71052, year = year, quarter = quarter)
#'
#' }
cbsadiv_in_z <- function(cbsadiv, zip, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_cbsadiv_zip(cbsadiv,
                                                       minimal = TRUE,
                                                       year = year,
                                                       quarter = quarter
      )))) {
    return(TRUE)
  }
  return(FALSE)
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
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' cd_in_z(cd = 2204, zip = 71052, year = 2019, quarter = 4)
#'
#' }
cd_in_z <- function(cd, zip, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_cd_zip(cd,
                                                  minimal = TRUE,
                                                  year = year,
                                                  quarter = quarter
      )))) {
    return(TRUE)
  }
  return(FALSE)
}




#' @name ctysb_in_z
#' @title ctysb_in_z
#' @description Given a countysub and zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param countysub The countysub to determine overlap with zip
#' @param zip The zip to determine overlap with countysub
#' @seealso
#' * [rhud::z_in_trt()]
#' * [rhud::z_in_county()]
#' * [rhud::z_in_cbsa()]
#' * [rhud::z_in_cbsadiv()]
#' * [rhud::z_in_ctysb()]
#' * [rhud::z_cd()]
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
#' ctysb_in_z(countysub = 2203194756, zip = 71052, year = 2019, quarter = 2)
#'
#' }
ctysb_in_z <- function(countysub, zip, year, quarter) {

  if (missing(year)) {
    args <- hud_rec_cw_yr()
    year = args[1]
  }

  if (missing(quarter)) {
    args <- hud_rec_cw_yr()
    quarter = args[2]
  }

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_countysub_zip(countysub,
                                                         minimal = TRUE,
                                                         year = year,
                                                         quarter = quarter
      )))) {
    return(TRUE)
  }
  return(FALSE)
}
