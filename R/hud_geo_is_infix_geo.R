#' @name %z_in_trt%
#' @title %z_in_trt%
#' @description Given a zip code and a tract, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with tract
#' @param tract The tract to determine overlap with zip
#' @export
#' @examples
#' \dontrun{
#'
#' 71052 %z_in_trt% 22031950600
#'
#' }
`%z_in_trt%` <- function(zip, tract) {

  args <- hud_rec_cw_yr()

  # TODO: We might want to allow using names also..
  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_tract(zip[i],
                                                                        minimal = TRUE,
                                                                        year = args[1],
                                                                        quarter = args[2])))) %in% as.character(tract))) {
      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }


  return(res)
}



#' @name %z_in_cty%
#' @title %z_in_cty%
#' @description Given a zip code and a county, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with tract
#' @param county The county to determine overlap with zip
#' @export
#' @examples
#' \dontrun{
#'
#' 71052 %z_in_cty% 22031
#'
#' }
`%z_in_cty%` <- function(zip, county) {

  args <- hud_rec_cw_yr()

  if (as.character(county) %in%
      as.character(suppressMessages(hud_cw_zip_county(zip,
                                                      minimal = TRUE,
                                                      year = args[1],
                                                      quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)

}


#' @name %z_in_cbsa%
#' @title %z_in_cbsa%
#' @description Given a zip code and a cbsa, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with cbsa
#' @param cbsa The cbsa to determine overlap with zip
#' @export
#' @examples
#' \dontrun{
#'
#' 71052 %z_in_cbsa% 43340
#'
#' }
`%z_in_cbsa%` <- function(zip, cbsa) {

  args <- hud_rec_cw_yr()

  if (as.character(cbsa) %in%
      as.character(suppressMessages(hud_cw_zip_cbsa(zip,
                                                    minimal = TRUE,
                                                    year = args[1],
                                                    quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}


#' @name %z_in_cbsadiv%
#' @title %z_in_cbsadiv%
#' @description Given a zip code and a cbsadiv, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with cbsadiv
#' @param cbsadiv The cbsadiv to determine overlap with zip
#' @export
#' @examples
#' \dontrun{
#'
#' 71052 %z_in_cbsadiv% 43340
#'
#' }
`%z_in_cbsadiv%` <- function(zip, cbsadiv) {

  args <- hud_rec_cw_yr()

  if (as.character(cbsadiv) %in%
      as.character(suppressMessages(hud_cw_zip_cbsadiv(zip,
                                                       minimal = TRUE,
                                                       year = args[1],
                                                       quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}


#' @name %z_in_ctysb%
#' @title %z_in_ctysb%
#' @description Given a zip code and a countysub, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with countysub
#' @param countysub The countysub to determine overlap with zip
#' @export
#' @examples
#' \dontrun{
#'
#' 71052 %z_in_ctysb% 2203194756
#'
#' }
`%z_in_ctysb%` <- function(zip, countysub) {

  args <- hud_rec_cw_yr()

  if (as.character(ctysb) %in%
      as.character(suppressMessages(hud_cw_zip_countysub(zip,
                                                         minimal = TRUE,
                                                         year = args[1],
                                                         quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)

}


#' @name %z_in_cd%
#' @title %z_in_cd%
#' @description Given a zip code and a congressional district, determine if
#'   they overlap using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param zip The zip to determine overlap with a congressional district
#' @param cd The congressional district to determine overlap with zip
#' @export
#' @examples
#' \dontrun{
#'
#' 71052 %z_in_cd% 2204
#'
#' }
`%z_in_cd%` <- function(zip, cd) {

  args <- hud_rec_cw_yr()

  if (as.character(cd) %in%
      as.character(suppressMessages(hud_cw_zip_cd(zip,
                                                  minimal = TRUE,
                                                  year = args[1],
                                                  quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}






#' @name %trt_in_z%
#' @title %trt_in_z%
#' @description Given a tract and a zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param tract The tract to determine overlap with zip
#' @param zip The zip to determine overlap with tract
#' @export
#' @examples
#' \dontrun{
#'
#' 22031950600 %trt_in_z% 71052
#'
#' }
`%trt_in_z%` <- function(tract, zip) {

  args <- hud_rec_cw_yr()

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_tract_zip(tract,
                                                     minimal = TRUE,
                                                     year = args[1],
                                                     quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}



#' @name %cty_in_z%
#' @title %cty_in_z%
#' @description Given a county and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param county The county to determine overlap with zip.
#' @param zip The zip to determine overlap with county.
#' @export
#' @examples
#' \dontrun{
#'
#' 22031 %cty_in_z% 71052
#'
#' }
`%cty_in_z%` <- function(county, zip) {

  args <- hud_rec_cw_yr()

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_county_zip(county,
                                                      minimal = TRUE,
                                                      year = args[1],
                                                      quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}



#' @name %cbsa_in_z%
#' @title %cbsa_in_z%
#' @description Given a cbsa and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cbsa The cbsa to determine overlap with zip
#' @param zip The zip to determine overlap with cbsa
#' @export
#' @examples
#' \dontrun{
#'
#' 43340 %cbsa_in_z% 71052
#'
#' }
`%cbsa_in_z%` <- function(cbsa, zip) {

  args <- hud_rec_cw_yr()

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_cbsa_zip(cbsa,
                                                    minimal = TRUE,
                                                    year = args[1],
                                                    quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}


#' @name %cbsadiv_in_z%
#' @title %cbsadiv_in_z%
#' @description Given a cbsadiv and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cbsadiv The cbsadiv to determine overlap with zip
#' @param zip The zip to determine overlap with cbsadiv
#' @export
#' @examples
#' \dontrun{
#'
#' 43340 %z_in_cbsadiv% 71052
#'
#' }
`%cbsadiv_in_z%` <- function(cbsadiv, zip) {

  args <- hud_rec_cw_yr()

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_cbsadiv_zip(cbsadiv,
                                                       minimal = TRUE,
                                                       year = args[1],
                                                       quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}


#' @name %cd_in_z%
#' @title %cd_in_z%
#' @description Given a congressional district and a zip, determine if they
#'   overlap using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param cd The cd to determine overlap with zip
#' @param zip The zip to determine overlap with cd
#' @export
#' @examples
#' \dontrun{
#'
#' 2204 %cd_in_z% 71052
#'
#' }
`%cd_in_z%` <- function(cd, zip) {

  args <- hud_rec_cw_yr()

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_cd_zip(cd,
                                                  minimal = TRUE,
                                                  year = args[1],
                                                  quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}



#' @name %ctysb_in_z%
#' @title %ctysb_in_z%
#' @description Given a countysub and zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#' @param ctysb The countysub to determine overlap with zip
#' @param zip The zip to determine overlap with countysub
#' @export
#' @examples
#' \dontrun{
#'
#' 2203194756 %ctysb_in_z% 71052
#'
#' }
`%ctysb_in_z%` <- function(countysub, zip) {

  args <- hud_rec_cw_yr()

  if (as.character(zip) %in%
      as.character(suppressMessages(hud_cw_countysub_zip(countysub,
                                                         minimal = TRUE,
                                                         year = args[1],
                                                         quarter = args[2]
      )))) {
    return(TRUE)
  }
  return(FALSE)
}

