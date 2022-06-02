#' @name %z_in_trt%
#' @title %z_in_trt%
#' @description Given a zip code and a tract, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) which have boundaries
#'   that intersect that of the queried geoids(rhs) that are excluded.
#'
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
  # There is a bit of overhead cost for doing individual queries because each
  # zip will need individual calls to hud_cw_zip_tract... Could optimize by
  # using internal functions...

  # Need to validate tract..

  tract <- unique(paste(trimws(as.character(tract), which = "both")))
  if (FALSE %in% numbers_only(tract)) stop("\nTract inputs must only be numbers.",
                                           call. = FALSE)
  if (any(nchar(tract) != 11)) stop("\nTract inputs are not all of length 11.",
                                 call. = FALSE)

  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_tract(zip[i],
                            minimal = TRUE,
                            year = args[1],
                            quarter = args[2])))) %in% as.character(tract))) {

      # What if zip does not have any data (invalid zip). Might want to catch
      # the warning and tell user... Furthermore, you could validate the
      # tract... using hud_tract_... but that would more API calls which might
      # not be worthwhile.

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

  # TODO: We might want to allow using names also..


  county <- unique(paste(trimws(as.character(county), which = "both")))
  if (FALSE %in% numbers_only(county)) stop("\nCounty inputs must only be numbers.",
                                           call. = FALSE)
  if (any(nchar(county) != 5)) stop("\nCounty inputs are not all of length 5.",
                                    call. = FALSE)


  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_county(zip[i],
                            minimal = TRUE,
                            year = args[1],
                            quarter = args[2])))) %in% as.character(county))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  cbsa <- unique(paste(trimws(as.character(cbsa), which = "both")))
  if (FALSE %in% numbers_only(cbsa)) stop("\nCbsa inputs must only be numbers.",
                                            call. = FALSE)
  if (any(nchar(cbsa) != 5)) stop("\nCbsa inputs are not all of length 5.",
                                    call. = FALSE)

  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_cbsa(zip[i],
                             minimal = TRUE,
                             year = args[1],
                             quarter = args[2])))) %in% as.character(cbsa))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  cbsadiv <- unique(paste(trimws(as.character(cbsadiv), which = "both")))
  if (FALSE %in% numbers_only(cbsadiv)) stop("\nCbsadiv inputs must only be numbers.",
                                          call. = FALSE)
  if (any(nchar(cbsadiv) != 5)) stop("\nCbsadiv inputs are not all of length 5.",
                                  call. = FALSE)

  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_cbsadiv(zip[i],
                           minimal = TRUE,
                           year = args[1],
                           quarter = args[2])))) %in% as.character(cbsadiv))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  countysub <- unique(paste(trimws(as.character(countysub), which = "both")))
  if (FALSE %in% numbers_only(countysub)) stop("\nCountysub inputs must only be numbers.",
                                             call. = FALSE)
  if (any(nchar(countysub) != 10)) stop("\nCountysub inputs are not all of length 10.",
                                     call. = FALSE)

  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_countysub(zip[i],
                        minimal = TRUE,
                        year = args[1],
                        quarter = args[2])))) %in% as.character(countysub))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  cd <- unique(paste(trimws(as.character(cd), which = "both")))
  if (FALSE %in% numbers_only(cd)) stop("\nCd inputs must only be numbers.",
                                               call. = FALSE)
  if (any(nchar(cd) != 4)) stop("\nCd inputs are not all of length 4.",
                                        call. = FALSE)

  res <- c()
  for (i in seq_len(length(zip))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_zip_cd(zip[i],
                                minimal = TRUE,
                                year = args[1],
                                quarter = args[2])))) %in% as.character(cd))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  zip <- unique(paste(trimws(as.character(zip), which = "both")))
  if (FALSE %in% numbers_only(zip)) stop("\nZip inputs must only be numbers.",
                                        call. = FALSE)
  if (any(nchar(zip) != 5)) stop("\nZip inputs are not all of length 5.",
                                call. = FALSE)

  res <- c()
  for (i in seq_len(length(tract))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_tract_zip(tract[i],
                             minimal = TRUE,
                             year = args[1],
                             quarter = args[2])))) %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  zip <- unique(paste(trimws(as.character(zip), which = "both")))
  if (FALSE %in% numbers_only(zip)) stop("\nZip inputs must only be numbers.",
                                         call. = FALSE)
  if (any(nchar(zip) != 5)) stop("\nZip inputs are not all of length 5.",
                                 call. = FALSE)

  res <- c()
  for (i in seq_len(length(county))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_county_zip(county[i],
                              minimal = TRUE,
                              year = args[1],
                              quarter = args[2])))) %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  zip <- unique(paste(trimws(as.character(zip), which = "both")))
  if (FALSE %in% numbers_only(zip)) stop("\nZip inputs must only be numbers.",
                                         call. = FALSE)
  if (any(nchar(zip) != 5)) stop("\nZip inputs are not all of length 5.",
                                 call. = FALSE)

  res <- c()
  for (i in seq_len(length(cbsa))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_cbsa_zip(cbsa[i],
                               minimal = TRUE,
                               year = args[1],
                               quarter = args[2])))) %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  zip <- unique(paste(trimws(as.character(zip), which = "both")))
  if (FALSE %in% numbers_only(zip)) stop("\nZip inputs must only be numbers.",
                                         call. = FALSE)
  if (any(nchar(zip) != 5)) stop("\nZip inputs are not all of length 5.",
                                 call. = FALSE)

  res <- c()
  for (i in seq_len(length(cbsadiv))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_cbsadiv_zip(cbsadiv[i],
                               minimal = TRUE,
                               year = args[1],
                               quarter = args[2])))) %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  zip <- unique(paste(trimws(as.character(zip), which = "both")))
  if (FALSE %in% numbers_only(zip)) stop("\nZip inputs must only be numbers.",
                                         call. = FALSE)
  if (any(nchar(zip) != 5)) stop("\nZip inputs are not all of length 5.",
                                 call. = FALSE)

  res <- c()
  for (i in seq_len(length(cd))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_cd_zip(cd[i],
                                 minimal = TRUE,
                                 year = args[1],
                                 quarter = args[2])))) %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
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

  zip <- unique(paste(trimws(as.character(zip), which = "both")))
  if (FALSE %in% numbers_only(zip)) stop("\nZip inputs must only be numbers.",
                                         call. = FALSE)
  if (any(nchar(zip) != 5)) stop("\nZip inputs are not all of length 5.",
                                 call. = FALSE)

  res <- c()
  for (i in seq_len(length(countysub))) {
    if (any(as.character(suppressWarnings(suppressMessages(hud_cw_countysub_zip(countysub[i],
                               minimal = TRUE,
                               year = args[1],
                               quarter = args[2])))) %in% as.character(zip))) {


      res <- c(res, TRUE)
    } else {
      res <- c(res, FALSE)
    }
  }

  return(res)
}

