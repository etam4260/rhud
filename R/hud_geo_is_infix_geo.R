#' @name %z_in_trt%
#' @title %z_in_trt%
#' @description Given a zip code and a tract, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
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
  tract <- geo_is_infix_rhs_cleansing(query = tract, "tract")

  res <- c()

  for (i in seq_len(length(zip))) {

    queried <- geo_is_infix_query_and_get_warnings(query = zip[i],
                                                   f = hud_cw_zip_tract,
                                                   year = args[1],
                                                   quarter = args[2],
                                                   querytype = "zip")

    if (any(queried %in% as.character(tract))) {
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
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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


#' @name %z_in_cbsa%
#' @title %z_in_cbsa%
#' @description Given a zip code and a cbsa, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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


#' @name %z_in_cbsadiv%
#' @title %z_in_cbsadiv%
#' @description Given a zip code and a cbsadiv, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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


#' @name %z_in_ctysb%
#' @title %z_in_ctysb%
#' @description Given a zip code and a countysub, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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


#' @name %z_in_cd%
#' @title %z_in_cd%
#' @description Given a zip code and a congressional district, determine if
#'   they overlap using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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






#' @name %trt_in_z%
#' @title %trt_in_z%
#' @description Given a tract and a zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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



#' @name %cty_in_z%
#' @title %cty_in_z%
#' @description Given a county and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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



#' @name %cbsa_in_z%
#' @title %cbsa_in_z%
#' @description Given a cbsa and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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


#' @name %cbsadiv_in_z%
#' @title %cbsadiv_in_z%
#' @description Given a cbsadiv and a zip, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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


#' @name %cd_in_z%
#' @title %cd_in_z%
#' @description Given a congressional district and a zip, determine if they
#'   overlap using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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



#' @name %ctysb_in_z%
#' @title %ctysb_in_z%
#' @description Given a countysub and zip code, determine if they overlap
#'   using the crosswalk files. Overlap will be described if
#'   any residential, business, other, or total addresses reside in both.
#'
#'   This means that it is possible that certain geoids(lhs) are not counted even
#'   though their boundaries intersect the queried geoids(rhs). This is likely
#'   because addresses do not lie in their intersecting region.
#'
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





#' @name geo_is_infix_rhs_cleansing
#' @title geo_is_infix_rhs_cleansing
#' @description Given a geographic identifier, described by the
#'   crosswalk files, determine whether it is the right length and has
#'   correct spacing.
#' @param query The geoid to query for
#' @param geoid_type The type of geoid, either:
#'    1) zip,
#'    2) tract,
#'    3) cd,
#'    4) cbsa,
#'    5) cbsadiv,
#'    6) county,
#'    7) countysub
geo_is_infix_rhs_cleansing <- function(query, geoid_type) {

  query <- unique(paste(trimws(as.character(query), which = "both")))

  if (geoid_type == "zip") {
    if (FALSE %in% numbers_only(query)) stop("\nZip inputs must only be numbers.",
                                             call. = FALSE)
    if (any(nchar(query) != 5)) stop("\nZip inputs are not all of length 5.",
                                      call. = FALSE)
  } else if (geoid_type == "tract") {
    if (FALSE %in% numbers_only(query)) stop("\nTract inputs must only be numbers.",
                                             call. = FALSE)
    if (any(nchar(query) != 11)) stop("\nTract inputs are not all of length 11.",
                                      call. = FALSE)
  } else if (geoid_type == "county") {
    if (FALSE %in% numbers_only(query)) stop("\nCounty inputs must only be numbers.",
                                              call. = FALSE)
    if (any(nchar(query) != 5)) stop("\nCounty inputs are not all of length 5.",
                                      call. = FALSE)
  } else if (geoid_type == "cbsa") {
    if (FALSE %in% numbers_only(query)) stop("\nCbsa inputs must only be numbers.",
                                            call. = FALSE)
    if (any(nchar(query) != 5)) stop("\nCbsa inputs are not all of length 5.",
                                    call. = FALSE)
  } else if (geoid_type == "cbsadiv") {
    if (FALSE %in% numbers_only(query)) stop("\nCbsadiv inputs must only be numbers.",
                                               call. = FALSE)
    if (any(nchar(query) != 5)) stop("\nCbsadiv inputs are not all of length 5.",
                                       call. = FALSE)
  } else if (geoid_type == "cd") {
    if (FALSE %in% numbers_only(query)) stop("\nCd inputs must only be numbers.",
                                             call. = FALSE)
    if (any(nchar(query) != 4)) stop("\nCd inputs are not all of length 4.",
                                     call. = FALSE)
  } else if (geoid_type == "countysub") {
    if (FALSE %in% numbers_only(query)) stop("\nCountysub inputs must only be numbers.",
                                             call. = FALSE)
    if (any(nchar(query) != 10)) stop("\nCountysub inputs are not all of length 10.",
                                     call. = FALSE)
  }
  return(query)
}


#' @name geo_is_infix_query_and_get_warnings
#' @title geo_is_infix_query_and_get_warnings
#' @description
#' @param query The geoids to query for crosswalk
#' @param f The function used query the crosswalk files.
#' @param year The year to query for.
#' @param quarter The quarter to query for.
geo_is_infix_query_and_get_warnings <- function(query,
                                                f,
                                                year,
                                                quarter,
                                                querytype) {

  res <- c()
  tryCatch(
    {
      res <- suppressMessages(f(query,
               minimal = TRUE,
               year = year,
               quarter = quarter))
    },
    error = function(cond)
    {
      stop(cond$message, call. = FALSE)
    },
    warning = function(cond)
    {
      # Might be more efficient to save the errored geoids when used instead
      # of having to regex it...
      warning(paste("\nThe ", querytype, " ", query ," inputted is not valid.",
                    " No data was found for year: ", year , " and quarter: ", quarter,
                    sep = ""
              ), call. = FALSE)

    }
  )
  return(res)
}

