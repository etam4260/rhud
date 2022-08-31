#' @import tibble


#' @name crosswalk
#' @title Crosswalk a Dataset
#' @description Using the US Housing and Urban Development USPS Crosswalk files,
#'  crosswalk an entire dataset.
#'  Currently supported crosswalks:
#'   1) zip-tract
#'   2) zip-county
#'   3) zip-cbsa
#'   4) zip-cbsadiv (Available 4th Quarter 2017 onwards)
#'   5) zip-cd
#'   6) tract-zip
#'   7) county-zip
#'   8) cbsa-zip
#'   9) cbsadiv-zip (Available 4th Quarter 2017 onwards)
#'   10) cd-zip
#'   11) zip-countysub (Available 2nd Quarter 2018 onwards)
#'   12) countysub-zip (Available 2nd Quarter 2018 onwards)
#' @param data A dataframe or tibble with rows describing measurements at a zip,
#'   county, county subdivision (countysub), congressional district (cd),
#'   census tract, core base statistical area (cbsa), or core based
#'   statistical area division (cbsadiv) geographic identifier.
#'   1) zip
#'   2) tract
#'   3) county
#'   4) countysub
#'   5) cbsa
#'   6) cbsadiv
#'   7) cd
#' @param geoid A character vector describing the current geoid
#'   that the dataset is described in: must be
#'   zip, county, countysub, cd,
#'   tract, cbsa, or cbsadiv geographic identifier.
#'   1) zip
#'   2) tract
#'   3) county
#'   4) countysub
#'   5) cbsa
#'   6) cbsadiv
#'   7) cd
#' @param geoid_col A character or numeric vector of length one:
#'   the column containing the geographic identifier; must be
#'   zip, county, county subdivision (countysub), congressional district (cd),
#'   census tract, core base statistical area (cbsa), and core based
#'   statistical area division (cbsadiv) geographic identifier.
#'   Supply either the name of the column or the index.
#'   All elements in this column must be numbers only at the proper length.
#'   For example, zip codes must be 5 digit numbers.
#' @param cw_geoid A character vector of length one: the geoid to crosswalk
#'   the dataset to; must be zip, county, county subdivision (countysub),
#'   congressional district (cd),
#'   census tract, core base statistical area (cbsa), or core based
#'   statistical area division (cbsadiv) geoid.
#'   1) zip
#'   2) tract
#'   3) county
#'   4) countysub
#'   5) cbsa
#'   6) cbsadiv
#'   7) cd
#' @param cw_geoid_col A character or numeric vector: the columns in the dataset
#'   to distribute according to method ratio.
#'   If method is empty, no allocation method will be applied --
#'   the crosswalk file will just be merged to the dataset.
#'   All elements in these columns must be numbers only.
#' @param method A character vector: the allocation method to use --
#'   residential, business, other, or total. If method is empty, no allocation
#'   method will be applied -- the crosswalk file will just be merged
#'   to the dataset.
#'   1) res
#'   2) bus
#'   3) tot
#'   4) oth
#' @param year A character or numeric vector: gets the year that this data was
#'   recorded. Can specify multiple years. Default is the previous year.
#' @param quarter A character or numeric vector:
#'   gets the quarter of the year that this data was recorded.
#'   Defaults to the first quarter of the year.
#' @param key A character vector of length one with the key obtained from HUD
#'   (US Department of Housing and Urban Development)
#'   USER website.
#' @param to_tibble A logical: if TRUE, return the data in a tibble format
#'   rather than a data frame.
#' @seealso
#' * [rhud::crosswalk()]
#' * [rhud::hud_cw_zip_tract()]
#' * [rhud::hud_cw_zip_county()]
#' * [rhud::hud_cw_zip_cbsa()]
#' * [rhud::hud_cw_zip_cbsadiv()]
#' * [rhud::hud_cw_zip_countysub()]
#' * [rhud::hud_cw_zip_cd()]
#' * [rhud::hud_cw_tract_zip()]
#' * [rhud::hud_cw_county_zip()]
#' * [rhud::hud_cw_cbsa_zip()]
#' * [rhud::hud_cw_cbsadiv_zip()]
#' * [rhud::hud_cw_cd_zip()]
#' * [rhud::hud_cw_countysub_zip()]
#' * [rhud::hud_cw()]
#' @export
#' @returns A dataframe or tibble containing the crosswalked dataset.
#' @examples
#' \dontrun{
#' sample <- data.frame(population = c(42134, 12413, 13132),
#'                      county = c(24047, 24045, 24043))
#'
#' crosswalk(data = sample, geoid = "county", geoid_col = "county",
#'           cw_geoid = "zip")
#'
#' crosswalk(data = sample, geoid = "county", geoid_col = "county",
#'           cw_geoid = "zip", cw_geoid_col = "population", method = "res")
#'
#' crosswalk(data = sample, geoid = "county", geoid_col = "county",
#'           cw_geoid = "zip", cw_geoid_col = "population", method = "bus")
#'
#' crosswalk(data = sample, geoid = "county", geoid_col = "county",
#'           cw_geoid = "zip", cw_geoid_col = "population", method = "bus",
#'           year = 2018, quarter = 1)
#'
#' }
crosswalk <- function(data,
                      geoid,
                      geoid_col,
                      cw_geoid,
                      cw_geoid_col = NA,
                      method = NA,
                      year = format(Sys.Date() - 365, "%Y"),
                      quarter = 1,
                      key = Sys.getenv("HUD_KEY"),
                      to_tibble = getOption("rhud_use_tibble", FALSE)) {

  is_internet_available()
  result <- NULL

  args <- crosswalk_a_dataset_input_check_cleansing(data, geoid, geoid_col,
                                            cw_geoid, cw_geoid_col, method,
                                            year,
                                            quarter, key)


  geoid <- args$geoid
  geoid_col <- args$geoid_col
  cw_geoid <- args$cw_geoid

  cw_geoid_col <- args$cw_geoid_col

  method <- args$method
  year <- args$year
  quarter <- args$quarter
  key <- args$key

  cw_data <- switch(
    geoid,

    "zip" = switch(
      cw_geoid,
      "county" =    hud_cw_zip_county(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
      "countysub" = hud_cw_zip_countysub(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
      "cd" =        hud_cw_zip_cd(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
      "tract" =     hud_cw_zip_tract(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
      "cbsa" =      hud_cw_zip_cbsa(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
      "cbsadiv" =   hud_cw_zip_cbsadiv(data[, geoid_col], year = year,
                                      quarter = quarter, key = key)
    ),

    "county" =      hud_cw_county_zip(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
    "countysub" =   hud_cw_countysub_zip(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
    "cd" =          hud_cw_cd_zip(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
    "tract" =       hud_cw_tract_zip(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
    "cbsa" =        hud_cw_cbsa_zip(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),
    "cbsadiv" =     hud_cw_cbsadiv_zip(data[, geoid_col], year = year,
                                      quarter = quarter, key = key),

    stop(paste("\nCrosswalk from",
               toupper(geoid), "to",
               toupper(cw_geoid),
               "is not supported. Type ?crosswalk to see information on ",
               "what is available.",
               sep = " "), call. = FALSE)
  )

  # If no columns are provides, assume just want to merge...
  # If no method is provided, assume merge and crosswalk
  if (is.na(cw_geoid_col) || is.na(method)) {
    message("\n* No method or cw_geoid_col specified: ",
            "will just merge the datasets.")

    result <- merge(cw_data, data, by.x = 1, by.y = geoid_col)

  } else if (!is.na(cw_geoid_col) && !is.na(method)) {

    merged <- merge(cw_data, data, by.x = 1, by.y = geoid_col)


    cw_data <- NULL
    data <- NULL

    # Can replace this with a switch statement instead....
    allocation_method <- switch(
      method,
      "residential" = "res_ratio",
      "res" = "res_ratio",
      "res_ratio" = "res_ratio",

      "business" = "bus_ratio",
      "bus" = "bus_ratio",
      "bus_ratio" = "bus_ratio",

      "other" = "oth_ratio",
      "oth" = "oth_ratio",
      "oth_ratio" = "oth_ratio",

      "total" = "tot_ratio",
      "tot" = "tot_ratio",
      "tot_ratio" = "tot_ratio",

      stop("\nThe method specified might be invalid. Check the documentation.",
           call. = FALSE)
    )

    for (i in seq_len(nrow(merged))) {
      merged[i, cw_geoid_col] <- as.numeric(merged[i, cw_geoid_col]) *
        as.numeric(merged[i, allocation_method])
    }

    result <- merged
  }

  if (to_tibble) {
    result <- as_tibble(merged)
  }

  result
}
