#' @name crosswalk
#' @title crosswalk
#' @description Using the crosswalk files, crosswalk an entire dataset.
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
#' @param data A dataset with rows describing measurements at a zip,
#'   county, countysub, cd,
#'   tract, cbsa, or cbsadiv geographic id.
#' @param geoid The current geoid that the dataset is described in: must be
#'   zip, county, countysub, cd,
#'   tract, cbsa, or cbsadiv geographic id.
#' @param geoid_col The column containing the geographic identifier; must be
#'   zip, county, countysub, cd,
#'   tract, cbsa, or cbsadiv geographic id.
#'   Supply either the name of the column or the index.
#' @param cw_geoid The geoid to crosswalk the dataset to.
#' @param cw_geoid_col The columns in the dataset to distribute
#'   according to method ratio.
#'   If method is empty, no allocation method will be applied --
#'   the crosswalk file will just be attached to the dataset.
#' @param method The allocation method to use: residential,
#'   business, other, or total.
#'   If method is empty, no allocation method will be applied --
#'   the crosswalk file will just be attached to the dataset.
#' @param year The year measurement was taken.
#' @param quarter The quarter of year measurement was taken.
#' @param key The key obtain from HUD USER website.
#' @export
#' @returns A dataframe containing the crosswalked dataset.
crosswalk <- function(data, geoid, geoid_col, cw_geoid, cw_geoid_col = NA,
                      method = NA,
                      year = format(Sys.Date() - 365, "%Y"),
                      quarter = 1, key = Sys.getenv("HUD_KEY")) {

  args <- crosswalk_a_dataset_input_check_cleansing(data, geoid, geoid_col,
                                            cw_geoid, cw_geoid_col, method,
                                            year,
                                            quarter, key)

  geoid <- args[[1]]
  geoid_col <- args[[2]]
  cw_geoid <- args[[3]]

  cw_geoid_col <- args[[4]]

  method <- args[[5]]
  year <- args[[6]]
  quarter <- args[[7]]
  key <- args[[8]]


  if (geoid == "zip" && cw_geoid %in% c("county", "countysub", "tract",
                                       "cbsa", "cbsadiv", "cd")) {
    if (cw_geoid == "county") {
      cw_data <- hud_cw_zip_county(data[, geoid_col], year = year,
                                   quarter = quarter, key = key)
    } else if (cw_geoid == "countysub") {
      cw_data <- hud_cw_zip_countysub(data[, geoid_col], year = year,
                                      quarter = quarter, key = key)
    } else if (cw_geoid == "cd") {
      cw_data <- hud_cw_zip_cd(data[, geoid_col], year = year,
                               quarter = quarter, key = key)
    } else if (cw_geoid == "tract") {
      cw_data <- hud_cw_zip_tract(data[, geoid_col], year = year,
                                  quarter = quarter, key = key)
    } else if (cw_geoid == "cbsa") {
      cw_data <- hud_cw_zip_cbsa(data[, geoid_col], year = year,
                                 quarter = quarter, key = key)
    } else if (cw_geoid == "cbsadiv") {
      cw_data <- hud_cw_zip_cbsadiv(data[, geoid_col], year = year,
                                    quarter = quarter, key = key)
    }
  } else if (geoid == "county" && cw_geoid == "zip") {
    cw_data <- hud_cw_county_zip(data[, geoid_col], year = year,
                                 quarter = quarter, key = key)
  } else if (geoid == "countysub"  && cw_geoid == "zip") {
    cw_data <- hud_cw_countysub_zip(data[, geoid_col], year = year,
                                    quarter = quarter, key = key)
  } else if (geoid == "cd"  && cw_geoid == "zip") {
    cw_data <- hud_cw_cd_zip(data[, geoid_col], year = year,
                             quarter = quarter, key = key)
  } else if (geoid == "tract"  && cw_geoid == "zip") {
    cw_data <- hud_cw_tract_zip(data[, geoid_col], year = year,
                                quarter = quarter, key = key)
  } else if (geoid == "cbsa" && cw_geoid == "zip") {
    cw_data <- hud_cw_cbsa_zip(data[, geoid_col], year = year,
                               quarter = quarter, key = key)
  } else if (geoid == "cbsadiv" && cw_geoid == "zip") {
    cw_data <- hud_cw_cbsadiv_zip(data[, geoid_col], year = year,
                                  quarter = quarter, key = key)
  } else {
    stop(paste("\nCrosswalk from",
               toupper(geoid), "to",
               toupper(w_geoid),
               "is not supported. Type ?crosswalk to see information on
               what is available.",
               sep = " "))
  }

  # If no columns are provides, assume just want to merge...
  # If not method is provided, assume just merge...
  if (is.na(cw_geoid_col) || is.na(method)) {

    return(merge(cw_data, data, by.x = 6, by.y = geoid_col))
  } else if (!is.na(cw_geoid_col) && !is.na(method)) {

    merged <- merge(cw_data, data, by.x = 6, by.y = geoid_col)

    # clear memory
    cw_data <- NULL
    data <- NULL

    # apply method to columns specified.
    if (method == "residential" || method == "res" || "res_ratio") {
      for (i in seq_len(nrow(merged))) {
        merged[i, cw_geoid_col] <- merged[i, cw_geoid_col] *
          merged[i, "res_ratio"]
      }
    } else if (method == "business" || method == "bus" || "bus_ratio") {
      for (i in seq_len(nrow(merged))) {
        merged[i, cw_geoid_col] <- merged[i, cw_geoid_col] *
          merged[i, "bus_ratio"]
      }
    } else if (method == "other" || method == "oth" || "oth_ratio") {
      for (i in seq_len(nrow(merged))) {
        merged[i, cw_geoid_col] <- merged[i, cw_geoid_col] *
          merged[i, "oth_ratio"]
      }
    } else if (method == "total" || method == "tot" || "tot_ratio") {
      for (i in seq_len(nrow(merged))) {
        merged[i, cw_geoid_col] <- merged[i, cw_geoid_col] *
          merged[i, "tot_ratio"]
      }
    }
  }

  return(merged)
}
