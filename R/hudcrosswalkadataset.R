#' @name crosswalk
#' @title crosswalk
#' @description Will crosswalk a dataset given the geoid column and fields that
#'   should be crosswalked. This function does not handle large crosswalks very
#'   well. Performance enhancements have not been implemented.
#' @param dataset The dataset
#' @param type The crosswalk types:
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
#' @param geoid_col The column where the geoid is located.
#' @param year The years to query for.
#' @param quarter The quarters in the year to query for.
#' @param crosswalkable_fields The fields in dataset that are able to be
#'   crosswalked. Should only be numeric values in them. If not specified, will
#'   assume all fields that are of type numeric double or integer are
#'   crosswalkable. This may be problematic if numbers are used as categorical
#'   data such as a customer ID. You can choose to not include or include columns.
#' @param ratio Can either choose residential, business, others, or total
#'   ratios. If one is supplied it will apply to all crosswalk able fields. If
#'   the same length is provided, will crosswalk it with those specific ratios
#'   for each column respectively. Defaults to the total ratio of buildings.
#'   1) "res"
#'   2) "bus"
#'   3) "oth"
#' 4) "tot"
#' @param round Since numbers are multiplied by a ratio, discrete integers will
#'   become continuous.
#' @param key The key obtained at HUD User.
#' @returns The entire dataset crosswalked to a different geoid.
crosswalk <- function(dataset, type, geoid_col, crosswalkable_fields = NA, ratio = "tot", round = FALSE, year = format(Sys.Date() - 365, "%Y"), quarter = 1, key = Sys.getenv("HUD_KEY")) {
  # Check for the key set.
  if(key == "") stop("Did you forget to set the key? Please go to https://www.huduser.gov/hudapi/public/register?comingfrom=1 to and sign up and get a token. Then save this to your environment using Sys.setenv('HUD_KEY' = YOUR_KEY)")
  # Check if user specified the right ratio types that they want
  if(any(ratio != "tot" && ratio != "res" && ratio != "bus" && ratio != "oth")) stop("Fields in ratio should only be 'tot', 'res', 'bus', or 'oth'.")

  # Make sure length of ratio specified is same as length of
  # crosswalkable_fields. Or can can be length 1 to be applied to all fields.
  if(length(ratio) != length(crosswalkable_fields) && length(ratio) != 1) stop("Length of ratio and crosswalkable_fields should be equal or ratio should only be of length 1.")

  # If negative values specified in crosswalkable_fields, make sure its
  # positive version is in range of the dataset.
  if(all(is.integer(crosswalkable_fields) || is.double(crosswalkable_fields)) && all(is.negative(crosswalkable_fields)) && !all(-crosswalkable_fields %in% rep(1:ncol(dataset)))) stop("If you are choosing to choose columns to not crosswalk, please make sure their positive value corresponds to an existing column index.")

  # If positive values, make sure they are of integer or double types. Make sure
  # they are all positive. Make sure that the indices specified are in the
  # length of the number of columns in dataset.
  if(all(is.integer(crosswalkable_fields) || is.double(crosswalkable_fields)) && all(!is.negative(crosswalkable_fields)) && !all(crosswalkable_fields %in% rep(1:ncol(dataset)))) stop("Column numbers for crosswalkable_fields are out of bounds.")

  # Make sure length of ratio specified is same as length of
  # crosswalkable_fields. Or can can be length 1 to be applied to all fields.
  if(all(is.character(crosswalkable_fields) && !all(crosswalkable_fields %in% colnames(dataset)))) stop("If column name inputs for crosswalkable_fields, they should be column names in the dataset.")

  # I want to generalize this function to not need a geoid_col. Is there a way
  # to detect if certain columns specify a geographic identifer?

  # Make sure type field has white space trimmed.
  type <- paste(trimws(as.character(type), which = "both"))

  # Determine if user supplied missing geoid_col or missing dataset.
  if(missing(geoid_col)) stop("Please make sure the dataset includes the crosswalked geoid you want.")
  if(missing(dataset)) stop("Please specify the dataset you want to transform.")

  # Check if inputs of year and quarter are valid.
  if(FALSE %in% numbers_only(year)) stop("Year input must only be numbers.")
  if(FALSE %in% numbers_only(quarter)) stop("Quarter input must only be numbers.")

  # Remove all non-identifiable geoids in the dataset. These are NA values NULLs
  # or non-numeric values in the field. Will first try to correct these
  # fields. For example, if given 1102 as a county, will assume it is missing
  # a 0 at the front.
  dataset <- crosswalk_function_input_check_cleansing(dataset, type, geoid_col)

  message("Making the API calls to HUD. This might take a bit...")
  if(type == "1" || type == "zip-tract") {
    crosswalk_dataset <- hud_cw_zip_tract(unlist(unique(dataset[[geoid_col]])),
                                          year = year, quarter = quarter)
  } else if(type == "2" || type == "zip-county") {
    crosswalk_dataset <- hud_cw_zip_county(unlist(unique(dataset[[geoid_col]])),
                                           year = year, quarter = quarter)
  } else if(type == "3" || type == "zip-cbsa") {
    crosswalk_dataset <- hud_cw_zip_cbsa(unlist(unique(dataset[[geoid_col]])),
                                         year = year, quarter = quarter)
  } else if(type == "4" || type == "zip-cbsadiv") {
    crosswalk_dataset <- hud_cw_zip_cbsadiv(unlist(unique(dataset[[geoid_col]])),
                                            year = year, quarter = quarter)
  } else if(type == "5" || type == "zip-cd") {
    crosswalk_dataset <- hud_cw_zip_cd(unlist(unique(dataset[[geoid_col]])),
                                       year = year, quarter = quarter)
  } else if(type == "6" || type == "tract-zip") {
    crosswalk_dataset <- hud_cw_tract_zip(unlist(unique(dataset[[geoid_col]])),
                                          year = year, quarter = quarter)
  } else if(type == "7" || type == "county-zip") {
    crosswalk_dataset <- hud_cw_county_zip(unlist(unique(dataset[[geoid_col]])),
                                           year = year, quarter = quarter)
  } else if(type == "8" || type == "cbsa-zip") {
    crosswalk_dataset <- hud_cw_cbsa_zip(unlist(unique(dataset[[geoid_col]])),
                                         year = year, quarter = quarter)
  } else if(type == "9" || type == "cbsadiv-zip") {
    crosswalk_dataset <- hud_cw_cbsadiv_zip(unlist(unique(dataset[[geoid_col]])),
                                            year = year, quarter = quarter)
  } else if(type == "10" || type == "cd-zip") {
    crosswalk_dataset <- hud_cw_cd_zip(unlist(unique(dataset[[geoid_col]])),
                                       year = year, quarter = quarter)
  } else if(type == "11" || type == "zip-countysub") {
    crosswalk_dataset <- hud_cw_zip_countysub(unlist(unique(dataset[[geoid_col]])),
                                              year = year, quarter = quarter)
  } else if(type == "12" || type == "countysub-zip") {
    crosswalk_dataset <- hud_cw_countysub_zip(unlist(unique(dataset[[geoid_col]])),
                                              year = year, quarter = quarter)
  } else {
    stop("You did not specify a valid value in the type argument.")
  }

  message("Crosswalking the dataset...")

  # Merge the two datasets...
  crosswalked <- merge(crosswalk_dataset[c(strsplit(type, "-")[[1]][1], strsplit(type, "-")[[1]][2], paste(ratio, "_ratio", sep=""))],
                       dataset, by.x = 1, by.y = as.numeric(geoid_col),
                       all.x = TRUE, all.y = FALSE, sort = TRUE)

  # For convertable_cols, should be able to supply both the names of the columns
  # as well as the column numbers... Will go through the dataset and find those
  # that are numeric, integer, or double types. Will assume cols that are
  # numbers should be crosswalkable -- can be multiplied by a ratio number.
  if(is.na(crosswalkable_fields)) {
    crosswalkable_fields <- data_types(dataset = crosswalked, sample = 1)
    crosswalkable_fields <- as.vector(which(crosswalkable_fields == "double" | crosswalkable_fields == "integer" | crosswalkable_fields == "numeric"))
  }

  # If crosswalkable fields supplied as negative, return the all the columns
  # but not the ones marked as negative.
  if(all(is.negative(crosswalkable_fields))) crosswalkable_fields <- rep(1:ncol(dataset))[-c(1,2)]

  # Multiply the crosswalkable fields in dataset by the tot ratio and return...
  # For now a very inefficient for loop, should be optimized for parallel
  # computations.
  # An issue is that we can't assume numeric data is continuous, it might be ordinal or nominal...
  for(i in 1:nrow(crosswalked)) {
    for(j in crosswalkable_fields[3:length(crosswalkable_fields)]) {
      if(round) {
        crosswalked[i, j] <- eval(round(crosswalked[i, j] * crosswalked[i, 3]), baseenv())
      } else {
        crosswalked[i, j] <- crosswalked[i, j] * crosswalked[i, 3]
      }
    }
  }

  return(crosswalked)
}


#' @name crosswalk_function_input_check_cleansing
#' @title crosswalk_function_input_check_cleansing
#' @description Helper function used to clean user inputted dataset which could
#'   contain geoid formats which are not identifiable by the computer.
#' @param geoid The geoid the current dataset has.
#' @param column The column of geoids.
#' @returns A dataframe with only the valid geoids.
#' @noRd
#' @noMd
crosswalk_function_input_check_cleansing <- function(dataset, geoid, column) {
  #   1) zip-tract
  #   2) zip-county
  #   3) zip-cbsa
  #   4) zip-cbsadiv (Available 4th Quarter 2017 onwards)
  #   5) zip-cd
  #   6) tract-zip
  #   7) county-zip
  #   8) cbsa-zip
  #   9) cbsadiv-zip (Available 4th Quarter 2017 onwards)
  #   10) cd-zip
  #   11) zip-countysub (Available 2nd Quarter 2018 onwards)
  #   12) countysub-zip (Available 2nd Quarter 2018 onwards)

  if(geoid == "zip" || geoid == "zip-county"|| geoid == "zip-cbsa"||
     geoid == "zip-cd"|| geoid == "zip-cbsadiv" || geoid == "zip-countysub") {
    dataset[[column]] <- add_leading_zeros("zip", dataset[[column]])
    dataset <- dataset[!fivenumbers(dataset[[column]]), ]
  } else if(geoid == "tract-zip") {
    dataset[[column]] <- add_leading_zeros("tract", dataset[[column]])
    dataset <- dataset[!elevennumbers(dataset[[column]]), ]
  } else if(geoid == "cbsa-zip") {
    dataset[[column]] <- add_leading_zeros("cbsa", dataset[[column]])
    dataset <- dataset[!fivenumbers(dataset[[column]]), ]
  } else if(geoid == "cbsadiv-zip") {
    dataset[[column]] <- add_leading_zeros("cbsadiv", dataset[[column]])
    dataset <- dataset[!fivenumbers(dataset[[column]]), ]
  } else if(geoid == "cd-zip") {
    # Try to fix cd entries which should be a 4 digit number...
    dataset[[column]] <- add_leading_zeros("cd", dataset[[column]])
    dataset <- dataset[!fournumbers(dataset[[column]]), ]
  } else if(geoid == "county-zip") {
    # Try to fix entries in the column dataset that should specify a 5 digit
    # county number but may have truncated zeros at the front.
    dataset[[column]] <- add_leading_zeros("county", dataset[[column]])
    dataset <- dataset[!fivenumbers(dataset[[column]]), ]
  } else if(geoid == "countysub-zip") {
    # Try to fix countysub entries which should be 10 digit number.
    dataset[[column]] <- add_leading_zeros("countysub", dataset[[column]])
    dataset <- dataset[!tennumbers(dataset[[column]]), ]
  }
  return(dataset)
}


#' @name data_types
#' @title data_types
#' @description Helper function to determine all datatypes in a dataset.
#' @param geoid The geoid the current dataset has.
#' @param sample How many entries in each column to check for data types.
#' @returns A list showing the data types and feature types of dataset columns.
#' @noRd
#' @noMd
data_types <- function(dataset, sample = 1) {
  # A list of empty type classifications made for data.
  types <- rep("", ncol(dataset))
  names(types) <- colnames(dataset)

  # Sample data from each column of dataset and determine the most common
  # datatype this is likely the data type of column.
  types <- sapply(dataset, function(i) {
    sampled_data <- sample.int(length(i), sample)
    the_types <- sapply(i[sampled_data], typeof)
    names(sort(table(the_types), decreasing = TRUE)[1])
  })

  return(types)
}
