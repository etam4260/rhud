#' @import utils

#' @name simple_detect_geoid_in_data
#' @title detect_geoid_in_data
#' @description Determines if the current dataset contains a column specifying a
#'   geoid compatible with hudr functions.
#' @param dataset A dataframe.
#' @param sample_size The number geoids in dataset to check to make sure it follows proper structure.
#' @param exists_threshold A sensitivity measurement. If higher, then less likely to detect a possible geoid in a dataset.
#' @param data_match_threshold A sensitivity measurement to determine if the samples structure of data matches a threshold.
#' @returns The column name containing the geoid column.
#' @noRd
#' @noMd
simple_detect_geoid_col_in_data <- function(dataset, exists_threshold = 3, sample_size = 5, data_match_threshold = .9) {

  dataset_names <- colnames(dataset)

  # This is a compiled list of brainstormed ideas of how many different ways
  # a user could name the geoid field in a dataset. Therefore if we get
  # a close match with one of them, then it is likely a dataset that contains one
  # of these types of geoids.

  # We also have to take into account the structure of the data within the dataset.
  # So for example, we need to know if the number of characters match what we want.
  # We can also take into account if they are all numbers or not.
  # Basically a corpus of possible options...

  all_geoid_possibilities <- list(
    different_possible_zipcode_names <- c("zip", "zipcode", "zip-code", "zip+4"),
    different_possible_tract_names <- c("tract", "census tract", "census area", "census district", "meshblock", "tract code", "ctname", "ct"),
    different_possible_cbsa_names <- c("cbsa", "core based statistical area", "CBSA Code", "CBSA Name", "metropolitan", "micropolitan"),
    different_possible_cbsadiv_names <- c("cbsadiv", "metropolitan", "core based statistical area division", "cbsa division", "cbsadiv code", "cbsadiv name"),
    different_possible_cd_names <- c("cd", "congressional district", "electoral divisions", "congressional"),
    different_possible_county_names <- c("county", "county name", "county code", "county fips", "fips code", "fips"),
    different_possible_countysub_names <- c("countysub", "county subdistrict", "county division", "minor civil divisions", "mcds", "census county divisions")
  )

  # Create a stack to represent a leaderboard.
  # Last item is where we start.
  leaderboard <- list(length(dataset_names))
  for(i in 1:length(dataset_names)) {
    leaderboard[[i]] <- list(index = Inf, score = Inf, likely = Inf)
  }

  # i represents the current column.
  # j represents the current geoid type we are looking for a match for.
  # z represents all the different ways the geoid can be spelled.

  for(i in 1:length(dataset_names)) {
    for(j in 1:length(all_geoid_possibilities)) {
      for(z in 1:length(all_geoid_possibilities[[j]])) {
        score <- adist(dataset_names[i], all_geoid_possibilities[[j]][z])
        if(score < leaderboard[[i]]$score) {
          leaderboard[[i]] <- list(index = i, score = score, likely = j)
        }
      }
    }
  }

  # Sort the leaderboard by the score.
  sorted_leaderboard <- leaderboard[order(sapply(leaderboard,'[[',2))]

  # First item is the best choice...
  return(sorted_leaderboard)
}



#' @name calculate_structure_of_data
#' @title calculate structure_of_data
#' @description A helper function to determine whether the index column in
#'   dataset specifies a geoid like structure.
#' @param geoid The type of geoid we expect.
#' @param dataset The dataset inputted.
#' @param sample_size The number of elements to randomly check for zip like
#'   structured data.
#' @param data_match_threshold A sensitivity measurement to determine if the
#'   sampled structure of data matches a threshold.
#' @returns The column name containing the geoid column.
#' @noRd
#' @noMd
calculate_structure_of_data <- function(geoid, index, dataset, sample_size, data_match_threshold) {

  # Must be 5 digit or 5-4 digit
  # Sample the dataset to see if the geoid is found.
  # If we know the geoid and not the index.
  sampled <- sample.int(nrow(dataset), sample_size)
  matched <- c(rep(0,length(sampled)))

  # Loop through the sampled data and check for proper structure...
  i = 1
  while(i < length(sampled)) {
    # Check if column is all numbers and 5 digits long, or check if 5 numbers, a
    # delimiter and 4 numbers or check if 9 numbers long.
    sampled_data = dataset[sampled[i], index]

    if(geoid == "zip") {
      if(fivenumbers(sampled) || fivenumsthenfournums(sampled)) {
        matched[i] <- 1
      }
    } else if(geoid == "tract") {
      if(elevennumbers(sampled)) {
        matched[i] <- 1
      }
    } else if(geoid == "cbsa") {
      if(fivenumbers(sampled)) {
        matched[i] <- 1
      }
    } else if(geoid == "cbsadiv") {
      if(fivenumbers(sampled)) {
        matched[i] <- 1
      }
    } else if(geoid == "cd") {
      if(fournumbers(sampled)) {
        matched[i] <- 1
      }
    } else if(geoid == "county") {
      if(fivenumbers(sampled)) {
        matched[i] <- 1
      }
    } else if(geoid == "countysub") {
      if(tennumbers(sampled)) {
        matched[i] <- 1
      }
    }
    i = i + 1
  }

  # Check the final sampled data to determine if we are at an okay threshold
  # determined by the inputted data_match_threshold.
  if(length(matched[matched == 1])/length(matched) > data_match_threshold) {
    return(TRUE)
  }
}


# Could we pass these two 'models' through a sigmoid function...? to determine
# if neuron activates or not?

#' @name calculate_structure_of_data
#' @title calculate structure_of_data
#' @noRd
#' @noMd
unsupervised_classifier_detect_geoid_col_in_data <- function(dataset) {

}

#' @name supervised_classifer_detect_geoid_col_in_data
#' @title supervised_classifer_detect_geoid_col_in_data
#' @noRd
#' @noMd
supervised_classifier_detect_geoid_col_in_data <- function(dataset) {

}

#' @name combined_classifer_detect_geoid_col_in_data
#' @title combined_classifier_detect_geoid_col_in_data
#' @noRd
#' @noMd
combined_classifier_detect_geoid_col_in_data <- function(dataset) {

}
