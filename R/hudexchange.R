#' @import openxlsx

# Implementation thought process was adapted from
# https://github.com/dteck/HUD
# Inspiration to build an API
# came from
# https://github.com/ropensci/rnoaa


# An R interface for accessing HUD EXCHANGE (US Department of Housing and Urban Development)
# API.
# The HUD EXCHANGE has three main datasets:
# CDBG-DR (Community Development Block Grant - Disaster Recovery)
# NSP (Neighborhood Stabilization Program)
# RIF (Rural Innovation Fund)


# Currently there is no data for RIF (Rural Innovation Fund) and therefore
# there are no functions to grab that data. A main issue with these datasets
# is that the information is not provided in a tabular format, but rather as
# dictionary-like. These scripts will unroll and format it into a dataframe.


#' hudcdbg
#' @name hudcdbg
#' @title hudcdbg
#' @description This will grab data from https://drgr.hud.gov/public/data_downloads.html?programName=DR%20CDBG
#' and format it into a dataframe. It will unroll each field to remove NA values in cells.
#' @param file The specific file needed. Currently you can specify 1 -> 4. Will default to 1:
#' 1) DR CDBG CDBG-DR Financial Report by Appropriation (xlsx - 0.1 MB - 1/1/2022)
#' 2) DR CDBG CDBG-DR Financial Report by Grantee (xlsx - 0.1 MB - 1/1/2022)
#' 3) DR CDBG CDBG-DR Financial Report Monthly Summary (xlsx - 0.1 MB - 1/1/2022)
#' 4) DR CDBG CDBG-DR Performance by Activity (xlsx - 2.2 MB - 1/1/2022)
#' @export
hudcdbg <- function(file = 1) {
  if(as.integer(file) == 1) {
    data <- read.xlsx("https://drgr.hud.gov/public/downloads/DR-CDBG/CDBG-DR%20Financial%20Report%20by%20Appropriation.xlsx")

    # Rename first two rows to their 'apropriate' names.
    colnames(data)[1] <- "Appropriation Abbreviation"
    colnames(data)[2] <- "Appropriation"

    # First column is just totals. Get rid of that.
    data <- data[-1, ]
    nonNA <- which(!is.na(data[ ,1]))
    for(i in seq(1, length(nonNA) - 1)) {
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 1] <- data[nonNA[i], 1]
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 2] <- data[nonNA[i], 2]
    }

    nonNA <- which(!is.na(data[ ,3]))
    for(i in seq(1, length(nonNA) -1 )) {
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 3] <- data[nonNA[i], 3]
    }
    # Get rid of rows with "Total" in Grantee. These are just summations of rows
    # below.
    data <- data[-which(data$Grantee == "Total"), ]
  } else if(as.integer(file) == 2) {
    data <- read.xlsx("https://drgr.hud.gov/public/downloads/DR-CDBG/CDBG-DR%20Financial%20Report%20by%20Grantee.xlsx")
    colnames(data)[2] <- "Appropriation Abbreviation"
    colnames(data)[3] <- "Appropriation"

    data <- data[-c(1), ]
    nonNA <- which(!is.na(data[ ,1]))
    for(i in seq(1, length(nonNA) - 1)) {
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 1] <- data[nonNA[i], 1]
    }
    nonNA <- which(!is.na(data[ ,2]))
    for(i in seq(1, length(nonNA) - 1)) {
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 2] <- data[nonNA[i], 2]
    }
    nonNA <- which(!is.na(data[ ,3]))
    for(i in seq(1, length(nonNA) - 1)) {
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 3] <- data[nonNA[i], 3]
    }
    nonNA <- which(!is.na(data[ ,4]))
    for(i in seq(1, length(nonNA) - 1)) {
      data[c(seq(nonNA[i], nonNA[i+1] - 1)), 4] <- data[nonNA[i], 4]
    }
    data <- data[-which(data$Grantee == "Total"), ]
    data <- data[-which(data$`Appropriation Abbreviation` == "Total"), ]
  } else if(as.integer(file) == 3) {
    data <- read.xlsx("https://drgr.hud.gov/public/downloads/DR-CDBG/CDBG-DR%20Financial%20Report%20Monthly%20Summary.xlsx")
  } else if(as.integer(file) == 4) {
    data <- read.xlsx("https://drgr.hud.gov/public/downloads/DR-CDBG/CDBG-DR%20Performance%20by%20Activity.xlsx")
    colnames(data)[1] <- "Appropriation Abbreviation"
    colnames(data)[2] <- "Appropriation"

    cname <- colnames(data)[11]
    colnames(data)[11] <- paste(cname, data[1,11], sep = ": ")
    colnames(data)[12] <- paste(cname, data[1,12], sep = ": ")
    colnames(data)[13] <- paste(cname, data[1,13], sep = ": ")

    cname <- colnames(data)[14]
    colnames(data)[14] <- paste(cname, data[1,14], sep = ": ")
    colnames(data)[15] <- paste(cname, data[1,15], sep = ": ")
    colnames(data)[16] <- paste(cname, data[1,16], sep = ": ")

    cname <- colnames(data)[17]
    colnames(data)[17] <- paste(cname, data[1,17], sep = ": ")
    colnames(data)[18] <- paste(cname, data[1,18], sep = ": ")
    colnames(data)[19] <- paste(cname, data[1,19], sep = ": ")

    data <- data[-c(1), ]

    for(j in seq(1,9)) {
      nonNA <- which(!is.na(data[ ,j]))
      for(i in seq(1, length(nonNA) - 1)) {
        data[c(seq(nonNA[i], nonNA[i+1] - 1)), j] <- data[nonNA[i], j]
      }
    }

    data[is.na(data)] <- 0
  }

  return(data)
}


#' hudnsp
#' @name hudnsp
#' @title hudnsp
#' @description This will grab data from https://drgr.hud.gov/public/data_downloads.html?programName=NSP
#' and format it into a dataframe. It will unroll each field to remove NA values in cells.
#' @param file The specific file needed.
#' @export
hudnsp <- function(file = 1) {

}
