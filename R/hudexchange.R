#' @import rio
#' @import zoo

# A R interface for accessing HUD EXCHANGE (US Department of Housing and Urban
# Development) DRGR Datasets at https://drgr.hud.gov/public/index.html# The HUD
# EXCHANGE has three main datasets: CDBG-DR (Community Development Block Grant -
# Disaster Recovery) NSP (Neighborhood Stabilization Program) RIF (Rural
# Innovation Fund)

# Currently there is no data for RIF (Rural Innovation Fund) and therefore
# there are no functions to grab that data. A main issue with these datasets
# is that the information is not provided in a tabular format, but rather as
# dictionary-like. These scripts will unroll those fields and format it into
# a dataframe.

#' @name hud_cdbg
#' @title hud_cdbg
#' @description This will grab data from
#'   https://drgr.hud.gov/public/data_downloads.html?programName=DR%20CDBG and
#'   format it into a dataframe. It will unroll each column to replace NA values
#'   with the first non-NA value above it.
#' @param file The specific file needed. Currently you can specify 1 -> 4. Will
#'   default to 1: 1) DR CDBG CDBG-DR Financial Report by Appropriation (xlsx -
#'   0.1 MB - 1/1/2022) 2) DR CDBG CDBG-DR Financial Report by Grantee (xlsx -
#'   0.1 MB - 1/1/2022) 3) DR CDBG CDBG-DR Financial Report Monthly Summary
#'   (xlsx - 0.1 MB - 1/1/2022) 4) DR CDBG CDBG-DR Performance by Activity (xlsx
#'   - 2.2 MB - 1/1/2022)
#' @returns A dataframe.
#' @export
#' @examples
#' \dontrun{
#' library(hudr)
#'
#' Sys.setenv("HUD_KEY" = "q3r2rjimd129fj121jid")
#' hud_cdbg(1)
#'
#' hud_cdbg(2)
#'
#' hud_cdbg(file = 3)
#'
#' hud_cdbg(file = 4)
#' }
hud_cdbg <- function(file = 1) {

  if(as.integer(file) > 4 || as.integer(file) < 1) stop("File number out of range.")

  if(as.integer(file) == 1) {
    data <- suppressMessages(import(paste("https://drgr.hud.gov/public/",
                                          "downloads/DR-CDBG/CDBG-DR",
                                          "%20Financial",
                                          "%20Report",
                                          "%20by",
                                          "%20Appropriation.xlsx", sep = "")))

    # Rename first two rows to their 'appropriate' names.
    names(data)[1] <- "Appropriation Abbreviation"
    names(data)[2] <- "Appropriation"

    # First column is just totals. Get rid of that.
    data <- data[-1, ]

    data <- na.locf(data)

    data <- data[-which(data$Grantee == "Total"), ]
  } else if(as.integer(file) == 2) {
    data <- suppressMessages(import(paste("https://drgr.hud.gov/public/",
                                          "downloads/DR-CDBG/CDBG-DR",
                                          "%20Financial",
                                          "%20Report",
                                          "%20by",
                                          "%20Grantee.xlsx", sep = "")))

    # Rename first two rows to their 'apropriate' names.
    colnames(data)[2] <- "Appropriation Abbreviation"
    colnames(data)[3] <- "Appropriation"


    # First column is just totals. Get rid of that.
    # Get rid of rows with "Total" in Grantee. These are just summations of rows
    # below.
    data <- data[-c(1), ]

    data <- na.locf(data)

    data <- data[-which(data$Grantee == "Total"), ]
    data <- data[-which(data$`Appropriation Abbreviation` == "Total"), ]
  } else if(as.integer(file) == 3) {
    data <- suppressMessages(import(paste("https://drgr.hud.gov/public/",
                                          "downloads/DR-CDBG/CDBG-DR",
                                          "%20Financial",
                                          "%20Report",
                                          "%20Monthly",
                                          "%20Summary.xlsx", sep = "")))
  } else if(as.integer(file) == 4) {
    data <- suppressMessages(import(paste("https://drgr.hud.gov/public/",
                                    "downloads/DR-CDBG/CDBG-DR",
                                    "%20Performance",
                                    "%20by",
                                    "%20Activity.xlsx", sep = "")))

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

    data <- na.locf(data)
  }

  return(data)
}


