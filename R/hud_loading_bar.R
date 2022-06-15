#' @name download_bar
#' @title download_bar
#' @description A simple loading bar for the number of queries completed:
#'   prints the loading bar to the R console.
#' @param done The number of operations done.
#' @param total The number of total operations needed.
#' @param current The current item being worked on: in this case the url.
#' @param error The errored out URLs.
#' @param percentage Can supply a percentage instead of done and total.
#' @noRd
#' @noMd
download_bar <- function(done = NULL, total = NULL,
                         percentage = NULL, current = NULL, error = NULL) {
  done_perc <- done / total
  remain_perc <- (total - done) / total

  # Loading bar is 25 character long.
  done_bars <- paste(rep("=", round(done_perc * 50)), collapse = "")
  remain_bars <- paste(rep("-", round(remain_perc * 50)), collapse = "")

  if (round(remain_perc * 50) - (remain_perc * 50) == .5) {
    # if they are both .5, then they make sure not to round both up.
    remain_bars <- paste(rep("-", floor(remain_perc * 50)), collapse = "")
  }

  # TODO: For the num_done/total we might want it to show
  # the number of successful queries instead... or maybe even show
  # the errorred ones as a separate number.
  # "Error: 6"

  # TODO: Show the ETA for download time. (num api calls * 60)/2000
  url <- substr(current,
                regexec("https://www.huduser.gov/hudapi/public/(.*)",
                        current)[[1]][2],
                nchar(current)
                )

  # TODO: Need to add current working item when downloading. How best to show
  # that?? Might show the last part of the url...

  #[cw, firstgeoid, secondgeoid, year, quarter]
  #[fmr, firstgeoid, secondgeoid, year]
  #[il, firstgeoid, year]
  #[chas, firstgeoid, year]
  #[misc, firstgeoid, secondgeoid]

  # Create the entire loading bar
  loading <- paste("Downloading:\t",
                   "[", paste(done_bars, remain_bars, sep = ""), "]\t",
                   round(done_perc * 100, digits = 0), "%\t",
                   done, "/", total, "\t", error, "\t", url, sep = "")

  message(paste("\r", loading, sep = ""), appendLF = FALSE)
}
