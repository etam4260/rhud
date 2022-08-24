#' @name download_bar
#' @title Query Download Bar
#' @description A simple loading bar for the number of queries completed:
#'   prints the loading bar to the R console.
#' @param done A number: the number of operations.
#' @param total A number: the number of total operations needed.
#' @param current The current item being worked on: in most cases, the url
#'   endpoint
#' @param error A number: the total number of error out urls.
#' @param percentage Can supply a percentage instead of done and total.
#' @noRd
#' @noMd
download_bar <- function(done = NULL, total = NULL,
                         percentage = NULL, current = NULL, error = NULL) {

  # If user set quiet loading... then don't display download bar...
  # but still make sure to give warnings for failed queries.

  if (getOption("rhud_quiet_loads", FALSE)) {

    done_perc <- done / total
    remain_perc <- (total - done) / total

    # Loading bar is 25 character long.
    done_bars <- paste(rep("=", round(done_perc * 50)), collapse = "")
    remain_bars <- paste(rep("-", round(remain_perc * 50)), collapse = "")

    if (round(remain_perc * 50) - (remain_perc * 50) == .5) {
      # if they are both .5, then they make sure not to round both up.
      remain_bars <- paste(rep("-", floor(remain_perc * 50)), collapse = "")
    }

    url <- substr(current,
                  regexec("https://www.huduser.gov/hudapi/public/(.*)",
                          current)[[1]][2],
                  nchar(current)
                  )

    # Create the entire loading bar
    loading <- paste("Downloading:\t",
                     "[", paste(done_bars, remain_bars, sep = ""), "]\t",
                     round(done_perc * 100, digits = 0), "%\t",
                     done, "/", total, "\t", error, "\t", url, sep = "")

    message(paste("\r", loading, sep = ""), appendLF = FALSE)
  }
}
