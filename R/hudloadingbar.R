#' @name download_bar
#' @title download_bar
#' @description A simple loading bar for the number of queries completed:
#'   prints the loading bar to the R console.
#' @param done The number of operations done.
#' @param total The number of total operations needed.
#' @param current The current item being worked on.
#' @param percentage Can supply a percentage instead of done and total.
#' @noRd
#' @noMd
download_bar <- function(done = NULL, total = NULL,
                         percentage = NULL, current = NULL) {
  done_perc <- done/total
  remain_perc <- (total - done)/total

  # Loading bar is 25 character long.
  done_bars <- paste(rep("=", ceiling(done_perc * 50)), collapse = "")
  remain_bars <- paste(rep("-", floor(remain_perc * 50)), collapse = "")

  # Create the entire loading bar
  loading <- paste("Downloading:\t",
                   round(done_perc * 100, digits = 0), "%\t",
                   done, "/", total,
                   "\t[", paste(done_bars, remain_bars, sep = ""), "]", sep = "")

  message(paste('\r', loading, sep = ""), appendLF = FALSE)
}
