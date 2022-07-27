#' @name hud_chas_col_names
#' @title Column Names for HUD CHAS Dataset
#' @description Gives the names of all the columns expected from the
#'   CHAS dataset. This is hardcoded and may change: used to deal with
#'   missing data columns for some CHAS year ranges by properly merging and
#'   adding NA to the empty.
#' @examples
#' \dontrun{
#' rhud_website("github-pages")
#' rhud_website("github")
#' rhud_website()
#' }
#' @noMd
#' @noRd
hud_chas_col_names <- function() {

  return(c("geoname", "sumlevel", "year",
           "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",
           "A10", "A11", "A12", "A13", "A14", "A15", "A16", "A17",
           "A18",
           "B1", "B2", "B3", "B4", "B5", "B6", "B7",
           "B8", "B9",
           "C1", "C2", "C3", "C4", "C5", "C6",
           "D1", "D2", "D3",
           "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
           "E1", "E2", "E3", "E5", "E6", "E7", "E9", "E10", "E11",
           "E13", "E14", "E15", "E17", "E18", "E19", "E21", "E22",
           "E23",
           "F1", "F2", "F3", "F5", "F6", "F7", "F9", "F10", "F11",
           "F13", "F14", "F15", "F17", "F18", "F19", "F21", "F22",
           "F23",
           "G1", "G2", "G3", "G5", "G6", "G7", "G9", "G10", "G11",
           "G13", "G14", "G15", "G17", "G18", "G19",
           "H1", "H2", "H4", "H5", "H7", "H8", "H10", "H11", "H13",
           "H14", "H16",
           "I1", "I2", "I4", "I5", "I7", "I8", "I10", "I11",
           "I13", "I14", "I16",
           "J1", "J2", "J4", "J5", "J7", "J8", "J10",
           "J11", "J13", "J14", "J16"))
}
