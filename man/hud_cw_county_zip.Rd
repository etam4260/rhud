% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hud_crosswalk.R
\name{hud_cw_county_zip}
\alias{hud_cw_county_zip}
\title{Crosswalk County to Zip}
\usage{
hud_cw_county_zip(
  county,
  year = format(Sys.Date() - 365, "\%Y"),
  quarter = 1,
  minimal = FALSE,
  key = Sys.getenv("HUD_KEY"),
  to_tibble
)
}
\arguments{
\item{county}{5 digit county code.}

\item{year}{Gets the year that this data was recorded. Can specify multiple
years. Default is the previous year.}

\item{quarter}{Gets the quarter of the year that this data was recorded.
Defaults to the first quarter of the year.}

\item{minimal}{Return just the crosswalked geoids if true. Otherwise, return
all fields. This does not remove duplicates.}

\item{key}{The key obtained from HUD
(US Department of Housing and Urban Development)
USER website.}

\item{to_tibble}{If TRUE, return the data in a tibble format
rather than a data frame.}
}
\value{
This function returns a dataframe containing crosswalk data for
county to zip for all combinations of "county", "year", and "quarter"
inputs.

These measurements include res-ratio, bus-ratio,
oth-ratio, tot-ratio. For more details on these measurements, visit
https://www.huduser.gov/portal/dataset/uspszip-api.html
}
\description{
This function queries the USPS Crosswalks API provided by US
Department of Housing and Urban Development (HUD USER).
This returns the crosswalk for
county to zip.
}
\examples{
\dontrun{
library(rhud)

hud_cw_county_zip(county = 22031, year = c('2017'), quarter = c('1'))

hud_cw_county_zip(county = '22031', year = c('2010'), quarter = c('1'),
   minimal = TRUE)
}
}
\seealso{
\itemize{
\item \code{\link[=hud_cw_zip_tract]{hud_cw_zip_tract()}}
\item \code{\link[=hud_cw_zip_county]{hud_cw_zip_county()}}
\item \code{\link[=hud_cw_zip_cbsa]{hud_cw_zip_cbsa()}}
\item \code{\link[=hud_cw_zip_cbsadiv]{hud_cw_zip_cbsadiv()}}
\item \code{\link[=hud_cw_zip_countysub]{hud_cw_zip_countysub()}}
\item \code{\link[=hud_cw_zip_cd]{hud_cw_zip_cd()}}
\item \code{\link[=hud_cw_tract_zip]{hud_cw_tract_zip()}}
\item \code{\link[=hud_cw_county_zip]{hud_cw_county_zip()}}
\item \code{\link[=hud_cw_cbsa_zip]{hud_cw_cbsa_zip()}}
\item \code{\link[=hud_cw_cbsadiv_zip]{hud_cw_cbsadiv_zip()}}
\item \code{\link[=hud_cw_cd_zip]{hud_cw_cd_zip()}}
\item \code{\link[=hud_cw_countysub_zip]{hud_cw_countysub_zip()}}
\item \code{\link[=hud_cw]{hud_cw()}}
}
}
\keyword{API}
\keyword{Crosswalks}
