% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hud_geo_is_in_geo.R
\name{z_in_trt}
\alias{z_in_trt}
\title{Zip Addresses in Tract Addresses?}
\usage{
z_in_trt(zip, tract, year, quarter, key = Sys.getenv("HUD_KEY"))
}
\arguments{
\item{zip}{A character or numeric vector: the zip(s) to determine
overlap with tract(s).}

\item{tract}{A character or numeric vector: the tract(s) to determine
overlap with zip(s).}

\item{year}{A character or numeric vector: gets the year that this data was
recorded. Can specify multiple
years. Default is the previous year.}

\item{quarter}{A character or numeric vector: gets the quarter of the year
that this data was recorded.
Defaults to the first quarter of the year.}

\item{key}{A character vector of length one: the API key for this user.
You must go to HUD and sign up
for an account and request for an API key.}
}
\value{
If zip(s) exist in the tract(s) specified, then TRUE is returned.
}
\description{
Given zip code(s) and tract(s), determine if they overlap
using the (United States Postal Service)
USPS Crosswalk files. Overlap will be described if
any residential, business, other, or total addresses reside in both.
}
\examples{
\dontrun{

z_in_trt(zip = 71052, tract = 22031950600, year = 2019, quarter = 2)

}
}
\seealso{
\itemize{
\item \code{\link[=z_in_trt]{z_in_trt()}}
\item \code{\link[=z_in_cty]{z_in_cty()}}
\item \code{\link[=z_in_cbsa]{z_in_cbsa()}}
\item \code{\link[=z_in_cbsadiv]{z_in_cbsadiv()}}
\item \code{\link[=z_in_ctysb]{z_in_ctysb()}}
\item \link{z_in_cd}
\item \code{\link[=trt_in_z]{trt_in_z()}}
\item \code{\link[=cty_in_z]{cty_in_z()}}
\item \code{\link[=cbsa_in_z]{cbsa_in_z()}}
\item \code{\link[=cbsadiv_in_z]{cbsadiv_in_z()}}
\item \code{\link[=cd_in_z]{cd_in_z()}}
\item \code{\link[=ctysb_in_z]{ctysb_in_z()}}
}
}
