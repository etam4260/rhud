% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hud_fmr.R
\name{hud_fmr_metroarea_zip}
\alias{hud_fmr_metroarea_zip}
\title{Fair Markets Rent Metroarea Queries for Zip}
\usage{
hud_fmr_metroarea_zip(
  metroarea,
  year = format(Sys.Date() - 365, "\%Y"),
  key = Sys.getenv("HUD_KEY"),
  to_tibble = getOption("rhud_use_tibble", FALSE)
)
}
\arguments{
\item{metroarea}{A character vector: metroarea(s) to query for.}

\item{year}{A character of numeric vector: gets the year
that this data was recorded.
Can specify multiple year(s). Default is the
previous year.}

\item{key}{A character vector of length one with the key obtained from HUD
(US Department of Housing and Urban Development)
USER website.}

\item{to_tibble}{A logical: if TRUE, return the data in a tibble format
rather than a data frame.}
}
\value{
A data frame with fair markets rent for zip codes in metro areas for
all combinations of "metroarea" and "year" inputs.
}
\description{
This function queries for metroarea(s) and returns
FMR calculation. If the metroarea is not
a small area, it will return only single
measurement for that metroarea. If the metrarea is considered a
small area, it will return data at a zip code level.
}
\examples{
\dontrun{
hud_fmr_metroarea_zip("METRO47900M47900", year = c(2018))

hud_fmr_metroarea_zip("METRO29180N22001", year = c(2019))

hud_fmr_metroarea_zip("METRO10380M10380", year = c(2020))
}
}
\seealso{
\itemize{
\item \code{\link[=hud_fmr_state_metroareas]{hud_fmr_state_metroareas()}}
\item \code{\link[=hud_fmr_state_counties]{hud_fmr_state_counties()}}
\item \code{\link[=hud_fmr_metroarea_zip]{hud_fmr_metroarea_zip()}}
\item \code{\link[=hud_fmr_county_zip]{hud_fmr_county_zip()}}
\item \code{\link[=hud_fmr]{hud_fmr()}}
}
}
\keyword{API}
\keyword{Fair}
\keyword{Markets}
\keyword{Rent}
