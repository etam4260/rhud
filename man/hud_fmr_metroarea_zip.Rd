% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hudfmr.R
\name{hud_fmr_metroarea_zip}
\alias{hud_fmr_metroarea_zip}
\title{hud_fmr_metroarea_zip}
\usage{
hud_fmr_metroarea_zip(
  metroarea,
  year = format(Sys.Date() - 365, "\%Y"),
  key = Sys.getenv("HUD_KEY")
)
}
\arguments{
\item{metroarea}{A metroarea to query for.}

\item{year}{Gets the year that this data was recorded.
Can specify multiple years. Default is the
previous year.}

\item{key}{The API key for this user. You must go to HUD and sign up
for an account and request for an API key.}
}
\value{

}
\description{
This function queries a metroarea and returns the FMR
calculation at a zip level resolution. If not small areas will
return data for that metroarea.
If the metroarea is considered
a small area, then it will return data at a zip code level.
ONLY ALLOWS SINGLE QUERIES.
}
\keyword{API}
\keyword{Fair}
\keyword{Markets}
\keyword{Rent}