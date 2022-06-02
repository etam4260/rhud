% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hud_geo_is_infix_geo.R
\name{\%cty_in_z\%}
\alias{\%cty_in_z\%}
\title{\%cty_in_z\%}
\usage{
county \%cty_in_z\% zip
}
\arguments{
\item{county}{The county to determine overlap with zip.}

\item{zip}{The zip to determine overlap with county.}
}
\description{
Given a county and a zip, determine if they overlap
using the crosswalk files. Overlap will be described if
any residential, business, other, or total addresses reside in both.
}
\examples{
\dontrun{

22031 \%cty_in_z\% 71052

}
}