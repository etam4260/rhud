
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hudr <img src='man/figures/logo.png' align="right" width="139"/>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/etam4260/hudr/workflows/R-CMD-check/badge.svg)](https://github.com/etam4260/hudr/actions)
[![Codecov test
coverage](https://codecov.io/gh/etam4260/hudr/branch/main/graph/badge.svg)](https://codecov.io/gh/etam4260/hudr?branch=main)
[![cran
version](https://www.r-pkg.org/badges/version/hudr)](https://cran.r-project.org/package=hudr)
[![cran
checks](https://cranchecks.info/badges/worst/hudr)](https://cranchecks.info/pkgs/hudr)
[![rstudio mirror
downloads](https://cranlogs.r-pkg.org/badges/hudr?color=C9A115)](https://github.com/r-hub/cranlogs.app)
<!-- badges: end -->

The goal of this project is to provide an easy to use interface to
access various open source APIs using the RESTFUL interface provided by
HUD.

HUD has two main websites for gaining access to various different data
sets: HUD User and Exchange.

## HUD User: <https://www.huduser.gov/portal/datasets>

“HUD User is an information source containing reports and reference
documents for the U.S. Department of Housing and Urban Development. HUD
USER was established by the HUD’s Office of Policy Development and
Research in 1978 (HUD User Home Page \| HUD USER).”

HUD User maintains an API to gain access to their data. However, their
API system can be rather confusing and provides their information in
JSON format rather than a data frame like object. Although there exist
file downloadables, R users may want to be able to extract specific bits
of the data into memory.

## HUD Exchange: <https://www.hudexchange.info/programs/drgr/>

“The Department of Housing and Urban Development has a website known as
HUD Exchange which is a comprehensive online platform that provides
tools, resources, and contact information for the organizations and
individuals that partner with HUD. These often include nonprofit groups
and state and municipal governments, but also include borrowers,
lenders, and brokers involved in HUD’s multifamily loan programs
(Disaster Recovery Grant Reporting System, 2022).”

The DRGR Disaster Recovery Grant Reporting System which HUD Exchange
uses to supply public data does not have an active API for retrieving
information. However, there does exist data sets freely available online
which can be directly downloadable into R. This package provides the
ability to directly get such data.

## Citation

Please cite this package using:

Tam E., Reilly A., Ghaedi H., (2022). hudr: An R Interface to the HUD
(US Department of Housing and Urban Development) APIs. R package version
0.0.0.9000. <https://github.com/etam4260/hudr>.

## Available Data

The APIs and datasets which this library interfaces are listed below.
The HUD also provide miscellaneous supplemental APIs under them.

1.  HUD User

    -   Crosswalk
    -   Fair Markets Rent
        -   Small Areas Fair Markets Rent
        -   List States
        -   List Small Areas
    -   Income Limits
    -   Comprehensive Housing and Affordability Strategy
        -   List Counties in State
        -   List MCDs in State
        -   List All Cities in State

2.  HUD Exchange

    -   Community Development Block Grant - Disaster Recovery

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("etam4260/hudr")
```

For more details on how to setup and utilize this package. Please go to
<https://etam4260.github.io/hudr/>. Select \[Setup\] in the navigation
bar.

## Contributors

-   Emmet Tam(<https://github.com/etam4260>)\[<emmet_tam@yahoo.com>\]
-   Allison Reilly()\[<areilly2@umd.edu>\]
-   Hamed Ghaedi()\[<hghaedi@terpmail.umd.edu>\]

## Disclaimers

-   License: MIT
-   To get citation information for hudr in R, type citation(package =
    ‘hudr’)
-   This interface uses the HUD User Data API but is not endorsed or
    certified by HUD User.
-   The limit on the maximum number of API calls is 1200 queries a min.
-   This is a WIP so please report any issues or bugs to:
    <https://github.com/etam4260/hudr/issues>
-   This is open source, so please fork and introduce some pull
    requests!

## References

Disaster Recovery Grant Reporting System. (2022). HUD Exchange.
Retrieved February 19, 2022, from
<https://www.hudexchange.info/programs/drgr/>

HUD User Home Page: HUD USER. HUD User Home Page \| HUD USER. (n.d.).
Retrieved February 24, 2022, from
<https://www.huduser.gov/portal/home.html>
