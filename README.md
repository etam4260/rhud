
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hudr

<!-- badges: start -->
<!-- badges: end -->

This library contains an interface to the US Department of Housing and
Urban Development datasets. The APIs which this library interfaces with:

-   Crosswalk

-   Fair Markets Rent

-   Income Limits

-   Comprehensive Housing and Affordability Strategy

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("etam4260/hudr")
```

## Setup

The US Department of Housing and Urban Development requires users to
gain an access key before querying their systems. You must go to
<https://www.huduser.gov/hudapi/public/register?comingfrom=1> and make
an account. From there you need to make a new token. Make sure to save
the token somewhere as you will only be able to view it once. You can
now supply the ‘key’ argument for each function.

## Examples

### Crosswalk API

This is a basic example which shows you how to query the CROSSWALK
dataset.

``` r
library(hudr)

# Type 7 corresponds to county level data. The query is a 5 digit fips code.
# The year and quarter specifies when these measurements were made. The key
# is the token you got from https://www.huduser.gov/hudapi/public/register?comingfrom=1
key = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjM5OGJlNjBkNjYzMjM1ZmE2NzQxYWY5ZmViM2QzMDBkNDY3NTliYjgzMzhmNjJiZTE3ZDc4MmE0YWNhYjU2ZmMyMTIxMjM1MjJkYTVjNzY1In0.eyJhdWQiOiI2IiwianRpIjoiMzk4YmU2MGQ2NjMyMzVmYTY3NDFhZjlmZWIzZDMwMGQ0Njc1OWJiODMzOGY2MmJlMTdkNzgyYTRhY2FiNTZmYzIxMjEyMzUyMmRhNWM3NjUiLCJpYXQiOjE2NDI5ODg1MTgsIm5iZiI6MTY0Mjk4ODUxOCwiZXhwIjoxOTU4NTIxMzE3LCJzdWIiOiIyOTA3NCIsInNjb3BlcyI6W119.Ke0N8s797ohuGArbGb7rAMsLKDAWqP6mdItM8KjFQjHDMn8NYBazD8WopijiezC4wgV-n4n41NW4tSivV8yVow"

hudcw(type = 7, query = '22031', year = '2010', quarter = '4', key = key)
#>     fips zip   res_ratio bus_ratio oth_ratio   tot_ratio year quarter
#> 1  22031   0 0.427990000 0.6806280 0.5479450 0.440830000 2010       4
#> 2  22031   0 0.183273000 0.0890052 0.1506850 0.178556000 2010       4
#> 3  22031   0 0.111750000 0.0663176 0.1506850 0.109810000 2010       4
#> 4  22031   0 0.081690400 0.0453752 0.0136986 0.079535000 2010       4
#> 5  22031   0 0.070285600 0.0523560 0.0410959 0.069248100 2010       4
#> 6  22031   0 0.043674300 0.0174520 0.0410959 0.042401900 2010       4
#> 7  22031   0 0.042790200 0.0279232 0.0136986 0.041900100 2010       4
#> 8  22031   0 0.027849000 0.0139616 0.0410959 0.027264400 2010       4
#> 9  22031   0 0.010520700 0.0069808 0.0000000 0.010286900 2010       4
#> 10 22031   0 0.000176819 0.0000000 0.0000000 0.000167266 2010       4
```

#### Crosswalk Data Fields

zip/fips/fipstract/CBSA/Congressional District/County Subdistrict &gt;
Zip, Tract, County, CD or CBSA code

year &gt; Value of year

quarter &gt; Quarter of the year

res\_ratio &gt; The ratio of residential addresses in the ZIP – Tract,
County, or CBSA part to the total number of residential addresses in the
entire ZIP. (for type 1-5) The ratio of residential addresses in the
Zip, Tract, County, or CBSA - ZIP part to the total number of
residential addresses in the entire Zip, Tract, County, or CBSA. (for
type 6-10)

bus\_ratio &gt; The ratio of business addresses in the ZIP – Tract,
County, or CBSA part to the total number of business addresses in the
entire ZIP. (for type 1-5) The ratio of business addresses in the Tract,
County, or CBSA – ZIP part to the total number of business addresses in
the entire Tract, County, or CBSA. (for type 6-10)

oth\_ratio &gt; The ratio of other addresses in the ZIP – Tract to the
total number of other addresses in the entire ZIP. (for type 1-5) The
ratio of other addresses in the Tract, County, or CBSA – ZIP part to the
total number of other addresses in the entire Tract, County, or CBSA.
(for type 6-10)

tot\_ratio &gt; The ratio of all addresses in the ZIP – Tract to the
total number of all types of addresses in the entire ZIP. (for type 1-5)
The ratio of all addresses in the Tract, County, or CBSA-ZIP part to the
total number of all types of addresses in the entire Tract, County, or
CBSA. (for type 6-10)

### Fair Markets Rent API

This is a basic example to show you how to query the Fair Markets Rent
API. Querying a state will return a dataframe with rows representing
counties within the state. A county fips + subdivision 10 digit code
will return a 1 row dataframe for that county.

``` r
key = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjM5OGJlNjBkNjYzMjM1ZmE2NzQxYWY5ZmViM2QzMDBkNDY3NTliYjgzMzhmNjJiZTE3ZDc4MmE0YWNhYjU2ZmMyMTIxMjM1MjJkYTVjNzY1In0.eyJhdWQiOiI2IiwianRpIjoiMzk4YmU2MGQ2NjMyMzVmYTY3NDFhZjlmZWIzZDMwMGQ0Njc1OWJiODMzOGY2MmJlMTdkNzgyYTRhY2FiNTZmYzIxMjEyMzUyMmRhNWM3NjUiLCJpYXQiOjE2NDI5ODg1MTgsIm5iZiI6MTY0Mjk4ODUxOCwiZXhwIjoxOTU4NTIxMzE3LCJzdWIiOiIyOTA3NCIsInNjb3BlcyI6W119.Ke0N8s797ohuGArbGb7rAMsLKDAWqP6mdItM8KjFQjHDMn8NYBazD8WopijiezC4wgV-n4n41NW4tSivV8yVow"

head(hudfmr(query = 'VA', year = '2021', key = key))
#>   state year town           county
#> 1    VA 2021   NA  Accomack County
#> 2    VA 2021   NA Albemarle County
#> 3    VA 2021   NA  Alexandria city
#> 4    VA 2021   NA Alleghany County
#> 5    VA 2021   NA    Amelia County
#> 6    VA 2021   NA   Amherst County
#>                                                                          metro
#> 1                                                          Accomack County, VA
#> 2                                       Charlottesville, VA HUD Metro FMR Area
#> 3                 Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 4 Alleghany County-Clifton Forge city-Covington city, VA HUD Nonmetro FMR Area
#> 5                                                             Richmond, VA MSA
#> 6                                                            Lynchburg, VA MSA
#>   metrostatus fips efficiency onebedroom twobedroom threebedroom fourbedroom
#> 1          NA   NA         NA         NA         NA           NA          NA
#> 2          NA   NA         NA         NA         NA           NA          NA
#> 3          NA   NA         NA         NA         NA           NA          NA
#> 4          NA   NA         NA         NA         NA           NA          NA
#> 5          NA   NA         NA         NA         NA           NA          NA
#> 6          NA   NA         NA         NA         NA           NA          NA
#>   fmrpercentile statename smallareastatus
#> 1            NA  Virginia               0
#> 2            NA  Virginia               0
#> 3            NA  Virginia               1
#> 4            NA  Virginia               0
#> 5            NA  Virginia               0
#> 6            NA  Virginia               0

hudfmr(query = '0100199999', year = '2017', key = key)
#>   county or CBSA year town             county              metro metrostatus
#> 1     0100199999 2017   NA Autauga County, AL Montgomery, AL MSA           1
#>   fips efficiency onebedroom twobedroom threebedroom fourbedroom fmrpercentile
#> 1   NA         NA         NA         NA           NA          NA            NA
#>   statename smallareastatus
#> 1        NA               0
```

#### Fair Markets Rent Data Fields

1.  state/county or CBSA

-   Name of the county if it is a county.

2.  year

-   Value of year

3.  counties\_msa &gt;

-   Names of all counties belonging to the Metro Area if it is a Metro
    Area (MSA).

town\_name &gt; Town name - applicable for North East regions

metro\_status &gt; value will be “1” if it is a metropolitan county.
Otherwise value will be “0”.

metro\_name &gt; Metro area name if metro\_status is “1”

smallarea\_status &gt; value will be “1” if it is a small area.
Otherwise value will be “0”.

Efficiency &gt; Efficiency FMR

One-Bedroom &gt; 1-bedroom FMR

Two-Bedroom &gt; 2-bedroom FMR

Three-Bedroom &gt; 3-bedroom FMR

Four-Bedroom &gt; 4-bedroom FMR

## Contributors

-   Emmet Tam(<https://github.com/etam4260>)

## Disclaimers

-   This is a WIP so please report any issues or bugs to:
    <https://github.com/etam4260/hudr/issues>
-   License: MIT
-   To get citation information for hudr in R, type citation(package =
    ‘hudr’)
-   This is open source, so please fork and introduce some pull
    requests!
