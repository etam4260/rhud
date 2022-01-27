
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hudr

<!-- badges: start -->
<!-- badges: end -->

This library contains an interface to the US Department of Housing and
Urban Development datasets. The goal of this project is to provide an
easy to use interface to access various open source datasets provided by
HUD. This library only contains a few datasets.

## Available Datasets

The APIs which this library interfaces with:

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
an account.

From there you need to make a new token. Make sure to save the token
somewhere as you will only be able to view it once. You can now supply
the ‘key’ argument.

To reduce the need to supply the key in every function call, you can use
the setkey(key) function to save the key to the package environment.

``` r
library(hudr)

# The function will not output anything. You can now run the other functions
# without supplying the key argument.
setkey("eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjM5OGJlNjBkNjYzMjM1ZmE2NzQxYWY5ZmViM2QzMDBkNDY3NTliYjgzMzhmNjJiZTE3ZDc4MmE0YWNhYjU2ZmMyMTIxMjM1MjJkYTVjNzY1In0.eyJhdWQiOiI2IiwianRpIjoiMzk4YmU2MGQ2NjMyMzVmYTY3NDFhZjlmZWIzZDMwMGQ0Njc1OWJiODMzOGY2MmJlMTdkNzgyYTRhY2FiNTZmYzIxMjEyMzUyMmRhNWM3NjUiLCJpYXQiOjE2NDI5ODg1MTgsIm5iZiI6MTY0Mjk4ODUxOCwiZXhwIjoxOTU4NTIxMzE3LCJzdWIiOiIyOTA3NCIsInNjb3BlcyI6W119.Ke0N8s797ohuGArbGb7rAMsLKDAWqP6mdItM8KjFQjHDMn8NYBazD8WopijiezC4wgV-n4n41NW4tSivV8yVow")
```

## Examples

### Crosswalk API

This is a basic example which shows you how to query the CROSSWALK API.

``` r
# Type 7 corresponds to county level data. The query is a 5 digit fips code.
# The year and quarter specifies when these measurements were made. The key argument
# is the token you got from https://www.huduser.gov/hudapi/public/register?comingfrom=1
# This example does not have the key because we already set it up using setkey()
hudcw(type = 7, query = '22031', year = '2010', quarter = '4')
#>     fips   zip   res_ratio bus_ratio oth_ratio   tot_ratio year quarter
#> 1  22031 71052 0.427990000 0.6806280 0.5479450 0.440830000 2010       4
#> 2  22031 71078 0.183273000 0.0890052 0.1506850 0.178556000 2010       4
#> 3  22031 71049 0.111750000 0.0663176 0.1506850 0.109810000 2010       4
#> 4  22031 71032 0.081690400 0.0453752 0.0136986 0.079535000 2010       4
#> 5  22031 71027 0.070285600 0.0523560 0.0410959 0.069248100 2010       4
#> 6  22031 71030 0.043674300 0.0174520 0.0410959 0.042401900 2010       4
#> 7  22031 71046 0.042790200 0.0279232 0.0136986 0.041900100 2010       4
#> 8  22031 71063 0.027849000 0.0139616 0.0410959 0.027264400 2010       4
#> 9  22031 71419 0.010520700 0.0069808 0.0000000 0.010286900 2010       4
#> 10 22031 71065 0.000176819 0.0000000 0.0000000 0.000167266 2010       4
```

##### Crosswalk Data Fields

1.  zip/fips/fipstract/CBSA/Congressional District/County
    Subdistrict &gt;
    -   Zip, Tract, County, CD or CBSA code
2.  year
    -   Value of year
3.  quarter
    -   Quarter of the year
4.  res\_ratio
    -   The ratio of residential addresses in the ZIP – Tract, County,
        or CBSA part to the total number of residential addresses in the
        entire ZIP. (for type 1-5) The ratio of residential addresses in
        the Zip, Tract, County, or CBSA - ZIP part to the total number
        of residential addresses in the entire Zip, Tract, County, or
        CBSA. (for type 6-10)
5.  bus\_ratio
    -   The ratio of business addresses in the ZIP – Tract, County, or
        CBSA part to the total number of business addresses in the
        entire ZIP. (for type 1-5) The ratio of business addresses in
        the Tract, County, or CBSA – ZIP part to the total number of
        business addresses in the entire Tract, County, or CBSA. (for
        type 6-10)
6.  oth\_ratio
    -   The ratio of other addresses in the ZIP – Tract to the total
        number of other addresses in the entire ZIP. (for type 1-5) The
        ratio of other addresses in the Tract, County, or CBSA – ZIP
        part to the total number of other addresses in the entire Tract,
        County, or CBSA. (for type 6-10)
7.  tot\_ratio
    -   The ratio of all addresses in the ZIP – Tract to the total
        number of all types of addresses in the entire ZIP. (for type
        1-5) The ratio of all addresses in the Tract, County, or
        CBSA-ZIP part to the total number of all types of addresses in
        the entire Tract, County, or CBSA. (for type 6-10)

### Fair Markets Rent API

This is a basic example to show you how to query the FAIR MARKETS RENT
API.

Querying a state will return a dataframe with rows representing counties
within the state. A county fips + subdivision 10 digit code will return
a 1 row dataframe for that county.

``` r
# Querying the Fair Markets Rent dataset for Virginia in the year of 2021. This selected
# the first few rows and the first four columns.
head(hudfmr(query = 'VA', year = '2021'))[ ,c(1,2,3,4)]
#>   state year town           county
#> 1    VA 2021   NA  Accomack County
#> 2    VA 2021   NA Albemarle County
#> 3    VA 2021   NA  Alexandria city
#> 4    VA 2021   NA Alleghany County
#> 5    VA 2021   NA    Amelia County
#> 6    VA 2021   NA   Amherst County

# Querying the Fair Markets Rent dataset for a county in the year of 2017.
hudfmr(query = '0100199999', year = '2017')
#>   county or CBSA year town             county              metro metrostatus
#> 1     0100199999 2017   NA Autauga County, AL Montgomery, AL MSA           1
#>   efficiency onebedroom twobedroom threebedroom fourbedroom smallareastatus
#> 1        587        682        822         1054        1425               0
```

##### Fair Markets Rent Data Fields

1.  state/county or CBSA
    -   Name of the county if it is a county.
2.  year
    -   Value of year
3.  counties\_msa
    -   Names of all counties belonging to the Metro Area if it is a
        Metro Area (MSA).
4.  town\_name
    -   Town name - applicable for North East regions
5.  metro\_status
    -   value will be “1” if it is a metropolitan county. Otherwise
        value will be “0”.
6.  metro\_name
    -   Metro area name if metro\_status is “1”
7.  smallarea\_status
    -   value will be “1” if it is a small area. Otherwise value will be
        “0”.
8.  Efficiency
    -   Efficiency FMR
9.  One-Bedroom
    -   1-bedroom FMR
10. Two-Bedroom
    -   2-bedroom FMR
11. Three-Bedroom
    -   3-bedroom FMR
12. Four-Bedroom
    -   4-bedroom FMR

### Income Limits API

This is a basic example showing you how to query the INCOME LIMITS API.

Querying a state or (fips + subdivision 10 digit fips) both return a
single row.

``` r
# Querying the Fair Markets Rent dataset for Virginia in the year of 2021.
# Grabbing the first 6 columns
head(hudil(query = 'VA', year = '2021'))[ ,c(1,2,3,4,5,6)]
#>   state year median_income verylowil50p1 verylowil50p2 verylowil50p3
#> 1    VA 2021         93000         32550         37200         41850

# Querying the Fair Markets Rent dataset for a county in the year of 2017.
# Grabbing the first 6 columns
hudil(query = '0100199999', year = '2017')[ ,c(1,2,3,4,5,6)]
#>   county or CBSA town metrostatus           areaname year median_income
#> 1     0100199999                1 Montgomery, AL MSA 2017         59700
```

##### Income Limits Data Fields

1.  county\_name
    -   Name of the county if it is a county.
2.  counties\_msa
    -   Names of all counties belonging to the Metro Area if it is a
        Metro Area (MSA).
3.  town\_name
    -   Town name - applicable for North East regions
4.  metro\_status
    -   value will be “1” if it is a metropolitan county. Otherwise
        value will be “0”.
5.  metro\_name
    -   Metro area name if metro\_status is “1”
6.  year
    -   year of the data
7.  median\_income
    -   Median Income for the area
8.  Very Low
    -   (50%) Income Limits
    -   For more details on il50 check the website.
9.  Extremely Low
    -   (30%) Income Limits
    -   For more details on il30 check the website.
10. Low
    -   (80%) Income Limits
    -   For more details on il80 check the website.

### Comprehensive Housing and Affordability Strategy API

This is a basic example showing you how to query the COMPREHENSIVE
HOUSING AND AFFORDABILITY STRATEGY API.

This function returns a single row dataframe containing information
about household income levels, occupancy, housing problems, and burden
for a ‘region’

``` r
# Querying the Comprehensive Housing and Affordability Strategy dataset for a state in the years of 2014-2018.
# Return the first 6 rows. There are specific date ranges that are allowed. Please check the specifications.
# https://www.huduser.gov/portal/dataset/chas-api.html
head(hudchas(type = '2', stateId ='56', year = '2014-2018'))[,c(1,2,3,4,5,6,7,8,9)]
#>   geoname sumlevel      year      A1      A2      A3      A4      A5      A6
#> 1 Wyoming    State 2014-2018 11520.0 15235.0 26755.0 13990.0 12975.0 26965.0
```

##### Comprehensive Housing and Affordability Strategy Data Fields

I recommend checking out
<https://www.huduser.gov/portal/dataset/chas-api.html> for more details
on this as there are too many fields to display.

## Contributors

-   Emmet Tam(<https://github.com/etam4260>)

## Disclaimers

-   This interface uses the HUD User Data API but is not endorsed or
    certified by HUD User.
-   The limit on the maximum number of API calls is 1200 queries a min.
-   This is a WIP so please report any issues or bugs to:
    <https://github.com/etam4260/hudr/issues>
-   License: MIT
-   To get citation information for hudr in R, type citation(package =
    ‘hudr’)
-   This is open source, so please fork and introduce some pull
    requests!
