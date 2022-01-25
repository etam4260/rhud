
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hudr

<!-- badges: start -->
<!-- badges: end -->

This library contains an interface to the US Department of Housing and
Urban Development datasets. The APIs which this library interfaces with:

CROSSWALKS

FAIR MARKETS RENT

INCOME LIMITS

COMPREHENSIVE HOUSING AND AFFORDABILITY STRATEGY

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
#>     fips year quarter   zip   res_ratio bus_ratio oth_ratio   tot_ratio
#> 1  22031 2010       4 71052 0.427990000 0.6806280 0.5479450 0.440830000
#> 2  22031 2010       4 71078 0.183273000 0.0890052 0.1506850 0.178556000
#> 3  22031 2010       4 71049 0.111750000 0.0663176 0.1506850 0.109810000
#> 4  22031 2010       4 71032 0.081690400 0.0453752 0.0136986 0.079535000
#> 5  22031 2010       4 71027 0.070285600 0.0523560 0.0410959 0.069248100
#> 6  22031 2010       4 71030 0.043674300 0.0174520 0.0410959 0.042401900
#> 7  22031 2010       4 71046 0.042790200 0.0279232 0.0136986 0.041900100
#> 8  22031 2010       4 71063 0.027849000 0.0139616 0.0410959 0.027264400
#> 9  22031 2010       4 71419 0.010520700 0.0069808 0.0000000 0.010286900
#> 10 22031 2010       4 71065 0.000176819 0.0000000 0.0000000 0.000167266
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
API. Query a state will return a dataframe with rows representing
counties within the state. A county fips + subdivision 10 digit code
will return a 1 row dataframe for that county.

``` r
key = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6IjM5OGJlNjBkNjYzMjM1ZmE2NzQxYWY5ZmViM2QzMDBkNDY3NTliYjgzMzhmNjJiZTE3ZDc4MmE0YWNhYjU2ZmMyMTIxMjM1MjJkYTVjNzY1In0.eyJhdWQiOiI2IiwianRpIjoiMzk4YmU2MGQ2NjMyMzVmYTY3NDFhZjlmZWIzZDMwMGQ0Njc1OWJiODMzOGY2MmJlMTdkNzgyYTRhY2FiNTZmYzIxMjEyMzUyMmRhNWM3NjUiLCJpYXQiOjE2NDI5ODg1MTgsIm5iZiI6MTY0Mjk4ODUxOCwiZXhwIjoxOTU4NTIxMzE3LCJzdWIiOiIyOTA3NCIsInNjb3BlcyI6W119.Ke0N8s797ohuGArbGb7rAMsLKDAWqP6mdItM8KjFQjHDMn8NYBazD8WopijiezC4wgV-n4n41NW4tSivV8yVow"

hudfmr(query = 'VA', year = '2021', key = key)
#>     state year town                county
#> 1      VA 2021   NA       Accomack County
#> 2      VA 2021   NA      Albemarle County
#> 3      VA 2021   NA       Alexandria city
#> 4      VA 2021   NA      Alleghany County
#> 5      VA 2021   NA         Amelia County
#> 6      VA 2021   NA        Amherst County
#> 7      VA 2021   NA     Appomattox County
#> 8      VA 2021   NA      Arlington County
#> 9      VA 2021   NA        Augusta County
#> 10     VA 2021   NA           Bath County
#> 11     VA 2021   NA        Bedford County
#> 12     VA 2021   NA          Bland County
#> 13     VA 2021   NA      Botetourt County
#> 14     VA 2021   NA          Bristol city
#> 15     VA 2021   NA      Brunswick County
#> 16     VA 2021   NA       Buchanan County
#> 17     VA 2021   NA     Buckingham County
#> 18     VA 2021   NA      Buena Vista city
#> 19     VA 2021   NA       Campbell County
#> 20     VA 2021   NA       Caroline County
#> 21     VA 2021   NA        Carroll County
#> 22     VA 2021   NA   Charles City County
#> 23     VA 2021   NA      Charlotte County
#> 24     VA 2021   NA  Charlottesville city
#> 25     VA 2021   NA       Chesapeake city
#> 26     VA 2021   NA   Chesterfield County
#> 27     VA 2021   NA         Clarke County
#> 28     VA 2021   NA Colonial Heights city
#> 29     VA 2021   NA        Covington city
#> 30     VA 2021   NA          Craig County
#> 31     VA 2021   NA       Culpeper County
#> 32     VA 2021   NA     Cumberland County
#> 33     VA 2021   NA         Danville city
#> 34     VA 2021   NA      Dickenson County
#> 35     VA 2021   NA      Dinwiddie County
#> 36     VA 2021   NA          Emporia city
#> 37     VA 2021   NA          Essex County
#> 38     VA 2021   NA          Fairfax city
#> 39     VA 2021   NA        Fairfax County
#> 40     VA 2021   NA     Falls Church city
#> 41     VA 2021   NA       Fauquier County
#> 42     VA 2021   NA          Floyd County
#> 43     VA 2021   NA       Fluvanna County
#> 44     VA 2021   NA         Franklin city
#> 45     VA 2021   NA       Franklin County
#> 46     VA 2021   NA      Frederick County
#> 47     VA 2021   NA   Fredericksburg city
#> 48     VA 2021   NA            Galax city
#> 49     VA 2021   NA          Giles County
#> 50     VA 2021   NA     Gloucester County
#> 51     VA 2021   NA      Goochland County
#> 52     VA 2021   NA        Grayson County
#> 53     VA 2021   NA         Greene County
#> 54     VA 2021   NA    Greensville County
#> 55     VA 2021   NA        Halifax County
#> 56     VA 2021   NA          Hampton city
#> 57     VA 2021   NA        Hanover County
#> 58     VA 2021   NA     Harrisonburg city
#> 59     VA 2021   NA        Henrico County
#> 60     VA 2021   NA          Henry County
#> 61     VA 2021   NA       Highland County
#> 62     VA 2021   NA         Hopewell city
#> 63     VA 2021   NA  Isle of Wight County
#> 64     VA 2021   NA     James City County
#> 65     VA 2021   NA King and Queen County
#> 66     VA 2021   NA    King George County
#> 67     VA 2021   NA   King William County
#> 68     VA 2021   NA      Lancaster County
#> 69     VA 2021   NA            Lee County
#> 70     VA 2021   NA        Lexington city
#> 71     VA 2021   NA        Loudoun County
#> 72     VA 2021   NA         Louisa County
#> 73     VA 2021   NA      Lunenburg County
#> 74     VA 2021   NA        Lynchburg city
#> 75     VA 2021   NA        Madison County
#> 76     VA 2021   NA         Manassas city
#> 77     VA 2021   NA    Manassas Park city
#> 78     VA 2021   NA     Martinsville city
#> 79     VA 2021   NA        Mathews County
#> 80     VA 2021   NA    Mecklenburg County
#> 81     VA 2021   NA      Middlesex County
#> 82     VA 2021   NA     Montgomery County
#> 83     VA 2021   NA         Nelson County
#> 84     VA 2021   NA       New Kent County
#> 85     VA 2021   NA     Newport News city
#> 86     VA 2021   NA          Norfolk city
#> 87     VA 2021   NA    Northampton County
#> 88     VA 2021   NA Northumberland County
#> 89     VA 2021   NA           Norton city
#> 90     VA 2021   NA       Nottoway County
#> 91     VA 2021   NA         Orange County
#> 92     VA 2021   NA           Page County
#> 93     VA 2021   NA        Patrick County
#> 94     VA 2021   NA       Petersburg city
#> 95     VA 2021   NA   Pittsylvania County
#> 96     VA 2021   NA         Poquoson city
#> 97     VA 2021   NA       Portsmouth city
#> 98     VA 2021   NA       Powhatan County
#> 99     VA 2021   NA  Prince Edward County
#> 100    VA 2021   NA  Prince George County
#> 101    VA 2021   NA Prince William County
#> 102    VA 2021   NA        Pulaski County
#> 103    VA 2021   NA          Radford city
#> 104    VA 2021   NA   Rappahannock County
#> 105    VA 2021   NA         Richmond city
#> 106    VA 2021   NA       Richmond County
#> 107    VA 2021   NA          Roanoke city
#> 108    VA 2021   NA        Roanoke County
#> 109    VA 2021   NA     Rockbridge County
#> 110    VA 2021   NA     Rockingham County
#> 111    VA 2021   NA        Russell County
#> 112    VA 2021   NA            Salem city
#> 113    VA 2021   NA          Scott County
#> 114    VA 2021   NA     Shenandoah County
#> 115    VA 2021   NA          Smyth County
#> 116    VA 2021   NA    Southampton County
#> 117    VA 2021   NA   Spotsylvania County
#> 118    VA 2021   NA       Stafford County
#> 119    VA 2021   NA         Staunton city
#> 120    VA 2021   NA          Suffolk city
#> 121    VA 2021   NA          Surry County
#> 122    VA 2021   NA         Sussex County
#> 123    VA 2021   NA       Tazewell County
#> 124    VA 2021   NA   Virginia Beach city
#> 125    VA 2021   NA         Warren County
#> 126    VA 2021   NA     Washington County
#> 127    VA 2021   NA       Waynesboro city
#> 128    VA 2021   NA   Westmoreland County
#> 129    VA 2021   NA     Williamsburg city
#> 130    VA 2021   NA       Winchester city
#> 131    VA 2021   NA           Wise County
#> 132    VA 2021   NA          Wythe County
#> 133    VA 2021   NA           York County
#>                                                                            metro
#> 1                                                            Accomack County, VA
#> 2                                         Charlottesville, VA HUD Metro FMR Area
#> 3                   Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 4   Alleghany County-Clifton Forge city-Covington city, VA HUD Nonmetro FMR Area
#> 5                                                               Richmond, VA MSA
#> 6                                                              Lynchburg, VA MSA
#> 7                                                              Lynchburg, VA MSA
#> 8                   Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 9                                                    Staunton-Waynesboro, VA MSA
#> 10                                                               Bath County, VA
#> 11                                                             Lynchburg, VA MSA
#> 12                                                              Bland County, VA
#> 13                                                Roanoke, VA HUD Metro FMR Area
#> 14                                          Kingsport-Bristol-Bristol, TN-VA MSA
#> 15                                                          Brunswick County, VA
#> 16                                                           Buchanan County, VA
#> 17                                      Buckingham County, VA HUD Metro FMR Area
#> 18   Rockbridge County-Buena Vista city-Lexington city, VA HUD Nonmetro FMR Area
#> 19                                                             Lynchburg, VA MSA
#> 20                                                              Richmond, VA MSA
#> 21                           Carroll County-Galax city, VA HUD Nonmetro FMR Area
#> 22                                                              Richmond, VA MSA
#> 23                                                          Charlotte County, VA
#> 24                                        Charlottesville, VA HUD Metro FMR Area
#> 25                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 26                                                              Richmond, VA MSA
#> 27                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 28                                                              Richmond, VA MSA
#> 29  Alleghany County-Clifton Forge city-Covington city, VA HUD Nonmetro FMR Area
#> 30                                                Roanoke, VA HUD Metro FMR Area
#> 31                                        Culpeper County, VA HUD Metro FMR Area
#> 32                                                         Cumberland County, VA
#> 33                   Pittsylvania County-Danville city, VA HUD Nonmetro FMR Area
#> 34                                                          Dickenson County, VA
#> 35                                                              Richmond, VA MSA
#> 36                     Greensville County-Emporia city, VA HUD Nonmetro FMR Area
#> 37                                                              Essex County, VA
#> 38                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 39                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 40                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 41                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 42                                           Floyd County, VA HUD Metro FMR Area
#> 43                                        Charlottesville, VA HUD Metro FMR Area
#> 44                    Southampton County-Franklin city, VA HUD Nonmetro FMR Area
#> 45                                        Franklin County, VA HUD Metro FMR Area
#> 46                                                         Winchester, VA-WV MSA
#> 47                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 48                           Carroll County-Galax city, VA HUD Nonmetro FMR Area
#> 49                                           Giles County, VA HUD Metro FMR Area
#> 50                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 51                                                              Richmond, VA MSA
#> 52                                                            Grayson County, VA
#> 53                                        Charlottesville, VA HUD Metro FMR Area
#> 54                     Greensville County-Emporia city, VA HUD Nonmetro FMR Area
#> 55                                                            Halifax County, VA
#> 56                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 57                                                              Richmond, VA MSA
#> 58                                                          Harrisonburg, VA MSA
#> 59                                                              Richmond, VA MSA
#> 60                      Henry County-Martinsville city, VA HUD Nonmetro FMR Area
#> 61                                                           Highland County, VA
#> 62                                                              Richmond, VA MSA
#> 63                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 64                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 65                                                     King and Queen County, VA
#> 66                                                        King George County, VA
#> 67                                                              Richmond, VA MSA
#> 68                                                          Lancaster County, VA
#> 69                                                                Lee County, VA
#> 70   Rockbridge County-Buena Vista city-Lexington city, VA HUD Nonmetro FMR Area
#> 71                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 72                                                             Louisa County, VA
#> 73                                                          Lunenburg County, VA
#> 74                                                             Lynchburg, VA MSA
#> 75                                                            Madison County, VA
#> 76                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 77                  Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 78                      Henry County-Martinsville city, VA HUD Nonmetro FMR Area
#> 79                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 80                                                        Mecklenburg County, VA
#> 81                                                          Middlesex County, VA
#> 82                      Blacksburg-Christiansburg-Radford, VA HUD Metro FMR Area
#> 83                                        Charlottesville, VA HUD Metro FMR Area
#> 84                                                              Richmond, VA MSA
#> 85                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 86                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 87                                                        Northampton County, VA
#> 88                                                     Northumberland County, VA
#> 89                             Wise County-Norton city, VA HUD Nonmetro FMR Area
#> 90                                                           Nottoway County, VA
#> 91                                                             Orange County, VA
#> 92                                                               Page County, VA
#> 93                                                            Patrick County, VA
#> 94                                                              Richmond, VA MSA
#> 95                   Pittsylvania County-Danville city, VA HUD Nonmetro FMR Area
#> 96                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 97                 Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 98                                                              Richmond, VA MSA
#> 99                                                      Prince Edward County, VA
#> 100                                                             Richmond, VA MSA
#> 101                 Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 102                                        Pulaski County, VA HUD Metro FMR Area
#> 103                     Blacksburg-Christiansburg-Radford, VA HUD Metro FMR Area
#> 104                                   Rappahannock County, VA HUD Metro FMR Area
#> 105                                                             Richmond, VA MSA
#> 106                                                          Richmond County, VA
#> 107                                               Roanoke, VA HUD Metro FMR Area
#> 108                                               Roanoke, VA HUD Metro FMR Area
#> 109  Rockbridge County-Buena Vista city-Lexington city, VA HUD Nonmetro FMR Area
#> 110                                                         Harrisonburg, VA MSA
#> 111                                                           Russell County, VA
#> 112                                               Roanoke, VA HUD Metro FMR Area
#> 113                                         Kingsport-Bristol-Bristol, TN-VA MSA
#> 114                                                        Shenandoah County, VA
#> 115                                                             Smyth County, VA
#> 116                   Southampton County-Franklin city, VA HUD Nonmetro FMR Area
#> 117                 Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 118                 Washington-Arlington-Alexandria, DC-VA-MD HUD Metro FMR Area
#> 119                                                  Staunton-Waynesboro, VA MSA
#> 120                Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 121                                                             Surry County, VA
#> 122                                                             Richmond, VA MSA
#> 123                                                          Tazewell County, VA
#> 124                Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 125                                         Warren County, VA HUD Metro FMR Area
#> 126                                         Kingsport-Bristol-Bristol, TN-VA MSA
#> 127                                                  Staunton-Waynesboro, VA MSA
#> 128                                                      Westmoreland County, VA
#> 129                Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#> 130                                                        Winchester, VA-WV MSA
#> 131                            Wise County-Norton city, VA HUD Nonmetro FMR Area
#> 132                                                             Wythe County, VA
#> 133                Virginia Beach-Norfolk-Newport News, VA-NC HUD Metro FMR Area
#>           fips efficiency statename smallareastatus
#> 1   5100199999        481  Virginia               0
#> 2   5100399999        949  Virginia               0
#> 3   5151099999       1513  Virginia               1
#> 4   5100599999        495  Virginia               0
#> 5   5100799999        993  Virginia               0
#> 6   5100999999        633  Virginia               0
#> 7   5101199999        633  Virginia               0
#> 8   5101399999       1513  Virginia               1
#> 9   5101599999        690  Virginia               0
#> 10  5101799999        475  Virginia               0
#> 11  5101999999        633  Virginia               0
#> 12  5102199999        472  Virginia               0
#> 13  5102399999        641  Virginia               0
#> 14  5152099999        513  Virginia               0
#> 15  5102599999        499  Virginia               0
#> 16  5102799999        601  Virginia               0
#> 17  5102999999        564  Virginia               0
#> 18  5153099999        508  Virginia               0
#> 19  5103199999        633  Virginia               0
#> 20  5103399999        993  Virginia               0
#> 21  5103599999        472  Virginia               0
#> 22  5103699999        993  Virginia               0
#> 23  5103799999        472  Virginia               0
#> 24  5154099999        949  Virginia               0
#> 25  5155099999        960  Virginia               0
#> 26  5104199999        993  Virginia               0
#> 27  5104399999       1513  Virginia               1
#> 28  5157099999        993  Virginia               0
#> 29  5158099999        495  Virginia               0
#> 30  5104599999        641  Virginia               0
#> 31  5104799999        788  Virginia               0
#> 32  5104999999        626  Virginia               0
#> 33  5159099999        470  Virginia               0
#> 34  5105199999        472  Virginia               0
#> 35  5105399999        993  Virginia               0
#> 36  5159599999        525  Virginia               0
#> 37  5105799999        603  Virginia               0
#> 38  5160099999       1513  Virginia               1
#> 39  5105999999       1513  Virginia               1
#> 40  5161099999       1513  Virginia               1
#> 41  5106199999       1513  Virginia               1
#> 42  5106399999        601  Virginia               0
#> 43  5106599999        949  Virginia               0
#> 44  5162099999        563  Virginia               0
#> 45  5106799999        575  Virginia               0
#> 46  5106999999        814  Virginia               0
#> 47  5163099999       1513  Virginia               1
#> 48  5164099999        472  Virginia               0
#> 49  5107199999        504  Virginia               0
#> 50  5107399999        960  Virginia               0
#> 51  5107599999        993  Virginia               0
#> 52  5107799999        472  Virginia               0
#> 53  5107999999        949  Virginia               0
#> 54  5108199999        525  Virginia               0
#> 55  5108399999        472  Virginia               0
#> 56  5165099999        960  Virginia               0
#> 57  5108599999        993  Virginia               0
#> 58  5166099999        699  Virginia               0
#> 59  5108799999        993  Virginia               0
#> 60  5108999999        528  Virginia               0
#> 61  5109199999        472  Virginia               0
#> 62  5167099999        993  Virginia               0
#> 63  5109399999        960  Virginia               0
#> 64  5109599999        960  Virginia               0
#> 65  5109799999        538  Virginia               0
#> 66  5109999999        723  Virginia               0
#> 67  5110199999        993  Virginia               0
#> 68  5110399999        640  Virginia               0
#> 69  5110599999        472  Virginia               0
#> 70  5167899999        508  Virginia               0
#> 71  5110799999       1513  Virginia               1
#> 72  5110999999        613  Virginia               0
#> 73  5111199999        472  Virginia               0
#> 74  5168099999        633  Virginia               0
#> 75  5111399999        621  Virginia               0
#> 76  5168399999       1513  Virginia               1
#> 77  5168599999       1513  Virginia               1
#> 78  5169099999        528  Virginia               0
#> 79  5111599999        960  Virginia               0
#> 80  5111799999        485  Virginia               0
#> 81  5111999999        617  Virginia               0
#> 82  5112199999        795  Virginia               0
#> 83  5112599999        949  Virginia               0
#> 84  5112799999        993  Virginia               0
#> 85  5170099999        960  Virginia               0
#> 86  5171099999        960  Virginia               0
#> 87  5113199999        541  Virginia               0
#> 88  5113399999        534  Virginia               0
#> 89  5172099999        472  Virginia               0
#> 90  5113599999        510  Virginia               0
#> 91  5113799999        720  Virginia               0
#> 92  5113999999        493  Virginia               0
#> 93  5114199999        472  Virginia               0
#> 94  5173099999        993  Virginia               0
#> 95  5114399999        470  Virginia               0
#> 96  5173599999        960  Virginia               0
#> 97  5174099999        960  Virginia               0
#> 98  5114599999        993  Virginia               0
#> 99  5114799999        593  Virginia               0
#> 100 5114999999        993  Virginia               0
#> 101 5115399999       1513  Virginia               1
#> 102 5115599999        618  Virginia               0
#> 103 5175099999        795  Virginia               0
#> 104 5115799999        921  Virginia               0
#> 105 5176099999        993  Virginia               0
#> 106 5115999999        580  Virginia               0
#> 107 5177099999        641  Virginia               0
#> 108 5116199999        641  Virginia               0
#> 109 5116399999        508  Virginia               0
#> 110 5116599999        699  Virginia               0
#> 111 5116799999        472  Virginia               0
#> 112 5177599999        641  Virginia               0
#> 113 5116999999        513  Virginia               0
#> 114 5117199999        544  Virginia               0
#> 115 5117399999        472  Virginia               0
#> 116 5117599999        563  Virginia               0
#> 117 5117799999       1513  Virginia               1
#> 118 5117999999       1513  Virginia               1
#> 119 5179099999        690  Virginia               0
#> 120 5180099999        960  Virginia               0
#> 121 5118199999        483  Virginia               0
#> 122 5118399999        993  Virginia               0
#> 123 5118599999        472  Virginia               0
#> 124 5181099999        960  Virginia               0
#> 125 5118799999        676  Virginia               0
#> 126 5119199999        513  Virginia               0
#> 127 5182099999        690  Virginia               0
#> 128 5119399999        589  Virginia               0
#> 129 5183099999        960  Virginia               0
#> 130 5184099999        814  Virginia               0
#> 131 5119599999        472  Virginia               0
#> 132 5119799999        472  Virginia               0
#> 133 5119999999        960  Virginia               0

hudfmr(query = '0100199999', year = '2017', key = key)
#>   county or CBSA year town             county              metro metrostatus
#> 1     0100199999 2017   NA Autauga County, AL Montgomery, AL MSA           1
#>   smallareastatus
#> 1               0
```

#### Fair Markets Rent Data Fields

state/county or CBSA &gt; Name of the county if it is a county.

year &gt; Value of year

counties\_msa &gt; Names of all counties belonging to the Metro Area if
it is a Metro Area (MSA).

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
