# hudr 0.2.0.9000 -- 5/1/2022

## Changelog:

### Features:
* All function calls now support multiple geoids, multiple years,
  and multiple quarters queries if they contain these fields.
* Crosswalk a dataset functionality has been added but is not performant 
  and may have unexpected bugs.
* fmr (fair markets rent) has been decomposed into
  multiple functions. This now allows for 
  querying zip code level data or better known as
  (small areas fair markets rent).
* The ability to decompose geoids into their constituent parts is now
  available.
* Utility functions such as adding/removing zeros at the start of a
  number as well as removing delimiters is now available. 
* Error and warning messages from queries that are not found, 
  do not spit out during execution, but rather at the end. Warning and
  error messages are now more visually sound.
* Download bars now appear on the console when querying for data.
* Website documentation now includes details on understanding the general syntax
  of package functions: this is located on the setup page.
* Added the ability for the user to set the their keys to their working
  directory R profile, R_HOME profile, or HOME profile.

### Bug Fixes:

* A bug caused by missing fields in chas 
  (comprehensive housing and affordability) data is fixed.
* Issues caused by fmr queries having mixed between county-level and 
  zip code level data have been fixed.

# hudr 0.1.0.9000 -- 3/18/2022

* The USPS crosswalk functions are now working; the base hud_cw() function 
and its decomposed function calls can now accept multiple years, quarters,
and geoids to query.
* The Fair Markets Rent query functions are now working; they do not support
multiple geoid query calls, but does allows for multiple years and quarters.
* The Income Limits query functions are now working; they do not support
multiple geoid query calls, but does allow for multiple years and quarters.
* The Comprehensive Housing and Affordability functions are now functional.
The allow for querying multiple years but not for multiple geoids. 
* The website has been updated to include examples for the decomposed cw
(crosswalk) and chas(comprehensive housing and affordability) function calls
and contains more details on how to query.
* Development for the crosswalk() function has reached a workable state 
but has not yet been released. It's main functionality is to crosswalk 
an entire dataset.

# hudr 0.0.0.9000 -- 1/28/2022

* Added a `NEWS.md` file to track changes to the package.
