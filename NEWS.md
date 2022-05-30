rhud 0.4.0.9000 (6/10/2022)

### NEW FEATURES:
  
  * Caching is fully implemented, allowing user to cache data to where they 
  want as well as to the current working session. Cached data is stored as
  csv format.
  
### MINOR IMPROVEMENTS:

  * Type checking added for using hud_set_key() as a way of reducing 
  latent errors. 
  * Download bar now shows the current working item as well as the 
  number of failed downloads.
  * Using the hud_set_key method does not default to writing to the working 
  directory .Rprofile.
  * Introduction message to the package is now more informative.
  * Functions now check if user enters years below what is allowed.
  
### DOCUMENTATION FIXES
  
  * Improved sample case studies in the website documentation.
  * Added diagram for census geographies to the crosswalk website documentation
  to better communicate relationship among geographies.
  * Introduced some iconography into documentation to help 
  overall user experience. 
  * All functions are listed in the README.md page along with the range of years
  that they can be queried for.
  
rhud 0.3.0.9000 (5/30/2022)
============================

### NEW FEATURES:

  * Added options() for the rhud package, allowing a user to allow caching and 
  tibbles. 
   
  * User is now given a `to_tibble` option for rhud functions that return a
  data frame. This allows them to get tibbles rather than data frames as the type
  of data returned. 
  
### MINOR IMPROVEMENTS:

  * User is now presented with a message when attaching the package 
  showing the various options that can that can be set for the package as
  well as telling the user that they need to obtain a key from the HUD User
  website. 

### BUG FIXES

  * Fixed state level FMR data returning two copies of very_low and not low
  * Fixed issue with IL data when querying for county and cbsa returning 
  duplicate year and median income fields. 
  * Remove download bar from appearing when first loading in the 
  hud_nation_state_territories() in package environment when validating
  state inputs. 
  
### DOCUMENTATION FIXES

  * Removed crosswalk example on documentation website from case study and 
  placed the sample into the crosswalk vignette. 
  * The case studies tab of the documentation website now includes more 
  realistic use cases for the rhud package alongside several popular R packages.
  However, they are not complete and will be updated in the future. 
  * Added Shuyu Jin as a contributor to the package. 
  

rhud 0.2.0.9000 (5/1/2022)
============================

### NEW FEATURES:

  * All function calls now support multiple geoids, multiple years,
    and multiple quarters queries if they contain these fields.
  * Crosswalk a dataset functionality has been added but is not performant 
    and may have unexpected bugs.
  * fmr (fair markets rent) has been decomposed into
    multiple functions. This now allows for 
    querying zip code level data or better known as
    (small areas fair markets rent).
  * Added the ability for the user to set the their keys to their working
    directory .Rprofile and HOME .Rprofile through the hud_set_key() function 
    so keys are now persistent across sessions.
  
### MINOR IMPROVEMENTS:

  * Error and warning messages from queries that are not found, 
    do not spit out during execution, but rather at the end. Warning and
    error messages are now more visually sound.
  * Download bars now appear on the console when querying for data.

### BUG FIXES:

  * A bug caused by missing fields in chas 
    (comprehensive housing and affordability) data is fixed.
  * Issues caused by fmr queries having mixed between county-level and 
    zip code level data have been fixed.

### DOCUMENTATION FIXES

  * Website documentation now includes details on understanding the general syntax
    of package functions: this is located on the setup page.
  * The entire package has been renamed to rhud


rhud 0.1.0.9000 (3/18/2022)
=============================

### NEW FEATURES

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

rhud 0.0.0.9000 (1/28/2022)
=============================
  
  * Added a `NEWS.md` file to track changes to the package.
