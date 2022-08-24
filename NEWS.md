rhud 0.5.0.9000 (8/17/2022)
============================

#### New Features
  * rhud website now has a search bar.
  
  * Functions containing the to_tibble argument are now all defaulted to     
    getOption(“rhud_use_tibble”, FALSE).
    
  * Added functionality to quiet the download bar (WIP).
  
  * New feature includes the rhud_sitrep() function which report if api key 
    is set, api is reachable, and current cache settings (WIP).

#### Minor Bug Fixes and Improvements:

  * hud_get_key() now raises informative error if no key is set.
  
  * Deleted occurrences of fake key, for example, changed Sys.setenv(“HUD_KEY”   
    =“q3r2rjimd129fj121jid”) to Sys.setenv(“HUD_KEY” = “YOUR KEY HERE”).
    
  * Used mnemonic names hud_{dataset}{geography}{resolution} to replace 
    hud_{1}{2}{3}.
    
  * Removed most naked abbreviations in function documentation and website 
    documentation.
    
  * @title now gives more descriptive details on each function.
  
  * Improved naming of package unit tests.
  
  * When package is attached, the user will be prompted with a startup message
    only if no key is set.
    
  * Occurrences of library(rhud) has been removed from the @examples 
    documentation.
    
#### Documentation Fixes

  * Deleted some duplicated articles which were generated in the past.
  
  * Improved sample case studies in the website documentation.
  
  * Added diagram for census geographies to the crosswalk website 
    documentation to better communicate relationship among geographies.
    
  * Introduced some iconography into documentation to help overall user 
    experience.
    
  * Reorganized readme to have important information at top vs bottom.
  
  * Function documentation now includes more information on which R data 
    types are accepted.


rhud 0.4.0.9000 (6/10/2022)
============================

#### New Features
  
  * Caching is fully implemented, allowing user to cache data to where they 
    want as well as to the current working session. Currently, this uses the 
    R.cache library which use the .Rcache file extension. This, however, might
    be removed in favor of a solution which allows different caching file 
    formats.
    
  * Added the ability to set a user agent. 
    
  * Added the ability to determine the most recent data for each program 
    provided by HUD USER API via hud_rec_cw_yr(), hud_rec_fmr_yr(), 
    hud_rec_il_yr().
    
  * %z_in_trt%`, `%z_in_cty%`, `%z_in_ctysb%`, `%z_in_cd%`, `%z_in_cbsa%`, `
    %z_in_cbsadiv%`are now available to help determine whether a singular 
    zip code overlaps a geographic identifier using the crosswalk files. 
    Currently supports only numeric identifiers, but may in the future 
    provide support for names. Their non-infix counterparts are also available.
    
    
  * %trt_in_z%`, `%cty_in_z%`, `%ctysb_in_z%`, `%cd_in_z%`, `%cbsa_in_z%`, 
    `%cbsadiv_in_z%`are now available to help determine whether a tract, cd, 
    county, countysub, cbsa, or cbsadiv overlaps a zip code using the 
    crosswalk files. Currently supports only numeric identifiers, but may 
    in the future provide support for names. Their non-infix counterparts 
    are also available.
  
#### Minor Bug Fixes and Improvements:

  * Fixed some bugs caused by hud_set_key not properly setting to the .Rprofile.
  
  * Fix some issues with numerics being passed with less digits because of
    leading 0 truncation. (WIP)
    
  * Type checking added for using hud_set_key() as a way of reducing 
    latent errors.
    
  * Download bar now shows the current working url as well as the 
    number of failed downloads. 
    
  * Using the hud_set_key method does not default to writing to the working 
    directory .Rprofile.
    
  * Introduction message to the package is now more informative.
  
  * Functions now check if user enters years below what is allowed.
  
  * State queries for hud_fmr no longer does double queries.
  
  * User is presented with a description of the entire package when typing
    part of its name in the console.
    
  * Attempt two at removing download bar from appearing when first
    loading in the hud_nation_state_territories() in package environment when
    validating state inputs. 
  
#### Documentation Fixes
  
  * Added documentation to tell user that queries will be a combination of the 
    geoid, year, and quarter inputs. 
    
  * Included in README.md how to setup the key and provide a simple example.
  
  * Improved sample case studies in the website documentation. (WIP)
  
  * Added diagram for census geographies to the crosswalk website documentation
    to better communicate relationship among geographies. (WIP)
    
  * Introduced some iconography into documentation to help 
    overall user experience.  (WIP)
    
  * All functions are listed in the README.md page along with the range of years
    that they can be queried for.
    
  * Description of package now shows up when typing it in console.
  
  * Fixed examples provided by in documentation to be runnable and 
    not throw errors. 
  
rhud 0.3.0.9000 (5/30/2022)
============================

#### New Features

  * Added options() for the rhud package, allowing a user to get tibbles instead
    of dataframes.
   
  * User is now given a `to_tibble` option for rhud functions that return a
    data frame. This allows them to get tibbles rather than data frames as 
    the type of data returned. 
  
#### Minor Bug Fixes and Improvements:

  * Fixed state level FMR data returning two copies of very_low and not low.
  
  * Fixed issue with IL data when querying for county and cbsa returning 
    duplicate year and median income fields. 
  
  * User is now presented with a message when attaching the package 
    showing the various options that can that can be set for the package as
    well as telling the user that they need to obtain a key from the HUD User
    website.
    
  * Remove download bar from appearing when first loading in the 
    hud_nation_state_territories() in package environment when validating
    state inputs. 
  
#### Documentation Fixes

  * Removed crosswalk example on documentation website from case study and 
    placed the sample into the crosswalk vignette. 
    
  * The case studies tab of the documentation website now includes more 
    realistic use cases for the rhud package alongside several popular 
    R packages. However, they are not complete and will be updated in the future. 
    
  * Added Shuyu Jin as a contributor to the package. 
  

rhud 0.2.0.9000 (5/1/2022)
============================

#### New Features

  * All function calls now support multiple geoids, multiple years,
    and multiple quarters queries if they contain these fields.
    
  * Crosswalk a dataset functionality has been added but is not performant 
    and may have unexpected bugs.
    
  * fmr (fair market rents) has been decomposed into
    multiple functions. This now allows for 
    querying zip code level data or better known as
    (small areas fair market rents).
    
  * Added the ability for the user to set the their keys to their working
    directory .Rprofile and HOME .Rprofile through the hud_set_key() function 
    so keys are now persistent across sessions.
  
#### Minor Bug Fixes and Improvements:

  * Error and warning messages from queries that are not found, 
    do not spit out during execution, but rather at the end. Warning and
    error messages are now more visually sound.
    
  * Download bars now appear on the console when querying for data.
  
  * A bug caused by missing fields in chas 
    (comprehensive housing and affordability) data is fixed.
    
  * Issues caused by fmr queries having mixed between county-level and 
    zip code level data have been fixed.

#### Documentation Fixes

  * Website documentation now includes details on understanding the 
    general syntax of package functions: this is located on the setup page.
    
  * The entire package has been renamed to rhud


rhud 0.1.0.9000 (3/18/2022)
=============================

#### New Features

  * The USPS crosswalk functions are now working; the base hud_cw() function 
    and its decomposed function calls can now accept multiple years, quarters,
    and geoids to query.
    
  * The Fair Market Rents query functions are now working; they do not support
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
