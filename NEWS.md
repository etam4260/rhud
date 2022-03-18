# hudr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

# hudr 0.1.0.9000

* The USPS Crosswalk query functions are now working. Both the base hud_cw() function and its decomposed function calls can now accept multiple years, quarters, and geoids to query for.
* The Fair Markets Rent query functions are now working. Does not currently support multiple geoid query calls yet, but does work for multiple years and quarters.
* The Income Limits query functions are now working. Does not currently support multiple geoid query calls yet, but does work for multiple years and quarters.
* The Comprehensive Housing and Affordability functions are now working. Can query for multiple years but not yet for multiple geoids. 
* The website has been updated to include examples for the decomposed CW(Crosswalk) and CHAS(Comprehensive Housing and Affordability) function calls as well as providing more rich detail for getting data from different geographic resolutions.

* Development for the crosswalk() function has reached a workable state but has not yet been released. It's main functionality is to crosswalk an entire dataset.
* Development for machine learning approach for a dataset geoid detection function (the ability to detect geographic identifers in a dataset and convert them into a format understood by mapping tools) has begun. The hope is that this will be applied to some of the major mapping tools and software available that allows easy and consistent transformation into different geographic data structures. This is likely to be in a separate package, but may be applied directly here.

