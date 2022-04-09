# Should allow user to provide county name/state names/abbreviations. All string
# inputs that can be numeric inputs can be supplied as a numeric input.
test_that("Crosswalk All Types", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  # For these tests, none of  the Crosswalk queries should throw an error.
  # Need to also check for if these queries result a satiable result; need
  # to add least have 1 row or more in data frame returned.

  # Lets try a ZIP code in Alabama for now for 1 -> 5 as well as 11.
  zip_tract <- hud_cw(type = 1, query = '35213',
                      year = c('2010', '2011'), quarter = c('1'))

  expect_true(nrow(zip_tract) >= 1)

  zip_county <- hud_cw(type = '2', query = '35213',
                       year = c('2016', '2020'), quarter = c('2'))

  expect_true(nrow(zip_county) >= 1)

  zip_cbsa <- hud_cw(type = 3, query = 35213,
                     year = c('2012', '2011'), quarter = c('3'))

  expect_true(nrow(zip_cbsa) >= 1)

  zip_cbsadiv <- hud_cw(type = 4, query = '22031',
                        year = c('2017', '2019'), quarter = c('4'))

  expect_true(nrow(zip_cbsadiv) >= 1)

  zip_cd <- hud_cw(type = '5', query = '35213',
                   year = c(2011, '2012'), quarter = c('1','2'))

  expect_true(nrow(zip_cd) >= 1)

  # A census tract code consists of STATE+COUNTY+TRACT. So in this
  # STATE = 48
  # COUNTY = 201
  # TRACT = 223100
  tract_zip <- hud_cw(type = 6, query = '48201223100',
                      year = c('2017', '2010'),
                      quarter = c('1','2','3'))

  expect_true(nrow(tract_zip) >= 1)

  # Testing county to zip cross_walk. Assuming this to be the most popular.
  county_zip <- hud_cw(type = 7, query = '22031',
                       year = c('2010', '2011'), quarter = c('1','2','3','4'))

  expect_true(nrow(county_zip) >= 1)

  # Try all counties in MD
  county_zip <- hud_cw(type = 7, query = substr(hud_counties("md")$fips_code,1,5),
                       year = c('2010'), quarter = c('1'))
  expect_true(nrow(county_zip) >= 1)

  # A core based Statistical Area to zip crosswalk.
  # CBSA defines Micropolitan and Metropolitan
  cbsa_zip <- hud_cw(type = 8, query = '10140',
                     year = c('2010', '2011'), quarter = c('2','1'))

  expect_true(nrow(cbsa_zip) >= 1)

  # Must be a core base statistical area division code which apply to
  # metropolitan areas.
  cbsadiv_zip <- hud_cw(type = 9, query = '10380',
                        year = c('2017'), quarter = c('4'))

  expect_true(nrow(cbsadiv_zip) >= 1)
  # Must be state fip + congressional district code
  # STATE = 22
  # CONGRESSIONAL DIS = 02
  cd_zip <- hud_cw(type = 10, query = '2202',
                   year = c('2010', '2011'), quarter = c('4','3'))

  expect_true(nrow(cd_zip) >= 1)

  # Testing ZIP CODE _> COUNTYSUB
  zip_countysub <- hud_cw(type = 11, query = '35213',
                          year = c('2019', '2020'), quarter = c('2','3'))

  expect_true(nrow(zip_countysub) >= 1)
  # User might not provide a 'set' of years or quarters, so should make sure to
  # check that Sometimes the API might not provide data because out of range
  # years. Also testing Countysub _> ZIP
  # STATE = 46
  # COUNTY = 067
  # SUB = 20300
  # Not sure if R has a package for all these types of GEOIDs
  countysub_zip <- hud_cw(type = 12, query = '4606720300 ',
                          year = c('2019', '2019', '2019'),
                          quarter = c('4','4'))

  expect_true(nrow(countysub_zip) >= 1)
})


test_that("Crosswalk Different Years", {
  # Error when years are in the future
  expect_error(hud_cw(type = 7, query = '22031',
                      year = c('2010', '2011', '2024'),
                      quarter = c('1','2','3','4')))
})


test_that("Different Quarters", {
  # Error when quarters are not from 1 to 4
  expect_error(hud_cw(type = 7, query = '22031',
                      year = c('2010', '2011'),
                      quarter = c('1','2','3','4','5')))
})


test_that("Wrong Queries", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  expect_error(hud_cw(type = 7, query = '22031241231',
                      year = c('2010', '2011'),
                      quarter = c('1','2','3','4')))

  # No zip code named 99999
  expect_warning(hud_cw(type = 1, query = '99999',
                        year = c('2010', '2011'),
                        quarter = c('1','1','2','3','4')))
})


test_that("Not Convertable to Integer", {
  # Character arguments that aren't numbers in any of them. Throws errors.
  skip_if(Sys.getenv("HUD_KEY") == "")
  expect_error(hud_cw(type = 'dwqji', query = '22031',
                      year = c('2010', '2011'),
                      quarter = c('1','2','3','4')))

  expect_error(hud_cw(type = '7', query = '22031ada',
                      year = c('2010', '2011'),
                      quarter = c('1','2','3','4')))

  expect_error(hud_cw(type = '7', query = '22031',
                      year = c('2010', '2011adaadda'),
                      quarter = c('1','2ada','3','4')))

  expect_error(hud_cw(type = 7, query = '22031',
                      year = c('2010', '2011'),
                      quarter = c('1','2as','3','4')))
})
