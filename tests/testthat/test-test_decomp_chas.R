test_that("test hud_chas_nation()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  # First make simple query call to hud_chas_nation() with no arguments.
  # Will choose default 2014-2018.
  one_year <- hud_chas_nation()
  # Just expect one row to return... weak check.
  expect_true(nrow(one_year) == 1)
  expect_true(check_is_not_list(one_year))

  # Try querying multiple years from the nation.
  two_years <- hud_chas_nation(year = c("2014-2018", "2012-2016"))
  expect_true(nrow(two_years) == 2)
  expect_true(check_is_not_list(two_years))

  # Try querying a year which is not allowed... should expect warning
  expect_error(hud_chas_nation(year = c("2017-2022", "2321-142131")))

  # Try integer inputs. It should also throw warning.
  expect_error(hud_chas_nation(year = c(2018, 2019)))
})

test_that("test hud_chas_state" ,{
  skip_if(Sys.getenv("HUD_KEY") == "")
  # Try querying for a state using abbreviation...
  # Try lowercase too... Try uppercase too...
  # Try weird cases...

  # None of these should throw errors...
  test <- hud_chas_state(state = "VA")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "California")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "46")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "Ca")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "ca")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "CA")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "VIRGINIA")
  expect_true(nrow(test) == 1)

  test <- hud_chas_state(state = "vIRGINIa")
  expect_true(nrow(test) == 1)

  # Try querying for all states in nation
  hudstates <- hud_states()
  hudstates <- hudstates[as.numeric(hudstates$state_num) < 57, ]
  hudstates <- hudstates[hudstates$state_code != 'DC', ]

  all_states_full <- hud_chas_state(hudstates$state_name)
  expect_true(nrow(all_states_full) >= 1)
  expect_true(check_is_not_list(all_states_full))

  # Try to query for all state codes?
  all_state_abbr <- hud_chas_state(hudstates$state_code)
  expect_true(nrow(all_state_abbr) >= 1)
  expect_true(check_is_not_list(all_state_abbr))

  # Try to query for all state name?
  all_state_num <- hud_chas_state(hudstates$state_num)
  expect_true(nrow(all_state_num) >= 1)
  expect_true(check_is_not_list(all_state_num))

  # Check if they are identical according to R...
  expect_true(identical(all_states_full, all_state_abbr) &&
                identical(all_states_full, all_state_num))
})

test_that("test hud_chas_county()",{
  skip_if(Sys.getenv("HUD_KEY") == "")
  # Simple query only a single county.
  test <- hud_chas_county(county = "06105")
  expect_true(nrow(test) == 1)
  # Query counties that have multiple...
  # Need to deal with cases when leading zero might get truncated.

  test <- hud_chas_county(county = c(06105, 06113))
  expect_true(nrow(test) == 2)
  # Query multiple character counties

  test <- hud_chas_county(county = c("06105","06115"))
  expect_true(nrow(test) == 2)
  # Query multiple counties with multiple years.
  test <- hud_chas_county(county = c("06105","06115"),
                          year = c("2013-2017", "2014-2018"))
  expect_true(nrow(test) == 4)

  # Query for all counties in Maryland.
  all_md_counties <- hud_counties("MD")

  # Only use the first 5 numbers in fips code.
  all_md <- hud_chas_county(county = substr(all_md_counties$fips_code,0,5))
  expect_true(nrow(all_md) >= 1)
  expect_true(check_is_not_list(all_md))
})

test_that("test hud_chas_mcd()",{
  skip_if(Sys.getenv("HUD_KEY") == "")

  all_md <- hud_chas_mcd("md")

  expect_true(nrow(all_md) >= 1)
  expect_true(check_is_not_list(all_md))
})

test_that("test hud_chas_place()",{
  skip_if(Sys.getenv("HUD_KEY") == "")

  all_md <- hud_chas_place("md")

  expect_true(nrow(all_md) >= 1)
  expect_true(check_is_not_list(all_md))
})
