test_that("test hud_chas_nation()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  # First make simple query call to hud_chas_nation() with no arguments.
  # Will choose default 2014-2018.
  one_year <- hud_chas_nation()
  # Just expect one row to return... weak check.
  expect_true(nrow(one_year) == 1)

  # Try querying multiple years from the nation.
  two_years <- hud_chas_nation(year = c("2014-2018", "2012-2016"))
  expect_true(nrow(two_years) == 2)

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
  test <- hud_chas_county(county = c("06105","06115"), year = c("2013-2017", "2014-2018"))
  expect_true(nrow(test) == 4)
})

test_that("test hud_chas_mcd()",{
  skip_if(Sys.getenv("HUD_KEY") == "")
  # Simple query
  test <- hud_chas_mcd("VA", "94135")
  expect_true(nrow(test) == 1)
  # Try using number
  test <- hud_chas_mcd(51, 94135)
  expect_true(nrow(test) == 1)
  # Try using full name
  # Mgiht return 06, be we need 6 as state fips...
  test <- hud_chas_mcd("California", "93140")
  expect_true(nrow(test) == 1)
  # Try dealing with multiple years
  test <- hud_chas_mcd("VA", "94135", year = c("2014-2018","2013-2017"))
  expect_true(nrow(test) == 2)
  # Need to make sure states correspond to the right MCDs...
  test <- hud_chas_mcd(c("MD", "VA"), c("90812", "94135"))
  expect_true(nrow(test) == 2)
})

test_that("test hud_chas_place()",{
  skip_if(Sys.getenv("HUD_KEY") == "")
  # Simple query
  test <- hud_chas_place("VA", "48996")
  expect_true(nrow(test) == 1)
  # Try using number
  test <- hud_chas_place(51, 48996)
  expect_true(nrow(test) == 1)
  # Try using full name
  test <- hud_chas_place("6", "17727")
  expect_true(nrow(test) == 1)
  # Try dealing with multiple years
  test <- hud_chas_place("MD", 53625, year = c("2014-2018","2013-2017"))
  expect_true(nrow(test) == 2)
  # Need to make sure states correspond to the right places...

  # Right now CA doesn't work. It seems like those states with leading 0 doesn't seem to be found...
  # Furthermore, the format of the place should all be the same. Cant mix abbreviation and code in it...
  # might keep it that way...
  test <- hud_chas_place(c("MD", "VA"), c("53700", "48952"))
  expect_true(nrow(test) == 2)
})
