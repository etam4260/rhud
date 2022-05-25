
test_that("test simple crosswalk for zip", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  sample <- data.frame(mes = c(1232, 2453, 4564), zip = c(21206, 21224, 20854))

  # Crosswalk zip to every other geoid...

  ### Crosswalk zip to tract in quarter 1, 2018
  expect_error(crosswalk(sample, "zip", "zip", "tract", year = 2018,
                         quarter = 1), NA)
  expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "res",
                         2018, 1), NA)

  # Use business
  expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "bus",
                         2018, 1), NA)
  # Use total
  expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "tot",
                         2018, 1), NA)
  # Use other
  expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "oth",
                         2018, 1), NA)


  ### Crosswalk zip to county in quarter 1, 2018
  expect_error(crosswalk(sample, "zip", "zip", "county",
                         year = 2018, quarter = 1), NA)
  expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "res",
                         2018, 1), NA)

  # Use business
  expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "bus",
                         2018, 1), NA)
  # Use total
  expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "tot",
                         2018, 1), NA)
  # Use other
  expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "oth",
                         2018, 1), NA)


  ### Crosswalk zip to cbsa in quarter 1, 2018
  expect_error(crosswalk(sample, "zip", "zip", "cbsa",
                         year = 2018, quarter = 1), NA)
  expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "res",
                         2018, 1), NA)

  # Use business
  expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "bus",
                         2018, 1), NA)
  # Use total
  expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "tot",
                         2018, 1), NA)
  # Use other
  expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "oth",
                         2018, 1), NA)


  ### Crosswalk zip to cd in quarter 1, 2018
  expect_error(crosswalk(sample, "zip", "zip", "cd",
                         year = 2018, quarter = 1), NA)
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "res",
                         2018, 1), NA)

  # Use business
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "bus",
                         2018, 1), NA)
  # Use total
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "tot",
                         2018, 1), NA)
  # Use other
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "oth",
                         2018, 1), NA)



  ### Crosswalk zip to cd in quarter 1, 2018
  expect_error(crosswalk(sample, "zip", "zip", "cd",
                         year = 2018, quarter = 1), NA)
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "res",
                         2018, 1), NA)

  # Use business
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "bus",
                         2018, 1), NA)
  # Use total
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "tot",
                         2018, 1), NA)
  # Use other
  expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "oth",
                         2018, 1), NA)
})


test_that("test awkward crosswalk",{
  sample <- data.frame(mes = c(1232, 1232, 1232), zip = c(21206, 21206, 21206))

  expect_error(crosswalk(sample, "zip", "zip", "county", year = 2018,
                         quarter = 1), NA)
  expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "res",
           2018, 1), NA)

  # Not all of length 5
  sample <- data.frame(mes = c(1232, 1232, 1232), zip = c(21206, 21206, 212068))
  expect_error(crosswalk(sample, "zip", "zip", "county", year = 2018,
            quarter = 1))
})
