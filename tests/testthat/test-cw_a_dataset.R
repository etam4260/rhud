
test_that("crosswalk() for Zipcodes and Simple Examples", {
  skip_if_no_key()

  sample <- data.frame(mes = c(1232, 2453, 4564), zip = c(21206, 21224, 20854))

  # Crosswalk zip to every other geoid...

  ### Crosswalk zip to tract in quarter 1, 2018
  res <- expect_error(crosswalk(sample, "zip", "zip", "tract", year = 2018,
                         quarter = 1), NA)
  expect_equal(length(res), 9)

  res <- expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "res",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use business
  res <- expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "bus",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use total
  res <- expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "tot",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use other
  res <- expect_error(crosswalk(sample, "zip", "zip", "tract", "mes", "oth",
                         2018, 1), NA)
  expect_equal(length(res), 9)


  ### Crosswalk zip to county in quarter 1, 2018
  res <- expect_error(crosswalk(sample, "zip", "zip", "county",
                         year = 2018, quarter = 1), NA)
  expect_equal(length(res), 9)

  res <- expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "res",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use business
  res <- expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "bus",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use total
  res <- expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "tot",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use other
  res <- expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "oth",
                         2018, 1), NA)
  expect_equal(length(res), 9)


  ### Crosswalk zip to cbsa in quarter 1, 2018
  res <- expect_error(crosswalk(sample, "zip", "zip", "cbsa",
                         year = 2018, quarter = 1), NA)
  expect_equal(length(res), 9)

  res <- expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "res",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use business
  res <- expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "bus",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use total
  res <- expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "tot",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use other
  res <- expect_error(crosswalk(sample, "zip", "zip", "cbsa", "mes", "oth",
                         2018, 1), NA)
  expect_equal(length(res), 9)


  ### Crosswalk zip to cd in quarter 1, 2018
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd",
                         year = 2018, quarter = 1), NA)
  expect_equal(length(res), 9)

  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "res",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use business
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "bus",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use total
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "tot",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use other
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "oth",
                         2018, 1), NA)
  expect_equal(length(res), 9)



  ### Crosswalk zip to cd in quarter 1, 2018
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd",
                         year = 2018, quarter = 1), NA)
  expect_equal(length(res), 9)

  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "res",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use business
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "bus",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use total
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "tot",
                         2018, 1), NA)
  expect_equal(length(res), 9)

  # Use other
  res <- expect_error(crosswalk(sample, "zip", "zip", "cd", "mes", "oth",
                         2018, 1), NA)
  expect_equal(length(res), 9)
})


test_that("crosswalk() for Awkward Inputs", {
  skip_if_no_key()

  sample <- data.frame(mes = c(1232, 1232, 1232), zip = c(21206, 21206, 21206))

  res <- expect_error(crosswalk(sample, "zip", "zip", "county", year = 2018,
                         quarter = 1), NA)
  expect_equal(length(res), 9)

  res <- expect_error(crosswalk(sample, "zip", "zip", "county", "mes", "res",
           2018, 1), NA)
  expect_equal(length(res), 9)

  # Not all of length 5
  sample <- data.frame(mes = c(1232, 1232, 1232), zip = c(21206, 21206, 212068))

  res <- expect_error(crosswalk(sample, "zip", "zip", "county", year = 2018,
            quarter = 1))

})
