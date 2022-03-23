# Read in the dataset...
dataset_va <- read.csv("dataset_va")

test_that("dataset crosswalk basic county to zip", {
  # Crosswalk a dataset made of only Virginia data from IHP(Individuals and
  # Household Programs) dataset. Assume year is previous year and quarter is 1.
  # Does not round data and will use the 'tot' ratio method.
  expect_output(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54))
  # Will assume any output to console that does not throw an error is good for now.
})

test_that("dataset crosswalk different years and quarters", {
  # Need to make sure to handle cases where years and quarters are out of range...
  expect_output(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "2020", quarter = 4))

  expect_output(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "2019", quarter = 3))

  expect_output(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "2017", quarter = 2))

  expect_output(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "2016", quarter = 1))

  # Errors out because year should not be past current time...
  expect_error(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "08318", quarter = 1))

  # Errors out because year should not be so far in the past...
  expect_error(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "1567", quarter = 1))

  # Errors out because there isn't a 7 quarter in a year...
  expect_error(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, year = "1567", quarter = 7))
})


test_that("dataset crosswalk oth, res, bus, tot ratios", {
  # Crosswalks based on residential addresses ratio...
  res <- crosswalk(dataset = dataset_va, type = "county-zip", ratio = "res", geoid_col = 54, year = "2020", quarter = 4)

  # Crosswalks based on business addresses ratio...
  bus <- crosswalk(dataset = dataset_va, type = "county-zip", ratio = "bus", geoid_col = 54, year = "2019", quarter = 3)

  # Crosswalks based on total addresses ratio...
  tot <- crosswalk(dataset = dataset_va, type = "county-zip", ratio = "tot", geoid_col = 54, year = "2017", quarter = 2)

  # Crosswalks based on other addresses ratio...
  oth <- crosswalk(dataset = dataset_va, type = "county-zip", ratio = "oth", geoid_col = 54, year = "2016", quarter = 1)

  # Need to check that none of the datasets are equal. This is a very
  # weak check and needs to be improved.
  expect_false(identical(res, bus))
  expect_false(identical(res, tot))
  expect_false(identical(res, oth))

  expect_false(identical(bus, tot))
  expect_false(identical(bus, oth))

  expect_false(identical(tot, oth))
})


test_that("dataset crosswalk rounding", {
  # Will set round argument to TRUE. Issue is that round is defined as a base R
  # function and so may cause problems... Just makes sure no errors are thrown.
  output <- expect_output(crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, round = TRUE, year = "2020", quarter = 4))
  # Sample one value in the final output. This is the SUM ONA amount which should be
  # whole values and should not contain decimals values.
  expect_false(hudr::decimal_num(output[2,11]))
})


test_that("dataset crosswalk negative fields and positive fields", {
  # Base crosswalk function assumes that any fields that are integer and numeric
  # will be valid fields to transform. We need to deal with the issue of some
  # fields being ordinal or nominal but are represented as integers. This
  # function should work when specifying negative values such as -c(1,2,3).
  neg_cross <- crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, round = TRUE, year = "2021", quarter = 4)

  # Weak test to see if data is returned...
  expect_true(nrow(neg_cross) >= 3)

  # Assume that when using(not with the negative sign) the c(1,2,3) and
  # c("percentage_share", "numberoccurences", "people") will crosswalk only
  # those fields.
  pos_cross <- crosswalk(dataset = dataset_va, type = "county-zip", geoid_col = 54, round = TRUE, year = "2021", quarter = 4)

  # Weak test to see if data is returned...
  expect_true(nrow(pos_cross) >= 3)

  # Use equal as relaxed comparison..
  expect_true(identical(neg_cross, pos_cross))
})
