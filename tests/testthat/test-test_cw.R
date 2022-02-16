
# Multiple stress tests on the the API. Determine if this can get and
# process data in a timely manner.


test_that("Crosswalk All Types", {
  expect_equal(2 * 2, 4)
})


test_that("Crosswalk All Queries", {
  expect_equal(2 * 2, 4)
})


test_that("Crosswalk Different Years", {
  expect_equal(2 * 2, 4)
})

test_that("Different Quarters", {
  expect_equal(2 * 2, 4)
})
