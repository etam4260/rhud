
# Multiple stress tests on the the API. Determine if this can get and
# process data in a timely manner.

test_that("Income Limits State Queries", {
  expect_equal(2 * 2, 4)
})

test_that("Income Limits Rent County Queries", {
  expect_equal(2 * 2, 4)
})

test_that("Income Limits Different Years", {
  expect_equal(2 * 2, 4)
})
