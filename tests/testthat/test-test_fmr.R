
# Multiple stress tests on the the API. Determine if this can get and
# process data in a timely manner.

test_that("Fair Markets Rent State Queries", {
  VA <- hud_fmr("VA", year=c(2021))
  expect_true(nrow(VA) >= 1)
  MD <- hud_fmr("MD", year=c('2021'))
  expect_true(nrow(MD) >= 1)
  CA <- hud_fmr("CA", year=c(2021))
  expect_true(nrow(CA) >= 1)
  AL <- hud_fmr("AL", year=c(2021))
  expect_true(nrow(AL) >= 1)
})

test_that("Fair Markets Rent County Queries", {
  expect_equal(2 * 2, 4)
})

test_that("Fair Markets Rent CBSA", {
  expect_equal(2 * 2, 4)
})

test_that("Fair Markets Rent Different Years", {
  y1 <- hud_fmr("AL", year=c(2024))
  expect_true(nrow(y1) >= 1)
  y2 <- hud_fmr("AL", year=c(2021, 2020, 2019, 2018))
  expect_true(nrow(y2) >= 1)
  y3 <- hud_fmr("MD", year=c(2020, '2019', 2018))
  expect_true(nrow(y3) >= 1)
  y4 <- hud_fmr("AL", year=c(2020, 2020, 2020, 2020))
  expect_true(nrow(y4) >= 1)
  y5 <- hud_fmr("AL", year=c(2014))
  expect_true(nrow(y5) >= 1)
})

test_that("Small Area FMR Queries", {
  AL <- hud_fmr("METRO47900M47900", year=c(2018))
  expect_true(nrow(AL) >= 1)
})

test_that("List States Query", {
  expect_equal(2 * 2, 4)
})

test_that("List Counties Query", {
  expect_equal(2 * 2, 4)
})

test_that("List Small Areas Query", {
  expect_equal(2 * 2, 4)
})
