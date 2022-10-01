test_that("hud_fmr() Simple State Queries", {
  skip_if_no_key()

  va <- hud_fmr("VA", year = c(2021))
  expect_true(length(va) == 2)
  md <- hud_fmr("MD", year = c("2021"))
  expect_true(length(md) == 2)
  ca <- hud_fmr("CA", year = c(2021))
  expect_true(length(ca) == 2)
  al <- hud_fmr("AL", year = c(2021))
  expect_true(length(al) == 2)
})

test_that("hud_fmr() Simple County Queries", {
  skip_if_no_key()

  # nrow returns the number or rows in the dataframe. length returns
  # the number of coolumns. ncol() can also be used.

  c1 <- hud_fmr("5100199999", year = c(2021))
  expect_true(nrow(c1) == 1)
  c2 <- hud_fmr("5100199999", year = c("2021"))
  expect_true(nrow(c2) == 1)
  c3 <- hud_fmr("5151099999", year = c(2021))
  expect_true(nrow(c3) == 1)

  # Make sure to check that everything is of type character
  # not numeric or factor. Also make sure nothing is of list.
  expect_true(is_valid_rhud_df(c1))
  expect_true(is_valid_rhud_df(c2))
  expect_true(is_valid_rhud_df(c3))
})


test_that("hud_fmr() Simple Metroarea Queries", {
  skip_if_no_key()

  sa1 <- hud_fmr("METRO47900M47900", year = c(2018))
  expect_true(nrow(sa1) == 443)
  sa2 <- hud_fmr("METRO29180N22001", year = c(2019))
  expect_true(nrow(sa2) == 1)
  sa3 <- hud_fmr("METRO10380M10380", year = c(2020))
  expect_true(nrow(sa3) == 1)

  expect_true(is_valid_rhud_df(sa1))
  expect_true(is_valid_rhud_df(sa2))
  expect_true(is_valid_rhud_df(sa3))
})


test_that("hud_fmr() Different Years Queries", {
  skip_if_no_key()

  y1 <- hud_fmr("AL", year = c(2020))
  expect_true(length(y1) == 2)
  y2 <- hud_fmr("AL", year = c(2021, 2020, 2019, 2018))
  expect_true(length(y2) == 2)
  y3 <- hud_fmr("MD", year = c(2020, "2019", 2018))
  expect_true(length(y3) == 2)
  y4 <- hud_fmr("AL", year = c(2020, 2020, 2020, 2020))
  expect_true(length(y4) == 2)
  y5 <- hud_fmr("AL", year = c(2017))
  expect_true(length(y5) == 2)
})
