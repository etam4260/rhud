test_that("hud_fmr() Simple State Queries", {
  skip_if_no_key()

  va <- hud_fmr("VA", year = c(2021))
  expect_true(length(va) >= 1)
  md <- hud_fmr("MD", year = c("2021"))
  expect_true(length(md) >= 1)
  ca <- hud_fmr("CA", year = c(2021))
  expect_true(length(ca) >= 1)
  al <- hud_fmr("AL", year = c(2021))
  expect_true(length(al) >= 1)
})

test_that("hud_fmr() Simple County Queries", {
  skip_if_no_key()

  c1 <- hud_fmr("5100199999", year = c(2021))
  expect_true(nrow(c1) >= 1)
  c2 <- hud_fmr("5100199999", year = c("2021"))
  expect_true(nrow(c2) >= 1)
  c3 <- hud_fmr("5151099999", year = c(2021))
  expect_true(nrow(c3) >= 1)

  # Make sure to check that everything is of type character
  # not numeric or factor. Also make sure nothing is of list.
  expect_true(is_valid_rhud_df(c1))
  expect_true(is_valid_rhud_df(c2))
  expect_true(is_valid_rhud_df(c3))
})


test_that("hud_fmr() Simple Metroarea Queries", {
  skip_if_no_key()

  sa1 <- hud_fmr("METRO47900M47900", year = c(2018))
  expect_true(nrow(sa1) >= 1)
  sa2 <- hud_fmr("METRO29180N22001", year = c(2019))
  expect_true(nrow(sa2) >= 1)
  sa3 <- hud_fmr("METRO10380M10380", year = c(2020))
  expect_true(nrow(sa3) >= 1)

  expect_true(is_valid_rhud_df(sa1))
  expect_true(is_valid_rhud_df(sa2))
  expect_true(is_valid_rhud_df(sa3))
})


test_that("hud_fmr() Different Years Queries", {
  skip_if_no_key()

  y1 <- hud_fmr("AL", year = c(2020))
  expect_true(length(y1) >= 1)
  y2 <- hud_fmr("AL", year = c(2021, 2020, 2019, 2018))
  expect_true(length(y2) >= 1)
  y3 <- hud_fmr("MD", year = c(2020, "2019", 2018))
  expect_true(length(y3) >= 1)
  y4 <- hud_fmr("AL", year = c(2020, 2020, 2020, 2020))
  expect_true(length(y4) >= 1)
  y5 <- hud_fmr("AL", year = c(2017))
  expect_true(length(y5) >= 1)
})
