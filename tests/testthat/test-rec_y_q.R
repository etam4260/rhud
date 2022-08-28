test_that("CW Recent Year and Quarter", {
  res <- hud_rec_cw_yr()

  # Don't check specific year or quarter...
  expect_equal(length(res), 2)
})


test_that("FMR Recent Year and Quarter", {
  res <- hud_rec_fmr_yr()

  expect_equal(length(res), 3)
})


test_that("IL Recent Year and Quarter", {
  res <- hud_rec_il_yr()

  expect_equal(length(res), 3)
})

