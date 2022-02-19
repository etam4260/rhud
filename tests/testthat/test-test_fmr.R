
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
  c1 <- hud_fmr("5100199999", year=c(2021))
  expect_true(nrow(c1) >= 1)
  c2 <- hud_fmr("5100199999", year=c('2021'))
  expect_true(nrow(c2) >= 1)
  c3 <- hud_fmr("5151099999", year=c(2021))
  expect_true(nrow(c3) >= 1)
})

# Unimplemented for SMALL Areas
# test_that("Small Area FMR Queries", {
#   sa1 <- hud_fmr("METRO47900M47900", year=c(2018))
#   expect_true(nrow(sa1) >= 1)
#   sa2 <- hud_fmr("METRO29180N22001", year=c(2019))
#   expect_true(nrow(sa2) >= 1)
#   sa3 <- hud_fmr("METRO10380M10380", year=c(2020))
#   expect_true(nrow(sa3) >= 1)
# })


test_that("Fair Markets Rent Different Years", {
  y1 <- hud_fmr("AL", year=c(2020))
  expect_true(nrow(y1) >= 1)
  y2 <- hud_fmr("AL", year=c(2021, 2020, 2019, 2018))
  expect_true(nrow(y2) >= 1)
  y3 <- hud_fmr("MD", year=c(2020, '2019', 2018))
  expect_true(nrow(y3) >= 1)
  y4 <- hud_fmr("AL", year=c(2020, 2020, 2020, 2020))
  expect_true(nrow(y4) >= 1)
  y5 <- hud_fmr("AL", year=c(2017))
  expect_true(nrow(y5) >= 1)
})
