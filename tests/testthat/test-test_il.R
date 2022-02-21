
test_that("Income Limits State Queries", {
  VA <- hud_il("VA", year=c(2021))
  expect_true(nrow(VA) >= 1)
  MD <- hud_il("MD", year=c('2021'))
  expect_true(nrow(MD) >= 1)
  CA <- hud_il("CA", year=c(2021))
  expect_true(nrow(CA) >= 1)
  AL <- hud_il("AL", year=c(2021))
  expect_true(nrow(AL) >= 1)
})

test_that("Income Limits Rent County Queries", {
  c1 <- hud_il("5100199999", year=c(2021))
  expect_true(nrow(c1) >= 1)
  c2 <- hud_il("5100199999", year=c('2021'))
  expect_true(nrow(c2) >= 1)
  c3 <- hud_il("5151099999", year=c(2021))
  expect_true(nrow(c3) >= 1)
})


test_that("Small Area IL Queries", {
  sa1 <- hud_il("METRO47900M47900", year=c(2018))
  expect_true(nrow(sa1) >= 1)
  sa2 <- hud_il("METRO29180N22001", year=c(2019))
  expect_true(nrow(sa2) >= 1)
  sa3 <- hud_il("METRO10380M10380", year=c(2020))
  expect_true(nrow(sa3) >= 1)
})

test_that("Income Limits Different Years", {
  y1 <- hud_il("VA", year=c(2021))
  expect_true(nrow(y1) >= 1)
  y2 <- hud_il("WY", year=c(2021, '2019'))
  expect_true(nrow(y2) >= 1)
  y3 <- hud_il("MD", year=c('2021', '2018'))
  expect_true(nrow(y3) >= 1)
  y4 <- hud_il("CA", year=c(2018, 2017))
  expect_true(nrow(y4) >= 1)
  y5 <- hud_il("NY", year=c('2021', '2019', '2021'))
  expect_true(nrow(y5) >= 1)
})
