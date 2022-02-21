test_that("All CHAS Types", {
  # Nation
  n <- hud_chas(1)
  expect_true(nrow(n) >= 1)
  # State
  s <- hud_chas('2', stateId = '56')
  expect_true(nrow(s) >= 1)
  # County
  c <- hud_chas('3','51','199')
  expect_true(nrow(c) >= 1)
  # MCD
  mcd <- hud_chas('4', '51', 94087)
  expect_true(nrow(mcd) >= 1)
  # City(Place)
  city <- hud_chas('5', '51', 48996)
  expect_true(nrow(city) >= 1)
})

test_that("Different Years CHAS", {
  # Only checking entire Nation CHAS and varying different year inputs.

  # 2014-2018
  # 2013-2017
  # 2012-2016
  # 2011-2015
  # 2010-2014
  # 2009-2013
  # 2008-2012
  # 2007-2011
  # 2006-2010

  y1 <- hud_chas(1, year = c("2014-2018", "2013-2017"))
  expect_true(nrow(y1) >= 1)
  y2 <- hud_chas(1, year = c("2013-2017"))
  expect_true(nrow(y2) >= 1)
  y3 <- hud_chas(1, year = c("2008-2012", "2007-2011", "2014-2018"))
  expect_true(nrow(y3) >= 1)
  y4 <- hud_chas(1, year = c("2012-2016", "2007-2011"))
  expect_true(nrow(y4) >= 1)
  y5 <- hud_chas(1, year = c("2014-2018", "2013-2017"))
  expect_true(nrow(y5) >= 1)
})


