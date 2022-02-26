test_that("All MCD In State Query", {
  mcd <- hud_minor_civil_divisions('CA')
  expect_true(nrow(mcd) >= 1)
})

test_that("All Cities in State Query", {
  cities <- hud_places("NY")
  expect_true(nrow(cities) >= 1)
})

test_that("List States Query", {
  states <- hud_states()
  expect_true(nrow(states) >= 1)
})

test_that("List Counties Query", {
  counties <- hud_counties('MD')
  expect_true(nrow(counties) >= 1)
})

test_that("List Small Areas Query", {
  # According to HUD small areas are metropolitan areas.
  metro <- hud_metropolitan()
  expect_true(nrow(metro) >= 1)
})
