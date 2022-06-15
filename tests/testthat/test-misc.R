test_that("All MCD In State Query", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  mcd <- hud_state_minor_civil_divisions("CA")
  expect_true(nrow(mcd) >= 1)

  mcd <- hud_state_minor_civil_divisions("Alabama")
  expect_true(nrow(mcd) >= 1)

  mcd <- hud_state_minor_civil_divisions("6")
  expect_true(nrow(mcd) >= 1)

  expect_warning(hud_state_minor_civil_divisions("CA", "qdqdwq"))
})

test_that("All Cities in State Query", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  cities <- hud_state_places("NY")
  expect_true(nrow(cities) >= 1)

  cities <- hud_state_places("Texas")
  expect_true(nrow(cities) >= 1)

  cities <- hud_state_places("8")
  expect_true(nrow(cities) >= 1)

  expect_warning(hud_state_places("CA", "qdqdwq"))
})

test_that("List States Query", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  states <- hud_nation_states_territories()
  expect_true(nrow(states) >= 1)
  expect_error(hud_nation_states_territories("qdqdwq"))
})

test_that("List Counties Query", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  counties <- hud_state_counties("MD")
  expect_true(nrow(counties) >= 1)

  counties <- hud_state_counties("Michigan")
  expect_true(nrow(counties) >= 1)

  counties <- hud_state_counties("Washington")
  expect_true(nrow(counties) >= 1)

  expect_warning(hud_state_counties("CA", "qdqdwq"))
})

test_that("List Small Areas Query", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  # According to HUD small areas include metropolitan areas.
  # Query for specify states
  metro <- hud_state_metropolitan("CA")
  expect_true(nrow(metro) >= 1)

  metro <- hud_state_metropolitan("1")
  expect_true(nrow(metro) >= 1)

  metro <- hud_state_metropolitan("Ohio")
  expect_true(nrow(metro) >= 1)
  expect_error(hud_state_metropolitan("qdqdwq"))
})
