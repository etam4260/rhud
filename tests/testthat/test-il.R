test_that("hud_il() Income Limits Simple State Queries", {
  skip_if_no_key()

  va <- hud_il("VA", year = c(2021))
  expect_true(nrow(va) == 1)
  md <- hud_il("MD", year = c("2021"))
  expect_true(nrow(md) == 1)
  ca <- hud_il("CA", year = c(2021))
  expect_true(nrow(ca) == 1)
  al <- hud_il("AL", year = c(2021))
  expect_true(nrow(al) == 1)

  # Check that columns returned are of type vectors
  expect_true(is_valid_rhud_df(va))
  expect_true(is_valid_rhud_df(md))
  expect_true(is_valid_rhud_df(ca))
  expect_true(is_valid_rhud_df(al))

  # Garbage collection
  va <- NULL
  md <- NULL
  ca <- NULL
  al <- NULL

  # Make multiple state queries
  mult_state <- hud_il(c("AL", "MD", "CA"), c(2018, 2017, 2019))
  expect_true(nrow(mult_state) == 9)
  expect_true(is_valid_rhud_df(mult_state))

  # Make multiple year queries
  mult_year <- hud_il(c("OH"), c(2020, 2019, 2018))
  expect_true(nrow(mult_year) == 3)
  expect_true(is_valid_rhud_df(mult_year))

  # Although HUD dataset classifies PR as puerto rico and a state:
  # it is not a state and the API doesn"t allow you to get data for this.
  # Same for DC. Should expect an error if trying to grab this data.
  expect_warning(hud_il("PR"))
  expect_warning(hud_il("DC"))

  # Try to query for all states... filter out US territories...
  hudstates <- hud_nation_states_territories()
  hudstates <- hudstates[as.numeric(hudstates$state_num) < 57, ]
  hudstates <- hudstates[hudstates$state_code != "DC", ]

  all_states_full <- hud_il(hudstates$state_name)
  expect_true(nrow(all_states_full) >= 1)
  expect_true(is_valid_rhud_df(all_states_full))

  # Try to query for all state codes?
  all_state_abbr <- hud_il(hudstates$state_code)
  expect_true(nrow(all_state_abbr) >= 1)
  expect_true(is_valid_rhud_df(all_state_abbr))

  # Try to query for all state name?
  all_state_num <- hud_il(hudstates$state_num)
  expect_true(nrow(all_state_num) >= 1)
  expect_true(is_valid_rhud_df(all_state_num))

  # Check if they are identical according to R...
  expect_true(identical(all_states_full, all_state_abbr) &&
              identical(all_states_full, all_state_num))
})

test_that("hud_il() Income Limits Simple County Queries", {
  skip_if_no_key()

  c1 <- hud_il("5100199999", year = c(2021))
  expect_true(nrow(c1) == 1)
  expect_true(is_valid_rhud_df(c1))
  c1 <- NULL

  c2 <- hud_il("5100199999", year = c("2020"))
  expect_true(nrow(c2) == 1)
  expect_true(is_valid_rhud_df(c2))
  c2 <- NULL

  c3 <- hud_il("5151099999", year = c(2019))
  expect_true(nrow(c3) == 1)
  expect_true(is_valid_rhud_df(c3))
  c3 <- NULL

  # Get all income limits for the state of Maryland, at county level
  all_md_counties <- hud_state_counties("MD")
  all_md <- hud_il(all_md_counties$fips_code)
  expect_true(nrow(all_md) == length(all_md_counties$fips_code))
  expect_true(is_valid_rhud_df(all_md))
})

test_that("hud_il() Income Limits Simple Metroarea Queries", {
  skip_if_no_key()

  sa1 <- hud_il("METRO47900M47900", year = c(2018))
  expect_true(nrow(sa1) == 1)
  expect_true(is_valid_rhud_df(sa1))
  sa1 <- NULL

  sa2 <- hud_il("METRO29180N22001", year = c(2019))
  expect_true(nrow(sa2) == 1)
  expect_true(is_valid_rhud_df(sa2))
  sa2 <- NULL

  sa3 <- hud_il("METRO10380M10380", year = c(2020))
  expect_true(nrow(sa3) == 1)
  expect_true(is_valid_rhud_df(sa3))
  sa3 <- NULL

  # Get all metropolitan areas in the state of Maryland and query
  # their income limits.
  all_md_metro <- hud_state_metropolitan("MD")
  all_md <- hud_il(all_md_metro$cbsa_code)

  expect_true(nrow(all_md) == length(all_md_metro$cbsa_code))
  expect_true(is_valid_rhud_df(all_md))
})

test_that("hud_il() Income Limits States Different Year Queries", {
  skip_if_no_key()

  y1 <- hud_il("VA", year = c(2021))
  expect_true(nrow(y1) == 1)
  expect_true(is_valid_rhud_df(y1))


  y2 <- hud_il("WY", year = c(2021, "2019"))
  expect_true(nrow(y2) == 2)
  expect_true(is_valid_rhud_df(y2))


  y3 <- hud_il("MD", year = c("2021", "2018"))
  expect_true(nrow(y3) == 2)
  expect_true(is_valid_rhud_df(y3))


  y4 <- hud_il("CA", year = c(2018, 2017))
  expect_true(nrow(y4) == 2)
  expect_true(is_valid_rhud_df(y4))


  y5 <- hud_il("NY", year = c("2021", "2019", "2021"))
  expect_true(nrow(y5) == 2)
  expect_true(is_valid_rhud_df(y5))

})
