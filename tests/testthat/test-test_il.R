test_that("Income Limits State Queries", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  VA <- hud_il("VA", year=c(2021))
  expect_true(nrow(VA) == 1)
  MD <- hud_il("MD", year=c('2021'))
  expect_true(nrow(MD) == 1)
  CA <- hud_il("CA", year=c(2021))
  expect_true(nrow(CA) == 1)
  AL <- hud_il("AL", year=c(2021))
  expect_true(nrow(AL) == 1)

  # Check that columns returned are of type vectors
  expect_true(check_is_not_list(VA))
  expect_true(check_is_not_list(MD))
  expect_true(check_is_not_list(CA))
  expect_true(check_is_not_list(AL))

  # Garbage collection
  VA <- NULL
  MD <- NULL
  CA <- NULL
  AL <- NULL

  # Make multiple state queries
  mult_state <- hud_il(c("AL","MD","CA"), c(2018, 2017, 2019))
  expect_true(nrow(mult_state) == 9)
  expect_true(check_is_not_list(mult_state))

  # Make multiple year queries
  mult_year <- hud_il(c("OH"), c(2020, 2019, 2018))
  expect_true(nrow(mult_year) == 3)
  expect_true(check_is_not_list(mult_year))

  # Although HUD dataset classifies PR as puerto rico and a state:
  # it is not a state and the API doesn't allow you to get data for this.
  # Same for DC. Should expect an error if trying to grab this data.
  expect_error(hud_il("PR"))
  expect_error(hud_il("DC"))

  # Try to query for all states... filter out US territories...
  hudstates <- hud_states()
  hudstates <- hudstates[as.numeric(hudstates$state_num) < 57, ]
  hudstates <- hudstates[hudstates$state_code != 'DC', ]

  all_states_full <- hud_il(hudstates$state_name)
  expect_true(nrow(all_states_full) >= 1)
  expect_true(check_is_not_list(all_states_full))

  # Try to query for all state codes?
  all_state_abbr <- hud_il(hudstates$state_code)
  expect_true(nrow(all_state_abbr) >= 1)
  expect_true(check_is_not_list(all_state_abbr))

  # Try to query for all state name?
  all_state_num <- hud_il(hudstates$state_num)
  expect_true(nrow(all_state_num) >= 1)
  expect_true(check_is_not_list(all_state_num))

  # Check if they are identical according to R...
  expect_true(identical(all_states_full, all_state_abbr) &&
              identical(all_states_full, all_state_num))
})

test_that("Income Limits Rent County Queries", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  c1 <- hud_il("5100199999", year=c(2021))
  expect_true(nrow(c1) == 1)
  expect_true(check_is_not_list(c1))
  c1 <- NULL

  c2 <- hud_il("5100199999", year=c('2021'))
  expect_true(nrow(c2) == 1)
  expect_true(check_is_not_list(c2))
  c2 <- NULL

  c3 <- hud_il("5151099999", year=c(2021))
  expect_true(nrow(c3) == 1)
  expect_true(check_is_not_list(c3))
  c3 <- NULL

  # Get all income limits for the state of Maryland, at county level
  all_md_counties <- hud_counties("MD")
  all_md <- hud_il(all_md_counties$fips_code)
  expect_true(nrow(all_md) == length(all_md_counties$fips_code))
  expect_true(check_is_not_list(all_md))
})

test_that("Small Area IL Queries", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  sa1 <- hud_il("METRO47900M47900", year=c(2018))
  expect_true(nrow(sa1) == 1)
  expect_true(check_is_not_list(sa1))
  sa1 <- NULL

  sa2 <- hud_il("METRO29180N22001", year=c(2019))
  expect_true(nrow(sa2) == 1)
  expect_true(check_is_not_list(sa2))
  sa2 <- NULL

  sa3 <- hud_il("METRO10380M10380", year=c(2020))
  expect_true(nrow(sa3) == 1)
  expect_true(check_is_not_list(sa3))
  sa3 <- NULL

  # Get all metropolitan areas in the state of Maryland and query
  # their income limits.
  all_md_metro <- hud_metropolitan("MD")
  all_md <- hud_il(all_md_metro$cbsa_code)

  expect_true(nrow(all_md) == length(all_md_metro$cbsa_code))
  expect_true(check_is_not_list(all_md))
})

test_that("Income Limits Different Years", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  y1 <- hud_il("VA", year=c(2021))
  expect_true(nrow(y1) == 1)
  expect_true(check_is_not_list(y1))


  y2 <- hud_il("WY", year=c(2021, '2019'))
  expect_true(nrow(y2) == 2)
  expect_true(check_is_not_list(y2))


  y3 <- hud_il("MD", year=c('2021', '2018'))
  expect_true(nrow(y3) == 2)
  expect_true(check_is_not_list(y3))


  y4 <- hud_il("CA", year=c(2018, 2017))
  expect_true(nrow(y4) == 2)
  expect_true(check_is_not_list(y4))


  y5 <- hud_il("NY", year=c('2021', '2019', '2021'))
  expect_true(nrow(y5) == 2)
  expect_true(check_is_not_list(y5))

})
