library(httptest)

# Sample test using httptest for making a mock web request and
# for saving expected behavior.

with_mock_dir("good_key", {
  test_that("HUD Good", {

    #va <- hud_fmr("VA", year = c(2021))
    #expect_true(nrow(va) > 1)
  })
})
