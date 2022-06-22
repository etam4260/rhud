test_that("test hud_fmr_state_metroareas()", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  va <- hud_fmr_state_metroareas("VA", year = c(2021))
  expect_true(nrow(va) >= 1)
  md <- hud_fmr_state_metroareas("MD", year = c("2021"))
  expect_true(nrow(md) >= 1)
  ca <- hud_fmr_state_metroareas("CA", year = c(2021))
  expect_true(nrow(ca) >= 1)
  al <- hud_fmr_state_metroareas("AL", year = c(2021))
  expect_true(nrow(al) >= 1)
})


test_that("test hud_fmr_state_counties()", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  va <- hud_fmr_state_counties("VA", year = c(2021))
  expect_true(nrow(va) >= 1)
  md <- hud_fmr_state_counties("MD", year = c("2021"))
  expect_true(nrow(md) >= 1)
  ca <- hud_fmr_state_counties("CA", year = c(2021))
  expect_true(nrow(ca) >= 1)
  al <- hud_fmr_state_counties("AL", year = c(2021))
  expect_true(nrow(al) >= 1)
})


test_that("test hud_fmr_county_zip()", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  c1 <- hud_fmr_county_zip("5100199999", year = c(2021))
  expect_true(nrow(c1) >= 1)
  c2 <- hud_fmr_county_zip("5100199999", year = c("2021"))
  expect_true(nrow(c2) >= 1)
  c3 <- hud_fmr_county_zip("5151099999", year = c(2021))
  expect_true(nrow(c3) >= 1)
})


test_that("test hud_fmr_metroarea_zip()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  sa1 <- hud_fmr_metroarea_zip("METRO47900M47900", year = c(2018))
  expect_true(nrow(sa1) >= 1)
  sa2 <- hud_fmr_metroarea_zip("METRO29180N22001", year = c(2019))
  expect_true(nrow(sa2) >= 1)
  sa3 <- hud_fmr_metroarea_zip("METRO10380M10380", year = c(2020))
  expect_true(nrow(sa3) >= 1)
})
