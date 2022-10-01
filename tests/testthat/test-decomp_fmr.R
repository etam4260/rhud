test_that("hud_fmr_state_metroareas() Simple Examples", {
  skip_if_no_key()

  va <- hud_fmr_state_metroareas("VA", year = c(2021))
  expect_true(nrow(va) == 19)
  md <- hud_fmr_state_metroareas("MD", year = c("2021"))
  expect_true(nrow(md) == 9)
  ca <- hud_fmr_state_metroareas("CA", year = c(2021))
  expect_true(nrow(ca) == 30)
  al <- hud_fmr_state_metroareas("AL", year = c(2021))
  expect_true(nrow(al) == 17)
})


test_that("hud_fmr_state_counties() Simple Examples", {
  skip_if_no_key()

  va <- hud_fmr_state_counties("VA", year = c(2021))
  expect_true(nrow(va) == 133)
  md <- hud_fmr_state_counties("MD", year = c("2021"))
  expect_true(nrow(md) == 24)
  ca <- hud_fmr_state_counties("CA", year = c(2021))
  expect_true(nrow(ca) == 58)
  al <- hud_fmr_state_counties("AL", year = c(2021))
  expect_true(nrow(al) == 67)
})


test_that("hud_fmr_county_zip() Simple Examples", {
  skip_if_no_key()

  c1 <- hud_fmr_county_zip("5100199999", year = c(2021))
  expect_true(nrow(c1) == 1)
  c2 <- hud_fmr_county_zip("5100199999", year = c("2021"))
  expect_true(nrow(c2) == 1)
  c3 <- hud_fmr_county_zip("5151099999", year = c(2021))
  expect_true(nrow(c3) == 506)
})


test_that("hud_fmr_metroarea_zip() Simple Examples", {
  skip_if_no_key()

  sa1 <- hud_fmr_metroarea_zip("METRO47900M47900", year = c(2018))
  expect_true(nrow(sa1) == 443)
  sa2 <- hud_fmr_metroarea_zip("METRO29180N22001", year = c(2019))
  expect_true(nrow(sa2) == 1)
  sa3 <- hud_fmr_metroarea_zip("METRO10380M10380", year = c(2020))
  expect_true(nrow(sa3) == 1)
})
