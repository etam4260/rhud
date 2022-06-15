test_that("Query hud_cw_zip_tract()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_zip_tract(zip = "35213", year = c("2010"), quarter = c("1"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_zip_tract(zip = "3521334",
                                year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_zip_tract(zip = c("3521334", "32133"),
                                year = c("2010"), quarter = c("1")))

  res <- hud_cw_zip_tract(zip = "35213", year = c("2010"),
                          quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_zip_county()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_zip_county(zip = 35213, year = c("2020"), quarter = c("2"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_zip_county(zip = "3521334",
                                 year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_zip_county(zip = c("3521334", "32133"),
                                 year = c("2010"), quarter = c("1")))

  res <- hud_cw_zip_county(zip = "35213", year = c("2010"),
                           quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_zip_cbsa()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_zip_cbsa(zip = 35213, year = c("2020"), quarter = c("2"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_zip_cbsa(zip = "3521334",
                               year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_zip_cbsa(zip = c("3521334", "32133"),
                               year = c("2010"), quarter = c("1")))

  res <- hud_cw_zip_cbsa(zip = "35213", year = c("2010"),
                         quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_zip_cbsadiv()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_zip_cbsadiv(zip = 22031, year = c("2020"), quarter = c("2"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_zip_cbsadiv(zip = "3521334",
                                  year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_zip_cbsadiv(zip = c("3521334", "32133"),
                                  year = c("2010"), quarter = c("1")))

  res <- hud_cw_zip_cbsadiv(zip = "22031", year = c("2020"),
                            quarter = c("2"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_zip_cd()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_zip_cd(zip = 35213, year = c("2020"), quarter = c("2"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_zip_cd(zip = "3521334", year = c("2010"),
                             quarter = c("1")))
  expect_error(hud_cw_zip_cd(zip = c("3521334", "32133"),
                             year = c("2010"), quarter = c("1")))

  res <- hud_cw_zip_cd(zip = "35213", year = c("2010"),
                       quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})


test_that("Query hud_cw_tract_zip()", {

  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_tract_zip(tract = 48201223100, year = c("2017"),
                          quarter = c("1"))

  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_tract_zip(tract = "4820122310033",
                                year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_tract_zip(tract = c("35", "3213333"),
                                year = c("2010"), quarter = c("1")))

  res <- hud_cw_tract_zip(tract = "48201223100", year = c("2010"),
                          quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})


test_that("Query hud_cw_county_zip()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_county_zip(county = "22031", year = c("2010"), quarter = c("1"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_county_zip(county = "3521334",
                                 year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_county_zip(county = c("3521334", "32133"),
                                 year = c("2010"), quarter = c("1")))

  res <- hud_cw_county_zip(county = "22031", year = c("2010"),
                           quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)

  all_md_counties <- hud_state_counties("md")
  all_md <- hud_cw_county_zip(county = substr(all_md_counties$fips_code, 1, 5),
                              year = c("2010"), quarter = c("1"))
  expect_true(check_is_not_list(all_md))
})


test_that("Query hud_cw_cbsa_zip()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_cbsa_zip(cbsa = "10140", year = c("2017"), quarter = c("2"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_cbsa_zip(cbsa = "3521334",
                               year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_cbsa_zip(cbsa = c("3521334", "32133"),
                               year = c("2010"), quarter = c("1")))

  res <- hud_cw_cbsa_zip(cbsa = "10140", year = c("2010"),
                         quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_cbsadiv_zip()", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_cbsadiv_zip(cbsadiv = 10380, year = c("2017"), quarter = c("4"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_cbsadiv_zip(cbsadiv = "3521334",
                                  year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_cbsadiv_zip(cbsadiv = c("3521334", "32133"),
                                  year = c("2010"), quarter = c("1")))

  res <- hud_cw_cbsadiv_zip(cbsadiv = "10380", year = c("2017"),
                            quarter = c("4"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_cd_zip", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_cd_zip(cd = "2202", year = c("2010"), quarter = c("4"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_cd_zip(cd = "3521334", year = c("2010"),
                             quarter = c("1")))
  expect_error(hud_cw_cd_zip(cd = c("3521334", "32133"),
                             year = c("2010"), quarter = c("1")))

  res <- hud_cw_cd_zip(cd = "2202", year = c("2010"),
                       quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_zip_countysub", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_zip_countysub(zip = "35213", year = c("2019"), quarter = c("2"))
  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_zip_countysub(zip = "3521334",
                                    year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_zip_countysub(zip = c("3521334", "32133"),
                                    year = c("2010"), quarter = c("1")))

  res <- hud_cw_zip_countysub(zip = "35213", year = c("2019"),
                              quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})

test_that("Query hud_cw_countysub_zip", {

  skip_if(Sys.getenv("HUD_KEY") == "")
  res <- hud_cw_countysub_zip(countysub = "4606720300 ",
                              year = c("2019", "2019", "2019"),
                              quarter = c("4", "4"))

  expect_true(nrow(res) >= 1)

  expect_error(hud_cw_countysub_zip(countysub = "3521334",
                                    year = c("2010"), quarter = c("1")))
  expect_error(hud_cw_countysub_zip(countysub = c("3521334", "32133"),
                                    year = c("2010"), quarter = c("1")))

  res <- hud_cw_countysub_zip(countysub = "4606720300 ", year = c("2019"),
                              quarter = c("1"), minimal = TRUE)
  expect_true(is.vector(res) && length(res) >= 1)
})
