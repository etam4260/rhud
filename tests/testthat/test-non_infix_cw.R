test_that("z_in_trt() Simple Queries and Errors", {
  skip_if_no_key()

  # When numerics have leading zeros, it might not work...

  # Invalid zipcode 34321 should throw warning...

  expect_warning(expect_equal(z_in_trt(zip = c(35213, 34321),
                                       tract = c("01073010801")),
                              c(TRUE, FALSE)))

  expect_equal(z_in_trt(zip = c(77032, 77396), tract = c(24033800608)),
               c(FALSE, FALSE))

  expect_equal(z_in_trt(zip = c(20774, 20772), tract = c(24033800608)),
               c(TRUE, TRUE))


  expect_error(z_in_trt(zip = c(20774, 1), tract = c(24033800608)))


  expect_error(z_in_trt(zip = c(20774, 20772), tract = c(240338008)))

})


test_that("z_in_cty() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(z_in_cty(zip = 71052, county = 22031), TRUE)

  expect_equal(z_in_cty(zip = c(71049, 71052, 71419, 71027), county = 22031),
               c(TRUE, TRUE, TRUE, TRUE))

  expect_error(z_in_cty(zip = c(20774, 1), county = c(24033800)))


  expect_error(z_in_cty(zip = c(20774, 20772), county = c(240338008)))
})


test_that("z_in_cbsa() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(z_in_cbsa(zip = 71052, cbsa = 43340), TRUE)

  expect_equal(z_in_cbsa(zip = 98569, cbsa = 10140), TRUE)

  expect_equal(z_in_cbsa(zip = c(98520, 98541, 98526, 98571, 98559, 98583),
                         cbsa = 10140),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_error(z_in_cbsa(zip = c(98520, 12), cbsa = c(10140)))

  expect_error(z_in_cbsa(zip = c(20774, 12222), cbsa = c(1014033)))
})


test_that("z_in_trt() Simple Queries and Errors", {
  skip_if_no_key()

  expect_warning(expect_equal(z_in_cbsadiv(zip = 35235, cbsadiv = 13820),
                              FALSE))
  # The zip to cbsadiv and cbsadiv to zip API might be broken for any
  # years after 2017 and 4th quarter. Should report this to HUD...

  expect_warning(expect_equal(z_in_cbsadiv(zip = 35071, cbsadiv = 13820),
                              FALSE))


  expect_error(z_in_cbsadiv(zip = c(98520, 12), cbsadiv = c(1014220)))

  expect_error(z_in_cbsadiv(zip = c(20774, 122), cbsadiv = c(1014033)))
})


test_that("z_in_ctysb() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(z_in_ctysb(zip = 44214, countysub = 3910383426), TRUE)

  expect_equal(z_in_ctysb(zip = 44256, countysub = 3910383426), TRUE)

  expect_equal(z_in_ctysb(zip = c(44273, 44256, 44254, 44217, 44215, 44251),
                          countysub = 3910383426), c(TRUE, TRUE, TRUE,
                                                     TRUE, TRUE, TRUE))

  expect_error(z_in_ctysb(zip = c(98520, 12), countysub = c(1014220)))

  expect_error(z_in_ctysb(zip = c(20774, 122), countysub = c(1014033)))
})


test_that("z_in_cd() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(z_in_cd(zip = 70072, cd = 2202), TRUE)

  expect_equal(z_in_cd(zip = 70117, cd = 2202), TRUE)

  expect_equal(z_in_cd(zip = c(70743, 70808), cd = 2202),
               c(TRUE, TRUE))

  expect_error(z_in_cd(zip = c(98520, 12), cd = c(1014220)))

  expect_error(z_in_cd(zip = c(20774, 122), cd = c(1014033)))
})


test_that("trt_in_z() Simple Queries and Errors", {
  skip_if_no_key()
  # In this case we see that numeric inputs with leading zero get truncated..
  # need to fix that.

  expect_equal(trt_in_z(tract = "01073005600", zip = 35213), TRUE)

  expect_equal(trt_in_z(tract = "01073002306", zip = 35213), TRUE)

  expect_equal(trt_in_z(tract = c("01073010805", "01073010801"), zip = 35213),
               c(TRUE, TRUE))

  expect_error(trt_in_z(tract = c(98520, 12), zip = c(1014220)))

  expect_error(trt_in_z(tract = c(20774, 122), zip = c(1014033)))
})


test_that("cty_in_z() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(cty_in_z(county = 24027, zip = 21043), TRUE)

  expect_equal(cty_in_z(county = 24005, zip = 21043), TRUE)

  expect_equal(cty_in_z(county = c(24027, 24005), zip = 21043),
               c(TRUE, TRUE))

  expect_error(cty_in_z(county = c(98520, 12), zip = c(1014220)))

  expect_error(cty_in_z(county = c(20774, 122), zip = c(1014033)))
})


test_that("cbsa_in_z() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(cbsa_in_z(cbsa = 32300, zip = 24054), TRUE)

  expect_equal(cbsa_in_z(cbsa = 19260, zip = 24054), TRUE)

  expect_equal(cbsa_in_z(cbsa = c(32300, 19260), zip = 24054),
               c(TRUE, TRUE))

  expect_error(cbsa_in_z(cbsa = c(98520, 12), zip = c(1014220)))

  expect_error(cbsa_in_z(cbsa = c(20774, 122), zip = c(1014033)))
})


test_that("cbsadiv_in_z() Simple Queries and Errors", {

  skip_if_no_key()

  expect_warning(expect_equal(cbsadiv_in_z(cbsadiv = 32300, zip = 24054),
                              FALSE))

  expect_warning(expect_equal(cbsadiv_in_z(cbsadiv = 19260, zip = 24054),
                                           FALSE))

  expect_warning(expect_warning(cbsadiv_in_z(cbsadiv = c(32300, 19260),
                                             zip = 24054)))

  expect_error(cbsadiv_in_z(cbsadiv = c(98520, 12), zip = c(1014220)))

  expect_error(cbsadiv_in_z(cbsadiv = c(20774, 122), zip = c(1014033)))
})


test_that("cd_in_z() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(cd_in_z(cd = 5109, zip = 24059), TRUE)

  expect_equal(cd_in_z(cd = 5105, zip = 24059), TRUE)

  expect_warning(expect_equal(cd_in_z(cd = c(5109, 5105, 5106, 4332),
                                      zip = 24059),
                              c(TRUE, TRUE, TRUE, FALSE)))

  expect_error(cd_in_z(cd = c(98520, 12), zip = c(1014220)))

  expect_error(cd_in_z(cd = c(20774, 122), zip = c(1014033)))
})


test_that("ctysb_in_z() Simple Queries and Errors", {
  skip_if_no_key()

  expect_equal(ctysb_in_z(countysub = 3910371488, zip = 44273), TRUE)

  expect_equal(ctysb_in_z(countysub = 3916950666, zip = 44273), TRUE)

  expect_equal(ctysb_in_z(countysub = c(3910383426, 3910330660), zip = 44273),
               c(TRUE, TRUE))

  expect_error(ctysb_in_z(countysub = c(98520, 12), zip = c(1014220)))

  expect_error(ctysb_in_z(countysub = c(20774, 122), zip = c(1014033)))
})
