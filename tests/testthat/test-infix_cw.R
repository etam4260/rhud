test_that("test %z_in_trt%", {
  # When numerics have leading zeros, it might not work...
  skip_if(Sys.getenv("HUD_KEY") == "")

  # Invalid zipcode 34321 should throw warning....
  expect_warning(expect_equal(c(35213, 34321) %z_in_trt% c("01073010801"),
                              c(TRUE, FALSE)))

  expect_equal(c(77032, 77396) %z_in_trt% c(24033800608), c(FALSE, FALSE))

  expect_equal(c(20774, 20772) %z_in_trt% c(24033800608), c(TRUE, TRUE))


  expect_error(c(20774, 1) %z_in_trt% c(24033800608))


  expect_error(c(20774, 20772) %z_in_trt% c(240338008))
})


test_that("test %z_in_cty%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(71052 %z_in_cty% 22031, TRUE)

  expect_equal(c(71049, 71052, 71419, 71027) %z_in_cty% 22031, c(TRUE,
                                                                 TRUE,
                                                                 TRUE, TRUE))

  expect_error(c(20774, 1) %z_in_cty% c(24033800))


  expect_error(c(20774, 20772) %z_in_cty% c(240338008))
})


test_that("test %z_in_cbsa%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(71052 %z_in_cbsa% 43340, TRUE)

  expect_equal(98569 %z_in_cbsa% 10140, TRUE)

  expect_equal(c(98520, 98541, 98526, 98571, 98559, 98583) %z_in_cbsa% 10140,
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_error(c(98520, 12) %z_in_cbsa% c(10140))

  expect_error(c(20774, 12222) %z_in_cbsa% c(1014033))
})


test_that("test %z_in_cbsadiv%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_warning(expect_equal(35235 %z_in_cbsadiv% 13820, FALSE))
  # The zip to cbsadiv and cbsadiv to zip API might be broken for any
  # years after 2017 and 4th quarter. Should report this to HUD...

  expect_warning(expect_equal(35071 %z_in_cbsadiv% 13820, FALSE))


  expect_error(c(98520, 12) %z_in_cbsadiv% c(1014220))

  expect_error(c(20774, 122) %z_in_cbsadiv% c(1014033))
})



test_that("test %z_in_ctysb%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(44214 %z_in_ctysb% 3910383426, TRUE)

  expect_equal(44256 %z_in_ctysb% 3910383426, TRUE)

  expect_equal(c(44273, 44256, 44254, 44217, 44215, 44251) %z_in_ctysb%
                 3910383426,
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))

  expect_error(c(98520, 12) %z_in_ctysb% c(1014220))

  expect_error(c(20774, 122) %z_in_ctysb% c(1014033))
})

test_that("test %z_in_cd%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(70072 %z_in_cd% 2202, TRUE)

  expect_equal(70117 %z_in_cd% 2202, TRUE)

  expect_equal(c(70743, 70808) %z_in_cd% 2202,
               c(TRUE, TRUE))

  expect_error(c(98520, 12) %z_in_cd% c(1014220))

  expect_error(c(20774, 122) %z_in_cd% c(1014033))
})

test_that("test %trt_in_z%", {
  skip_if(Sys.getenv("HUD_KEY") == "")
  # In this case we see that numeric inputs with leading zero get truncated..
  # need to fix that.

  expect_equal("01073005600" %trt_in_z% 35213, TRUE)

  expect_equal("01073002306" %trt_in_z% 35213, TRUE)

  expect_equal(c("01073010805", "01073010801") %trt_in_z% 35213,
               c(TRUE, TRUE))

  expect_error(c(98520, 12) %trt_in_z% c(1014220))

  expect_error(c(20774, 122) %trt_in_z% c(1014033))
})

test_that("test %cty_in_z%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(24027 %cty_in_z% 21043, TRUE)

  expect_equal(24005 %cty_in_z% 21043, TRUE)

  expect_equal(c(24027, 24005) %cty_in_z% 21043,
               c(TRUE, TRUE))

  expect_error(c(98520, 12) %cty_in_z% c(1014220))

  expect_error(c(20774, 122) %cty_in_z% c(1014033))
})

test_that("test %cbsa_in_z%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(32300 %cbsa_in_z% 24054, TRUE)

  expect_equal(19260 %cbsa_in_z% 24054, TRUE)

  expect_equal(c(32300, 19260) %cbsa_in_z% 24054,
               c(TRUE, TRUE))

  expect_error(c(98520, 12) %cbsa_in_z% c(1014220))

  expect_error(c(20774, 122) %cbsa_in_z% c(1014033))
})

test_that("test %cbsadiv_in_z%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_warning(expect_equal(32300 %cbsadiv_in_z% 24054, FALSE))

  expect_warning(expect_equal(19260 %cbsadiv_in_z% 24054, FALSE))

  expect_warning(expect_warning(c(32300, 19260) %cbsadiv_in_z% 24054))

  expect_error(c(98520, 12) %cbsadiv_in_z% c(1014220))

  expect_error(c(20774, 122) %cbsadiv_in_z% c(1014033))
})

test_that("test %cd_in_z%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(5109 %cd_in_z% 24059, TRUE)

  expect_equal(5105 %cd_in_z% 24059, TRUE)

  expect_warning(expect_equal(c(5109, 5105, 5106, 4332) %cd_in_z% 24059,
               c(TRUE, TRUE, TRUE, FALSE)))

  expect_error(c(98520, 12) %cd_in_z% c(1014220))

  expect_error(c(20774, 122) %cd_in_z% c(1014033))
})

test_that("test %ctysb_in_z%", {
  skip_if(Sys.getenv("HUD_KEY") == "")

  expect_equal(3910371488 %ctysb_in_z% 44273, TRUE)

  expect_equal(3916950666 %ctysb_in_z% 44273, TRUE)

  expect_equal(c(3910383426, 3910330660) %ctysb_in_z% 44273,
               c(TRUE, TRUE))

  expect_error(c(98520, 12) %ctysb_in_z% c(1014220))

  expect_error(c(20774, 122) %ctysb_in_z% c(1014033))
})
