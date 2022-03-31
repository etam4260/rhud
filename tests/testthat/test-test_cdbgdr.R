test_that("Main CDBG-DR data files from DRGR Public Portal", {
  file <- hud_cdbg(1)
  expect_true(nrow(file) >= 1)

  file <- hud_cdbg(2)
  expect_true(nrow(file) >= 1)

  file <- hud_cdbg(3)
  expect_true(nrow(file) >= 1)

  file <- hud_cdbg(4)
  expect_true(nrow(file) >= 1)
})
