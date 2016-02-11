context("Testing missing value conversion")

test_that("NAs handled in convert", {  
  expect_error(mnsl2hvc(c(NA)), "zero")
  expect_equal(hvc2mnsl(mnsl2hvc(c(NA, "10RP 2/2"))), c(NA, "10RP 2/2"))
})

test_that("NAs handled in checks", {
  expect_equal(check_mnsl(NA), as.character(NA))
  expect_equal(in_gamut(NA), as.logical(NA)) #wtf
  expect_equal(check_mnsl(c(NA, "10RP 2/2")), c(NA, "10RP 2/2"))
})
