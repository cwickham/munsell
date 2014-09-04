test_that("NAs handled", {
  expect_equal(check_mnsl(NA), as.character(NA))
  expect_equal(in_gamut(NA), as.logical(NA)) #wtf
  expect_equal(check_mnsl(c(NA, "10RP 2/2")), c(NA, "10RP 2/2"))
})




