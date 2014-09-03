test_that("NA convert", {  
  expect_error(mnsl2hvc(c(NA)), "zero")
  expect_equal(hvc2mnsl(mnsl2hvc(c(NA, "10RP 2/2"))), c(NA, "10RP 2/2"))
})