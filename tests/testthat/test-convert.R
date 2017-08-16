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

context("Checking fix = TRUE cases")

test_that("Out of gamut handling", {  
  expect_warning(check_mnsl("5Y 8/14"))
  expect_equal(check_mnsl("5Y 8/14", fix = TRUE), "5Y 8/10")
})

test_that("Passing of fix = TRUE works", {
  HVC <- data.frame(hue = "5Y", value = 8, chroma = 10, stringsAsFactors = FALSE)
  ANS <- c("5PB 1/4",  "5PB 2/6",  "5PB 3/8",  "5PB 4/10", "5PB 5/10",
    "5PB 6/10", "5PB 7/10", "5PB 8/6",  "5PB 9/2")
  # There is some inconsistency in what gives a warning vs error.
  expect_error(mnsl2hvc("5Y 8/14"))
  expect_equal(mnsl2hvc("5Y 8/14", fix = TRUE), HVC)
  expect_warning(hvc2mnsl("5PB", 1:9, 10))
  expect_equal(hvc2mnsl("5PB", 1:9, 10, fix = TRUE), ANS)
  expect_warning(mnsl2hex("5Y 8/14"))
  expect_equal(mnsl2hex("5Y 8/14", fix = TRUE), "#E9C938")
})
