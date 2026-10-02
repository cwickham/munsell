test_that("label.size sets the text size in plots", {
  skip_if_not_installed("ggplot2")
  text_size <- function(p) {
    layer <- Filter(function(l) inherits(l$geom, "GeomText"), p$layers)[[1]]
    layer$aes_params$size
  }
  # default unchanged
  expect_equal(text_size(hue_slice("5R")), 2)
  # argument passes through in each function
  expect_equal(text_size(hue_slice("5R", label.size = 4)), 4)
  expect_equal(text_size(chroma_slice(2, label.size = 4)), 4)
  expect_equal(text_size(complement_slice("5R", label.size = 4)), 4)
  expect_equal(text_size(plot_closest(0.1, 0.1, 0.3, label.size = 4)), 4)
})