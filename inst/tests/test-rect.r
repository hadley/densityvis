context("Rect binning")

test_that("counts correct for simple example", {
  x <- c(1, 1, 1, 2, 2)
  y <- c(1, 1, 1, 2, 2)
  
  breaks <- c(0.5, 1.5, 2.5)
  bins <- rect_bin(x, y, xbreaks = breaks, ybreaks = c(0.5, 1.5, 2.5))
  
  expect_equal(bins$count, c(3, 0, 0, 2))
})