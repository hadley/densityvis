context("Interval binning")

test_that("counts agree with hist", {
  x <- runif(100)
  x_hist <- hist(x, plot = F)
  x_bin <-  interval_bin(x, breaks = x_hist$breaks)  
  expect_equal(x_hist$counts, x_hist$count)

  ab_hist <- hist(baseball$ab, plot = F)
  ab_bin <- interval_bin(baseball$ab, breaks = ab_hist$breaks)
  expect_equal(ab_hist$counts, ab_hist$count)
})

test_that("integers binned correctly", {
  x <- rep(1:10, 10:1)

  bins_r <- interval_bin(x, breaks = 0:11, open = "right")
  expect_equal(bins_r$count, c(0, 10:1))
  
  bins_l <- interval_bin(x, breaks = 0:11, open = "left")
  expect_equal(bins_l$count, c(10:1, 0))
  
})