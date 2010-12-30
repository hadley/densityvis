context("Intervals")

test_that("Counts agree with hist", {
  x <- runif(100)
  x_hist <- hist(x, plot = F)
  x_bin <-  interval_bin(x, breaks = x_hist$breaks)  
  expect_equal(x_hist$counts, x_hist$count)

  ab_hist <- hist(baseball$ab, plot = F)
  ab_bin <- interval_bin(baseball$ab, breaks = xhist$breaks)
  expect_equal(ab_hist$counts, ab_hist$count)

  
})