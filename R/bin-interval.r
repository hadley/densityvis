#' Bin data into intervals (1d).
#'
#' There is no argument to \code{interval_bin} that specifies whether the
#' intervals are right-open or left-open, because the breaks should specify
#' this using small floating point adjustments.
#'
#' @export
#' @param x a numeric vector of positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param breaks a vector of break points that should enclose all x values
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @return A data frame with three columns:
#'   \item{left}{the left end of the interval}
#'   \item{right}{the right end of the interval}
#'   \item{count}{the number of observations in that interval}
#' @export
#' @S3method plot interval_bin
#' @examples
#' x <- interval_bin(runif(100))
#' # Simple plot methoded included in package
#' plot(x)
#'
#' # Selecting breaks:
#' interval_bin(runif(100), breaks = interval_breaks(bins = 10))
#' interval_bin(runif(100), breaks = interval_breaks(binwidth = 0.1))
#' interval_bin(runif(100), breaks = seq(0, 1, by = 0.2))
#' interval_bin(runif(100), breaks = scott_breaks())
#' interval_bin(runif(100), breaks = dhist_breaks())
interval_bin <- function(x, weight = NULL, breaks = interval_breaks(), na.rm = FALSE) {
  data <- clean_x(x, weight, na.rm = na.rm)
  if (is.function(breaks)) breaks <- breaks(x)  
  
  bin <- findInterval(data$x, breaks, all.inside = TRUE)
  count <- vaggregate(data$weight, bin, sum, .default = 0)
  
  structure(data.frame(
    left = breaks[-length(breaks)],
    right = breaks[-1],
    count = count
  ), class = c("interval_bin", "data.frame"))
}


plot.interval_bin <- function(x, ...) {
  xlim <- range(x$left, x$right)
  ylim <- range(0, x$count)

  plot(xlim, ylim, type = "n")
  with(x, rect(left, 0, right, count))
}