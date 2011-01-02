#' Discretise continuous variable, equal interval length.
#' Cut numeric vector into intervals of equal length.
#' 
#' @param x numeric vector
#' @param n number of intervals to create, OR
#' @param length of each interval
#' @param ... other arguments passed on to \code{\link{cut}}
#' @keywords manip
#' @seealso \code{\link{cut_number}}
#' @export
#' @examples 
#' table(cut_interval(1:100, n = 10))
#' table(cut_interval(1:100, n = 11))
#' table(cut_interval(1:100, length = 10))
cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, interval_breaks(x, bins = n, binwidth = length), 
    include.lowest = TRUE, ...)
}

#' Discretise continuous variable, equal number of points.
#' Cut numeric vector into intervals containing equal number of points.
#' 
#' @param x numeric vector
#' @param n number of intervals to create
#' @param ... other arguments passed on to \code{\link{cut}}
#' @keywords manip
#' @seealso \code{\link{cut_interval}}
#' @export
#' @examples 
#' table(cut_number(runif(1000), n = 10))
cut_number <- function(x, n = NULL, ...) {
  probs <- seq(0, 1, length = n + 1)
  qs <- quantile(x, probs, na.rm = TRUE)
  cut(x, qs, include.lowest = TRUE, ...)
}
