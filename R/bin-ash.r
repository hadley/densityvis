#' Average shifted histogram.
#'
#' Computes bins for \code{m} evenly spaced origins, then averages the 
#' counts over those bins.
#'
#' @param x a numeric vector of positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param bins desired number of bins
#' @param binwidth desired bin width (specify this or \code{bins})
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @param m smoothing parameter - this many different starting origins between
#'   \code{min(range)} and \code{min(range) - binwidth} will be generated.
#' @param range range of values to use, if different to range of data.
#' @return A data frame with three columns:
#'   \item{left}{the left end of the interval}
#'   \item{right}{the right end of the interval}
#'   \item{count}{the number of observations in that interval}
#' @examples
#' x <- ash_1d(runif(100))
#' # Simple plot methoded included in package
#' plot(x)
#'
#' plot(ash_1d(baseball$b, m = 10))
#' plot(ash_1d(baseball$b, bins = 100))
ash_1d <- function(x, weight = NULL, binwidth = NULL, bins = 20, m = 5, na.rm = FALSE, range = NULL) {
  data <- clean_x(x, weight, na.rm = na.rm)

  if (is.null(range)) {
    range <- range(data$x)
  }
  if (is.null(binwidth)) {
    binwidth <- diff(range) / bins
    message("Using binwidth ", format(binwidth, digits = 3))
  }
  
  origins <- round_any(range[1], binwidth, floor) - 
    seq(1, 0, length = m + 1)[-1] * binwidth
  
  bin_count <- function(origin) {
    breaks <- seq(origin, max(range) + binwidth, binwidth)
    bin <- findInterval(data$x, breaks)
    vaggregate(data$weight, bin, sum, .default = 0, .n = length(breaks) - 1)
  }
  
  mid <- seq(min(range), max(range), binwidth)
  count <- rowMeans(do.call("cbind", lapply(origins, bin_count)))

  structure(data.frame(
    left = mid - binwidth / 2,
    right = mid + binwidth / 2,
    count = count), class = c("interval_bin", "data.frame"))
}
