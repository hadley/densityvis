#' Bin data into intervals (1d)
#'
#' @export
interval_bin <- function(x, weight = NULL, breaks = interval_breaks(x), drop = FALSE, na.rm = FALSE) {
  
  # If !na.rm, remove missing values with a warning.  
  # Otherwise just remove them
  missing <- is.na(x)
  nmissing <- sum(missing)
  if (!na.rm && nmissing > 0) {
    warning("Removing ", nmissing, " missing values")
  }

  # Check weights, and throw out missing values and zero-weight observations
  if (is.null(weight)) {
    weight <- rep.int(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }
  
  if (all(missing | weight == 0)) return()  
  x <- x[!missing & weight > 0]  
  
  bin <- findInterval(x, breaks, all.inside = TRUE)
  count <- vaggregate(weight, bin, sum, na.rm = TRUE, .default = 0)
  
  data.frame(
    left = breaks[-length(breaks)],
    right = breaks[-1],
    count = count
  )
}

#' Calculate breaks for interval (1d) bins
#'
#' @export
interval_breaks <- function(x, nbins = 30, binwidth=NULL, origin=NULL, range=NULL, right = TRUE, width=0.9) {
  
  if (is.null(range)) {
    range <- range(x, na.rm = TRUE, finite = TRUE)
  }
  if (is.null(binwidth)) {
    binwidth <- diff(range) / nbins
  }
  
  # If x is an integer, place breaks between values
  if (is.integer(x)) return(seq.int(min(x) - 1, max(x), 1) + 0.5)
  # If x is a point mass, make a single bin
  if (diff(range) < 1e-07) return(range)

  if (is.null(origin)) {
    breaks <- fullseq(range, binwidth)
  } else {
    breaks <- seq(origin, max(range) + binwidth, binwidth)
  }
  
  # Adapt break fuzziness from base::hist - this protects from floating
  # point rounding errors
  diddle <- 1e-07 * median(diff(breaks))
  if (right) {
    fuzz <- c(-diddle, rep.int(diddle, length(breaks) - 1))
  } else {
    fuzz <- c(rep.int(-diddle, length(breaks) - 1), diddle) 
  }
  sort(breaks) + fuzz
  
}


# Discretise continuous variable, equal interval length.
# Cut numeric vector into intervals of equal length.
# 
# @arguments numeric vector
# @arguments number of intervals to create, OR
# @arguments length of each interval
# @arguments other arguments passed on to \code{\link{cut}}
# @keyword manip
# @seealso \code{\link{cut_number}}
# 
#X table(cut_interval(1:100, n = 10))
#X table(cut_interval(1:100, n = 11))
#X table(cut_interval(1:100, length = 10))
cut_interval <- function(x, n = NULL, length = NULL, ...) {
  cut(x, interval_breaks(x, nbins = n, binwidth = length), 
    include.lowest = TRUE, ...)
}

# Discretise continuous variable, equal number of points.
# Cut numeric vector into intervals containing equal number of points.
# 
# @arguments numeric vector
# @arguments number of intervals to create, OR
# @arguments length of each interval
# @arguments other arguments passed on to \code{\link{cut}}
# @keyword manip
# @seealso \code{\link{cut_interval}}
#X table(cut_number(runif(1000), n = 10))
cut_number <- function(x, n = NULL, ...) {
  probs <- seq(0, 1, length = n + 1)
  qs <- quantile(x, probs, na.rm = TRUE)
  cut(x, qs, include.lowest = TRUE, ...)
}
