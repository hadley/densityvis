#' Kernel density estimates.
#' 
#' Offers less flexibility than \code{\link{local_density}} but can be much
#' faster and has no external dependencies.
#' 
#' 1d case uses linear interpolation on the results of \code{\link{density}}.
#' 2d case uses a modified version of \code{\link[MASS]{kde2d}}.
#'
#' Only normal kernel/weight function currently implemented.
#'
#' @param x a numeric vector of positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param bandwidth the banwidth. Either a numeric vector, or function that
#'   computes the binwidth given the data.
#  @param kernel the kernel used for smoothing. Defaults to 
#'   \code{"gaussian"}. See \code{\link{density}} for full list
#' @param n number of points to use for interpolation
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @export
#' @examples
#' dens <- kernel_density_1d(baseball$g)
#' plot(dens)
#' plot(kernel_density_1d(baseball$g, bandwidth = 4))
#' plot(kernel_density_1d(baseball$g, bandwidth = 1))
kernel_density_1d <- function(x, weight = NULL, bandwidth = "bw.nrd", kernel = "gaussian", n = 512, na.rm = FALSE) {
  data <- clean_x(x, weight, na.rm = na.rm)
  h <- compute_bandwidth(bandwidth, data$x)
  
  blanket <- if (sum(data$w) != 1) suppressWarnings else identity
  d <- blanket(density(data$x, weights = data$w, bw = h, n = n))
  
  predict <- function(x) approx(d$x, d$y, x, yleft = 0, yright = 0)$y
  structure(predict, xlim = range(d$x), class = "density_1d")
}


kernel_density_2d <- function(x, y, weights = 1, scale = TRUE, xbandwidth = "bw.nrd", ybandwidth = "bw.nrd", n = 512, na.rm = FALSE) {
  
  data <- clean_xy(x, y, weight, na.rm = TRUE)
  n_in <- nrow(data)

  xh <- compute_bandwidth(xbandwidth, data$x)
  yh <- compute_bandwidth(ybandwidth, data$y)
  
  # From http://web.mit.edu/piantado/www/blog/nonindep-density.R
  function(x, y) {
    n_in <- length(x)

    ax <- outer(x, data$x, "-") / xh
    ay <- outer(y, data$y, "-") / yh
    matrix(dnorm(ax) * dnorm(ay), n_out, n_in) * 
      matrix(w, nrow = n_out, ncol = n_in) / (sum(w) * xh * yh)
    
  }
}

compute_bandwidth <- function(bw, x) {
  if (is.numeric(bw)) return(bw)
  if (is.character(bw)) bw <- match.fun(bw)

  if (is.function(bw)) {
    bw <- bw(x)
    message("Using bandwidth ", format(bw, digits = 3))
    bw
  } else {
    stop("Invalid bandwidth")
  }
}
