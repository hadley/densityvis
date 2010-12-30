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
#' @param h the binwidth. Either a numeric vector, or function that computes
#'   the binwidth given the data.
#' grid <- seq(min(diamonds$price), max(diamonds$price), length = 100)
#' dens <- kernel_density(diamonds$price, grid)
#' plot(grid, dens, type = "l")
kernel_density <- function(x, grid, w = 1, scale = TRUE, h = "bw.nrd", n = 512, na.rm = FALSE) {
  if (is.character(h)) h <- match.fun(h)

  stopifnot(is.numeric(x))
  stopifnot(is.numeric(grid))
  stopifnot(is.numeric(w))
  
  finite_row <- apply(x, 1, function(x) any(is.finite(x)))
  if (any(!finite_row)) {
    x <- x[!finite_row, ]
    if (!na.rm) message(sum(!finite_row), " missing/infinite values removed")
  }
  
  if (!is.matrix(x)) {
    # 1d case: use density + linear interpolation
    if (is.function(h)) {
      h <- h(x)
      message("Using bandwidth ", format(h, digits = 3))
    }
    
    d <- density(x, weights = w, bw = h, from = min(grid), to = max(grid), 
      n = n)
    approx(d$x, d$y, grid)$y
  } else {
    # 2d case: use modified version of kde2d
    stopifnot(ncol(x) == ncol(grid))
    stopifnot(ncol(x) <= 2)

    if (is.function(h)) {
      h <- c(h(x[, 1], h(x[, 2])))
      message("Using bandwidth ", paste(format(h, digits = 3), 
        collapse = ", "))
    } else {
      h <- rep(h, length.out = 2L)
    }
    # http://web.mit.edu/piantado/www/blog/nonindep-density.R
    ngrid <- nrow(grid)
    nx <- nrow(x)
    
    ax <- outer(grid[, 1], x[, 1], "-") / h[1]
    ay <- outer(grid[, 2], x[, 2], "-") / h[2]
    matrix(dnorm(ax) * dnorm(ay), ngrid, nx) * 
      matrix(w, nrow = n, ncol = nx) / (sum(w) * h[1] * h[2])
  }
}
