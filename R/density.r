plot.density_1d <- function(x, ...) {
  xlim <- attr(x, "xlim")
  grid <- seq(xlim[1], xlim[2], length = 200)
  dens <- x(grid)
  
  plot(grid, dens, type = "l")
}

