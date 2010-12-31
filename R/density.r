plot.density_1d <- function(x, ...) {
  xlim <- attr(x, "xlim")
  grid <- seq(xlim[1], xlim[2], length = 200)
  dens <- x(grid)
  
  plot(grid, dens, type = "l")
}

plot.density_2d <- function(x, ...) {
  xlim <- attr(x, "xlim")
  ylim <- attr(x, "ylim")
  grid <- expand.grid(
    x = seq(xlim[1], xlim[2], length = 50),
    y = seq(ylim[1], ylim[2], length = 50))
    
  dens <- x(grid$x, grid$y)
  col <- cscale(dens, seq_gradient_pal(low = "grey95", high = "black"))
  
  plot(grid$x, grid$y, col = col, pch = 20)
}

