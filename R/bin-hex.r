hex_bin <- function(x, y, weight, binwidth = hex_binwidth(range(x), range(y))) {
  # Convert binwidths into bounds + nbins
  xbnds <- c(
    round_any(min(x), binwidth[1], floor) - 1e-6, 
    round_any(max(x), binwidth[1], ceiling) + 1e-6
  )
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- c(
    round_any(min(y), binwidth[1], floor) - 1e-6, 
    round_any(max(y), binwidth[2], ceiling) + 1e-6
  )
  ybins <- diff(ybnds) / binwidth[2]
  
  # Call hexbin
  hb <- hexbin(
    x, xbnds = xbnds, xbins = xbins,  
    y, ybnds = ybnds, shape = ybins / xbins, 
  )
  
  # Convert to data frame
  data.frame(
    hcell2xy(hb), 
    count = hb@count, 
    density = hb@count / sum(hb@count, na.rm=TRUE)
  )
}

hex_binwidth <- function(xrange, yrange, bins) {
   c( 
     diff(xrange) / bins,
     diff(yrange) / bins
   )
}

# Binning algorithms are available for various lattices in dimensions 2-8
# (Conway and Sloane 1982). The following subroutine is a fast FORTRAN
# implementation of hexagonal binning. The key observation is that hexagon
# centers fall on two staggered lattices whose cells are rectangles. Presuming
# the long side of the rectangle is in the y direction, dividing the y
# coordinates by square root (3) [SQRT(3)] makes the cells square. Thus the
# algorithm uses two lattices with square cells. The first lattice has points
# at the integers with [0, 0] as the lower left value. The second lattice is
# shifted so that the lower left value is at [.5 , .5]. The x and y vectors
# are scaled into [0, SIZE] and [0, SIZE / SQRT(3)], respectively. SIZE
# determines the portions of the lattices that are used. For each data point,
# binning consists of finding one candidate lattice point from each lattice
# and then selecting the nearest of the two candidates.

#' Figure which hexagon each point belongs to.
#'
#' @return matrix giving position of closest hexagon center
#' @examples
#' x <- runif(1e4)
#' y <- runif(1e4)
#' res <- hex_pos(x, y, 0.5, 0.5)
#' plot(x, y, type = "n")
#' segments(x, y, res[, 1], res[, 2], col = "grey80")
#' points(unique(res), pch = 20, cex = 2)
hex_pos <- function(x, y, width, height) {
  height <- height / sqrt(3)
  
  minx <- min(x, na.rm = TRUE)
  miny <- min(y, na.rm = TRUE)

  # Scale to [0, nrows/ncols]
  sx <- (x - minx) / width
  sy <- (y - miny) / height
  
  # Find closest center: [0, 0] or [0.5, 0.5]?
  fx <- round(sx)
  fy <- round(sy)
  
  dist_0 <- 3 * (sx - fx)^2 + (sy - fy)^2
  dist_1 <- 3 * (sx - fx + 0.5)^2 + (sy - fy + 0.5)^2
  dist_2 <- 3 * (sx - fx + 0.5)^2 + (sy - fy - 0.5)^2
  dist_3 <- 3 * (sx - fx - 0.5)^2 + (sy - fy + 0.5)^2
  dist_4 <- 3 * (sx - fx - 0.5)^2 + (sy - fy - 0.5)^2
  dist_smallest <- pmin(dist_0, dist_1, dist_2, dist_3, dist_4)
  
  x_offset <- rep(0, length(x))
  x_offset[dist_smallest == dist_1] <- +0.5
  x_offset[dist_smallest == dist_2] <- +0.5
  x_offset[dist_smallest == dist_3] <- -0.5
  x_offset[dist_smallest == dist_4] <- -0.5

  y_offset <- rep(0, length(y))
  y_offset[dist_smallest ==  dist_1] <- +0.5
  y_offset[dist_smallest ==  dist_2] <- -0.5
  y_offset[dist_smallest ==  dist_3] <- +0.5
  y_offset[dist_smallest ==  dist_4] <- -0.5

  # Transform back to original coordinates
  cbind((fx - x_offset) * width + minx, (fy - y_offset) * height + miny)
  
}

#' Generate hexagon coordinates
#' Long axes is horizontal.  Edges clock-wise from far-left.
#'
#' @examples
#' x <- runif(1000)
#' y <- runif(1000)
#' res <- unique(hex_pos(x, y, 0.5, 0.5))
#' hexes <- hex_coord(res[, 1], res[, 2], 0.5, 0.5)
#' 
#' grid.newpage()
#' pushViewport(dataViewport(x, y))
#' grid.polygon(x = hexes[, 1], y = hexes[, 2], gp=gpar(fill="grey80"),
#'   id.length = rep(6, nrow(res)), default = "native")
#' grid.points(res[, 1], res[, 2], default = "native", size = unit(5, "mm"), pch = 20)
hex_coord <- function(x, y, width, height) {
  dx <- width / 6
  dy <- height / 2 / sqrt(3)

  hex_x <- rbind(x - 2 * dx, x - dx, x + dx, x + 2 * dx, x + dx, x - dx)
  hex_y <- rbind(y, y + dy, y + dy, y, y - dy, y - dy)
  
  cbind(as.vector(hex_x), as.vector(hex_y))
}
