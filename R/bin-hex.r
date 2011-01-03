#' Bin data into hexagons (2d).
#'
#' @param x a numeric vector of x positions
#' @param y a numeric vector of y positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param height height of each hexagon, if \code{NULL} computed from ybins
#' @param width width of each hexagon, if \code{NULL} computed from ybins
#' @param xbins number of horizontal bins, if \code{width} unspecified
#' @param ybins number of vertical bins, if \code{height} unspecified
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @export
#' @seealso \code{\link{hex_pos}} for algorithm that finds hexagon center
#'   closest to each point and \code{\link{hex_coord}} that generates
#'   coordinates of each hexagon.
#' @return A data frame with columns \code{x}, \code{y} and \code{freq},
#'   and attributes \code{width} and \code{height}.
#' @S3method plot bin_hex
#' @examples
#' plot(hex_bin(runif(1e4), runif(1e4)))
#' plot(hex_bin(rnorm(1e4), rnorm(1e4)))
#' 
#' data(baseball, package = "plyr")
#' bin <- hex_bin(baseball$g, baseball$ab)
hex_bin <- function(x, y, weight = NULL, width = NULL, height = NULL, xbins = 20, ybins = 20, na.rm = FALSE) {
  
  data <- clean_xy(x, y, weight)
  if (is.null(width)) width <- diff(range(data$x)) / xbins
  if (is.null(height)) height <- diff(range(data$y)) / ybins

  height <- height * sqrt(3)

  pos <- hex_pos(data$x, data$y, width, height)
  data$x <- pos[, 1]
  data$y <- pos[, 2]
  
  structure(
    count(data, c("x", "y"), "weight"),
    width = width,
    height = height,
    class = c("bin_hex", "data.frame")
  )
}

plot.bin_hex <- function(x, ...) {
  if (!require("scales")) {
    message("Scales package required for plotting 2d densities")
    return()
  }
  
  col <- cscale(x$freq, seq_gradient_pal(low = "grey95", high = "black"))
  hexes <- hex_coord(x$x, x$y, attr(x, "width"), attr(x, "height"))

  plot(hexes[, 1], hexes[, 2], type = "n")
  polygon(hexes, col = col, border = NA)
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

#' Find centre of closest hexagon.
#'
#' @param x numeric x position
#' @param y numeric y position
#' @param width of hexagon
#' @param height of hexagon
#' @return matrix giving position of closest hexagon center
#' @keywords internal
#' @export
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
  y_offset[dist_smallest == dist_1] <- +0.5
  y_offset[dist_smallest == dist_2] <- -0.5
  y_offset[dist_smallest == dist_3] <- +0.5
  y_offset[dist_smallest == dist_4] <- -0.5

  # Transform back to original coordinates
  cbind((fx - x_offset) * width + minx, (fy - y_offset) * height + miny)  
}

#' Generate hexagon coordinates.
#'
#' Long axis is horizontal. Edges clock-wise from far-left, separated by
#' row of missing values.
#'
#' @param x horizontal position of center
#' @param y vertical position of center
#' @param width hex width
#' @param height hex height
#' @export
#' @keywords internal
#' @return A two column matrix with 7 times as many rows as input.
#' @examples
#' x <- runif(1000)
#' y <- runif(1000)
#' res <- unique(hex_pos(x, y, 0.5, 0.5))
#' hexes <- hex_coord(res[, 1], res[, 2], 0.5, 0.5)
#' 
#' plot(hexes, type = "n")
#' polygon(hexes)
#' points(res)
hex_coord <- function(x, y, width, height) {
  dx <- width / 6
  dy <- height / 2 / sqrt(3)

  hex_x <- rbind(x - 2 * dx, x - dx, x + dx, x + 2 * dx, x + dx, x - dx, NA)
  hex_y <- rbind(y, y + dy, y + dy, y, y - dy, y - dy, NA)
  
  cbind(as.vector(hex_x), as.vector(hex_y))
}
