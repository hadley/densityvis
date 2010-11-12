\name{hex_coord}
\alias{hex_coord}
\title{Generate hexagon coordinates...}
\usage{hex_coord(x, y, width, height)}

\description{
  Generate hexagon coordinates Long axes is horizontal.
  Edges clock-wise from far-left.
}
\examples{x <- runif(1000)
y <- runif(1000)
res <- unique(hex_pos(x, y, 0.5, 0.5))
hexes <- hex_coord(res[, 1], res[, 2], 0.5, 0.5)

grid.newpage()
pushViewport(dataViewport(x, y))
grid.polygon(x = hexes[, 1], y = hexes[, 2], gp=gpar(fill="grey80"),
id.length = rep(6, nrow(res)), default = "native")
grid.points(res[, 1], res[, 2], default = "native", size = unit(5, "mm"), pch = 20)}
