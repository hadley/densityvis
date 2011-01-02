\name{hex_coord}
\alias{hex_coord}
\title{Generate hexagon coordinates.}
\usage{hex_coord(x, y, width, height)}

\description{
  Generate hexagon coordinates.
}

\details{
  Long axis is horizontal. Edges clock-wise from far-left,
  separated by row of missing values.
}
\keyword{internal}
\value{A two column matrix with 7 times as many rows as input.}
\arguments{
  \item{x}{horizontal position of center}
  \item{y}{vertical position of center}
  \item{width}{hex width}
  \item{height}{hex height}
}
\examples{x <- runif(1000)
y <- runif(1000)
res <- unique(hex_pos(x, y, 0.5, 0.5))
hexes <- hex_coord(res[, 1], res[, 2], 0.5, 0.5)

plot(hexes, type = "n")
polygon(hexes)
points(res)}
