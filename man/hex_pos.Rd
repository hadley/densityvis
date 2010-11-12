\name{hex_pos}
\alias{hex_pos}
\title{Figure which hexagon each point belongs to.}
\usage{hex_pos(x, y, width, height)}

\description{
  Figure which hexagon each point belongs to.
}
\value{matrix giving position of closest hexagon center}
\examples{x <- runif(1e4)
y <- runif(1e4)
res <- hex_pos(x, y, 0.5, 0.5)
plot(x, y, type = "n")
segments(x, y, res[, 1], res[, 2], col = "grey80")
points(unique(res), pch = 20, cex = 2)}
