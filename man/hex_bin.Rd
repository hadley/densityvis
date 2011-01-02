\name{hex_bin}
\alias{hex_bin}
\title{Bin data into hexagons (2d).}
\usage{hex_bin(x, y, weight, width, height, xbins=20, ybins=20,
    na.rm=FALSE)}

\description{
  Bin data into hexagons (2d).
}
\seealso{\code{\link{hex_pos}} for algorithm that finds hexagon center
closest to each point and \code{\link{hex_coord}} that generates
coordinates of each hexagon.}
\value{A data frame with columns \code{x}, \code{y} and \code{freq},
and attributes \code{width} and \code{height}.}
\arguments{
  \item{x}{a numeric vector of x positions}
  \item{y}{a numeric vector of y positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{height}{height of each hexagon, if \code{NULL} computed from ybins}
  \item{width}{width of each hexagon, if \code{NULL} computed from ybins}
  \item{xbin}{number of horizontal bins, if \code{width} unspecified}
  \item{ybin}{number of vertical bins, if \code{height} unspecified}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{plot(hex_bin(runif(1e4), runif(1e4)))
plot(hex_bin(rnorm(1e4), rnorm(1e4)))

bin <- hex_bin(baseball$g, baseball$ab)}
