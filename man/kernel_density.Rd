\name{kernel_density}
\alias{kernel_density}
\title{Kernel density estimates.}
\usage{kernel_density(x, grid, w=1, scale=TRUE, h="bw.nrd", n=512,
    na.rm=FALSE)}

\description{
  Kernel density estimates.
}

\details{
  Offers less flexibility than \code{\link{local_density}}
  but can be much faster and has no external dependencies.

  1d case uses linear interpolation on the results of
  \code{\link{density}}. 2d case uses a modified version of
  \code{\link[MASS]{kde2d}}.

  Only normal kernel/weight function currently implemented.
}
\arguments{
  \item{h}{the binwidth. Either a numeric vector, or function that computes
the binwidth given the data.
grid <- seq(min(diamonds$price), max(diamonds$price), length = 100)
dens <- kernel_density(diamonds$price, grid)
plot(grid, dens, type = "l")}
}
