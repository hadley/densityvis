\name{kernel_density_1d}
\alias{kernel_density_1d}
\title{1d dernel density estimate.}
\usage{kernel_density_1d(x, weight, bandwidth="bw.nrd",
    kernel="gaussian", n=512, na.rm=FALSE)}

\description{
  1d dernel density estimate.
}

\details{
  Offers less flexibility than
  \code{\link{local_density_1d}} but can be much faster and
  has no external dependencies.

  1d case uses linear interpolation on the results of
  \code{\link{density}}. 2d case uses a modified version of
  \code{\link[MASS]{kde2d}}.
}
\arguments{
  \item{x}{a numeric vector of positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{bandwidth}{the banwidth. Either a numeric vector, or function that
computes the binwidth given the data.}
  \item{kernel}{the kernel used for smoothing. Defaults to 
\code{"gaussian"}. See \code{\link{density}} for full list}
  \item{n}{number of points to use for interpolation}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{dens <- kernel_density_1d(baseball$g)
plot(dens)
plot(kernel_density_1d(baseball$g, bandwidth = 4))
plot(kernel_density_1d(baseball$g, bandwidth = 1))}
