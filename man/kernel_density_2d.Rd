\name{kernel_density_2d}
\alias{kernel_density_2d}
\title{2d dernel density estimate.}
\usage{kernel_density_2d(x, y, weight, xbandwidth="bw.nrd",
    ybandwidth="bw.nrd", na.rm=FALSE)}

\description{
  2d dernel density estimate.
}

\details{
  Only normal kernel/weight function currently implemented.
  This function is based on \code{\link[MASS]{kde2d}} but
  extended to allow weights, and to compute density at
  arbitrary position.
}
\arguments{
  \item{x}{a numeric vector of x positions}
  \item{y}{a numeric vector of y positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{xbandwidth}{bandwith for x direction.  Either a number or a 
function to calculate the binwidth from the data.}
  \item{ybandwidth}{bandwith for y direction.  Either a number or a 
function to calculate the binwidth from the data.}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{data(baseball, package = "plyr")
dens <- kernel_density_2d(baseball$g, baseball$ab)
plot(dens)}
