\name{local_density_2d}
\alias{local_density_2d}
\title{2d local density estimate.}
\usage{local_density_2d(x, y, weight, scale=TRUE, nn=0.7, h=0,
    kernel="tcub", degree=2, bounded=FALSE, na.rm=FALSE)}

\description{
  2d local density estimate.
}
\arguments{
  \item{x}{a numeric vector of x positions}
  \item{y}{a numeric vector of y positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{h}{constant bandwidth. Use if you want a constant bandwith, rather
than a bandwidth that varies. If both \code{nn} and \code{h}
are set, each is calculated for each location and the largest is used.
If \code{scale = TRUE} the unit of this value is standard deviations,
otherwise, it's in the units of the data.}
  \item{nn}{A vector of length two, giving the span, or nearest neighbour
fraction, the proportion of the total used to compute the density.}
  \item{scale}{should x and y positions be scaled independently?  Use
\code{FALSE} when variables are measured on the same original scale.}
  \item{kernel}{Weight function, default is tricubic. Other choices are
\code{"rect"}, '"trwt"', '"tria"', '"epan"', '"bisq"' and
'"gauss"'. Choices may be restricted when derivatives are required;
e.g. for confidence bands and some bandwidth selectors.}
  \item{degree}{degree of polynomial used for smoothing}
  \item{bounded}{if \code{TRUE} bounds density to range of data, otherwise
it takes non-zero values over the entire real plane.}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{data(baseball, package = "plyr")
dens <- local_density_2d(baseball$g, baseball$ab)
plot(dens)
baseball$dens <- dens(baseball$g, baseball$ab)

plot(local_density_2d(baseball$g, baseball$ab, nn = 0.4))
plot(local_density_2d(baseball$g, baseball$ab, nn = 0.1))
plot(local_density_2d(baseball$g, baseball$ab, bounded = TRUE))}
