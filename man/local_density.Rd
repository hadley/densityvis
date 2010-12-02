\name{local_density}
\alias{local_density}
\title{Local density estimation.}
\usage{local_density(x, grid, scale=TRUE, nn=0.7, h=0,
    kernel="tcub", degree=2, bounded=FALSE)}

\description{
  Local density estimation.
}

\details{
  This convenience function wraps up the important
  arguments of \code{\link[locfit]{locfit}} and
  \code{\link[locfit]{lp}}, and hopefully provides enough
  documentation so that you don't need to refer to the
  original documentation and book.

  Adaptive penalties are not available in this convenience
  function because they typically require a two-stage fit,
  where the first fit is used to estimate the local
  variance.

  If you recieve an error \code{newsplit: out of vertex
  space}, that indicates that your bandwidth is too small.
}
\value{a vector of predict values}
\seealso{"Local Regression and Likelihood," C. Loader.  Springer, 1999.}
\arguments{
  \item{scale}{should x and y positions be scaled independently?  Use
\code{FALSE} when variables are measured on the same original scale.}
  \item{nn}{span, or nearest neighbour fraction, the proportion of the total
observations used in predict}
  \item{h}{constant bandwidth. Use if you want a constant bandwith, rather
than a bandwidth that varies. If both \code{nn} and \code{constant}
are set, each is calculated for each location and the largest is used.
If \code{scale = TRUE} the unit of this value is standard deviations,
otherwise, it's in the units of the data.}
  \item{kernel}{Weight function, default is tricubic. Other choices are
\code{"rect"}, ‘"trwt"’, ‘"tria"’, ‘"epan"’, ‘"bisq"’ and
‘"gauss"’. Choices may be restricted when derivatives are required;
e.g. for confidence bands and some bandwidth selectors.}
  \item{degree}{degree of polynomial used for smoothing}
  \item{grid}{a matrix giving locations to use for predicted densities. If
not given, defaults to the unique locations of the input data.}
  \item{bounded}{if \code{TRUE} bound density to range of data, otherwise
bound to range of grid.}
}
\examples{grid <- seq(min(diamonds$price), max(diamonds$price), length = 1000)
dens <- local_density(diamonds$price, grid)
plot(grid, dens, type = "l")

dens <- local_density(diamonds$price, grid, h = 500, nn = 0, scale = F)
plot(grid, dens, type = "l")
dens <- local_density(diamonds$price, grid, h = 0.05, nn = 0, scale = T)

dens <- local_density(diamonds$price, grid, h = 0.05, nn = 0, scale = T)}
