\name{local_density_1d}
\alias{local_density_1d}
\title{1d local density estimate.}
\usage{local_density_1d(x, weight, scale=TRUE, nn=0.7, h=0,
    kernel="tcub", degree=2, bounded=FALSE, na.rm=FALSE)}

\description{
  1d local density estimate.
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
\value{a function of a single that returns the density at that location}
\seealso{"Local Regression and Likelihood," C. Loader.  Springer, 1999.}
\arguments{
  \item{x}{a numeric vector of x positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{nn}{span, or nearest neighbour fraction, the proportion of the total
observations used in predict}
  \item{h}{constant bandwidth. Use if you want a constant bandwith, rather
than a bandwidth that varies. If both \code{nn} and \code{h}
are set, each is calculated for each location and the largest is used.
If \code{scale = TRUE} the unit of this value is standard deviations,
otherwise, it's in the units of the data.}
  \item{kernel}{Weight function, default is tricubic. Other choices are
\code{"rect"}, '"trwt"', '"tria"', '"epan"', '"bisq"' and
'"gauss"'. Choices may be restricted when derivatives are required;
e.g. for confidence bands and some bandwidth selectors.}
  \item{degree}{degree of polynomial used for smoothing}
  \item{bounded}{if \code{TRUE} bounds density to range of data, otherwise
it takes non-zero values over the entire real line.}
  \item{scale}{if \code{TRUE} rescale \code{x} values to have standard 
deviation 1.}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{data(baseball, package = "plyr")
dens <- local_density_1d(baseball$g)
plot(dens)

# nn is like span in loess - it's a proportion
plot(local_density_1d(baseball$g, nn = 0.1))
plot(local_density_1d(baseball$g, nn = 0.2))
plot(local_density_1d(baseball$g, nn = 0.5))
plot(local_density_1d(baseball$g, nn = 1))

# when bounded = TRUE assumes that there are no possible values outside
# of the data range
plot(local_density_1d(baseball$g, nn = 0.2))
plot(local_density_1d(baseball$g, nn = 0.2, bounded = TRUE))

# When scale = FALSE, the bandwidth is specified in terms of the
# range of the original data.  When scale = TRUE, the data has been
# scaled to have sd 1
plot(local_density_1d(baseball$g, h = 0.5, nn = 0, scale = TRUE))
plot(local_density_1d(baseball$g, h = 0.5, nn = 0, scale = FALSE))}
