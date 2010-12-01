\name{dhist}
\alias{dhist}
\title{dhist.}
\usage{dhist(x, a=5 * diff(quantile(x, c(0.25, 0.75))), nbins=10,
    rx=range(x))}

\description{
  dhist. An another algorithm for computing histogram
  breaks.  Produces irregular bins.
}
\references{\url{http://pubs.research.avayalabs.com/pdfs/ALR-2007-003-paper.pdf}}
\examples{hist(mtcars$mpg, breaks=dhist)
hist(mtcars$mpg, breaks=dhist(mtcars$mpg, a=500))}
