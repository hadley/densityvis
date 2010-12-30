\name{dhist_breaks}
\alias{dhist_breaks}
\title{dhist.}
\usage{dhist_breaks(a, nbins=10, rx)}

\description{
  dhist. An another algorithm for computing histogram
  breaks.  Produces irregular bins.
}
\author{Lorraine Denby}
\references{\url{http://pubs.research.avayalabs.com/pdfs/ALR-2007-003-paper.pdf}}
\examples{hist(mtcars$mpg, breaks=dhist_breaks())
hist(mtcars$mpg, breaks=dhist_breaks(a=500))}
