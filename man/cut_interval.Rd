\name{cut_interval}
\alias{cut_interval}
\title{Discretise continuous variable, equal interval length.}
\usage{cut_interval(x, n, length, ...)}

\description{
  Discretise continuous variable, equal interval length.
  Cut numeric vector into intervals of equal length.
}
\keyword{manip}
\seealso{\code{\link{cut_number}}}
\arguments{
  \item{x}{numeric vector}
  \item{n}{number of intervals to create, OR}
  \item{length}{of each interval}
  \item{...}{other arguments passed on to \code{\link{cut}}}
}
\examples{table(cut_interval(1:100, n = 10))
table(cut_interval(1:100, n = 11))
table(cut_interval(1:100, length = 10))}
