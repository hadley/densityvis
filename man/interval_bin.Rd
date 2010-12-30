\name{interval_bin}
\alias{interval_bin}
\title{Bin data into intervals (1d).}
\usage{interval_bin(x, weight, breaks=interval_breaks(),
    na.rm=FALSE)}

\description{
  Bin data into intervals (1d).
}

\details{
  There is no argument to \code{interval_bin} that
  specifies whether the intervals are right-open or
  left-open, because the breaks should specify this using
  small floating point adjustments.
}
\value{A data frame with three columns:
\item{left}{the left end of the interval}
\item{right}{the right end of the interval}
\item{count}{the number of observations in that interval}}
\arguments{
  \item{x}{a numeric vector of positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{breaks}{a vector of break points that should enclose all x values}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{x <- interval_bin(runif(100))
# Simple plot methoded included in package
plot(x)

# Selecting breaks:
interval_bin(runif(100), breaks = interval_breaks(bins = 10))
interval_bin(runif(100), breaks = interval_breaks(binwidth = 0.1))
interval_bin(runif(100), breaks = seq(0, 1, by = 0.2))
interval_bin(runif(100), breaks = scott_breaks())
interval_bin(runif(100), breaks = dhist_breaks())}
