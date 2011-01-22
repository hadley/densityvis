\name{interval_bin}
\alias{interval_bin}
\title{Bin data into intervals (1d).}
\usage{interval_bin(x, weight, breaks=interval_breaks(),
    na.rm=FALSE, open="right")}

\description{
  Bin data into intervals (1d).
}

\details{
  To deal with floating point rounding issues, bin
  positions are rounded slighty up or slightly down
  (depending on whether \code{open} is right or left). This
  technique is adapted from \code{\link{hist}}.
}
\value{A data frame with three columns:
\item{left}{the left end of the interval}
\item{right}{the right end of the interval}
\item{count}{the number of observations in that interval}}
\seealso{\code{\link{ash_1d}}}
\arguments{
  \item{x}{a numeric vector of positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{breaks}{a vector of break points that should enclose all x values}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
  \item{open}{are intervals open on the \code{"left"} or the \code{"right"}.
This can make a big difference when your data is more discrete.}
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
