\name{ash_1d}
\alias{ash_1d}
\title{Average shifted histogram.}
\usage{ash_1d(x, weight, binwidth, bins=20, m=5, na.rm=FALSE,
    range)}

\description{
  Average shifted histogram.
}

\details{
  Computes bins for \code{m} evenly spaced origins, then
  averages the counts over those bins.
}
\value{A data frame with three columns:
\item{left}{the left end of the interval}
\item{right}{the right end of the interval}
\item{count}{the number of observations in that interval}}
\arguments{
  \item{x}{a numeric vector of positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{bins}{desired number of bins}
  \item{binwidth}{desired bin width (specify this or \code{bins})}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
  \item{m}{smoothing parameter - this many different starting origins between
\code{min(range)} and \code{min(range) - binwidth} will be generated.}
  \item{range}{range of values to use, if different to range of data.}
}
\examples{x <- ash_1d(runif(100))
# Simple plot methoded included in package
plot(x)

plot(ash_1d(baseball$b, m = 10))
plot(ash_1d(baseball$b, bins = 100))}
