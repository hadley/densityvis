\name{interval_breaks}
\alias{interval_breaks}
\title{Pick breaks for interval (1d) bins.}
\usage{interval_breaks(bins=20, binwidth, origin, range)}

\description{
  Pick breaks for interval (1d) bins.
}

\details{
  Specify either \code{bins} or \code{binwidth}.
}
\value{A function that takes a single parameter, a numeric x specifying
the data for which breaks are needed, and returns a vector of breaks.}
\arguments{
  \item{bins}{Desired number of bins}
  \item{binwidth}{Desired bin width}
  \item{origin}{Desired origin of first bin}
  \item{range}{Range of values to use, if different to range of data.}
}
