\name{interval_breaks}
\alias{interval_breaks}
\title{Calculate breaks for interval (1d) bins.}
\usage{interval_breaks(bins=20, binwidth, origin, range,
    right=TRUE)}

\description{
  Calculate breaks for interval (1d) bins.
}

\details{
  To deal with floating point rounding issues, bin
  positions are rounded slighty up or slightly down
  (depending on right vs. left bins).  This technique is
  adapted from \code{\link{hist}}.
}
