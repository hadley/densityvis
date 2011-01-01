\name{local_density_2d}
\alias{local_density_2d}
\title{2d local density estimate.}
\usage{local_density_2d(x, y, weight, scale=TRUE, nn=0.7, h=0,
    kernel="tcub", degree=2, bounded=FALSE, na.rm=FALSE)}

\description{
  2d local density estimate.
}
\arguments{
  \item{scale}{should x and y positions be scaled independently?  Use
\code{FALSE} when variables are measured on the same original scale.}
}
\examples{dens <- local_density_2d(baseball$g, baseball$ab)}
