\name{rect_bin}
\alias{rect_bin}
\title{Bin data into rectangles (2d).}
\usage{rect_bin(x, y, weight, xbreaks=interval_breaks(),
    ybreaks=interval_breaks(), na.rm=FALSE)}

\description{
  Bin data into rectangles (2d).
}
\arguments{
  \item{x}{a numeric vector of x positions}
  \item{y}{a numeric vector of y positions}
  \item{weight}{\code{NULL} or a numeric vector providing weights for each
observation}
  \item{breaks}{a break function, or a vector of break points that should
enclose all x values.}
  \item{breaks}{a break function, or a vector of break points that should
enclose all y values.}
  \item{na.rm}{If \code{TRUE} missing values will be silently removed, 
otherwise they will be removed with a warning.}
}
\examples{x <- rnorm(1e5)
y <- rnorm(1e5)
bins <- rect_bin(x, y)
plot(bins)

# Specifying bin widths
rect_bin(x, y, 
xbreaks = interval_breaks(binwidth = 0.5), 
ybreaks = interval_breaks(binwidth = 0.5))}
