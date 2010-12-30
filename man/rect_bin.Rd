\name{rect_bin}
\alias{rect_bin}
\title{Bin data into rectangles (2d)...}
\usage{rect_bin(x, y, weight, xbreaks=interval_breaks(),
    ybreaks=interval_breaks(), drop=FALSE, na.rm=FALSE)}

\description{
  Bin data into rectangles (2d)
}
\examples{bins <- rect_bin(runif(1000), runif(1000))
ggplot(bins) + geom_rect(aes(xmin = left, xmax = right, ymin = bottom, ymax = top, fill = count))}
