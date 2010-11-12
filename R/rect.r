#' Bin data into rectangles (2d)
#'
#' @export
#' bins <- rect_bin(runif(1000), runif(1000))
#' ggplot(bins) + geom_rect(aes(xmin = left, xmax = right, ymin = bottom, ymax = top, fill = count))
rect_bin <- function(x, y, weight = rep.int(1, length(x)), breaks = rect_breaks(x, y), drop = FALSE, na.rm = FALSE) {
    
  # If !na.rm, remove missing values with a warning.  
  # Otherwise just remove them
  missing <- is.na(x) | is.na(y)
  nmissing <- sum(missing)
  if (!na.rm && nmissing > 0) {
    warning("Removing ", nmissing, " missing values")
  }

  # Check weights, and throw out missing values and zero-weight observations
  if (is.null(weight)) {
    weight <- rep.int(1, length(x))
  } else {
    weight[is.na(weight)] <- 0
  }
  
  if (all(missing | weight == 0)) return()  
  x <- x[!missing & weight > 0]
  y <- y[!missing & weight > 0]
  
  xn <- length(breaks$x)
  yn <- length(breaks$y)
  xbin <- findInterval(x, breaks$x, all.inside = TRUE)
  ybin <- findInterval(y, breaks$y, all.inside = TRUE)
  
  bin <- (xbin - 1L) + (ybin - 1L) * (xn - 1L) + 1L
  count <- vaggregate(weight, bin, sum, na.rm = TRUE, 
    .default = 0, .n = (xn - 1L) * (yn - 1L))
  
  data.frame(
    top =    rep(breaks$y[-yn], xn - 1), 
    bottom = rep(breaks$y[-1],  xn - 1), 
    left =   rep(breaks$x[-xn], each = yn - 1),
    right =  rep(breaks$x[-1],  each = yn - 1),
    count = count
  )
}

#' Calculate breaks for rectangular (2d) bins
#'
#' @export
rect_breaks <- function(x, y, range = NULL, binwidth = NULL, bins = 20, origin = NULL) {
  if (is.null(range)) {
    range <- list(x = range(x, na.rm = TRUE), y = range(y, na.rm = TRUE ))
  }
  
  # Determine binwidth, if omitted
  if (is.null(binwidth)) {
    binwidth <- c(NA, NA)
    if (is.integer(x)) {
      binwidth[1] <- 1
    } else {
      binwidth[1] <- diff(range$x) / bins
    }
    if (is.integer(y)) {
      binwidth[2] <- 1
    } else {
      binwidth[2] <- diff(range$y) / bins
    }  
    message("Using binwidth (", format(binwidth[1], digits = 3), ", ", 
      format(binwidth[2], digits = 3), ")")    
  }
  stopifnot(is.numeric(binwidth))
  stopifnot(length(binwidth) == 2)
  
  if (is.null(origin)) {
    breaks <- list(
      fullseq(range$x, binwidth[1]),
      fullseq(range$y, binwidth[2])
    )
  } else {
    breaks <- list(
      seq(origin[1], max(range$x) + binwidth[1], binwidth[1]),
      seq(origin[2], max(range$y) + binwidth[2], binwidth[2])
    )
  }
  # stopifnot(is.list(breaks))
  # stopifnot(length(breaks) == 2)
  # stopifnot(all(sapply(breaks, is.numeric)))

  names(breaks) <- c("x", "y")
  breaks
}