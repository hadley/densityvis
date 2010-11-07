#' Bin data into rectangles (2d)
#'
#' @export
rect_bin <- function(x, y, weight, scales, breaks = rect_breaks(x, y), drop = TRUE, na.rm = FALSE) {
    
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
    
  bin <- (xbin - 1) + (ybin - 1) * xn
  count <- vaggregate(weight, bin, sum, na.rm = TRUE, .default = 0)

  data.frame(
    top =    rep(breaks$y[-yn], by = xn), 
    bottom = rep(breaks$y[-1],  by = xn), 
    left =   rep(breaks$x[-xn], times = yn),
    right =  rep(breaks$x[-1],  times = yn),
    count = count
  )
}

#' Calculate breaks for rectangular (2d) bins
#'
#' @export
rect_breaks <- function(x, y, range, binwidth = NULL, bins = NULL, origin = NULL) {
  # Determine binwidth, if omitted
  if (is.null(binwidth)) {
    binwidth <- c(NA, NA)
    if (is.integer(data$x)) {
      binwidth[1] <- 1
    } else {
      binwidth[1] <- diff(range$x) / bins
    }
    if (is.integer(data$y)) {
      binwidth[2] <- 1
    } else {
      binwidth[2] <- diff(range$y) / bins
    }      
  }
  stopifnot(is.numeric(binwidth))
  stopifnot(length(binwidth) == 2)
  
  # Determine breaks, if omitted
  if (is.null(breaks)) {
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
  }
  stopifnot(is.list(breaks))
  stopifnot(length(breaks) == 2)
  stopifnot(all(sapply(breaks, is.numeric)))
  names(breaks) <- c("x", "y")
}