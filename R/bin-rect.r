#' Bin data into rectangles (2d)
#'
#' @export
#' bins <- rect_bin(runif(1000), runif(1000))
#' ggplot(bins) + geom_rect(aes(xmin = left, xmax = right, ymin = bottom, ymax = top, fill = count))
rect_bin <- function(x, y, weight = NULL, xbreaks = interval_breaks(), ybreaks = interval_breaks(), drop = FALSE, na.rm = FALSE) {
    
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
  
  ok <- !missing & weight > 0
  if (all(!ok)) return()  
  if (any(!ok)) {
    x <- x[!ok]
    y <- y[!ok]    
  }
  
  if (is.function(xbreaks)) xbreaks <- xbreaks(x)
  if (is.function(ybreaks)) ybreaks <- ybreaks(y)
  
  xbin <- findInterval(x, xbreaks, all.inside = TRUE)
  ybin <- findInterval(y, ybreaks, all.inside = TRUE)
  
  xn <- length(xbreaks)
  yn <- length(ybreaks)
  bin <- (xbin - 1L) + (ybin - 1L) * (xn - 1L) + 1L

  count <- vaggregate(weight, bin, sum, na.rm = TRUE, 
    .default = 0, .n = (xn - 1L) * (yn - 1L))
  
  data.frame(
    top =    rep(ybreaks[-yn], xn - 1), 
    bottom = rep(ybreaks[-1],  xn - 1), 
    left =   rep(xbreaks[-xn], each = yn - 1),
    right =  rep(xbreaks[-1],  each = yn - 1),
    count = count
  )
}
