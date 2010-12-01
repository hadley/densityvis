#' Bin data into intervals (1d)
#'
#' @export
interval_bin <- function(x, weight = NULL, breaks = interval_breaks(), drop = FALSE, na.rm = FALSE) {
  
  # If !na.rm, remove missing values with a warning.
  # Otherwise just remove them
  missing <- is.na(x)
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
  if (any(!ok)) x <- x[ok]  
  
  if (is.function(breaks)) breaks <- breaks(x)  
  bin <- findInterval(x, breaks, all.inside = TRUE)
  count <- vaggregate(weight, bin, sum, na.rm = TRUE, .default = 0)
  
  data.frame(
    left = breaks[-length(breaks)],
    right = breaks[-1],
    count = count
  )
}

