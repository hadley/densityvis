
clean_x <- function(x, weight = NULL, na.rm = TRUE) {
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
  
  list(x = x, weight = weight)
}

clean_xy <- function(x, y, weight = NULL, na.rm = TRUE) {
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
 
  list(x = x, y = y, weight = weight) 
}