#' Bin data into rectangles (2d).
#'
#' @param x a numeric vector of x positions
#' @param y a numeric vector of y positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param breaks a break function, or a vector of break points that should
#'   enclose all x values.
#' @param breaks a break function, or a vector of break points that should
#'   enclose all y values.
#' @param xopen are x intervals open on the \code{"left"} or \code{"right"}.
#' @param xopen are y intervals open on the \code{"left"} or \code{"right"}.
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @export
#' @S3method plot rect_bin
#' @examples
#' x <- rnorm(1e5)
#' y <- rnorm(1e5)
#' bins <- rect_bin(x, y)
#' plot(bins)
#' 
#' # Specifying bin widths
#' rect_bin(x, y, 
#'   xbreaks = interval_breaks(binwidth = 0.5), 
#'   ybreaks = interval_breaks(binwidth = 0.5))
rect_bin <- function(x, y, weight = NULL, xbreaks = interval_breaks(), ybreaks = interval_breaks(), na.rm = FALSE, xopen = "right", yopen = "right") {
  
  data <- clean_xy(x, y, weight)
    
  if (is.function(xbreaks)) xbreaks <- xbreaks(data$x)
  if (is.function(ybreaks)) ybreaks <- ybreaks(data$y)
  xbreaks <- adjust_breaks(xbreaks, xopen)
  ybreaks <- adjust_breaks(xbreaks, yopen)
  
  xbin <- findInterval(data$x, xbreaks, all.inside = TRUE)
  ybin <- findInterval(data$y, ybreaks, all.inside = TRUE)
  
  xn <- length(xbreaks)
  yn <- length(ybreaks)
  
  bin <- (xbin - 1L) + (ybin - 1L) * (xn - 1L) + 1L
  count <- vaggregate(data$weight, bin, sum, na.rm = TRUE, 
    .default = 0, .n = (xn - 1L) * (yn - 1L))
  
  structure(data.frame(
    top =    rep(ybreaks[-yn], xn - 1), 
    bottom = rep(ybreaks[-1],  xn - 1), 
    left =   rep(xbreaks[-xn], each = yn - 1),
    right =  rep(xbreaks[-1],  each = yn - 1),
    count = count
  ), class = c("rect_bin", "data.frame"))
}

plot.rect_bin <- function(x, ...) {
  if (!require("scales")) {
    message("Scales package required for plotting 2d densities")
    return()
  }
  x <- subset(x, count > 0)
  
  xlim <- range(x$left, x$right)
  ylim <- range(x$top, x$bottom)
  col <- cscale(x$count, seq_gradient_pal(low = "grey95", high = "black"))
  
  plot(xlim, ylim, type = "n")
  with(x, rect(left, bottom, right, top, col = col, border = NA))
  with(x, text((left + right) / 2, (top + bottom) / 2, count, cex = 0.5))
}