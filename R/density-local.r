#' 1d local density estimate.
#'
#' This convenience function wraps up the important arguments of 
#' \code{\link[locfit]{locfit}} and \code{\link[locfit]{lp}}, and hopefully
#' provides enough documentation so that you don't need to refer to the 
#' original documentation and book.
#'
#' Adaptive penalties are not available in this convenience function because 
#' they typically require a two-stage fit, where the first fit is used to 
#' estimate the local variance.
#'
#' If you recieve an error \code{newsplit: out of vertex space}, that 
#' indicates that your bandwidth is too small.
#'
#' @param x a numeric vector of x positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param nn span, or nearest neighbour fraction, the proportion of the total
#'   observations used in predict 
#' @param h constant bandwidth. Use if you want a constant bandwith, rather
#'   than a bandwidth that varies. If both \code{nn} and \code{h}
#'   are set, each is calculated for each location and the largest is used.
#'   If \code{scale = TRUE} the unit of this value is standard deviations,
#'   otherwise, it's in the units of the data.
#' @param kernel Weight function, default is tricubic. Other choices are
#'   \code{"rect"}, '"trwt"', '"tria"', '"epan"', '"bisq"' and
#'   '"gauss"'. Choices may be restricted when derivatives are required;
#'   e.g. for confidence bands and some bandwidth selectors.
#' @param degree degree of polynomial used for smoothing
#' @param bounded if \code{TRUE} bounds density to range of data, otherwise
#'    it takes non-zero values over the entire real line.
#' @param scale if \code{TRUE} rescale \code{x} values to have standard 
#'   deviation 1.
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @return a function of a single that returns the density at that location
#' @seealso "Local Regression and Likelihood," C. Loader.  Springer, 1999.
#' @export
#' @examples
#' data(baseball, package = "plyr")
#' dens <- local_density_1d(baseball$g)
#' plot(dens)
#' 
#' # nn is like span in loess - it's a proportion
#' plot(local_density_1d(baseball$g, nn = 0.1))
#' plot(local_density_1d(baseball$g, nn = 0.2))
#' plot(local_density_1d(baseball$g, nn = 0.5))
#' plot(local_density_1d(baseball$g, nn = 1))
#'
#' # when bounded = TRUE assumes that there are no possible values outside
#' # of the data range
#' plot(local_density_1d(baseball$g, nn = 0.2))
#' plot(local_density_1d(baseball$g, nn = 0.2, bounded = TRUE))
#' 
#' # When scale = FALSE, the bandwidth is specified in terms of the
#' # range of the original data.  When scale = TRUE, the data has been
#' # scaled to have sd 1
#' plot(local_density_1d(baseball$g, h = 0.5, nn = 0, scale = TRUE))
#' plot(local_density_1d(baseball$g, h = 0.5, nn = 0, scale = FALSE))
local_density_1d <- function(x, weight = NULL, scale = TRUE, nn = 0.7, h = 0, kernel = "tcub", degree = 2, bounded = FALSE, na.rm = FALSE) {
  require("locfit")
  
  data <- clean_x(x, weight, na.rm = na.rm)
  
  xlim <- if (bounded) range(x) else NULL
  mod <- locfit(~ lp(x, scale = scale, nn = nn, h = h, deg = degree), 
    data = data, weight = weight,
    kern = kernel, xlim = xlim, maxk = 1000)
  
  predictd <- function(x) predict(mod, data.frame(x = x))
  structure(predictd, xlim = range(data$x), class = "density_1d")
}

#' 2d local density estimate.
#'
#' @param x a numeric vector of x positions
#' @param y a numeric vector of y positions
#' @param weight \code{NULL} or a numeric vector providing weights for each
#'   observation
#' @param h constant bandwidth. Use if you want a constant bandwith, rather
#'   than a bandwidth that varies. If both \code{nn} and \code{h}
#'   are set, each is calculated for each location and the largest is used.
#'   If \code{scale = TRUE} the unit of this value is standard deviations,
#'   otherwise, it's in the units of the data.
#' @param nn A vector of length two, giving the span, or nearest neighbour
#'   fraction, the proportion of the total used to compute the density. 
#' @param scale should x and y positions be scaled independently?  Use
#'  \code{FALSE} when variables are measured on the same original scale.
#' @param kernel Weight function, default is tricubic. Other choices are
#'   \code{"rect"}, '"trwt"', '"tria"', '"epan"', '"bisq"' and
#'   '"gauss"'. Choices may be restricted when derivatives are required;
#'   e.g. for confidence bands and some bandwidth selectors.
#' @param degree degree of polynomial used for smoothing
#' @param bounded if \code{TRUE} bounds density to range of data, otherwise
#'    it takes non-zero values over the entire real plane.
#' @param na.rm If \code{TRUE} missing values will be silently removed, 
#'   otherwise they will be removed with a warning.
#' @export
#' @examples
#' data(baseball, package = "plyr")
#' dens <- local_density_2d(baseball$g, baseball$ab)
#' plot(dens)
#'
#' plot(local_density_2d(baseball$g, baseball$ab, nn = 0.4))
#' plot(local_density_2d(baseball$g, baseball$ab, nn = 0.1))
#' plot(local_density_2d(baseball$g, baseball$ab, bounded = TRUE))
local_density_2d <- function(x, y, weight = NULL, scale = TRUE, nn = 0.7, h = 0, kernel = "tcub", degree = 2, bounded = FALSE, na.rm = FALSE) {
  require("locfit")
  
  data <- clean_xy(x, y, weight, na.rm = na.rm)
  
  xlim <- if (bounded) rbind(range(data$x), range(data$y)) else NULL
  mod <- locfit(~ lp(x, y, scale = scale, nn = nn, h = h, deg = degree), 
    data = data, weight = weight,
    kern = kernel, xlim = xlim, maxk = 1000)
    
  predictd <- function(x, y) predict(mod, newdata = cbind(x, y))
  structure(predictd, class = "density_2d",
    xlim = range(x),
    ylim = range(y))
}
