# Generate sequence of fixed size intervals covering range
# All locations are multiples of size
# 
# @arguments range
# @arguments interval size
# @keyword internal
# @seealso \code{\link{reshape}{round_any}}
fullseq <- function(range, size, pad = FALSE) {
  if (diff(range) < 1e-6) return(c(range[1] - size / 2, range[1] + size / 2))
  
  x <- seq(
    round_any(range[1], size, floor), 
    round_any(range[2], size, ceiling), 
    by=size
  )
  
  if (pad) {
    # Add extra bin on bottom and on top, to guarantee that we cover complete
    # range of data, whether right = T or F
    c(min(x) - size, x, max(x) + size)
  } else {
    x
  }  
}
