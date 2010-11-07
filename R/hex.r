hex_bin <- function(x, y, binwidth = hex_binwidth(range(x), range(y))) {
  # Convert binwidths into bounds + nbins
  xbnds <- c(
    round_any(min(x), binwidth[1], floor) - 1e-6, 
    round_any(max(x), binwidth[1], ceiling) + 1e-6
  )
  xbins <- diff(xbnds) / binwidth[1]

  ybnds <- c(
    round_any(min(y), binwidth[1], floor) - 1e-6, 
    round_any(max(y), binwidth[2], ceiling) + 1e-6
  )
  ybins <- diff(ybnds) / binwidth[2]
  
  # Call hexbin
  hb <- hexbin(
    x, xbnds = xbnds, xbins = xbins,  
    y, ybnds = ybnds, shape = ybins / xbins, 
  )
  
  # Convert to data frame
  data.frame(
    hcell2xy(hb), 
    count = hb@count, 
    density = hb@count / sum(hb@count, na.rm=TRUE)
  )
}

hex_binwidth <- function(xrange, yrange, bins) {
   c( 
     diff(xrange) / bins,
     diff(yrange) / bins
   )
}