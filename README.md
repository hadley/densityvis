# DensityVis

Tools for visualising densities, both discrete (binned histograms) and continuous (smooth density estimates).

All methods accept weights.

# Discrete densities

A discrete density is described by a tiling of the interval (1d) or plane (2d), along with a count of observations in each tile.

Binning data in 1d and 2d is tedious and tricky if you want to correctly deal with floating point (FP) issues. The `bin' package provides a fast and convenient interface for break calculation and binning in 1d and 2d.

* `bin_interval`: interval bins (1d)
* `bin_rect`: rectangular bins (2d) 
* `bin_hex`: hexagonal bins (2d)

# Continuous density

A discrete density is described by a function that maps location on the real interval or plane to the density at that location.

This package also provides two methods for continuous density estimation, `kernel_density` and `local_density`. `local_density` uses local regression as implemented in the `locfit` package, and provides a large number of options to control the output. However, it can be slow, so `kernel_density` provides a faster implementation that offers less control.

* `kernel_density_1d`
* `kernel_density_2d`
* `local_density_1d`
* `local_density_2d`

Both functions work with either 1d or 2d data, and both share a `grid` argument which specifies the locations where densities should be computed. This adds flexibility, allowing the function to be used to display the density over a regular grid, or just where the data points lie.