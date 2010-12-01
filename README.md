# DensityVis

Tools for displaying local density, both rough (binned) and smooth (local density estimation, using the `locfit' package):

All methods accept weights.

# Binning

Binning data in 1d and 2d is tedious and tricky if you want to correctly deal with floating point (FP) issues. The `bin' package provides a convenient interface for break calculation and binning in 1d and 2d.

* interval (1d) bins
* square (2d) bins
* hexagon (2d) bins

# Local density estimation

* 1d density, on grid 
* 1d density, at data location
* 2d density, on grid
* 2d density, at data location

There are many other ways to compute local density estimates - previously `ggplot2` used kernel density estimates provided by `density` and `MASS::kde2d`. These are no longer used because they are not very flexible.
