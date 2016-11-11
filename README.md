# superheat: An R package for supervised heatmaps

[![Travis-CI Build Status](https://travis-ci.org/rlbarter/superheat.svg?branch=master)](https://travis-ci.org/rlbarter/superheat)

*Superheat* is an R package for generating beautiful and customizable heatmaps. See the [vignette](https://rlbarter.github.io/superheat/) for usage.

If you find any bugs, have any feature requests or feedback, please submit an issue.


To download and install the development version of the package, use `devtools`:

``` r
install.packages("devtools")
devtools::install_github("rlbarter/superheat")
```

Once you've installed the package, you can load the library in the standard way:

``` r
library(superheat)
```



Basic usage:

``` r
superheat(iris[,-5])
```
