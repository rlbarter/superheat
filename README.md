# superheat: An R package for supervised heatmaps

*superheat* is an R package for generating supervised heatmaps. Supervised heatmaps serve two primary purposes: (1) visualization of complex data and (2) visualization of model fit. See the [vignette](https://cdn.rawgit.com/rlbarter/superheat/master/vignettes/Vignette.html) for usage.

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