---
output: html_document
---

# superheat: An R package for supervised heatmaps
# Testing some additional features 
7/20/2018 - farmic
* Issue: label color (left.label.col, bottom.label.col) was getting sorted by the row-order when it is meant for the membership/cluster. 
  * Added if-statement to only sort label color  when label is for "variable". 

7/18/2018 - farmic
* Issue: Wanted to display matrix with origin (0,0) on the top left corner, with ascending integer going downward on the y-axis. 
  * Added x.axis.reverse and y.axis.reverse to flip the heat-map and label's orientation. Have not tested with yr/yt options yet. 
* Issue: Wanted to smooth clusters with mean instead of median.
  * Added an option (smooth.heat.type) to allow using mean or median to smooth the cells when using smooth.heat. 

Normal Display of Matrix by membership   |  Reversed Y-axis
:-------------------------:|:-------------------------:
![mat](tools/Matrix.png) |  ![remat](tools/ReverseY_Matrix.png)


[![Travis-CI Build Status](https://travis-ci.org/rlbarter/superheat.svg?branch=master)](https://travis-ci.org/rlbarter/superheat)

*Superheat* is an R package for generating beautiful and customizable heatmaps. **See the [vignette](https://rlbarter.github.io/superheat/) for usage**.

If you find any bugs, have any feature requests or feedback, please submit an issue.


To download and install the development version of the package, use `devtools`:

```{r, eval = FALSE}
install.packages("devtools")
devtools::install_github("rlbarter/superheat")
```

Once you've installed the package, you can load the library in the standard way:

```{r}
library(superheat)
```



Basic usage:

```{r, fig.height = 9.5, fig.width = 8, fig.align = "center"}
superheat(mtcars,
          # normalize variables
          scale = T,
          # order rows/cols based on heirarchical clustering
          pretty.order.rows = TRUE,
          pretty.order.cols = TRUE,
          # plot miles per gallon next to the rows
          yr = mtcars$mpg,
          yr.axis.name = "miles per gallon",
          # plot correlation with mpg above columns
          yt = cor(mtcars)[, "mpg"],
          yt.plot.type = "bar",
          yt.axis.name = "correlation with mpg",
          # increase size of left labels
          left.label.size = 0.45)
```

![image1](tools/mtcars.png)
