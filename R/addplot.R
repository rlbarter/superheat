generate_add_on_plot <- function(X,
                                 y,
                                 location = c("top", "right"),
                                 membership,
                                 plot.type = c("scatter", "bar", "boxplot",
                                               "scattersmooth", "smooth",
                                               "scatterline", "line"),
                                 axis.name = NULL,
                                 col = NULL,
                                 axis.size = 10,
                                 axis.name.size = 10,
                                 axis.name.angle = NULL,
                                 num.ticks = 3,
                                 point.size = 2,
                                 point.alpha = 1,
                                 y.obs.col = NULL,
                                 y.cluster.col = NULL,
                                 y.bar.col = NULL,
                                 smoothing.method = c("loess", "lm"),
                                 y.line.size = NULL,
                                 y.line.col = NULL,
                                 smooth.se = TRUE) {

  
  # is the plot for clusters or for individual data points?
  if (((location == "top") && (length(y) != ncol(X))) |
      ((location == "right") && (length(y) != nrow(X)))) {
    clustered.plot <- T
  } else {
    clustered.plot <- F
  }
  
  # detect arguments
  plot.type <- match.arg(plot.type)

  
  
  # specify default axis name angle
  if (location == "top" && is.null(axis.name.angle)) {
    axis.name.angle <- 90
  }
  if (location == "right" && is.null(axis.name.angle)) {
    axis.name.angle <- 0
  }
  # specify default label colours
  if (is.null(y.obs.col) && is.null(y.cluster.col)) {
    y.cluster.col <- c("Grey 61", "Grey 43")
  }
  # specify default geom_bar outline color
  if (is.null(y.bar.col)) {
    y.bar.col <- "white"
  }
  # specify default line size
  if (is.null(y.line.size)) {
    y.line.size <- 1
  }
  # specify default line size
  if (is.null(y.line.col)) {
    y.line.col <- "grey61"
  }

  # hack for visible bindings
  n <- num.ticks

  # define an id variable corresponding to the length of the vector
  # whose purpose is to remember the original ordering of the vector
  id <- 1:length(y)

  # extract the defined ggplot2 themes
  # identify all variables defined in the current environment
  themes.arg.list <- c(as.list(environment()))
  # identify the variables that are also arguments for the themes() function
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  # remove variables that are not arguments for the themes() function
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]
  # define the theme for the heatmap
  theme <- do.call(themes, themes.arg.list)
  theme_top <- theme$theme_top
  theme_right <- theme$theme_right

  # define a dataframe whose columns are
  # y: the vector values
  # x: the id variable (original position of each value)
  # membership: the cluster membership for each position/obs
  # col: the colour of each data point
  y.df <- data.frame(y = y, x = id)
  if (clustered.plot) {
    y.df$membership <- levels(factor(membership))
  } else {
    y.df$membership <- factor(membership)
  }
  
  if (!is.null(y.obs.col)) {
    y.df$col <- y.obs.col
  }

  # if y is not being provided for all observations
  # (then it must be for clusters)
  # We need to make sure each value is plotted in the center of the cluster
  if (clustered.plot && (length(y) != nrow(X))) {
    
   midpoints <- clusteredPosition(y.df, membership)$midpoints
   # add position to y.df data frame
   y.df$x <- midpoints
  }
  
  # fix visible binding note
  x <- y.df$x
  y <- y.df$y
  
  # define the location of the ticks for the axis
  # for all plots other than a bar plot:
  # (for a bar plot, rather than starting from the minimum value,
  #  we want to start from 0, etc)
  ticks <- setTicks(plot.type, y.df, num.ticks) 

  # define number of clusters
  n.clusters <- length(unique(membership))
  n.obs <- length(membership)

  # define the base of the plot
  gg.add <- basePlot(location, plot.type, theme_top, theme_right, y.df)
  
  # make sure the additional plots are the correct distance from the edges
  limits.arg.list <- c(as.list(environment()))
  limits.arg.list <- limits.arg.list[names(formals(setLimits))]
  limits.arg.list <- limits.arg.list[!is.na(names(limits.arg.list))]
  gg.add <- do.call(setLimits, limits.arg.list)
  
  # define pretty breaks
  pretty.breaks <- scales::pretty_breaks(n = num.ticks, min.n = 3)(ticks)[-1]
  # clean up the axis breaks and name
  gg.add <- gg.add +
    ggplot2::scale_y_continuous(breaks = pretty.breaks,
                                name = paste(axis.name),
                                expand = c(0.05, 0.05))

  # Case 1: scatterplots when the observations are not coloured by y.obs.col
  if (plot.type == "scatter") {
    scatter.arg.list <- c(as.list(environment()))
    scatter.arg.list <- scatter.arg.list[names(formals(addScatter))]
    scatter.arg.list <- scatter.arg.list[!is.na(names(scatter.arg.list))]
    gg.add <- do.call(addScatter, scatter.arg.list)
  } else if (plot.type == "scattersmooth") {
    # add the scatterplot
    scatter.arg.list <- c(as.list(environment()))
    scatter.arg.list <- scatter.arg.list[names(formals(addScatter))]
    scatter.arg.list <- scatter.arg.list[!is.na(names(scatter.arg.list))]
    gg.add <- do.call(addScatter, scatter.arg.list)
    # add the smoothing line
    smooth.arg.list <- c(as.list(environment()))
    smooth.arg.list <- smooth.arg.list[names(formals(addSmooth))]
    smooth.arg.list <- smooth.arg.list[!is.na(names(smooth.arg.list))]
    gg.add <- do.call(addSmooth, smooth.arg.list)
  } else if (plot.type == "smooth") {
    smooth.arg.list <- c(as.list(environment()))
    smooth.arg.list <- smooth.arg.list[names(formals(addSmooth))]
    smooth.arg.list <- smooth.arg.list[!is.na(names(smooth.arg.list))]
    gg.add <- do.call(addSmooth, smooth.arg.list)
  } else if (plot.type == "line") {
    line.arg.list <- c(as.list(environment()))
    line.arg.list <- line.arg.list[names(formals(addLine))]
    line.arg.list <- line.arg.list[!is.na(names(line.arg.list))]
    gg.add <- do.call(addLine, line.arg.list)
  } else if (plot.type == "scatterline") {
    # add the scatterplot
    scatter.arg.list <- c(as.list(environment()))
    scatter.arg.list <- scatter.arg.list[names(formals(addScatter))]
    scatter.arg.list <- scatter.arg.list[!is.na(names(scatter.arg.list))]
    gg.add <- do.call(addScatter, scatter.arg.list)
    # add the line
    line.arg.list <- c(as.list(environment()))
    line.arg.list <- line.arg.list[names(formals(addLine))]
    line.arg.list <- line.arg.list[!is.na(names(line.arg.list))]
    gg.add <- do.call(addLine, line.arg.list)
  } else if (plot.type == "bar") {
    bar.arg.list <- c(as.list(environment()))
    bar.arg.list <- bar.arg.list[names(formals(addBar))]
    bar.arg.list <- bar.arg.list[!is.na(names(bar.arg.list))]
    gg.add <- do.call(addBar, bar.arg.list)
  } else if (plot.type == "boxplot") {
    box.arg.list <- c(as.list(environment()))
    box.arg.list <- box.arg.list[names(formals(addBoxplot))]
    box.arg.list <- box.arg.list[!is.na(names(box.arg.list))]
    gg.add <- do.call(addBoxplot, box.arg.list)
  }
  
  # rotate axis name
  gg.add <- gg.add +
    ggplot2::theme(axis.title.y =
                     ggplot2::element_text(angle = axis.name.angle))

  return(gg.add)
}
