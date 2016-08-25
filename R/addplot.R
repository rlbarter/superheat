generate_add_on_plot <- function(y,
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
                                 y.pal = NULL,
                                 y.bar.col = NULL,
                                 smoothing.method = c("loess", "lm"),
                                 y.line.size = NULL,
                                 smooth.se = TRUE) {

  # detect arguments
  plot.type <- match.arg(plot.type)

  # specify default axis name angle
  if (location == "top" && is.null(axis.name.angle)) {
    axis.name.angle <- 90
  }
  if (location == "right" && is.null(axis.name.angle)) {
    axis.name.angle <- 0
  }
  # specify default label colors
  if (is.null(y.obs.col) && is.null(y.pal)) {
    y.pal <- c("Grey 61", "Grey 43")
  }
  # specify default geom_bar outline color
  if (is.null(y.bar.col)) {
    y.bar.col <- "white"
  }
  # specify default line size
  if (is.null(y.line.size)) {
    y.line.size <- 1
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
  y.df$membership <- factor(membership)
  if (!is.null(y.obs.col)) {
    y.df$col <- y.obs.col
  }

  # fix visible binding note
  x <- y.df$x
  y <- y.df$y

  # define the location of the ticks for the axis
  # for all plots other than a bar plot:
  # (for a bar plot, rather than starting from the minimum value,
  #  we want to start from 0, etc)
  if (plot.type %in% c("scatter", "boxplot", "scattersmooth",
                       "smooth", "line", "scatterline")) {
    # equally spaced positions from the min to the max value
    ticks <- seq(min((y.df$y)), max((y.df$y)), length = num.ticks)
  } else if ((plot.type == "bar") && (min(y.df$y) > 0)) {
    # for a bar plot, if the minimum value is positive,
    # then make sure that the axis starts at 0, rather than the minimum
    ticks <- seq(0, max(y.df$y), length = num.ticks)
  } else if ((plot.type == "bar") && (max(y.df$y) < 0)) {
    # for a bar plot, if the maximum value is negative,
    # then make sure that the axis ends at 0, rather than the maximum
    ticks <- seq(min(y.df$y), 0, length = num.ticks)
  } else if ((plot.type == "bar") && (max(y.df$y) > 0) && (min(y.df$y) < 0)) {
    # for a bar plot, there are both positive and negative values,
    # the axis should range from the min to the max
    ticks <- seq(min(y.df$y), max(y.df$y), length = num.ticks)
  }

  # define number of clusters
  n.clusters <- length(unique(membership))
  n.obs <- length(membership)

  # top plot
  if (location == "top") {

    # for boxplots, we need to make sure that each boxplot
    # is centered over it's cluster
    if (plot.type == "boxplot") {
      # initiate an empty ggplot object with the top theme
      gg.add <- ggplot2::ggplot(y.df) +
        theme_top
    } else {
      # for all other plot types, the x-axis is the standard x-axis
      gg.add <- ggplot2::ggplot(y.df) +
        theme_top +
        ggplot2::scale_x_continuous(name = "", expand = c(0.01, 0.01))
    }
  } else if (location == "right") {
      # for boxplots, we need to make sure that each boxplot
      # is centered over it's cluster
      if (plot.type == "boxplot") {
        # initiate an empty ggplot object with the right theme
        gg.add <- ggplot2::ggplot(y.df) +
          # flip the coordinates so that the left vertical coordinate is
          # the x-axis
          ggplot2::coord_flip() +
          theme_right
      } else {
        # for all other plot types, the x-axis is the standard x-axis
        gg.add <- ggplot2::ggplot(y.df) +
          # flip the coordinates so that the left vertical coordinate is
          # the x-axis
          ggplot2::coord_flip() +
          theme_right +
          ggplot2::scale_x_continuous(name = "", expand = c(0.01, 0.01))
      }
  }

  # define pretty breaks
  pretty.breaks <- scales::pretty_breaks(n = num.ticks, min.n = 3)(ticks)[-1]
  # clean up the axis breaks and name
  gg.add <- gg.add +
    ggplot2::scale_y_continuous(breaks = pretty.breaks,
                                name = paste(axis.name),
                                expand = c(0.05, 0.05))

  # Case 1: scatterplots when the observations are not coloured by y.obs.col
  if (is.null(y.obs.col) &&
      (plot.type %in% c("scatter", "scattersmooth", "scatterline"))) {
    # add scatterplot onto gg.add
    gg.add <- gg.add +
      ggplot2::geom_point(ggplot2::aes(x = x,
                                       y = y,
                                       col = factor(membership)),
                          size = point.size,
                          alpha = point.alpha) +
      ggplot2::scale_color_manual(values = rep(y.pal, length = n.clusters))
    if (plot.type == "scattersmooth") {
      # if specified, add a smoothed line, curve, etc
      # (determined by smoothing.method)
      gg.add <- gg.add +
        ggplot2::stat_smooth(ggplot2::aes(x = x,
                                          y = y,
                                          col = factor(membership)),
                             method = smoothing.method,
                             size = y.line.size,
                             se = smooth.se)
    }
    if (plot.type == "scatterline") {
      # if specified, add lines connecting the points
      gg.add <- gg.add +
        ggplot2::geom_line(ggplot2::aes(x = x,
                                        y = y,
                                        col = factor(membership)),
                           size = y.line.size)
    }

  } else if (!is.null(y.obs.col) &&
             (plot.type %in% c("scatter","scattersmooth","scatterline"))) {
    # if colour is specified by y.obs.col, implement this colour option
    gg.add <- gg.add +
      ggplot2::geom_point(ggplot2::aes(x = x,
                                       y = y,
                                       factor(col, levels = unique(col))),
                          size = point.size,
                          alpha = point.alpha) +
      ggplot2::scale_color_manual(values = unique(y.obs.col))
    if (plot.type == "scattersmooth") {
      gg.add <- gg.add +
        ggplot2::stat_smooth(ggplot2::aes(x = x,
                                          y = y,
                                          col = factor(col,
                                                       levels = unique(col))),
                             method = smoothing.method,
                             size = y.line.size,
                             se = smooth.se)
    }
    if (plot.type == "scatterline") {
      gg.add <- gg.add +
        ggplot2::geom_line(ggplot2::aes(x = x,
                                        y = y,
                                        col = factor(col,
                                                     levels = unique(col))),
                           size = y.line.size)
    }
  } else if (plot.type == "smooth") {
    # for the "smooth" plot type, add a smoothed curve to the empty gg.add
      gg.add <- gg.add +
        ggplot2::stat_smooth(ggplot2::aes(x = x,
                                          y = y,
                                          col = factor(membership)),
                             method = smoothing.method,
                             size = y.line.size,
                             se = smooth.se) +
        ggplot2::scale_color_manual(values = rep(y.pal, length = n.clusters))
  } else if (plot.type == "line") {
    # for the "line" plot type, add a smoothed line to the empty gg.add
    gg.add <- gg.add +
      ggplot2::geom_line(ggplot2::aes(x = x,
                                      y = y,
                                      col = factor(membership)),
                         size = y.line.size) +
      ggplot2::scale_color_manual(values = rep(y.pal, length = n.clusters))
    } else if (plot.type == "boxplot") {
      # calculate the boxplot positions to be the center of each cluster
      # first identify each unique cluster and the number (size) of
      # data points in each cluster
      clust.boundary.df <- data.frame(cluster = unique(membership),
                                      size = as.vector(table(membership)))
      # calculate the position of the beginning and end of each cluster
      # (e.g. the grid lines on the heatmap), scaled by the
      # number of clusters
      clust.boundary.df <- clust.boundary.df %>%
        dplyr::mutate(increment = (size / sum(size)) * n())
      # specify a vector consisting of the location (axis boundary) of
      # each boxplot
      boundary <- c(1, 1 + cumsum(clust.boundary.df$increment))
      # calculate the center of each boudary, removing the NA's at either end
      midpoints <- matrix(c(c(boundary, NA), c(NA, boundary)),
                          ncol = 2, byrow = F)
      midpoints <- apply(midpoints, 1, mean)[2:(length(boundary))]

      # define the width of each cluster's boxplot
      # the values of boxplot.width must add up to the number of clusters
      # and the entries are proprotional to the number of observations in
      # each cluster
      boxplot.width <- table(membership) / (n.obs / n.clusters)
      # copy y.df in order to edit for the boxplots
      y.df.boxplot <- y.df
      # add a midpoints variable to y.df.boxplot
      # this specifies the location of each boxplot
      y.df.boxplot$midpoints <- factor(y.df.boxplot$membership)
      levels(y.df.boxplot$midpoints) <- midpoints
      # add the boxplots one cluster at a time onto the empty gg.add object
      gg.add <- gg.add +
        plyr::llply(unique(membership), function(i) {
          ggplot2::geom_boxplot(ggplot2::aes(x = as.numeric(as.vector(midpoints)),
                                             y = y,
                                             fill = factor(membership)),
                                width = boxplot.width[i],
                                # restrict to each cluster seperately
                                data = subset(y.df.boxplot, membership == i))
          }
        ) +
        ggplot2::scale_fill_manual(values = rep(y.pal,
                                                length = n.clusters)) +
        ggplot2::scale_x_continuous(limits = c(min(boundary),
                                               max(boundary)),
                                    expand = c(0, 0))
      # remove the y.df.boxplot object
      rm(y.df.boxplot)
  } else if (is.null(y.obs.col) && (plot.type == "bar")) {
    # for the "bar" plot type, add a bar plot to the empty gg.add
    # Case 1: no color specified for individual observations
    gg.add <- gg.add +
      ggplot2::geom_bar(ggplot2::aes(x = x,
                                     y = y,
                                     fill = factor(membership)),
                        col = y.bar.col,
                        position = ggplot2::position_dodge(0),
                        stat = "identity",
                        width = 1) +
      ggplot2::scale_fill_manual(values = rep(y.pal,
                                              length = n.clusters))
  } else if (!is.null(y.obs.col) && (plot.type == "bar")) {
    # for the "bar" plot type, add a bar plot to the empty gg.add
    # Case 2: color specified for individual observations
    gg.add <- gg.add +
      ggplot2::geom_bar(ggplot2::aes(x = x,
                                     y = y,
                                     fill = factor(membership)),
                        col = y.bar.col,
                        position = ggplot2::position_dodge(0),
                        stat = "identity",
                        width = 1) +
      ggplot2::scale_fill_manual(values = unique(y.obs.col))
  }

  # add a 0 intercept line for the barplots
  if ((plot.type == "bar") && (min(y.df$y) < 0) && (max(y.df$y) > 0)) {
    gg.add <- gg.add +
      ggplot2::geom_hline(yintercept = 0, col = "grey")
  }

  # rotate axis name
  gg.add <- gg.add +
    ggplot2::theme(axis.title.y =
                     ggplot2::element_text(angle = axis.name.angle))

  return(gg.add)
}
