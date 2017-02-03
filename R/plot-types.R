clusteredPosition <- function(y.df, membership) {
  if (length(unique(membership)) == length(membership)) {
    y.df$membership <- factor(unique(membership))
  }

  # calculate the positions to be the center of each cluster
  # first identify each unique cluster and the number (size) of
  # data points in each cluster
  clust.boundary.df <- data.frame(cluster = unique(membership),
                                  size = as.vector(table(membership)))
  # fix visible binding note in package check
  size <- clust.boundary.df$size
  # calculate the position of the beginning and end of each cluster
  # (e.g. the grid lines on the heatmap), scaled by the
  # number of clusters
  clust.boundary.df <- clust.boundary.df %>%
    dplyr::mutate(increment = (size / sum(size)) * length(membership))
  # specify a vector consisting of the location (axis boundary) of
  # each boxplot
  boundary <- c(1, 1 + cumsum(clust.boundary.df$increment))
  # calculate the center of each boudary, removing the NA's at either end
  midpoints <- matrix(c(c(boundary, NA), c(NA, boundary)),
                      ncol = 2, byrow = F)
  midpoints <- apply(midpoints, 1, mean)[2:(length(boundary))]


  return(list(clust.boundary.df = clust.boundary.df,
              midpoints = midpoints, boundary = boundary))

}

setTicks <- function(plot.type, y.df, num.ticks) {
  if (plot.type %in% c("scatter", "boxplot", "scattersmooth",
                       "smooth", "line", "scatterline")) {
    # equally spaced positions from the min to the max value
    ticks <- seq(min(y.df$y, na.rm = T),
                 max(y.df$y, na.rm = T), length = num.ticks)
  } else if ((plot.type == "bar") && (min(y.df$y, na.rm = T) >= 0)) {
    # for a bar plot, if the minimum value is positive,
    # then make sure that the axis starts at 0, rather than the minimum
    ticks <- seq(0, max(y.df$y, na.rm = T), length = num.ticks)
  } else if ((plot.type == "bar") && (max(y.df$y, na.rm = T) <= 0)) {
    # for a bar plot, if the maximum value is negative,
    # then make sure that the axis ends at 0, rather than the maximum
    ticks <- seq(min(y.df$y, na.rm = T), 0, length = num.ticks)
  } else if ((plot.type == "bar") &&
             (max(y.df$y, na.rm = T) > 0) &&
             (min(y.df$y, na.rm = T) < 0)) {
    # for a bar plot, there are both positive and negative values,
    # the axis should range from the min to the max
    ticks <- seq(min(y.df$y, na.rm = T), max(y.df$y, na.rm = T),
                 length = num.ticks)
  }

  return(ticks)
}



basePlot <- function(location, plot.type, theme_top, theme_right, y.df) {
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
        theme_top
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
        theme_right

    }
  }
  return(gg.add)
}




setLimits <- function(gg.add, y.df, membership, plot.type, clustered.plot, y) {
  boundary <- clusteredPosition(y.df, membership)$boundary

  if (plot.type != "boxplot") {
    if ((plot.type == "bar") && !clustered.plot) {
      gg.add <- gg.add +
        ggplot2::scale_x_continuous(name = "",
                                    expand = c(0, 0))
    } else if (clustered.plot) {
      gg.add <- gg.add +
        ggplot2::scale_x_continuous(name = "",
                                    limits = c(min(boundary, na.rm = T),
                                               max(boundary, na.rm = T)),
                                    expand = c(0, 0))
    } else {
      gg.add <- gg.add +
        ggplot2::scale_x_continuous(name = "",
                                    expand = c(0, 0),
                                    limits = c(0.5, length(y) + 0.5))
    }
  }
  return(gg.add)
}



addScatter <- function(gg.add, y.df, n.clusters, y.obs.col, clustered.plot,
                       y.cluster.col, n.obs, point.size, point.alpha) {

  # fix visible binding
  y <- y.df$y
  x <- y.df$x
  membership <- y.df$membership
  # normal situation: individual-level points & no clustering
  if ((is.null(y.obs.col) && # did not proide a color vector
        !identical(unique(y.obs.col), "grey50"))) { # color vector has not been set
    # add scatterplot onto gg.add
    gg.add <- gg.add +
      ggplot2::geom_point(ggplot2::aes(x = x,
                                       y = y,
                                       col = factor(membership)),
                          size = point.size,
                          alpha = point.alpha) +
      ggplot2::scale_color_manual(values = rep(y.cluster.col,
                                               length = n.clusters))
  } else if (!clustered.plot && # clustered
             !is.null(y.obs.col)) { # color vector specified
    # if colour is specified by y.obs.col, implement this colour option
    gg.add <- gg.add +
      ggplot2::geom_point(ggplot2::aes(x = x,
                                       y = y),
                          size = point.size,
                          alpha = point.alpha,
                          col = rep(y.obs.col, length = n.obs))
  }
  return(gg.add)
}



addSmooth <- function(gg.add, y.df, membership, smoothing.method,
                      y.line.size, y.line.col,
                      smooth.se, y.cluster.col, n.clusters) {
  # fix visible binding
  y <- y.df$y
  x <- y.df$x
  
  if (length(unique(membership)) == length(membership)) {
    gg.add <- gg.add +
      ggplot2::stat_smooth(ggplot2::aes(x = x,
                                        y = y),
                           col = y.line.col,
                           method = smoothing.method,
                           size = y.line.size,
                           se = smooth.se)
  } else {
    gg.add <- gg.add +
      ggplot2::stat_smooth(ggplot2::aes(x = x,
                                        y = y,
                                        col = factor(membership)),
                           method = smoothing.method,
                           size = y.line.size,
                           se = smooth.se) +
      ggplot2::scale_colour_manual(values = rep(y.cluster.col,
                                                length = n.clusters))
  }
  return(gg.add)
}


addLine <- function(gg.add, y.df, membership, y.cluster.col,
                    n.clusters, y.line.size, y.line.col) {
  # fix visible binding
  y <- y.df$y
  x <- y.df$x
  # for the "line" plot type, add a smoothed line to the empty gg.add
  if (length(unique(membership)) == length(membership)) {
    gg.add <- gg.add +
      ggplot2::geom_line(ggplot2::aes(x = x,
                                      y = y),
                         size = y.line.size,
                         col = y.line.col)
  } else {
    gg.add <- gg.add +
      ggplot2::geom_line(ggplot2::aes(x = x,
                                      y = y,
                                      col = factor(membership)),
                         size = y.line.size) +
      ggplot2::scale_colour_manual(values = rep(y.cluster.col,
                                              length = n.clusters))
  }

  return(gg.add)
}



addBoxplot <- function(gg.add, y.df, membership, n.obs,
                       n.clusters, y.cluster.col) {
  cluster.info <- clusteredPosition(y.df, membership)
  midpoints <- cluster.info$midpoints
  boundary <- cluster.info$boundary
  # define the width of each cluster's boxplot
  # the values of boxplot.width must add up to the number of clusters
  # and the entries are proprotional to the number of observations in
  # each cluster
  boxplot.width <- 0.9 * cluster.info$clust.boundary.df$increment
  names(boxplot.width) <- unique(membership)
  # copy y.df in order to edit for the boxplots
  y.df.boxplot <- y.df
  # add a midpoints variable to y.df.boxplot
  # this specifies the location of each boxplot
  y.df.boxplot$midpoints <- factor(y.df.boxplot$membership)
  levels(y.df.boxplot$midpoints) <- midpoints

  # fix visible binding warning
  y <- y.df.boxplot$y 

  # add the boxplots one cluster at a time onto the empty gg.add object
  gg.add <- gg.add +
    plyr::llply(unique(membership), function(i) {
      ggplot2::geom_boxplot(ggplot2::aes(x = as.numeric(as.vector(midpoints)),
                                         y = y,
                                         fill = factor(membership)),
                            width = boxplot.width[i],
                            # restrict to each cluster seperately
                            data = subset(y.df.boxplot, membership == i))
    }) +
    ggplot2::scale_fill_manual(values = rep(y.cluster.col,
                                            length = n.clusters)) +
    ggplot2::scale_x_continuous(limits = c(min(boundary),
                                           max(boundary)),
                                expand = c(0, 0))
  return(gg.add)
}



addBar <- function(gg.add, y.df, membership, clustered.plot,
                   y.bar.col, y.cluster.col, n.clusters,
                   y.obs.col) {
  
  
  cluster.info <- clusteredPosition(y.df, membership)
  # for the "bar" plot type, add a bar plot to the empty gg.add
  # Case 1: no color specified for individual observations
  if (is.null(y.obs.col)) {
    if (clustered.plot) {
      
      y.df.bar <- y.df
      y.df.bar$width <- cluster.info$clust.boundary.df$increment
      # fix visible binding note in check()
      width <- y.df.bar$width
      x <- y.df.bar$x
      y <- y.df.bar$y

      gg.add <- gg.add +
        ggplot2::geom_bar(ggplot2::aes(x = x,
                                       y = y,
                                       fill = factor(membership),
                                       width = width),
                          col = y.bar.col,
                          position = ggplot2::position_dodge(0),
                          stat = "identity",
                          data = y.df.bar) +
        ggplot2::scale_fill_manual(values = rep(y.cluster.col,
                                                length = n.clusters))
    } else {
      gg.add <- gg.add +
        ggplot2::geom_bar(ggplot2::aes(x = x,
                                       y = y,
                                       fill = factor(membership)),
                          col = y.bar.col,
                          position = ggplot2::position_dodge(0),
                          stat = "identity",
                          width = 1) +
        ggplot2::scale_fill_manual(values = rep(y.cluster.col,
                                                length = n.clusters))
    }
  } else {
    # for the "bar" plot type, add a bar plot to the empty gg.add
    # Case 2: color specified for individual observations

    gg.add <- gg.add +
      ggplot2::geom_bar(ggplot2::aes(x = x,
                                     y = y),
                        fill = y.obs.col,
                        col = y.bar.col,
                        position = ggplot2::position_dodge(0),
                        stat = "identity",
                        width = 1)
  }

  # add a 0 intercept line for the barplots
  if ((min(y.df$y, na.rm = T) < 0) &&
      (max(y.df$y, na.rm = T) > 0)) {
    gg.add <- gg.add +
      ggplot2::geom_hline(yintercept = 0, col = "grey")
  }
  return(gg.add)
}
