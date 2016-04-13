



generate_heat <- function(X,
                          membership.rows = NULL,
                          membership.cols = NULL,
                          order.x = NULL, # order of variables
                          order.y = NULL, # order of observations
                          heat.col.scheme = c("red", "purple", "blue", "grey", "green"),
                          heat.pal = NULL,
                          heat.pal.values = NULL,
                          legend.size = 2,
                          axis.size = 10,
                          label.size = 10,
                          cluster.hline = TRUE,
                          cluster.vline = TRUE,
                          cluster.hline.size = 0.5,
                          cluster.vline.size = 0.5,
                          cluster.hline.col = "black",
                          cluster.vline.col = "black") {



  heat.col.scheme <- match.arg(heat.col.scheme)

  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the heatmap
  theme <- do.call(themes, themes.arg.list)
  theme_heatmap <- theme$theme_heatmap

  if (is.null(heat.pal)) {
    if (heat.col.scheme == "red") {
      heat.pal <- c("#ffeda0", "#feb24c", "#f03b20")
    } else if (heat.col.scheme == "purple") {
      heat.pal <- c("#efedf5", "#bcbddc", "#756bb1")
    } else if (heat.col.scheme == "blue") {
      heat.pal <- c("#deebf7", "#9ecae1", "#3182bd")
    } else if (heat.col.scheme == "grey") {
      heat.pal <- c("#f0f0f0", "#bdbdbd", "#636363")
    } else if (heat.col.scheme == "green") {
      heat.pal <- c("#e5f5e0", "#a1d99b", "#31a354")
    }
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }



  if (is.null(order.x)) {
    order.x <- 1:ncol(X)
  }
  if (is.null(order.y)) {
    order.y <- 1:nrow(X)
  }



  # need to order obs by same order as residuals residuals
  X.order <- X[order.y, order.x]
  # convert the x-y correlation matrix to a long-form "tidy" data frame
  X.df <- to_df(X.order)



  if (cluster.hline) {
    rlines <- c(0,cumsum(table(membership.rows)))
  }
  if (cluster.vline) {
    clines <- c(0,cumsum(table(membership.cols)))
  }


  # plot color limits:
  max.col <- max(X.df$value)
  min.col <- min(X.df$value)
  range.X <- seq(min.col, max.col, length = 100)
  breaks <- signif(as.vector(quantile(range.X)), 1)


  # define variables to fix visible binding check -- bit of a hack
  x <- X.df$x
  y <- X.df$y
  value <- X.df$value

  if (is.null(heat.pal.values)) {
    probs <- seq(0, 1, length = length(heat.pal))
    quantiles <- quantile(X.df$value, probs)
    heat.pal.values <- (quantiles - min(quantiles))/(max(quantiles) - min(quantiles))
  }
  # make the plot
  gg.legend <- ggplot2::ggplot(X.df) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y,
                                      fill = value,
                                      color = "black")) +
    #ggplot2::coord_fixed() +
    ggplot2::scale_fill_gradientn(values = heat.pal.values,
                                  colors = heat.pal,
                                  name = "",
                                  breaks = breaks) +
    ggplot2::scale_y_continuous(name = "", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = "", expand = c(0,0)) +
    theme_heatmap



  if (cluster.vline) {
    for (i in 1:length(clines)) {
      gg.legend <- gg.legend + ggplot2::geom_vline(xintercept = clines[i] + 0.5,
                                                   size = cluster.vline.size,
                                                   col = cluster.vline.col)
    }
  }
  if (cluster.hline) {
    for (i in 1:length(rlines)) {
      gg.legend <- gg.legend + ggplot2::geom_hline(yintercept = rlines[i] + 0.5,
                                                   size = cluster.hline.size,
                                                   col = cluster.hline.col)
    }
  }

  gg.heat <- gg.legend + ggplot2::theme(legend.position = "none")


  return(list(gg.heat = gg.heat, gg.legend = gg.legend))
}





generate_smooth_heat <- function(X,
                          membership.rows = NULL,
                          membership.cols = NULL,
                          order.x = NULL, # order of variables
                          order.y = NULL, # order of observations
                          heat.col.scheme = c("red", "purple", "blue", "grey", "green"),
                          heat.pal = NULL,
                          heat.pal.values = NULL,
                          legend.size = 2,
                          axis.size = 10,
                          label.size = 10,
                          cluster.hline = TRUE,
                          cluster.vline = TRUE,
                          cluster.hline.size = 0.5,
                          cluster.vline.size = 0.5,
                          cluster.hline.col = "black",
                          cluster.vline.col = "black") {



  heat.col.scheme <- match.arg(heat.col.scheme)

  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the heatmap
  theme <- do.call(themes, themes.arg.list)
  theme_heatmap <- theme$theme_heatmap

  if (is.null(heat.pal)) {
    if (heat.col.scheme == "red") {
      heat.pal <- c("#ffeda0", "#feb24c", "#f03b20")
    } else if (heat.col.scheme == "purple") {
      heat.pal <- c("#efedf5", "#bcbddc", "#756bb1")
    } else if (heat.col.scheme == "blue") {
      heat.pal <- c("#deebf7", "#9ecae1", "#3182bd")
    } else if (heat.col.scheme == "grey") {
      heat.pal <- c("#f0f0f0", "#bdbdbd", "#636363")
    } else if (heat.col.scheme == "green") {
      heat.pal <- c("#e5f5e0", "#a1d99b", "#31a354")
    }
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }


  # add membership columns and rows vector to X.smooth.df
  if (is.null(membership.rows)) {
    membership.rows <- 1:nrow(X)
  }
  if (is.null(membership.cols)) {
    membership.cols <- 1:ncol(X)
  }



  if (is.null(order.x)) {
    order.x <- 1:ncol(X)
  }
  if (is.null(order.y)) {
    order.y <- 1:nrow(X)
  }





  # need to order obs by same order as residuals residuals
  X.order <- X[order.y, order.x]
  # convert the x-y correlation matrix to a long-form "tidy" data frame
  X.df <- to_df(X.order)
  # get an x-y matrix for row clusters
  rclust.mat <- matrix(rep(membership.rows,
                           times = ncol(X)),
                       byrow = F, ncol = ncol(X))
  rclust.df <- to_df(rclust.mat)
  rclust.df$x <- as.numeric(as.character(rclust.df$x))
  rclust.df$y <- as.numeric(as.character(rclust.df$y))
  names(rclust.df) <- c("rclust", "x", "y")
  # get an x-y matrix for col clusters
  cclust.mat <- matrix(rep(membership.cols,
                           times = nrow(X)),
                       byrow = T, nrow = nrow(X))
  cclust.df <- to_df(cclust.mat)
  cclust.df$x <- as.numeric(as.character(cclust.df$x))
  cclust.df$y <- as.numeric(as.character(cclust.df$y))
  names(cclust.df) <- c("cclust", "x", "y")


  X.df <- dplyr::inner_join(X.df, rclust.df, by = c("x", "y"))
  X.df <- dplyr::inner_join(X.df, cclust.df, by = c("x", "y"))



  rlines <- c(0,cumsum(table(membership.rows)))
  clines <- c(0,cumsum(table(membership.cols)))



  # smooth X within clusters
  X.smooth.df <- X.df
  # add membership columns and rows vector to X.smooth.df
  X.smooth.df$rclust <- rclust.df$rclust
  X.smooth.df$cclust <- cclust.df$cclust


  # average within cluster boxes:
  X.smooth.df <- X.smooth.df %>% dplyr::group_by(cclust, rclust) %>%
    dplyr::summarize(value = median(value, na.rm = T))
  X.smooth.df <- dplyr::ungroup(X.smooth.df)


  # obtain ranges of the rectangles for the smoothed heatmap
  ymin <- rlines[-length(rlines)]
  names(ymin) <- NULL
  rclust <- unique(membership.rows)
  ymax <- rlines[-1]
  names(ymax) <- NULL
  xmin <- clines[-length(clines)]
  names(xmin) <- NULL
  cclust <- unique(membership.cols)
  xmax = clines[-1]
  names(xmax) <- NULL

  rect_cranges <- data.frame(xmin = xmin, xmax = xmax, cclust = cclust)
  rect_rranges <- data.frame(ymin = ymin, ymax = ymax, rclust = rclust)



  # reduce to smoothed data frame
  X.df <- X.smooth.df

  X.df <- dplyr::inner_join(X.df, rect_rranges, by = "rclust")
  X.df <- dplyr::inner_join(X.df, rect_cranges, by = "cclust")


  # plot color limits:
  max.col <- max(X.df$value)
  min.col <- min(X.df$value)
  range.X <- seq(min.col, max.col, length = 100)
  breaks <- signif(as.vector(quantile(range.X)), 1)






  # define variables to fix visible binding check -- bit of a hack
  xmax = X.df$xmax
  xmin = X.df$xmin
  ymax = X.df$ymax
  ymin = X.df$ymin
  value <- X.df$value

  if (is.null(heat.pal.values)) {
    probs <- seq(0, 1, length = length(heat.pal))
    quantiles <- quantile(X.df$value, probs)
    heat.pal.values <- (quantiles - min(quantiles))/(max(quantiles) - min(quantiles))
  }
  # make the plot
  gg.legend <- ggplot2::ggplot(X.df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax,
                                      fill = value)) +
    #ggplot2::coord_fixed() +
    ggplot2::scale_fill_gradientn(values = heat.pal.values,
                                  colors = heat.pal,
                                  name = "",
                                  breaks = breaks) +
    ggplot2::scale_y_continuous(name = "", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = "", expand = c(0,0)) +
    theme_heatmap




  if (cluster.vline) {
    for (i in 1:length(clines)) {
      gg.legend <- gg.legend + ggplot2::geom_vline(xintercept = clines[i],
                                                   size = cluster.vline.size,
                                                   col = cluster.vline.col)
    }
  }
  if (cluster.hline) {
    for (i in 1:length(rlines)) {
      gg.legend <- gg.legend + ggplot2::geom_hline(yintercept = rlines[i],
                                                   size = cluster.hline.size,
                                                   col = cluster.hline.col)
    }
  }

  gg.heat <- gg.legend + ggplot2::theme(legend.position = "none")


  return(list(gg.heat = gg.heat, gg.legend = gg.legend))
}











