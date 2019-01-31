generate_heat <- function(X,
                          smooth.heat = FALSE,
                          smooth.heat.type = "median", # default to median
                          X.text = NULL,
                          X.text.size = 5,
                          X.text.angle = 0,
                          X.text.col = "black",

                          x.axis.reverse = F,
                          y.axis.reverse = F,

                          membership.rows = NULL,
                          membership.cols = NULL,
                          order.x = NULL, # order of variables
                          order.y = NULL, # order of observations
                          heat.col.scheme = c("viridis", "red", "purple",
                                              "blue", "grey", "green"),
                          heat.pal = NULL,
                          heat.pal.values = NULL,
                          heat.na.col = "grey50",
                          heat.lim = NULL,
                          extreme.values.na = TRUE,
                          legend.height = 0.1,
                          legend.width = 1.5,
                          legend.text.size = 12,
                          legend.num.ticks = 4,
                          legend.breaks = NULL,
                          axis.size = 10,
                          label.size = 10,
                          grid.hline = TRUE,
                          grid.vline = TRUE,
                          grid.hline.size = 0.5,
                          grid.vline.size = 0.5,
                          grid.hline.col = "black",
                          grid.vline.col = "black") {

  # obtain arguments
  heat.col.scheme <- match.arg(heat.col.scheme)
  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]
  # define the theme for the heatmap
  theme <- do.call(themes, themes.arg.list)
  theme_heatmap <- theme$theme_heatmap
  # set the default heatmap color options
  if (is.null(heat.pal)) {
    if (heat.col.scheme == "viridis") {
      heat.pal <- c("#440154FF", "#3B528BFF",
                    "#21908CFF", "#5DC863FF", "#FDE725FF")
    }
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

  # convert X to a matrix
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  # set the order of the columns and rows if not provided
  if (is.null(order.x)) {
    order.x <- 1:ncol(X)
  }
  if (is.null(order.y)) {
    order.y <- 1:nrow(X)
  }
  # order rows and columns by orderings provided
  X.order <- X[order.y, order.x]
  # convert the matrix to a long-form "tidy" data frame
  X.df <- matrixToDataFrame(X.order)

  # define the positions of the horizontal and
  # vertical grid lines based on the clusters
  if (grid.hline) {
    rlines <- c(0, cumsum(table(membership.rows)))
  }
  if (grid.vline) {
    clines <- c(0, cumsum(table(membership.cols)))
  }

  # plot color limits:
  if (is.null(heat.lim)) {
    max.col <- max(X.df$value, na.rm = T)
    min.col <- min(X.df$value, na.rm = T)
  } else {
    max.col <- heat.lim[2]
    min.col <- heat.lim[1]
    if (!extreme.values.na) {
      # if extreme.values.na = FALSE then make values larger than the
      # maximum range equal to the max color (rather than grey)
      X.df$value[X.df$value > max.col] <- max.col
      # if extreme.values.na = FALSE then make values smaller than the
      # minimum range equal to the min color (rather than grey)
      X.df$value[X.df$value < min.col] <- min.col
    }

  }

  range.X <- seq(min.col, max.col, length = 1000)

  # specify location of legend breaks
  if (is.null(legend.breaks)) {
    # make the breaks nicely spread
    breaks <- pretty(range.X, n = legend.num.ticks)
  } else {
    breaks <- legend.breaks
  }

  # define variables to fix visible binding check -- bit of a hack
  x <- X.df$x
  y <- X.df$y
  value <- X.df$value

  # make the default color transitions occur at the appropriate
  # quantiles of the data matrix
  if (is.null(heat.pal.values)) {
    probs <- seq(0, 1, length = length(heat.pal))
    quantiles <- quantile(X.df$value, probs, na.rm = T)
    heat.pal.values <- (quantiles - min(quantiles)) /
      (max(quantiles) - min(quantiles))
  }
  # make the plot
  gg.legend <- ggplot2::ggplot(X.df) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y,
                                      fill = value)) +
    ggplot2::scale_fill_gradientn(values = heat.pal.values,
                                  colours = heat.pal,
                                  name = "",
                                  breaks = breaks,
                                  na.value = heat.na.col,
                                  limits = heat.lim) +
    # ggplot2::scale_x_continuous(name = "", expand = c(0, 0)) +
    # ggplot2::scale_y_continuous(name = "", expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = legend.width * 10,
                                                   barheight = legend.height * 10)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = legend.text.size)) +
    theme_heatmap

  #### Origin flip ####
  if (x.axis.reverse) {
    gg.legend <- gg.legend + ggplot2::scale_x_reverse(name = "", expand = c(0, 0))
  } else {
    gg.legend <- gg.legend + ggplot2::scale_x_continuous(name = "", expand = c(0, 0))
  }

  if (y.axis.reverse) {
    gg.legend <- gg.legend + ggplot2::scale_y_reverse(name = "", expand = c(0, 0))
  } else {
    gg.legend <- gg.legend + ggplot2::scale_y_continuous(name = "", expand = c(0, 0))
  }

  # add grid lines if desired
  if (grid.vline) {
    for (i in 1:length(clines)) {
      gg.legend <- gg.legend +
        ggplot2::geom_vline(xintercept = clines[i] + 0.5,
                            size = grid.vline.size,
                            col = grid.vline.col)
    }
  }
  if (grid.hline) {
    for (i in 1:length(rlines)) {
      gg.legend <- gg.legend +
        ggplot2::geom_hline(yintercept = rlines[i] + 0.5,
                            size = grid.hline.size,
                            col = grid.hline.col)
    }
  }

  gg.heat <- gg.legend + ggplot2::theme(legend.position = "none")

  # if a text matrix is provided, plot it
  if (!is.null(X.text)) {

    col.values <- unique(as.vector(X.text.col))
    names(col.values) <- unique(as.vector(X.text.col))

    gg.heat <- gg.heat +
      generate_text_heat(X = X,
                         X.text = X.text,
                         X.text.size = X.text.size,
                         X.text.angle = X.text.angle,
                         X.text.col = X.text.col,
                         smooth.heat = smooth.heat,
                         membership.rows = membership.rows,
                         membership.cols = membership.cols) +
      ggplot2::scale_colour_manual(values = col.values) +
      ggplot2::scale_size(range = c(min(X.text.size), max(X.text.size)))

      if (x.axis.reverse) {
        gg.heat <- gg.heat + ggplot2::scale_x_reverse()
      }
      if (y.axis.reverse) {
        gg.heat <- gg.heat + ggplot2::scale_y_reverse()
      }
  }

  return(list(gg.heat = gg.heat, gg.legend = gg.legend,
              heat.pal.values = heat.pal.values))
}





generate_smooth_heat <- function(X,
                                 X.text = NULL,
                                 X.text.size = 5,
                                 X.text.angle = 0,
                                 X.text.col = "black",
                                 heat.lim = NULL,
                                 extreme.values.na = TRUE,
                                 smooth.heat = TRUE,
                                 smooth.heat.type = "median",
                                 membership.rows = NULL,
                                 membership.cols = NULL,
                                 order.x = NULL, # order of variables
                                 order.y = NULL, # order of observations
                                 heat.col.scheme = c("viridis", "red", "purple",
                                                     "blue", "grey", "green"),

                                 x.axis.reverse = F, ## myc
                                 y.axis.reverse = F, ## myc

                                 heat.pal = NULL,
                                 heat.pal.values = NULL,
                                 heat.na.col = "grey50",
                                 legend.width = 1.5,
                                 legend.height = 0.1,
                                 legend.text.size = 12,
                                 legend.num.ticks = 4,
                                 legend.breaks = NULL,
                                 axis.size = 10,
                                 label.size = 10,
                                 grid.hline = TRUE,
                                 grid.vline = TRUE,
                                 grid.hline.size = 0.5,
                                 grid.vline.size = 0.5,
                                 grid.hline.col = "black",
                                 grid.vline.col = "black") {


  heat.col.scheme <- match.arg(heat.col.scheme)

  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the heatmap
  theme <- do.call(themes, themes.arg.list)
  theme_heatmap <- theme$theme_heatmap

  if (is.null(heat.pal)) {
    if (heat.col.scheme == "viridis") {
      heat.pal <- c("#440154FF", "#3B528BFF",
                    "#21908CFF", "#5DC863FF", "#FDE725FF")
    }
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
  X.df <- matrixToDataFrame(X.order)
  # get an x-y matrix for row clusters
  rclust.mat <- matrix(rep(membership.rows,
                           times = ncol(X)),
                       byrow = F, ncol = ncol(X))
  rclust.df <- matrixToDataFrame(rclust.mat)
  rclust.df$x <- as.numeric(as.character(rclust.df$x))
  rclust.df$y <- as.numeric(as.character(rclust.df$y))
  names(rclust.df) <- c("rclust", "x", "y")
  # get an x-y matrix for col clusters
  cclust.mat <- matrix(rep(membership.cols,
                           times = nrow(X)),
                       byrow = T, nrow = nrow(X))
  cclust.df <- matrixToDataFrame(cclust.mat)
  cclust.df$x <- as.numeric(as.character(cclust.df$x))
  cclust.df$y <- as.numeric(as.character(cclust.df$y))
  names(cclust.df) <- c("cclust", "x", "y")


  X.df <- dplyr::inner_join(X.df, rclust.df, by = c("x", "y"))
  X.df <- dplyr::inner_join(X.df, cclust.df, by = c("x", "y"))


  rlines <- c(0, cumsum(table(membership.rows)))
  clines <- c(0, cumsum(table(membership.cols)))



  # smooth X within clusters
  X.smooth.df <- X.df
  # add membership columns and rows vector to X.smooth.df
  X.smooth.df$rclust <- rclust.df$rclust
  X.smooth.df$cclust <- cclust.df$cclust


  # average within cluster boxes:
  if(smooth.heat.type=="median"){
    X.smooth.df <- X.smooth.df %>% dplyr::group_by(cclust, rclust) %>%
      dplyr::summarize(value = median(value, na.rm = T))
    X.smooth.df <- dplyr::ungroup(X.smooth.df)
  }else if(smooth.heat.type=="mean"){
    X.smooth.df <- X.smooth.df %>% dplyr::group_by(cclust, rclust) %>%
      dplyr::summarize(value = mean(value, na.rm = T))
    X.smooth.df <- dplyr::ungroup(X.smooth.df)
  }

  # obtain ranges of the rectangles for the smoothed heatmap
  ymin <- rlines[-length(rlines)]
  names(ymin) <- NULL
  rclust <- unique(membership.rows)
  ymax <- rlines[-1]
  names(ymax) <- NULL
  xmin <- clines[-length(clines)]
  names(xmin) <- NULL
  cclust <- unique(membership.cols)
  xmax <- clines[-1]
  names(xmax) <- NULL

  rect_cranges <- data.frame(xmin = xmin, xmax = xmax, cclust = cclust)
  rect_rranges <- data.frame(ymin = ymin, ymax = ymax, rclust = rclust)

  # reduce to smoothed data frame
  X.df <- X.smooth.df

  X.df <- dplyr::inner_join(X.df, rect_rranges, by = "rclust")
  X.df <- dplyr::inner_join(X.df, rect_cranges, by = "cclust")

  # plot color limits:
  if (is.null(heat.lim)) {
    max.col <- max(X.df$value, na.rm = T)
    min.col <- min(X.df$value, na.rm = T)
  } else {
    max.col = heat.lim[2]
    min.col = heat.lim[1]
    if (!extreme.values.na) {
      # if extreme.values.na = FALSE then make values larger than the
      # maximum range equal to the max color (rather than grey)
      X.df$value[X.df$value > max.col] <- max.col
      # if extreme.values.na = FALSE then make values smaller than the
      # minimum range equal to the min color (rather than grey)
      X.df$value[X.df$value < min.col] <- min.col
    }
  }
  range.X <- seq(min.col, max.col, length = 100)
  # specify location of legend breaks
  if (is.null(legend.breaks)) {
    # make the breaks nicely spread
    breaks <- pretty(range.X, n = legend.num.ticks)
  } else {
    breaks <- legend.breaks
  }

  # define variables to fix visible binding check -- bit of a hack
  xmax <- X.df$xmax
  xmin <- X.df$xmin
  ymax <- X.df$ymax
  ymin <- X.df$ymin
  value <- X.df$value

  if (is.null(heat.pal.values)) {
    probs <- seq(0, 1, length = length(heat.pal))
    quantiles <- quantile(X.df$value, probs, na.rm = T)
    heat.pal.values <- (quantiles - min(quantiles)) /
      (max(quantiles) - min(quantiles))
  }
  # make the plot
  gg.legend <- ggplot2::ggplot(X.df) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xmin, xmax = xmax,
                                    ymin = ymin, ymax = ymax,
                                      fill = value)) +
    ggplot2::scale_fill_gradientn(values = heat.pal.values,
                                  colours = heat.pal,
                                  name = "",
                                  breaks = breaks,
                                  na.value = heat.na.col,
                                  limits = heat.lim) +
    ggplot2::scale_y_continuous(name = "", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = "", expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = legend.width * 10,
                                                   barheight = legend.height * 10)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size = legend.text.size)) +
    theme_heatmap

  #### Origin flip #### myc
  if (x.axis.reverse) {
    gg.legend <- gg.legend + ggplot2::scale_x_reverse(name = "", expand = c(0, 0))
  } else {
    gg.legend <- gg.legend + ggplot2::scale_x_continuous(name = "", expand = c(0, 0))
  }

  if (y.axis.reverse) {
    gg.legend <- gg.legend + ggplot2::scale_y_reverse(name = "", expand = c(0, 0))
  } else {
    gg.legend <- gg.legend + ggplot2::scale_y_continuous(name = "", expand = c(0, 0))
  }

  if (grid.vline) {
    for (i in 1:length(clines)) {
      gg.legend <- gg.legend +
        ggplot2::geom_vline(xintercept = clines[i],
                            size = grid.vline.size,
                            col = grid.vline.col)
    }
  }
  if (grid.hline) {
    for (i in 1:length(rlines)) {
      gg.legend <- gg.legend +
        ggplot2::geom_hline(yintercept = rlines[i],
                            size = grid.hline.size,
                            col = grid.hline.col)
    }
  }

  gg.heat <- gg.legend + ggplot2::theme(legend.position = "none")

  if (!is.null(X.text)) {
    col.values <- unique(as.vector(X.text.col))
    names(col.values) <- unique(as.vector(X.text.col))

    gg.heat <- gg.heat + generate_text_heat(X = X,
                                            X.text = X.text,
                                            X.text.size = X.text.size,
                                            X.text.angle = X.text.angle,
                                            X.text.col = X.text.col,
                                            smooth.heat = smooth.heat,
                                            membership.rows = membership.rows,
                                            membership.cols = membership.cols) +
      ggplot2::scale_colour_manual(values = col.values) +
      ggplot2::scale_size(range = c(min(X.text.size), max(X.text.size)))
  }

  return(list(gg.heat = gg.heat, gg.legend = gg.legend,
              heat.pal.values = heat.pal.values))
}



generate_text_heat <- function(X,
                               X.text,
                               X.text.size = 5,
                               X.text.angle = 0,
                               X.text.col = "black",
                               smooth.heat,
                               membership.rows = NULL,
                               membership.cols = NULL) {

  # if clustering, but not smoothing, then need to have same number of
  # rows/cols as X
  if ((length(unique(membership.rows)) != nrow(X)) && # are clustering
      (nrow(X.text) != nrow(X)) &&
      !smooth.heat) {
    stop(paste("X.text must have the same number of rows as",
               "X if heat.smooth = F"))
  }

  if ((length(unique(membership.cols)) != ncol(X)) &&
      (ncol(X.text) != ncol(X)) &&
      !smooth.heat) {
    stop(paste("X.text must have the same number of columns as",
               "X if heat.smooth = F"))
  }

  # if clustering, but smoothing, then need to have same number of
  # rows/cols as row/col clusters
  if ((length(unique(membership.rows)) != nrow(X)) && # are clustering
      (nrow(X.text) != length(unique(membership.rows))) &&
      smooth.heat) {
    stop(paste("X.text must have the same number of rows as",
               "row clusters if heat.smooth = T"))
  }

  if ((length(unique(membership.cols)) != ncol(X)) &&
      (ncol(X.text) != length(unique(membership.cols))) &&
      smooth.heat) {
    stop(paste("X.text must have the same number of columns as",
               "column clusters if heat.smooth = T"))
  }


  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  if (is.data.frame(X.text)) {
    X.text <- as.matrix(X.text)
  }


  # add membership columns and rows vector to X.smooth.df
  if (is.null(membership.rows)) {
    membership.rows <- 1:nrow(X.text)
  }
  if (is.null(membership.cols)) {
    membership.cols <- 1:ncol(X.text)
  }




  X.order <- X.text

  # convert the x-y correlation matrix to a long-form "tidy" data frame
  X.df <- getClusterDf(X.order,
                        smooth.heat = smooth.heat,
                        membership.rows = membership.rows,
                        membership.cols = membership.cols)


  X.df$size <- as.vector(X.text.size)
  X.df$angle <- as.vector(X.text.angle)
  X.df$col <- as.vector(X.text.col)

  # hacky fix for visible binding warning
  x <- X.df$x
  y <- X.df$y
  value <- X.df$value
  size <- X.df$size
  angle <- X.df$angle


  # make the plot
  gg.text <- ggplot2::geom_text(ggplot2::aes(x = x,
                                             y = y,
                                             label = value,
                                             size = as.numeric(size),
                                             angle = angle,
                                             col = col), data = X.df)

  return(gg.text)
}
