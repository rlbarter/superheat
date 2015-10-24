



generate_heat <- function(X,
                          membership.rows = NULL,
                          membership.cols = NULL,
                          order.x = NULL, # order of variables
                          order.y = NULL, # order of observations
                          heat.col.scheme = c("red", "purple", "blue", "grey", "green"),
                          heat.pal = NULL,
                          heat.pal.values = NULL,
                          heat.midpoint = NULL,
                          legend.size = 2,
                          axis.size = 10,
                          label.size = 10,
                          cluster.box = TRUE,
                          box.size = 0.5,
                          box.col = "black") {



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

  if (is.null(heat.midpoint)) {
    heat.midpoint = mean(X)
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



  if (cluster.box) {
    rlines <- c(0,cumsum(table(membership.rows)))
    clines <- c(0,cumsum(table(membership.cols)))
  }




  # plot colour limits:
  max.col <- max(X.df$value)
  min.col <- min(X.df$value)
  range.X <- seq(min.col, max.col, length = 100)
  breaks <- signif(as.vector(quantile(range.X)), 1)


  # define variables to fix visible binding check -- bit of a hack
  x <- X.df$x
  y <- X.df$y
  value <- X.df$value

  if (is.null(heat.pal.values)) {
    heat.pal.values <- c(0, 0.5, 1)
  }
  # make the plot
  gg.legend <- ggplot2::ggplot(X.df) +
    ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = value),
                         colour = "black") +
    #ggplot2::coord_fixed() +
    ggplot2::scale_fill_gradientn(values = heat.pal.values,
                                  colours = heat.pal,
                                  name = "",
                                  breaks = breaks) +
    ggplot2::scale_y_continuous(name = "", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = "", expand = c(0,0)) +
    theme_heatmap



  if (cluster.box) {
    for (i in 1:length(clines)) {
      gg.legend <- gg.legend + ggplot2::geom_vline(xintercept = clines[i] + 0.5,
                                                   size = box.size,
                                                   col = box.col)
    }
    for (i in 1:length(rlines)) {
      gg.legend <- gg.legend + ggplot2::geom_hline(yintercept = rlines[i] + 0.5,
                                                   size = box.size,
                                                   col = box.col)
    }
  }

  gg.heat <- gg.legend + ggplot2::theme(legend.position = "none")


  return(list(gg.heat = gg.heat, gg.legend = gg.legend))
}



