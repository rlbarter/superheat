# surrounding scatterplot



generate_scatter <- function(y,
                             location = c("top","right"),
                             membership,
                             plot.type = c("scatter","bar", "boxplot","scattersmooth", "smooth", "scatterline", "line"),
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
                             smoothing.method = c("loess","lm"),
                             y.line.size = NULL,
                             smooth.se = TRUE) {

  # specify axis name angle
  if (location == "top" && is.null(axis.name.angle)) axis.name.angle <- 90
  if (location == "right" && is.null(axis.name.angle)) axis.name.angle <- 0



  if (is.null(y.obs.col) && is.null(y.pal)) {
    y.pal <- c("Grey 61","Grey 43")
  }


  if (is.null(y.bar.col)) y.bar.col <- "white"

  if (is.null(y.line.size)) y.line.size <- 1



  id <- 1:length(y)

  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the  heatmap
  theme <- do.call(themes, themes.arg.list)
  theme_top <- theme$theme_top
  theme_right <- theme$theme_right





  # top scatterplot
  if (location == "top") {
    y.df <- data.frame(y = y, x = id)
    y.df$membership <- factor(membership)

    # fix for visible binding note
    x <- y.df$x
    y <- y.df$y

    if (!is.null(y.obs.col)) {
      y.df$col <- y.obs.col
    }




    if (plot.type %in% c("scatter", "boxplot", "scattersmooth", "smooth", "line", "scatterline")) {
      ticks <- seq(min((y.df$y)), max((y.df$y)), length = num.ticks)
    } else if ((plot.type == "bar") && (min(y.df$y) > 0)) {
      ticks <- seq(0, max(y.df$y), length = num.ticks)
    } else if ((plot.type == "bar") && (max(y.df$y) < 0)) {
      ticks <- seq(min(y.df$y), 0, length = num.ticks)
    } else if ((plot.type == "bar") && (max(y.df$y) > 0) && (min(y.df$y) < 0)) {
      ticks <- seq(min(y.df$y), max(y.df$y), length = num.ticks)
    }

    if (plot.type == "boxplot") {
      gg.top <- ggplot2::ggplot(y.df) +
        theme_top
      # calculate the midpoints of the boxplot positions
      selected.clusters.df <- data.frame(cluster = unique(membership),
                                         n = as.vector(table(membership)))
      selected.clusters.df <- selected.clusters.df %>%
        dplyr::mutate(increment = (n/sum(selected.clusters.df$n)) * length(unique(membership)))
      breaks <- c(1, 1 + cumsum(selected.clusters.df$increment))
      midpoints <- matrix(c(c(breaks, NA), c(NA, breaks)), ncol = 2, byrow = F)
      midpoints <- apply(midpoints, 1, mean)[2:(length(breaks))]
    } else {
      gg.top <- ggplot2::ggplot(y.df) +
        theme_top +
        ggplot2::scale_x_continuous(name = "", expand = c(0.01, 0.01))
    }





    gg.top <- gg.top +
      ggplot2::scale_y_continuous(breaks = (scales::pretty_breaks(n = num.ticks,
                                                                  min.n = 3))(ticks)[-1],
                                  name = paste(axis.name),
                                  expand = c(0.05, 0.05))
                                  #limits = c(min(y.df$y),
                                  #             max(y.df$y)))

    if (is.null(y.obs.col) && (plot.type %in% c("scatter","scattersmooth","scatterline"))) {
      gg.top <- gg.top +
        ggplot2::geom_point(ggplot2::aes(x = x,
                                         y = y,
                                         col = factor(membership)),
                            size = point.size,
                            alpha = point.alpha) +
        ggplot2::scale_color_manual(values = rep(y.pal, length = length(unique(membership))))
      if (plot.type == "scattersmooth") {
        gg.top <- gg.top + ggplot2::stat_smooth(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(membership)),
                                                  method = smoothing.method,
                                                size = y.line.size,
                                                se = smooth.se)
      }
      if (plot.type == "scatterline") {
        gg.top <- gg.top + ggplot2::geom_line(ggplot2::aes(x = x,
                                                             y = y,
                                                             col = factor(membership)),
                                                size = y.line.size)
      }

    } else if (!is.null(y.obs.col) && (plot.type %in% c("scatter","scattersmooth","scatterline"))) {
      gg.top <- gg.top +
        ggplot2::geom_point(ggplot2::aes(x = x,
                                         y = y,
                                         factor(col, levels = unique(col))),
                            size = point.size,
                            alpha = point.alpha) +
        ggplot2::scale_color_manual(values = unique(y.obs.col))
      if (plot.type == "scattersmooth") {
        gg.top <- gg.top + ggplot2::stat_smooth(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(col, levels = unique(col))),
                                                    method = smoothing.method,
                                                size = y.line.size,
                                                se = smooth.se)
      }
      if (plot.type == "scatterline") {
        gg.top <- gg.top + ggplot2::geom_line(ggplot2::aes(x = x,
                                                             y = y,
                                                             col = factor(col, levels = unique(col))),
                                                size = y.line.size)
      }
    } else if (plot.type == "smooth") {
        gg.top <- gg.top + ggplot2::stat_smooth(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(membership)),
                                                    method = smoothing.method,
                                                size = y.line.size,
                                                se = smooth.se) +
          ggplot2::scale_color_manual(values = rep(y.pal, length = length(unique(membership))))
    } else if (plot.type == "line") {
      gg.top <- gg.top + ggplot2::geom_line(ggplot2::aes(x = x,
                                                           y = y,
                                                           col = factor(membership)),
                                              size = y.line.size) +
        ggplot2::scale_color_manual(values = rep(y.pal, length = length(unique(membership))))
      } else if (plot.type == "boxplot") {
        w <- table(membership)/(length(membership)/length(unique(membership)))
        y.df.temp <- y.df
        y.df.temp$midpoints <- factor(y.df.temp$membership)
        levels(y.df.temp$midpoints) <- midpoints
        gg.top <- gg.top +
          plyr::llply(unique(membership), function(i) {ggplot2::geom_boxplot(ggplot2::aes(x = as.numeric(as.vector(midpoints)),
                                                                                          y = y,
                                                                                          fill = factor(membership)),
                                                                             width = w[i],
                                                                             data = subset(y.df.temp, membership == i)) }) +
          ggplot2::scale_fill_manual(values = rep(y.pal, length = length(unique(membership)))) +
          ggplot2::scale_x_continuous(limits = c(min(breaks), max(breaks)), expand = c(0,0))

        rm(y.df.temp)
    } else if (is.null(y.obs.col) && (plot.type == "bar")) {
      gg.top <- gg.top +
        ggplot2::geom_bar(ggplot2::aes(x = x,
                                       y = y,
                                       fill = factor(membership)),
                          col = y.bar.col,
                          position = ggplot2::position_dodge(0),
                          stat = "identity",
                          width = 1) +
        ggplot2::scale_fill_manual(values = rep(y.pal, length = length(unique(membership))))
    } else if (!is.null(y.obs.col) && (plot.type == "bar")) {
      gg.top <- gg.top +
        ggplot2::geom_bar(ggplot2::aes(x = x,
                                       y = y,
                                       fill = factor(membership)),
                          col = y.bar.col,
                          position = ggplot2::position_dodge(0),
                          stat = "identity",
                          width = 1) +
        ggplot2::scale_fill_manual(values = unique(y.obs.col))
    }


    if ((plot.type == "bar") && (min(y.df$y) < 0) && (max(y.df$y) > 0)) {
      gg.top <- gg.top +
        ggplot2::geom_hline(yintercept = 0, col = "grey")
    }

    # rotate axis name
    gg.top <- gg.top + ggplot2::theme(axis.title.y = ggplot2::element_text(angle = axis.name.angle))

    return(gg.top)
  }










  # right scatterplot
  if (location == "right") {
    y.df <- data.frame(y = y, x = id)
    y.df$membership <- factor(membership)

    # fix for visible binding note
    x <- y.df$x
    y <- y.df$y


    if (!is.null(y.obs.col)) {
      y.df$col <- y.obs.col
    }

    if (plot.type == "boxplot") {
      gg.right <- ggplot2::ggplot(y.df) +
        ggplot2::coord_flip() +
        theme_right
        # calculate midpoints of the boxplot positions
      selected.clusters.df <- data.frame(cluster = unique(membership),
                                       n = as.vector(table(membership)))
      selected.clusters.df <- selected.clusters.df %>%
        dplyr::mutate(increment = (n/sum(selected.clusters.df$n)) * length(unique(membership)))
      breaks <- c(1, 1 + cumsum(selected.clusters.df$increment))
      midpoints <- matrix(c(c(breaks, NA), c(NA, breaks)), ncol = 2, byrow = F)
      midpoints <- apply(midpoints, 1, mean)[2:(length(breaks))]
    } else {
      gg.right <- ggplot2::ggplot(y.df) +
        ggplot2::coord_flip() +
        theme_right +
        ggplot2::scale_x_continuous(name = "", expand = c(0.01, 0.01))
    }



    if (plot.type %in% c("scatter", "boxplot", "scattersmooth", "smooth", "line", "scatterline")) {
      ticks <- seq(min((y.df$y)), max((y.df$y)), length = num.ticks)
    } else if ((plot.type == "bar") && (min(y.df$y) > 0)) {
      ticks <- seq(0, max(y.df$y), length = num.ticks)
    } else if ((plot.type == "bar") && (max(y.df$y) < 0)) {
      ticks <- seq(min(y.df$y), 0, length = num.ticks)
    } else if ((plot.type == "bar") && (max(y.df$y) > 0) && (min(y.df$y) < 0)) {
      ticks <- seq(min(y.df$y), max(y.df$y), length = num.ticks)
    }



    gg.right <- gg.right +
      ggplot2::scale_y_continuous(breaks = (scales::pretty_breaks(n = num.ticks,
                                                                  min.n = 3))(ticks)[-1],
                                  name = paste(axis.name),
                                  expand = c(0.05, 0.05))
                                  #limits = c(min(y.df$y),
                                  #           max(y.df$y)))


    if (is.null(y.obs.col) && (plot.type %in% c("scatter", "scattersmooth", "scatterline"))) {
      gg.right <- gg.right +
        ggplot2::geom_point(ggplot2::aes(x = x,
                                         y = y,
                                         col = factor(membership)),
                            size = point.size,
                            alpha = point.alpha) +
        ggplot2::scale_color_manual(values = rep(y.pal, length = length(unique(membership))))
      if (plot.type == "scattersmooth") {
        gg.right <- gg.right + ggplot2::stat_smooth(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(membership)),
                                                    method = smoothing.method,
                                                    size = y.line.size,
                                                    se = smooth.se)
      }
      if (plot.type == "scatterline") {
        gg.right <- gg.right + ggplot2::geom_line(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(membership)),
                                                    size = y.line.size)
      }
    } else if (!is.null(y.obs.col) && (plot.type %in% c("scatter", "scattersmooth", "scatterline"))) {
      gg.right <- gg.right +
        ggplot2::geom_point(ggplot2::aes(x = x,
                                         y = y,
                                         col = factor(col, levels = unique(col))),
                            size = point.size,
                            alpha = point.alpha) +
        ggplot2::scale_color_manual(values = unique(y.obs.col))
      if (plot.type == "scattersmooth") {
        gg.right <- gg.right + ggplot2::stat_smooth(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(col, levels = unique(col))),
                                                    method = smoothing.method,
                                                    size = y.line.size,
                                                    se = smooth.se)
      }
      if (plot.type == "scatterline") {
        gg.right <- gg.right + ggplot2::geom_line(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(col, levels = unique(col))),
                                                    size = y.line.size)
      }
    } else if (plot.type == "smooth") {
        gg.right <- gg.right + ggplot2::stat_smooth(ggplot2::aes(x = x,
                                                                 y = y,
                                                                 col = factor(membership)),
                                                    method = smoothing.method,
                                                    size = y.line.size,
                                                    se = smooth.se) +
          ggplot2::scale_color_manual(values = rep(y.pal, length = length(unique(membership))))
    } else if (plot.type == "line") {
      gg.right <- gg.right + ggplot2::geom_line(ggplot2::aes(x = x,
                                                               y = y,
                                                               col = factor(membership)),
                                                  size = y.line.size) +
        ggplot2::scale_color_manual(values = rep(y.pal, length = length(unique(membership))))
    } else if (plot.type == "boxplot") {
      w <- table(membership)/(length(membership)/length(unique(membership)))
      y.df.temp <- y.df
      y.df.temp$midpoints <- factor(y.df.temp$membership)
      levels(y.df.temp$midpoints) <- midpoints
      gg.right <- gg.right +
        plyr::llply(unique(membership), function(i) {ggplot2::geom_boxplot(ggplot2::aes(x = as.numeric(as.vector(midpoints)),
                                       y = y,
                                       fill = factor(membership)),
                                       width = w[i],
                                       data = subset(y.df.temp, membership == i)) }) +
        ggplot2::scale_fill_manual(values = rep(y.pal, length = length(unique(membership)))) +
        ggplot2::scale_x_continuous(limits = c(min(breaks), max(breaks)), expand = c(0,0))
      rm(y.df.temp)
    } else if (is.null(y.obs.col) && (plot.type == "bar")) {
      gg.right <- gg.right +
        ggplot2::geom_bar(ggplot2::aes(x = x,
                                         y = y,
                                         fill = factor(membership)),
                          col = y.bar.col,
                          position = ggplot2::position_dodge(0),
                          stat = "identity",
                          width = 1) +
        ggplot2::scale_fill_manual(values = rep(y.pal, length = length(unique(membership))))
    } else if (!is.null(y.obs.col) && (plot.type == "bar")) {
      gg.right <- gg.right +
        ggplot2::geom_bar(ggplot2::aes(x = x,
                                       y = y,
                                       fill = factor(col, levels = unique(col))),
                          col = y.bar.col,
                          position = ggplot2::position_dodge(0),
                          stat = "identity",
                          width = 1) +
        ggplot2::scale_fill_manual(values = unique(y.obs.col))
    }

    if ((plot.type == "bar") && (min(y.df$y) < 0) && (max(y.df$y) > 0)) {
      gg.right <- gg.right +
        ggplot2::geom_hline(yintercept = 0, col = "grey")
    }

    # rotate axis name
    gg.right <- gg.right + ggplot2::theme(axis.title.x = ggplot2::element_text(angle = axis.name.angle))


    return(gg.right)
  }


}







