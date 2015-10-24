
themes <- function(legend.size = 2,
                   axis.size = 10,
                   axis.name.size = 10) {

  ### themes
  # heatmap
  theme_heatmap <- ggplot2::theme()
  theme_heatmap$legend.direction <- "horizontal"
  theme_heatmap$legend.key.width <- grid::unit(legend.size, "line")
  theme_heatmap$axis.ticks = ggplot2::element_blank()
  theme_heatmap$axis.text.x = ggplot2::element_blank()
  theme_heatmap$axis.text.y = ggplot2::element_blank()


  # right scatterplot
  theme_right <- ggplot2::theme_bw()
  theme_right$rect <- ggplot2::element_blank()
  theme_right$strip.text <- ggplot2::element_blank()
  theme_right$plot.title <- ggplot2::element_blank()
  theme_right$plot.margin <- structure(c(0, 0, 0, 0),
                                             unit = "lines",
                                             valid.unit = 3L,
                                             class = "unit")
  theme_right$panel.grid.major.y <- ggplot2::element_blank()
  theme_right$panel.grid.minor <- ggplot2::element_blank()
  theme_right$axis.text.y <- ggplot2::element_blank()
  theme_right$axis.title.y <- ggplot2::element_blank()
  theme_right$axis.ticks.y <- ggplot2::element_blank()
  theme_right$axis.text.x <- ggplot2::element_text(size = axis.size)
  theme_right$axis.title.x <- ggplot2::element_text(size = axis.name.size)
  # left scatterplot
  theme_left <- theme_right

  # top scatterplot
  theme_top <- ggplot2::theme_bw()
  theme_top$rect <- ggplot2::element_blank()
  theme_top$strip.text <- ggplot2::element_blank()
  theme_top$plot.title <- ggplot2::element_blank()
  theme_top$plot.margin <- structure(c(0, 0, 0, 0),
                                           unit = "lines",
                                           valid.unit = 3L,
                                           class = "unit")
  theme_top$panel.grid.major.x <- ggplot2::element_blank()
  theme_top$panel.grid.minor <- ggplot2::element_blank()
  theme_top$axis.text.x <- ggplot2::element_blank()
  theme_top$axis.title.x <- ggplot2::element_blank()
  theme_top$axis.ticks.x <- ggplot2::element_blank()
  theme_top$axis.text.y <- ggplot2::element_text(size = axis.size)
  theme_top$axis.title.y <- ggplot2::element_text(angle = 90, size = axis.name.size)
  # bottom scatterplot
  theme_bottom <- theme_top



  # labels
  theme_clust_labels <- ggplot2::theme_bw()
  theme_clust_labels$line <- ggplot2::element_blank()
  theme_clust_labels$rect <- ggplot2::element_blank()
  theme_clust_labels$strip.text <- ggplot2::element_blank()
  theme_clust_labels$axis.text <- ggplot2::element_blank()
  theme_clust_labels$plot.title <- ggplot2::element_blank()
  theme_clust_labels$axis.title <- ggplot2::element_blank()
  theme_clust_labels$legend.position <- "none"
  theme_clust_labels$plot.margin <- structure(c(0, 0, 0, 0),
                                              unit = "lines",
                                              valid.unit = 3L,
                                              class = "unit")
  theme_clust_labels$axis.ticks <- ggplot2::element_blank()
  theme_clust_labels$axis.title.x <- ggplot2::element_blank()
  theme_clust_labels$axis.title.y <- ggplot2::element_blank()



  return(list(theme_top = theme_top,
              theme_right = theme_right,
              theme_bottom = theme_bottom,
              theme_left = theme_left,
              theme_clust_labels = theme_clust_labels,
              theme_heatmap = theme_heatmap))
}

