
#' Generate clustered heatmap labels.
#'
#'
#'
#' @param membership a vector specifying the cluster membership.
#' @param label.pal a vector specifying the cluster/variable label color palette.
#' @param label.text.col a character or character vector specifying the
#'          cluster/variable label text color.
#' @param bottom.text.size the size of the bottom heatmap label text. The
#'          default is 5.
#' @param left.text.size the size of the left heatmap label text. The
#'          default is 5.
#' @param location will these labels be on the bottom ("bottom") or on the left
#'          ("left)?
#' @param text.angle number of degrees to rotate the text on the left
#'          cluster/variable labels.
#' @return A ggplot2 object of heatmap labels.
#' @importFrom magrittr "%>%"



generate_cluster_label <- function(membership,
                                   location = c("bottom", "left"),
                                   label.pal = NULL,
                                   label.text.col = NULL,
                                   bottom.text.size = 5,
                                   left.text.size = 5,
                                   text.angle = NULL) {


  location <- match.arg(location)


  if ((location == "bottom") && is.null(text.angle))
    text.angle <- 0
  if ((location == "left") && is.null(text.angle))
    text.angle <- 90

  # define themes
  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the labels
  theme <- do.call(themes, themes.arg.list)
  theme_clust_labels <- theme$theme_clust_labels





  cluster.size <- table(membership)
  cluster.size <- cluster.size[cluster.size != 0]
  cluster.names <- names(cluster.size)

  cluster.size <- as.vector(cluster.size)
  n.cluster <- length(cluster.size)


  if (!is.null(label.pal) && (length(label.pal) != n.cluster)) {
    label.pal <- rep(label.pal, length = n.cluster)
  }

  if (!is.null(label.text.col) && (length(label.text.col) != n.cluster)) {
    label.text.col <- rep(label.text.col, length = n.cluster)
  }



  if (is.null(label.pal))
    label.pal = rep(c("Grey 71","Grey 53"), length = n.cluster)

  if (is.null(label.text.col))
    label.text.col <- "black"


  label.text.col <- factor(label.text.col, levels = unique(label.text.col))
  label.text.col <- droplevels(label.text.col)
  names(label.text.col) <- label.text.col


  label.pal <- factor(label.pal, levels = unique(label.pal))
  label.pal <- droplevels(label.pal)
  names(label.pal) <- label.pal

  # make the proportions of the label rectangles match the number of observations
  # in the heatmap clusters
  selected.clusters.df <- data.frame(cluster = cluster.names,
                                     col = label.pal,
                                     n = cluster.size)
  selected.clusters.df$id <- 1:nrow(selected.clusters.df)
  selected.clusters.df <- selected.clusters.df %>%
    dplyr::mutate(increment = (n/sum(selected.clusters.df$n)) * n.cluster)
  breaks <- c(1, 1 + cumsum(selected.clusters.df$increment))
  selected.clusters.df$breaks <- breaks[-(nrow(selected.clusters.df) + 1)]



  if (is.null(cluster.names)) {
    selected.clusters.df <- selected.clusters.df %>%
      dplyr::mutate(cluster.names = cluster)
  } else {
    selected.clusters.df$cluster.names = cluster.names
    }

  # define variables to fix visible binding check -- bit of a hack
  n <- selected.clusters.df$n
  cluster <- selected.clusters.df$cluster
  increment <- selected.clusters.df$increment

  if (location == "left") {
    label.pal <- as.character(label.pal)
    names(label.pal) <- label.pal
    gg.left <- ggplot2::ggplot(selected.clusters.df,
                                     ggplot2::aes(xmin = 0,
                                                  xmax = 1,
                                                  ymin = breaks,
                                                  ymax = breaks + increment,
                                                  fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = as.character(label.pal)) +
      ggplot2::geom_text(ggplot2::aes(x = 0.5,
                                      y = breaks + increment/2,
                                      label = cluster.names),
                         hjust = "centre",
                         vjust = "centre",
                         size = left.text.size,
                         angle = text.angle,
                         col = label.text.col) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_continuous(expand = c(0,0))

    return(gg.left)
  }


  if (location == "bottom") {

    label.pal <- as.character(label.pal)
    names(label.pal) <- label.pal
    suppressWarnings(
    gg.bottom <- ggplot2::ggplot(selected.clusters.df,
                                       ggplot2::aes(xmin = breaks,
                                                    xmax = breaks + increment,
                                                    ymin = 0,
                                                    ymax = 1,
                                                    fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = as.character(label.pal)) +
      ggplot2::geom_text(ggplot2::aes(y = 0.5,
                                      x = breaks + increment/2,
                                      label = cluster.names),
                         hjust = "centre",
                         vjust = "centre",
                         size = bottom.text.size,
                         col = label.text.col,
                         angle = text.angle) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)))

    return(gg.bottom)
  }


}










#' Generate heatmap variable labels.
#'
#'
#'
#' @param names a vector specifying the label names.
#' @param label.pal a vector specifying the cluster/variable label color palette.
#' @param label.text.col a character or character vector specifying the
#'          cluster/variable label text color.
#' @param bottom.text.size the size of the bottom heatmap label text. The
#'          default is 5.
#' @param left.text.size the size of the left heatmap label text. The
#'          default is 5.
#' @param location will these labels be on the bottom ("bottom") or on the left
#'          ("left)?
#' @param text.angle number of degrees to rotate the text on the left
#'          cluster/variable labels.
#' @return A ggplot2 object of heatmap labels.
#' @importFrom magrittr "%>%"


generate_var_label <- function(names,
                               location = c("bottom", "left"),
                               label.pal = NULL,
                               label.text.col = NULL,
                               bottom.text.size = 5,
                               left.text.size = 5,
                               text.angle = NULL) {


  location <- match.arg(location)

  if ((location == "bottom") && is.null(text.angle))
    text.angle <- 0
  if ((location == "left") && is.null(text.angle))
    text.angle <- 90

  # define themes
  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the labels
  theme <- do.call(themes, themes.arg.list)
  theme_clust_labels <- theme$theme_clust_labels


  if (!is.null(label.pal) && (length(label.pal) != length(names))) {
    label.pal <- rep(label.pal, length = length(names))
  }

  if (!is.null(label.text.col) && (length(label.text.col) != length(names))) {
    label.text.col <- rep(label.text.col, length = length(names))
  }


  if (is.null(label.pal))
    label.pal = rep(c("Grey 71","Grey 53"), length = length(names))

  if (is.null(label.text.col))
    label.text.col <- "black"




  label.text.col <- factor(label.text.col, levels = unique(label.text.col))
  label.text.col <- droplevels(label.text.col)
  names(label.text.col) <- label.text.col

  label.pal <- factor(label.pal, levels = unique(label.pal))
  label.pal <- droplevels(label.pal)
  names(label.pal) <- label.pal


  # make the proportions of the label rectangles match the number of observations
  # in the heatmap clusters
  variable <- names # fix for visible binding note


  variables.df <- data.frame(variable = names,
                                     col = label.pal,
                                     n = 1)
  variables.df$id <- 1:nrow(variables.df)
  variables.df <- variables.df %>%
    dplyr::mutate(increment = (n/sum(variables.df$n)) * 1)
  breaks <- c(1, 1 + cumsum(variables.df$increment))
  variables.df$breaks <- breaks[-(nrow(variables.df) + 1)]






  # define variables to fix visible binding check -- bit of a hack
  n <- variables.df$n
  cluster <- variables.df$cluster
  increment <- variables.df$increment

  if (location == "left") {
    label.pal <- as.character(label.pal)
    names(label.pal) <- label.pal
    gg.left <- ggplot2::ggplot(variables.df,
                               ggplot2::aes(xmin = 0,
                                            xmax = 1,
                                            ymin = breaks,
                                            ymax = breaks + increment,
                                            fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = label.pal) +
      ggplot2::geom_text(ggplot2::aes(x = 0.5, y = breaks + increment/2, label = variable),
                         hjust = "centre",
                         vjust = "centre",
                         size = left.text.size,
                         angle = text.angle,
                         col = label.text.col) +
      ggplot2::scale_y_continuous(expand = c(0,0)) +
      ggplot2::scale_x_continuous(expand = c(0,0))

    return(gg.left)
  }


  if (location == "bottom") {
    label.pal <- as.character(label.pal)
    names(label.pal) <- label.pal
    gg.bottom <- ggplot2::ggplot(variables.df,
                                 ggplot2::aes(xmin = breaks,
                                              xmax = breaks + increment,
                                              ymin = 0,
                                              ymax = 1,
                                              fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = label.pal) +
      ggplot2::geom_text(ggplot2::aes(y = 0.5,
                                      x = breaks + increment/2,
                                      label = variable),
                         hjust = "centre",
                         vjust = "centre",
                         size = bottom.text.size,
                         col = label.text.col,
                         angle = text.angle) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0))

    return(gg.bottom)
  }


}









generate_title <- function(title = NULL,
                           title.size = 5) {
  theme <- themes()
  theme_clust_labels <- theme$theme_clust_labels


  df <- data.frame(x = 0, y = 0, title = title)
  # define variables to fix visible binding check -- bit of a hack
  x <- df$x
  y <- df$y
  gg.title <- ggplot2::ggplot(df) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = title), size = title.size) +
    theme_clust_labels


  return(gg.title)

}






generate_names <- function(name = NULL,
                           name.size = 5,
                           location = c("left", "bottom")) {
  theme <- themes()
  theme_clust_labels <- theme$theme_clust_labels


  df <- data.frame(x = 0, y = 0, name = name)
  # define variables to fix visible binding check -- bit of a hack
  x <- df$x
  y <- df$y
  if (location == "bottom") {
    gg.name <- ggplot2::ggplot(df) +
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = name), size = name.size)
  } else if (location == "left") {
    gg.name <- ggplot2::ggplot(df) +
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = name), size = name.size, angle = 90)
  }

  gg.name <- gg.name + theme_clust_labels


  return(gg.name)

}
