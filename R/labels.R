

generate_cluster_label <- function(membership,
                                   location = c("bottom", "left"),
                                   label.col = NULL,
                                   label.text.col = NULL,
                                   label.text.alignment = NULL,
                                   bottom.label.text.size = 5,
                                   left.label.text.size = 5,
                                   text.angle = NULL) {


  # set the defaults for the label alignment
  if (is.null(label.text.alignment) & location == "bottom") {
    label.text.alignment <- "center"
  }
  if (is.null(label.text.alignment) & location == "left") {
    label.text.alignment <- "right"
  }

  location <- match.arg(location)

  # set the ggplot parameters for alignment
  if (label.text.alignment == "center") {
    alignment <- "center"
    pos <- 0.5
  } else if (label.text.alignment == "left") {
    alignment <- 0
    pos <- 0
  } else if (label.text.alignment == "right") {
    alignment <- 1
    pos <- 1
  }

  # set the default angles for the labels
  if ( (location == "bottom") && is.null(text.angle) )
    text.angle <- 0
  if ( (location == "left") && is.null(text.angle) )
    text.angle <- 0

  # define themes
  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the labels
  theme <- do.call(themes, themes.arg.list)
  theme_clust_labels <- theme$theme_clust_labels

  # determine the size of the cluster labels
  cluster.size <- table(membership)
  cluster.size <- cluster.size[cluster.size != 0]
  cluster.names <- names(cluster.size)

  cluster.size <- as.vector(cluster.size)
  n.cluster <- length(cluster.size)

  # set the colors of the labels
  if (!is.null(label.col) && (sum(!is.na(label.col)) != n.cluster)) {
    label.col <- rep(label.col[!is.na(label.col)], length = n.cluster)
  }
  # set the colors of the label text
  if (!is.null(label.text.col) && (sum(!is.na(label.text.col)) != n.cluster)) {
    label.text.col <- rep(label.text.col[!is.na(label.text.col)],
                          length = n.cluster)
  }
  # set the default colors of the labels
  if (is.null(label.col)) {
    label.col <- rep(c("grey95", "grey80"), length = n.cluster)
  }
  # set the default color of the label text
  if (is.null(label.text.col)) {
    label.text.col <- "black"
  }

  # set the label names as correctly ordered factors
  label.text.col <- factor(label.text.col[!is.na(label.text.col)],
                           levels = unique(label.text.col[!is.na(label.text.col)]))
  label.text.col <- droplevels(label.text.col)
  names(label.text.col) <- label.text.col

  # set the label colours as correctly ordered factors
  label.col <- factor(label.col[!is.na(label.col)],
                      levels = unique(label.col[!is.na(label.col)]))
  label.col <- droplevels(label.col)
  names(label.col) <- label.col

  # make the proportions of the label rectangles match the num of observations
  # in the heatmap clusters
  selected.clusters.df <- data.frame(cluster = cluster.names,
                                     col = label.col,
                                     n = cluster.size)
  selected.clusters.df$id <- 1:nrow(selected.clusters.df)
  selected.clusters.df <- selected.clusters.df %>%
    dplyr::mutate(increment = (n / sum(selected.clusters.df$n)) * n.cluster)
  breaks <- c(1, 1 + cumsum(selected.clusters.df$increment))
  selected.clusters.df$breaks <- breaks[ -(nrow(selected.clusters.df) + 1) ]

  # set the cluster names to be the specified names (if provided)
  if (is.null(cluster.names)) {
    selected.clusters.df <- selected.clusters.df %>%
      dplyr::mutate(cluster.names = cluster)
  } else {
    selected.clusters.df$cluster.names <- cluster.names
    }

  # define variables to fix visible binding check -- bit of a hack
  n <- selected.clusters.df$n
  cluster <- selected.clusters.df$cluster
  increment <- selected.clusters.df$increment

  if (location == "left") {
    # add right padding on cluster names if the label is right-justified
    if (label.text.alignment == "right") {
      selected.clusters.df$cluster.names.aligned <- paste0(cluster.names, " ")
    } else {
      selected.clusters.df$cluster.names.aligned <- cluster.names
    }


    label.col <- as.character(label.col)
    names(label.col) <- label.col
    gg.left <- ggplot2::ggplot(selected.clusters.df,
                                     ggplot2::aes(xmin = 0,
                                                  xmax = 1,
                                                  ymin = breaks,
                                                  ymax = breaks + increment,
                                                  fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = as.character(label.col)) +
      ggplot2::geom_text(ggplot2::aes(x = pos,
                                      y = breaks + increment / 2,
                                      label = cluster.names.aligned),
                         hjust = alignment,
                         vjust = "centre",
                         size = left.label.text.size,
                         angle = text.angle,
                         col = label.text.col) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::scale_x_continuous(expand = c(0, 0))

    return(gg.left)
  }


  if (location == "bottom") {
    # add right padding on cluster names if the label is right-justified
    if (label.text.alignment == "right") {
      selected.clusters.df$cluster.names.aligned <- paste0(cluster.names, " ")
    } else {
      selected.clusters.df$cluster.names.aligned <- cluster.names
    }


    label.col <- as.character(label.col)
    names(label.col) <- label.col
    suppressWarnings(
    gg.bottom <- ggplot2::ggplot(selected.clusters.df,
                                       ggplot2::aes(xmin = breaks,
                                                    xmax = breaks + increment,
                                                    ymin = 0,
                                                    ymax = 1,
                                                    fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = as.character(label.col)) +
      ggplot2::geom_text(ggplot2::aes(y = pos,
                                      x = breaks + increment / 2,
                                      label = cluster.names.aligned),
                         hjust = alignment,
                         vjust = "centre",
                         size = bottom.label.text.size,
                         col = label.text.col,
                         angle = text.angle) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)))

    return(gg.bottom)
  }


}










generate_var_label <- function(names,
                               location = c("bottom", "left"),
                               label.col = NULL,
                               label.text.col = NULL,
                               label.text.alignment = NULL,
                               bottom.label.text.size = 5,
                               left.label.text.size = 5,
                               text.angle = NULL) {

  location <- match.arg(location)

  # set the defaults for the label alignment
  if (is.null(label.text.alignment) & location == "bottom") {
    label.text.alignment <- "center"
  }
  if (is.null(label.text.alignment) & location == "left") {
    label.text.alignment <- "right"
  }

  # set the ggplot parameters for the text alignment
  if (label.text.alignment == "center") {
    alignment <- "center"
    pos <- 0.5
  } else if (label.text.alignment == "left") {
    alignment <- 0
    pos <- 0
  } else if (label.text.alignment == "right") {
    alignment <- 1
    pos <- 1
  }

  # set the default angles
  if ((location == "bottom") && is.null(text.angle)) {
    text.angle <- 0
  }
  if ((location == "left") && is.null(text.angle)) {
    text.angle <- 0
  }
  # define themes
  themes.arg.list <- c(as.list(environment()))
  themes.arg.list <- themes.arg.list[names(formals(themes))]
  themes.arg.list <- themes.arg.list[!is.na(names(themes.arg.list))]

  # define the theme for the labels
  theme <- do.call(themes, themes.arg.list)
  theme_clust_labels <- theme$theme_clust_labels

  # set the label colors
  if (!is.null(label.col) && (sum(!is.na(label.col)) != length(names))) {
    label.col <- rep(label.col[!is.na(label.col)], length = length(names))
  }

  # set the label text colors
  if (!is.null(label.text.col) &&
      (sum(!is.na(label.text.col)) != length(names))) {
    label.text.col <- rep(label.text.col[!is.na(label.text.col)],
                          length = length(names))
  }

  if (is.null(label.col)) {
    label.col <- rep(c("grey95", "grey80"), length = length(names))
  }

  if (is.null(label.text.col))
    label.text.col <- "black"



  # specify the colors as factors (in correct order)
  label.text.col <- factor(label.text.col[!is.na(label.text.col)],
                           levels = unique(label.text.col[!is.na(label.text.col)]))
  label.text.col <- droplevels(label.text.col)
  names(label.text.col) <- label.text.col

  label.col <- factor(label.col[!is.na(label.col)],
                      levels = unique(label.col[!is.na(label.col)]))
  label.col <- droplevels(label.col)
  names(label.col) <- label.col


  # make the proportions of the label rectangles match the num of observations
  # in the heatmap clusters
  variable <- names # fix for visible binding note


  variables.df <- data.frame(variable = names,
                             col = label.col,
                             n = 1)
  variables.df$id <- 1:nrow(variables.df)
  variables.df <- variables.df %>%
    dplyr::mutate(increment = (n / sum(variables.df$n)) * 1)
  breaks <- c(1, 1 + cumsum(variables.df$increment))
  variables.df$breaks <- breaks[ -(nrow(variables.df) + 1) ]






  # define variables to fix visible binding check -- bit of a hack
  n <- variables.df$n
  increment <- variables.df$increment

  if (location == "left") {
    # add right padding on variable if the label is right-justified
    if (label.text.alignment == "right") {
      variables.df$variable.aligned <- paste0(variable, " ")
    } else {
      variables.df$variable.aligned <- variable
    }

    label.col <- as.character(label.col)
    names(label.col) <- label.col
    gg.left <- ggplot2::ggplot(variables.df,
                               ggplot2::aes(xmin = 0,
                                            xmax = 1,
                                            ymin = breaks,
                                            ymax = breaks + increment,
                                            fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = label.col) +
      ggplot2::geom_text(ggplot2::aes(x = pos,
                                      y = breaks + increment / 2,
                                      label = variable.aligned),
                         hjust = alignment,
                         vjust = "centre",
                         size = left.label.text.size,
                         angle = text.angle,
                         col = label.text.col) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::scale_x_continuous(expand = c(0, 0))

    return(gg.left)
  }


  if (location == "bottom") {
    # add right padding on variable if the label is right-justified
    if (label.text.alignment == "right") {
      variables.df$variable.aligned <- paste0(variable, " ")
    } else {
      variables.df$variable.aligned <- variable
    }

    label.col <- as.character(label.col)
    names(label.col) <- label.col
    gg.bottom <- ggplot2::ggplot(variables.df,
                                 ggplot2::aes(xmin = breaks,
                                              xmax = breaks + increment,
                                              ymin = 0,
                                              ymax = 1,
                                              fill = col)) +
      ggplot2::geom_rect() +
      theme_clust_labels +
      ggplot2::scale_fill_manual(values = label.col) +
      ggplot2::geom_text(ggplot2::aes(y = pos,
                                      x = breaks + increment / 2,
                                      label = variable.aligned),
                         hjust = alignment,
                         vjust = "centre",
                         size = bottom.label.text.size,
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
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = title),
                       size = title.size) +
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
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = name),
                         size = name.size)
  } else if (location == "left") {
    gg.name <- ggplot2::ggplot(df) +
      ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = name),
                         size = name.size, angle = 90)
  }

  gg.name <- gg.name + theme_clust_labels

  return(gg.name)

}
