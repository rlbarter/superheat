
#' Generate supervised heatmaps.
#'
#' superheat is used to generate an exploratory plot that consists of a
#'        (clustered) matrix X displayed as heatmap. Scatterplots, boxplots,
#'        barplots and line plots can be plotted over the columns and rows
#'        of the heatmap, adding an additional layer of information.
#'
#'
#' @param X a matrix or data frame whose values are to be plotted in the heatmap.
#' @param X.text a matrix containing text entries to be plotted on
#'          top of the heatmap cells. The number of rows must match either the
#'          number of columns of \code{X} or the number of column clusters of
#'          \code{X} (depending on whether the columns are clustered). Similarly
#'          for the rows.
#' @param yt a vector of values to plot above the heatmap. The length of
#'          \code{yt} must be equal to the number of columns of \code{X}.
#' @param yr a vector of values to plot to the right of the heatmap.
#'          The length of \code{yr} must be equal to the number of rows of \code{X}.
#' @param membership.rows a vector specifying the cluster membership of the rows/observations in X. If not specified, the default settings
#'          are to perform hierarchical clustering on the rows.
#' @param membership.cols an optional vector specifying the cluster membership
#'          of the columns/variables in X. If not specified, the default settings
#'          are to present the columns in the order they are provided in \code{X}.


#' @param order.cols a vector of specifying the ordering of the columns of
#'          \code{X}. If clustering is performed (or a membership vector is
#'          provided), then this vector specifies the order within the clusters.
#'          Note that this vector must be a rearranged \code{1:ncol(X)} vector.
#' @param order.rows a vector of specifying the ordering of the rows of
#'          \code{X}. If clustering is performed (or a membership vector is
#'          provided), then this vector specifies the order within the clusters.
#'          Note that this vector must be a rearranged 1:nrow(X) vector.


#' @param n.clusters.rows a number specifying the number of row clusters. The
#'          default is 0 (indicating no clustering of the rows). This argument
#'          is ignored if \code{membership.rows} is provided.
#' @param n.clusters.cols a number specifying the number of column clusters. The
#'          default is 0 (indicating no clustering of the columns). This argument
#'          is ignored if \code{membership.columns} is provided.
#' @param clustering.method the clustering method to use whenever
#'          \code{n.clusters.cols} or \code{n.clusters.rows} is specified.
#'          The default ("kmeans") is to use K-means clustering, the alternative
#'          option ("hierarchical") performs heirarchical clustering. Antoher
#'          (suggested) alternative is to provide a membership vector.




#' @param smooth.heat a logical specifying whether or not to smooth the color
#'          of the heatmap within clusters.
#' @param scale a logical specifying whether or not to cener and scale the
#'          columns of X.


#' @param left.label the type of label provided to the left of the heatmap.
#'          The default is "cluster" (which provides the cluster names) if
#'          clustering was performed on the rowss. Otherwise, the default is
#'          "variable" (which provides the variable names). The final option
#'          ("none") removes the label all together.
#' @param bottom.label the type of label provided to the left of the heatmap.
#'          The default is "cluster" (which provides the cluster names) if
#'          clustering was performed on the columns. Otherwise, the default is
#'          "variable" (which provides the variable names). The final option
#'          ("none") removes the label all together.



#' @param heat.col.scheme A character specifying the heatmap colour scheme.
#'          The default is "red", and other options include "purple", "blue",
#'          "grey" and "green". If you wish to supply your own colour scheme,
#'          you can use the \code{heat.pal} argument.
#' @param heat.pal a vector specifying a manual heatmap color palette. This
#'          corresponds to the \code{color} argumnet for the ggplot2
#'          \code{\link[ggplot2]{scale_color_gradientn}} function.
#' @param heat.pal.values a vector specifying the location of each color in the
#'          color palette. Each entry should be a number between 0 and 1. This
#'          corresponds to the \code{values} argumnet for the ggplot2
#'          \code{\link[ggplot2]{scale_color_gradientn}} function. The default
#'          values correspond to the corresponding quantiles.


#'
#' @param X.text.size a single number or a matrix of numbers (whose dimension
#'          matches that of \code{X.text}) that specifies the size of each text
#'          entry in \code{X.text}.
#' @param X.text.col a single character string or a matrix of character strings
#'          (whose dimension matches that of \code{X.text}) that specifies the
#'          colors of each text entry in \code{X.text}.
#'
#'





#'
#' @param yt.plot.type a character specifying the \code{yt} plot type. The default is
#'          "scatter", and other options include "bar", "scattersmooth",
#'          "smooth", "boxplot", "scatterline" and "line".
#' @param yr.plot.type character specifying the \code{yr} plot type. The default is
#'          "scatter", and other options include "bar", "scattersmooth",
#'          "smooth", "boxplot", "scatterline" and "line".
#'
#' @param legend logical. If set to \code{FALSE}, then no legend is given.

#' @param grid.hline a logical specifying whether horizontal lines are
#'          plotted around the row clusters in the heatmap.
#' @param grid.vline a logical specifying whether vertical lines are
#'          plotted around the column clusters in the heatmap.
#' @param grid.hline.size the thickness of the cluster lines. The default is 0.5.
#' @param grid.vline.size the thickness of the cluster lines. The default is 0.5.
#' @param grid.hline.col the colour of the cluster lines.
#' @param grid.vline.col the colour of the cluster lines.
#'
#' @param smoothing.method if \code{plot.type = "scattersmooth"} or \code{"smooth"}
#'          this argument specifies the smoothing method to use. The default is
#'          \code{loess}. The alternative option is \code{lm}.
#' @param smooth.se a logical specifying whether the smoothed \code{yt} and \code{yr}
#'          curves should have a standard errors around them?



#' @param yt.axis a logical specifying the presence of an axis for the \code{yt}
#'          plot.
#' @param yr.axis a logical specifying the presence of an axis for the \code{yr}
#'          plot.
#' @param yt.axis.name a character specifying the \code{yt} axis label.
#' @param yr.axis.name a character specifying the \code{yr} axis label.
#' @param yr.axis.size a number specifying the size of the numbers on
#'          the axis.
#' @param yt.axis.size a number specifying the size of the numbers on
#'          the axis.
#' @param yt.axis.name.size a number specifying the size of the axis name.
#' @param yr.axis.name.size a number specifying the size of the axis name.
#' @param yt.axis.name.angle a number specifying the angle of the axis name.
#' @param yr.axis.name.angle a number specifying the angle of the axis name.
#' @param yt.num.ticks the number of ticks on the \code{yt} axis. This does
#'          not always work perfectly as it is coerced into looking pretty.
#' @param yr.num.ticks the number of ticks on the \code{yr} axis. This does
#'          not always work perfectly as it is coerced into looking pretty.
#' @param yt.plot.size a number specifying the size of the \code{yt} plot.
#' @param yr.plot.size a number specifying the size of the \code{yr} plot.
#' @param yt.line.size the thickness of the (smoothing) line in the \code{yt} plot.
#' @param yr.line.size the thickness of the (smoothing) line in the \code{yr} plot.
#'
#' @param yt.obs.col a vector specifying the colour of individual points in the
#'          \code{yt} plot.
#' @param yr.obs.col a vector specifying the colour of individual points in the
#'          \code{yr} plot.
#' @param yt.pal a vector the same length as the number of clusters which specifies
#'          the colour of each cluster in \code{yt}.
#' @param yr.pal a vector the same length as the number of clusters which specifies
#'          the colour of each cluster in \code{yr}.
#' @param yt.bar.col a character which specifies the colour of the boundary of the
#'          bars in the barplot of \code{yt}.
#' @param yr.bar.col a character which specifies the colour of the boundary of the
#'          bars in the barplot of \code{yr}.
#' @param yt.point.size the size of the points in the \code{yt} scatterplot. The
#'          default is 2.
#' @param yr.point.size the size of the points in the \code{yr} scatterplot. The
#'          default is 2.
#' @param yt.point.alpha the transparancy of the points in the \code{yt} scatterplot.
#'          The default is 1, which corresponds to no transparancy.
#' @param yr.point.alpha the transparancy of the points in the \code{yr} scatterplot.
#'          The default is 1, which corresponds to no transparancy.
#'
#' @param bottom.label.text.size the size of the bottom heatmap label text. The
#'          default is 5.
#' @param left.label.text.size the size of the left heatmap label text. The
#'          default is 5.
#' @param bottom.label.text.angle number of degrees to rotate the text on the bottom
#'          cluster/variable labels. The default is 0.
#' @param left.label.text.angle number of degrees to rotate the text on the left
#'          cluster/variable labels. The default is 90.
#' @param bottom.label.size a number specifying the size of the bottom
#'          cluster/variable label panel.
#' @param left.label.size a number specifying the size of the left
#'          cluster/variable label panel.
#'
#' @param left.label.pal a vector specifying the left cluster/variable label color
#'          palette.
#' @param bottom.label.pal a vector specifying the bottom cluster/variable label color
#'          palette.
#' @param left.label.text.col a character or character vector specifying the
#'          left cluster/variable label text color.
#' @param bottom.label.text.col a character or character vector specifying the
#'          bottom cluster/variable label text color.
#'
#' @param column.title a string specifying the overall column name (will appear
#'          below the bottom.labels).
#' @param row.title a string specifying the overall row name (will appear to the
#'          left of the left.labels).
#' @param column.title.size a number specifying the size of the column name. The
#'          default is 5.
#' @param row.title.size a number specifying the size of the row name. The
#'          default is 5.

#' @param legend.size a number specifying the size of the legend. The default
#'        is 2.
#' @param padding the amount (in cm) of white space (padding) around the plot.
#'          The default is 1cm.
#' @param title a character string specifying a main heading.
#' @param title.size the size of the title. The default is 5.
#' @param print.plot a logical specifying whether or not to output the plot.
#'
#'
#' @return \code{plot} a plot with the properties specified by the above arguments.
#' @return \code{membership.cols} the column cluster membership vector
#' @return \code{membership.rows} the row cluster membership vector
#' @examples
#' # plot a heatmap of the numerical iris variables
#' # cluster by species and plot Sepal.Length on the right
#' superheat(X = iris[,-c(1, 5)],
#'             yr = iris[,1],
#'             yr.axis.name = "Sepal.Length",
#'             membership.rows = iris$Species)
#' @importFrom magrittr "%>%"
#' @export




superheat <- function(X,
                      X.text = NULL,
                      yt = NULL,
                      yr = NULL,
                      membership.rows = NULL, # membership for rows
                      membership.cols = NULL, # membership for cols

                      n.clusters.rows = NULL,
                      n.clusters.cols = NULL,
                      clustering.method = c("kmeans", "hierarchical"),

                      order.cols = NULL, # how to order within clusters (must be an integer vector e..g c(1,3,2) means the ordering is the first, third then second observation
                      order.rows = NULL,

                      smooth.heat = FALSE,
                      scale = FALSE,

                      left.label = NULL,
                      bottom.label = NULL,

                      heat.col.scheme = c("red", "purple", "blue", "grey", "green"),
                      heat.pal = NULL,
                      heat.pal.values = NULL,

                      X.text.size = 5,
                      X.text.col = "black",

                      yt.plot.type = c("scatter","bar","boxplot","scattersmooth","smooth", "scatterline", "line"),
                      yr.plot.type = c("scatter","bar","boxplot","scattersmooth","smooth", "scatterline", "line"),

                      legend = TRUE,
                      legend.size = 2,

                      grid.hline = TRUE,
                      grid.vline = TRUE,
                      grid.hline.size = 0.5,
                      grid.vline.size = 0.5,
                      grid.hline.col = "black",
                      grid.vline.col = "black",

                      smoothing.method = c("loess","lm"),
                      smooth.se = TRUE,

                      yt.axis = T,
                      yr.axis = T,
                      yt.num.ticks = 3,
                      yr.num.ticks = 3,
                      yt.plot.size = 0.3,
                      yr.plot.size = 0.3,
                      yt.axis.name = NULL,
                      yr.axis.name = NULL,
                      yr.axis.size = 10,
                      yt.axis.size = 10,
                      yr.axis.name.size = 10,
                      yt.axis.name.size = 10,
                      yr.axis.name.angle = NULL,
                      yt.axis.name.angle = NULL,
                      yt.obs.col = NULL,
                      yr.obs.col = NULL,
                      yt.pal = NULL,
                      yr.pal = NULL,
                      yt.bar.col = NULL,
                      yr.bar.col = NULL,
                      yt.point.size = 2,
                      yt.point.alpha = 1,
                      yr.point.size = 2,
                      yr.point.alpha = 1,
                      yr.line.size = NULL,
                      yt.line.size = NULL,

                      bottom.label.text.size = 5,
                      left.label.text.size = 5,
                      bottom.label.text.angle = NULL,
                      left.label.text.angle = NULL,
                      bottom.label.size = 0.1,
                      left.label.size = 0.1,
                      left.label.pal = NULL,
                      bottom.label.pal = NULL,
                      left.label.text.col = NULL,
                      bottom.label.text.col = NULL,

                      column.title = NULL,
                      row.title = NULL,
                      column.title.size = 5,
                      row.title.size = 5,

                      padding = 1,
                      title = NULL,
                      title.size = 5,

                      print.plot = TRUE) {




  smoothing.method <- match.arg(smoothing.method)
  yt.plot.type <- match.arg(yt.plot.type)
  yr.plot.type <- match.arg(yr.plot.type)

  if (is.null(colnames(X))) colnames(X) <- 1:ncol(X)
  if (is.null(rownames(X))) rownames(X) <- 1:nrow(X)





  if (scale) {
    X <- scale(X)
  }
  ##################### run error check on arguments
  stop.arg.list <- c(as.list(environment()))
  stop.arg.list <- stop.arg.list[names(formals(stop_errors))]
  stop.arg.list <- stop.arg.list[!is.na(names(stop.arg.list))]

  do.call(stop_errors, stop.arg.list)


  ##################### set variables ##########################
  heat.col.scheme <- match.arg(heat.col.scheme)

  if (is.null(yr.axis.name)) {
    yr.axis.name <- eval(substitute(internala(yr)))
  }

  if (is.null(yt.axis.name)) {
    yt.axis.name <- eval(substitute(internala(yt)))
  }


  if (!is.null(membership.cols) | (!is.null(n.clusters.cols) && n.clusters.cols > 0)) {
    cluster.cols <- TRUE
  } else {
    cluster.cols = FALSE
  }

  if (!is.null(membership.rows) | (!is.null(n.clusters.rows) && n.clusters.rows > 0)) {
    cluster.rows <- TRUE
  } else {
    cluster.rows = FALSE
  }




  if (is.null(left.label) && !cluster.rows) {
    left.label <- "variable"
  } else if (is.null(left.label) && cluster.rows) {
    left.label <- "cluster"
  }



  if (is.null(bottom.label) && cluster.cols) {
    bottom.label <- "cluster"
  } else if (is.null(bottom.label) && !cluster.cols) {
    bottom.label <- "variable"
  }

  # remove variable labels if more than 50 rows
  if (left.label == "variable") {
    if (nrow(X) > 50) {
      # warning('Cannot set "left.label" to "variable" when nrow(X) exceeds 50')
      left.label <- "none"
    }
  }

  if (bottom.label == "variable") {
    if (ncol(X) > 50) {
      # warning('Cannot set "bottom.label" to "variable" when ncol(X) exceeds 50')
      bottom.label <- "none"
    }
  }


  # remove the cluster boxes if we have more than 100 cols/rows
  if (!cluster.cols & ((bottom.label == "variable") | (bottom.label == "none"))) {
    if (ncol(X) > 100) {
    #  warning('"grid.vline" set to FALSE when ncol(X) exceeds 100 and when "bottom.label" is set to either "variable" or "none"')
      grid.vline <- FALSE
    }
  }

  if (!cluster.rows & ((left.label == "variable") | (left.label == "none"))) {
    if (nrow(X) > 100) {
    #  warning('"grid.hline" set to FALSE when nrow(X) exceeds 100 and when "left.label" is set to either "variable" or "none"')
      grid.hline <- FALSE
    }
  }



  ##################### perform clustering if needed ######################


  if (is.null(membership.rows) && cluster.rows) {
    n.clusters <- n.clusters.rows
    cluster.arg.list <- c(as.list(environment()))
    cluster.arg.list <- cluster.arg.list[names(formals(generate_cluster))]
    cluster.arg.list <- cluster.arg.list[!is.na(names(cluster.arg.list))]


    # do a heat to make sure that valid args have been provided
    membership.rows <- do.call(generate_cluster, cluster.arg.list)
  }

  if (is.null(membership.cols) && cluster.cols) {
    n.clusters <- n.clusters.cols
    cluster.arg.list <- c(as.list(environment()))
    cluster.arg.list <- cluster.arg.list[names(formals(generate_cluster))]
    cluster.arg.list <- cluster.arg.list[!is.na(names(cluster.arg.list))]
    cluster.arg.list$X <- t(cluster.arg.list$X)

    # do a heat to make sure that valid args have been provided
    membership.cols <- do.call(generate_cluster, cluster.arg.list)
  }






  ######################## the default if clustering was not performed ###############


  if (!cluster.cols) {
    membership.cols <- 1:ncol(X)
  }
  if (!cluster.rows) {
    membership.rows <- 1:nrow(X)
  }






  ####################### Set the observation order within clusters ####################

  if (is.null(order.rows)) {
    order.rows <- 1:nrow(X)
  }
  if (is.null(order.cols)) {
    order.cols <- 1:ncol(X)
  }


  if (cluster.rows) {
    order.df.rows <- data.frame(membership.rows = membership.rows[order.rows], order.rows = order.rows)
    order.df.rows <- order.df.rows %>% dplyr::arrange(membership.rows)
    order.df.rows <- dplyr::ungroup(order.df.rows)
  } else {
    order.df.rows <- data.frame(membership.rows = 1, order.rows = order.rows)
  }

  if (cluster.cols) {
    order.df.cols <- data.frame(membership.cols = membership.cols[order.cols], order.cols = order.cols)
    order.df.cols <- order.df.cols %>% dplyr::arrange(membership.cols)
    order.df.cols <- dplyr::ungroup(order.df.cols)
  } else {
    order.df.cols <- data.frame(membership.cols = 1, order.cols = order.cols)
  }


  ###################### Reorder X, yr and yt ######################

  X <- X[order.df.rows$order.rows, order.df.cols$order.cols]
  if (!is.null(yr)) {
    yr <- yr[order.df.rows$order.rows]
  }
  if (!is.null(yt)) {
    yt <- yt[order.df.cols$order.cols]
  }
  membership.rows <- membership.rows[order.df.rows$order.rows]
  membership.cols <- membership.cols[order.df.cols$order.cols]




  ################### Generate the heatmap #########################




  if (grid.hline) {
    if (left.label == "variable") {
      membership.r <- 1:nrow(X)
    } else if (left.label == "cluster") {
      membership.r <- membership.rows
    }
  }
  if (grid.vline) {
    if (bottom.label == "variable") {
      membership.c <- 1:ncol(X)
    } else if (bottom.label == "cluster") {
      membership.c <- membership.cols
    }
  }


  heat.arg.list <- c(as.list(environment()))
  heat.arg.list <- heat.arg.list[names(formals(generate_heat))]
  heat.arg.list <- heat.arg.list[!is.na(names(heat.arg.list))]

  if (smooth.heat) {
    heat <- do.call(generate_smooth_heat, heat.arg.list)
  } else {
    heat <- do.call(generate_heat, heat.arg.list)
  }
  gg.heat <- heat$gg.heat
  if (legend) {
    gg.legend <- heat$gg.legend
  }







  ######################## Generate the scatterplots ########################

  if (!is.null(yt)) {
    y <- yt
    y.obs.col <- yt.obs.col
    y.pal <- yt.pal
    y.bar.col <- yt.bar.col
    y.line.size <- yt.line.size
    membership <- membership.cols
    location <- "top"
    axis.name <- yt.axis.name
    axis.size <- yt.axis.size
    axis.name.size <- yt.axis.name.size
    axis.name.angle <- yt.axis.name.angle
    point.size <- yt.point.size
    point.alpha <- yt.point.alpha
    plot.type <- yt.plot.type
    num.ticks <- yt.num.ticks

    scatter.arg.list <- c(as.list(environment()))
    scatter.arg.list <- scatter.arg.list[names(formals(generate_scatter))]
    scatter.arg.list <- scatter.arg.list[!is.na(names(scatter.arg.list))]


    gg.top <- do.call(generate_scatter, scatter.arg.list)
  }

  if (!is.null(yr)) {
    y <- yr
    y.obs.col <- yr.obs.col
    y.pal <- yr.pal
    y.bar.col <- yr.bar.col
    y.line.size <- yr.line.size
    membership <- membership.rows
    location <- "right"
    axis.name <- yr.axis.name
    axis.size <- yr.axis.size
    axis.name.size <- yr.axis.name.size
    axis.name.angle <- yr.axis.name.angle
    point.size <- yr.point.size
    point.alpha <- yr.point.alpha
    plot.type <- yr.plot.type
    num.ticks <- yr.num.ticks

    scatter.arg.list <- c(as.list(environment()))
    scatter.arg.list <- scatter.arg.list[names(formals(generate_scatter))]
    scatter.arg.list <- scatter.arg.list[!is.na(names(scatter.arg.list))]


    gg.right <- do.call(generate_scatter, scatter.arg.list)
  }







  ##################### Generate the heatmap labels ##############################






  if (bottom.label == "variable") {
    names <- colnames(X)
    location <- "bottom"
    label.pal <- bottom.label.pal
    label.text.col <- bottom.label.text.col
    text.angle <- bottom.label.text.angle

    var.arg.list <- c(as.list(environment()))
    var.arg.list <- var.arg.list[names(formals(generate_var_label))]
    var.arg.list <- var.arg.list[!is.na(names(var.arg.list))]

    gg.bottom <- do.call(generate_var_label, var.arg.list)
  } else if (bottom.label == "cluster") {

    location <- "bottom"
    membership <- membership.cols
    label.pal = bottom.label.pal
    label.text.col = bottom.label.text.col
    text.angle <- bottom.label.text.angle

    clust.lab.arg.list <- c(as.list(environment()))
    clust.lab.arg.list <- clust.lab.arg.list[names(formals(generate_cluster_label))]
    clust.lab.arg.list <- clust.lab.arg.list[!is.na(names(clust.lab.arg.list))]

    gg.bottom <- do.call(generate_cluster_label, clust.lab.arg.list)
  }







  if (left.label == "variable") {
    names <- rownames(X)
    location <- "left"
    label.pal = left.label.pal
    label.text.col = left.label.text.col
    text.angle <- left.label.text.angle

    var.arg.list <- c(as.list(environment()))
    var.arg.list <- var.arg.list[names(formals(generate_var_label))]
    var.arg.list <- var.arg.list[!is.na(names(var.arg.list))]

    gg.left <- do.call(generate_var_label, var.arg.list)
  } else if (left.label == "cluster") {

    location <- "left"
    membership <- membership.rows
    label.pal = left.label.pal
    label.text.col = left.label.text.col
    text.angle <- left.label.text.angle

    clust.lab.arg.list <- c(as.list(environment()))
    clust.lab.arg.list <- clust.lab.arg.list[names(formals(generate_cluster_label))]
    clust.lab.arg.list <- clust.lab.arg.list[!is.na(names(clust.lab.arg.list))]

    gg.left <- do.call(generate_cluster_label, clust.lab.arg.list)
  }




  ################ Generate title ###############

  if (!is.null(title)) {
    gg.title <- generate_title(title = title, title.size = title.size)
  }


  if (!is.null(column.title)) {
    gg.column.title <- generate_names(name = column.title,
                                     name.size = column.title.size,
                                     location = "bottom")
  }

  if (!is.null(row.title)) {
    gg.row.title <- generate_names(name = row.title,
                                  name.size = row.title.size,
                                  location = "left")
  }



  ################### Generate desired layout #################################

  layout.arg.list <- c(as.list(environment()))
  layout.arg.list <- layout.arg.list[names(formals(generate_layout))]
  layout.arg.list <- layout.arg.list[!is.na(names(layout.arg.list))]


  layout <- do.call(generate_layout, layout.arg.list)


  # place grobs in layout
  grob.arg.list <- c(as.list(environment()))
  grob.arg.list <- grob.arg.list[names(formals(generate_grobs))]
  grob.arg.list <- grob.arg.list[!is.na(names(grob.arg.list))]

  grob.layout <- do.call(generate_grobs, grob.arg.list)

  if (print.plot) {
    grid::grid.newpage()
    grid::grid.draw(grob.layout)
  }

  to.return <- list(layout = layout, plot = grob.layout, membership.cols = membership.cols, membership.rows = membership.rows)

  return(invisible(to.return))


}
