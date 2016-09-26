
#' Generate supervised heatmaps.
#'
#' Superheat is used to generate and customize heatmaps.
#'        Scatterplots, boxplots, barplots, line plots and boxplots can
#'        be plotted adjacent to the columns and rows of the heatmap,
#'        adding an additional layer of information.
#'
#'
#' @param X a matrix whose values are to be plotted in the heatmap.
#' @param X.text a matrix containing text entries to be plotted on
#'          top of the heatmap cells. The number of rows/columns must match
#'          either the number of rows/columns of \code{X} or the number of
#'          row/column clusters of \code{X}.
#' @param yt a vector of values to plot above the heatmap (the "top plot").
#'          The length of \code{yt} must be equal to the number of columns
#'          of \code{X}.
#' @param yr a vector of values to plot to the right of the heatmap (the
#'          "right plot"). The length of \code{yr} must be equal to the
#'          number of rows of \code{X}.
#' @param yt.plot.type a character specifying the \code{yt} plot type. The default is
#'          "scatter", and other options include "bar", "scattersmooth",
#'          "smooth", "boxplot", "scatterline" and "line". A special type
#'          "dendrogram", can only be used when \code{yt} is not specified
#'          and clustering is not performed.
#'          Instead, a dendrogram will be plotted next to the rows, and the
#'          rows will be reordered accordingly.
#' @param yr.plot.type character specifying the \code{yr} plot type. The default is
#'          "scatter", and other options include "bar", "scattersmooth",
#'          "smooth", "boxplot", "scatterline", "line". A special type
#'          "dendrogram", can only be used when \code{yr} is not specified
#'          and clustering is not performed. Instead, a dendrogram will be plotted
#'          next to the rows, and the rows will be reordered accordingly.
#' @param membership.rows a vector specifying the cluster membership
#'          of the rows in X.
#' @param membership.cols a vector specifying the cluster membership
#'          of the columns in X.


#' @param order.cols a vector of specifying the ordering of the
#'          columns of \code{X}. If the columns are clustered, then this
#'          vector specifies the order within the clusters. Note that
#'          this vector must be a rearranged \code{1:ncol(X)} vector which
#'          specifies the new location of each column.
#' @param order.rows a vector of specifying the ordering of the rows of
#'          \code{X}. If the rows are clustered, then this vector
#'          specifies the order within the clusters. Note that this
#'          vector must be a rearranged \code{1:nrow(X)} vector which
#'          specifies the new location of each row.


#' @param n.clusters.rows a number specifying the number of row clusters to
#'          generate. The default is 0 (indicating no clustering of the rows).
#'          This argument is ignored if \code{membership.rows} is provided.
#' @param n.clusters.cols a number specifying the number of column clusters to
#'          generate. The default is 0 (indicating no clustering of the columns).
#'          This argument is ignored if \code{membership.columns} is provided.
#' @param clustering.method the clustering method to use whenever
#'          \code{n.clusters.cols} or \code{n.clusters.rows} is specified.
#'          The default ("kmeans") is to use K-means clustering, the alternative
#'          option ("hierarchical") performs hierarchical clustering. Another
#'          (suggested) alternative is to provide a row and/or column
#'          membership vector.
#' @param dist.method the distance method to use for hierarchical clustering.
#'          This must be one of "euclidean", "maximum", "manhattan",
#'          "canberra", "binary" or "minkowski".
#'





#' @param smooth.heat a logical specifying whether or not to smooth the colour
#'          of the heatmap within clusters (by taking the median value).
#' @param scale a logical specifying whether or not to center and scale the
#'          columns of X.


#' @param left.label a character specifying the type of the label provided to
#'          the left of the heatmap. If clustering was performed on the rows,
#'          then the default type is "cluster" (which provides the cluster
#'          names). Otherwise, the default is "variable" (which provides the
#'          variable names). The final option, "none", removes the left labels
#'          all together.
#' @param bottom.label a character specifying the type of the label provided
#'          to the left of the heatmap. If clustering was performed on the
#'          columns, then the default type is "cluster" (which provides the
#'          cluster names). Otherwise, the default is "variable" (which
#'          provides the variable names). The final option, "none", removes the
#'          label all together.



#' @param heat.col.scheme A character specifying the heatmap colour scheme.
#'          The default is "red", and other options include "viridis", purple",
#'          "blue", "grey" and "green". If you wish to supply your own colour
#'          scheme, use the \code{heat.pal} argument.
#' @param heat.pal a vector of colour names specifying a manual heatmap colour
#'          palette. This corresponds to the \code{colour} argument for the
#'          ggplot2 \code{\link[ggplot2]{scale_colour_gradientn}} function.
#' @param heat.pal.values a vector specifying the location of each colour in the
#'          colour palette specified by \code{heat.pal}. Each entry should be a
#'          number between 0 and 1. This corresponds to the \code{values}
#'          argument for the ggplot2
#'          \code{\link[ggplot2]{scale_colour_gradientn}} function. The default
#'          values are the corresponding quantiles.
#' @param heat.na.col the color for NA values in the heatmap.


#' @param X.text.size a single number or a matrix of numbers (whose dimension
#'          matches that of \code{X.text}) that specifies the size of each text
#'          entry in \code{X.text}.
#' @param X.text.angle a single number or a matrix of numbers (whose dimension
#'          matches that of \code{X.text}) that specifies the angle of each text
#'          entry in \code{X.text}.
#' @param X.text.col a single character string or a matrix of character strings
#'          (whose dimension matches that of \code{X.text}) that specifies the
#'          colours of each text entry in \code{X.text}.
#'
#'
#'

#'
#' @param legend logical. If set to \code{FALSE}, then no legend is provided.

#' @param grid.hline a logical specifying whether horizontal grid lines are
#'          plotted in the heatmap.
#' @param grid.vline a logical specifying whether vertical grid lines are
#'          plotted in the heatmap.
#' @param grid.hline.size the thickness of the horizontal grid lines.
#'          The default is 0.5.
#' @param grid.vline.size the thickness of the vertical grid lines.
#'          The default is 0.5.
#' @param grid.hline.col the colour of the horizontal grid lines.
#' @param grid.vline.col the colour of the vertical grid lines.
#' @param force.grid.hline a logical describing whether or not to force the
#'          horizontal grid lines to appear (relevant only when X has more
#'          than 50 rows). Note that by default there are no horizontal
#'          grid lines when there are more than 50 rows.
#' @param force.grid.vline a logical describing whether or not to force the
#'          vertical grid lines to appear (relevant only when X has more
#'          than 50 columns). Note that by default there are no vertical
#'          grid lines when there are more than 50 columns.
#'
#' @param smoothing.method if \code{plot.type = "scattersmooth"} or
#'          \code{"smooth"}, this argument specifies the smoothing method to
#'          use. The default is "loess" for a curve. The alternative option is
#'          "lm" for a line.
#' @param smooth.se a logical specifying whether the smoothed \code{yt} and \code{yr}
#'          curves have standard error curves.



#' @param yt.axis a logical specifying the presence of an axis for the
#'          \code{yt} plot.
#' @param yr.axis a logical specifying the presence of an axis for the
#'          \code{yr} plot.
#' @param yt.axis.name a character specifying the \code{yt} axis name.
#' @param yr.axis.name a character specifying the \code{yr} axis name.
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
#' @param yt.line.size the thickness of the (smoothing) line in the \code{yt}
#'          plot.
#' @param yr.line.size the thickness of the (smoothing) line in the \code{yr}
#'          plot.
#'
#' @param yt.obs.col a vector specifying the colour of individual points in the
#'          \code{yt} plot.
#' @param yr.obs.col a vector specifying the colour of individual points in the
#'          \code{yr} plot.
#' @param yt.cluster.col a vector the same length as the number of clusters
#'          which specifies the colour of each cluster in \code{yt}.
#' @param yr.cluster.col a vector the same length as the number of clusters
#'          which specifies the colour of each cluster in \code{yr}.
#' @param yt.bar.col a character which specifies the colour of the boundary of
#'          the bars in the barplot of \code{yt}.
#' @param yr.bar.col a character which specifies the colour of the boundary of
#'          the bars in the barplot of \code{yr}.
#' @param yt.point.size the size of the points in the \code{yt} scatterplot.
#'          The default is 2.
#' @param yr.point.size the size of the points in the \code{yr} scatterplot.
#'          The default is 2.
#' @param yt.point.alpha the transparency of the points in the \code{yt}
#'          scatterplot. The default is 1, which corresponds to no
#'          transparency.
#' @param yr.point.alpha the transparency of the points in the \code{yr}
#'          scatterplot. The default is 1, which corresponds to no
#'          transparency.
#'
#' @param bottom.label.text.size the size of the bottom heatmap label text. The
#'          default is 5.
#' @param left.label.text.size the size of the left heatmap label text. The
#'          default is 5.
#' @param bottom.label.text.angle number of degrees to rotate the text on the
#'          bottom cluster/variable labels. The default is 90.
#' @param left.label.text.angle number of degrees to rotate the text on the
#'          left cluster/variable labels. The default is 0.
#' @param bottom.label.size a number specifying the size of the bottom
#'          cluster/variable label panel.
#' @param left.label.size a number specifying the size of the left
#'          cluster/variable label panel.
#' @param bottom.label.text.alignment the text alignment of the label text. The
#'          default is "center". Alternate options are "left" and "right".
#' @param left.label.text.alignment the text alignment of the label text. The
#'          default is "center". Alternate options are "left" and "right".
#'
#' @param left.label.col a vector specifying the left cluster/variable label
#'          colour palette.
#' @param bottom.label.col a vector specifying the bottom cluster/variable
#'          label colour palette.
#' @param left.label.text.col a character or character vector specifying the
#'          left cluster/variable label text colour.
#' @param bottom.label.text.col a character or character vector specifying the
#'          bottom cluster/variable label text colour.
#'
#' @param force.bottom.label a logical describing whether or not to force the
#'          bottom labels to appear (relevant only when X has more than 50
#'          columns). Note that by default there are no labels when there are
#'          more than 50 columns.
#' @param force.left.label a logical describing whether or not to force the
#'          left labels to appear (relevant only when X has more than 50
#'          rows). Note that by default there are no labels when there are
#'          more than 50 rows.
#'
#' @param column.title a string specifying the overall column name (located
#'          below the bottom.labels).
#' @param row.title a string specifying the overall row name (located to the
#'          left of the left.labels).
#' @param column.title.size a number specifying the size of the column name. The
#'          default is 5.
#' @param row.title.size a number specifying the size of the row name. The
#'          default is 5.

#' @param legend.height a number specifying the height of the legend. The default
#'        is 0.1.
#' @param legend.width a number specifying the width of the legend. The default
#'        is 1.5.
#' @param legend.text.size a number specifying the size of the numbers on the
#'        legend axis. The default is 12.
#' @param padding the amount (in cm) of white space (padding) around the plot.
#'          The default is 1 cm.
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
#' # save the superheat object to access the membership vectors
#' sh <- superheat(X = iris[,-c(1, 5)],
#'                 yr = iris[,1],
#'                 yr.axis.name = "Sepal.Length",
#'                 membership.rows = iris$Species)
#' sh$membership.rows
#' @importFrom magrittr "%>%"
#' @importFrom stats "as.dist" "cor" "cutree" "dist" "hclust" "kmeans"
#'                   "median" "quantile"
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
                      dist.method = c("euclidean", "maximum", "manhattan",
                                      "canberra", "binary", "minkowski"),

                      order.cols = NULL,
                      order.rows = NULL,

                      smooth.heat = FALSE,
                      scale = FALSE,

                      left.label = NULL,
                      bottom.label = NULL,

                      heat.col.scheme = c("red", "viridis", "purple", "blue",
                                          "grey", "green"),
                      heat.pal = NULL,
                      heat.pal.values = NULL,
                      heat.na.col = "grey50",

                      X.text.size = 5,
                      X.text.col = "black",
                      X.text.angle = 0,

                      yt.plot.type = c("scatter", "bar", "boxplot",
                                       "scattersmooth", "smooth",
                                       "scatterline", "line",
                                       "dendrogram"),
                      yr.plot.type = c("scatter", "bar", "boxplot",
                                       "scattersmooth","smooth",
                                       "scatterline", "line",
                                       "dendrogram"),

                      legend = TRUE,
                      legend.height = 0.1,
                      legend.width = 1.5,
                      legend.text.size = 12,

                      grid.hline = TRUE,
                      grid.vline = TRUE,
                      grid.hline.size = 0.5,
                      grid.vline.size = 0.5,
                      grid.hline.col = "black",
                      grid.vline.col = "black",
                      force.grid.hline = F,
                      force.grid.vline = F,

                      smoothing.method = c("loess", "lm"),
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
                      yt.cluster.col = NULL,
                      yr.cluster.col = NULL,
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
                      bottom.label.size = 0.2,
                      left.label.size = 0.2,
                      left.label.col = NULL,
                      bottom.label.col = NULL,
                      left.label.text.col = NULL,
                      bottom.label.text.col = NULL,
                      left.label.text.alignment = c("center", "left", "right"),
                      bottom.label.text.alignment = c("center", "left",
                                                      "right"),
                      force.left.label = F,
                      force.bottom.label = F,

                      column.title = NULL,
                      row.title = NULL,
                      column.title.size = 5,
                      row.title.size = 5,

                      padding = 1,
                      title = NULL,
                      title.size = 5,

                      print.plot = TRUE) {
  # The primary superheat function for plotting super heatmaps.

  # match the arguments to those provided
  smoothing.method <- match.arg(smoothing.method)
  yt.plot.type <- match.arg(yt.plot.type)
  yr.plot.type <- match.arg(yr.plot.type)
  heat.col.scheme <- match.arg(heat.col.scheme)
  dist.method <- match.arg(dist.method)

  # if there are no column/row names, number them numerically
  if (is.null(colnames(X))) {
    colnames(X) <- 1:ncol(X)
  }
  if (is.null(rownames(X))) {
    rownames(X) <- 1:nrow(X)
  }

  # should you standardize the matrix
  # (each column will have mean 0 and variance 1)
  if (scale) {
    X <- scale(X)
  }
  # run error check on arguments
  stop.arg.list <- c(as.list(environment()))
  stop.arg.list <- stop.arg.list[names(formals(stop_errors))]
  stop.arg.list <- stop.arg.list[!is.na(names(stop.arg.list))]
  do.call(stop_errors, stop.arg.list)




  # set a logical to true if we are plotting a dendogram
  if (yr.plot.type == "dendrogram") {
    row.dendrogram <- T
  }

  if (yt.plot.type == "dendrogram") {
    col.dendrogram <- T
  }


  # if there is no yt or yr axis name provided, set the name to the name of
  # the object provided by the yr/yt argument
  if (is.null(yr.axis.name)) {
    yr.axis.name <- eval(substitute(internala(yr)))
  }
  if (is.null(yt.axis.name)) {
    yt.axis.name <- eval(substitute(internala(yt)))
  }

  # if there is a column (row) membership vector or a number of clusters to
  # generate is provided, then set cluster.cols to TRUE
  if (!is.null(membership.cols) |
      (!is.null(n.clusters.cols) && n.clusters.cols > 0)) {
    cluster.cols <- TRUE
  } else {
    cluster.cols <- FALSE
  }
  if (!is.null(membership.rows) |
      (!is.null(n.clusters.rows) && n.clusters.rows > 0)) {
    cluster.rows <- TRUE
  } else {
    cluster.rows <- FALSE
  }

  # how many column clusters
  if (cluster.cols) {
    if (!is.null(n.clusters.cols)) {
      n.col.clusters <- n.clusters.cols
    } else if (!is.null(membership.cols)) {
      n.col.clusters <- length(unique(membership.cols))
    }
  }

  # how many row clusters
  if (cluster.rows) {
    if (!is.null(n.clusters.rows)) {
      n.row.clusters <- n.clusters.rows
    } else if (!is.null(membership.rows)) {
      n.row.clusters <- length(unique(membership.rows))
    }
  }
  if ((!cluster.cols && !is.null(yt) && (length(yt) != ncol(X))) |
      ((cluster.cols && !is.null(yt) && (length(yt) != n.col.clusters)) &&
       (cluster.cols && !is.null(yt) && (length(yt) != ncol(X))))) {
    stop(paste("'yt' must have length equal to either the number of columns",
               "of 'X' or the number of column clusters of 'X'."))
  }

  if ((!cluster.rows && !is.null(yr) && (length(yr) != nrow(X))) |
      ((cluster.rows && !is.null(yr) && (length(yr) != n.row.clusters)) &&
       (cluster.rows && !is.null(yr) && (length(yr) != nrow(X))))) {
    stop(paste("'yr' must have length equal to either the number of rows",
               "of 'X' or the number of row clusters of 'X'."))
  }

  # shoot an error if a top plot is provided and is set to boxplot
  # but the columns are not clustered. Reason being that boxplots need to
  # aggregate data.
  if (!is.null(yt) && !cluster.cols && (yt.plot.type == "boxplot")) {
    stop("Cannot set yt.plot.type = 'boxplot' without clustering the columns.")
  }
  # shoot an error if a right plot is provided and is set to boxplot
  # but the rows are not clustered. Reason being that boxplots need to
  # aggregate data.
  if (!is.null(yr) && !cluster.rows && (yr.plot.type == "boxplot")) {
    stop("Cannot set yr.plot.type = 'boxplot' without clustering the rows.")
  }


  # spit out an error if someone tries to put in a dendrogram without
  # doing hierarchical clustering
  if (cluster.cols && (yt.plot.type == "dendrogram")) {
    stop("Cannot perform column clustering while placing a dendrogram")
  }
  if (cluster.rows && (yr.plot.type == "dendrogram")) {
    stop("Cannot perform row clustering while placing a dendrogram")
  }

  if (!is.null(yr) && (yr.plot.type == "dendrogram")) {
    stop("Cannot set 'yr' when placing a dendrogram")
  }
  if (!is.null(yt) && (yt.plot.type == "dendrogram")) {
    stop("Cannot set 'yt' when placing a dendrogram")
  }



  # if there are no row labels provided and cluster.rows is FALSE,
  # then set the default label type to be "variable",
  # otherwise set it to "TRUE"cluster"
  if (is.null(left.label) && !cluster.rows) {
    left.label <- "variable"
  } else if (is.null(left.label) && cluster.rows) {
    left.label <- "cluster"
  }

  # if there are no bottom labels provided and cluster.cols is FALSE,
  # then set the default label type to be "variable",
  # otherwise set it to "TRUE"cluster"
  if (is.null(bottom.label) && cluster.cols) {
    bottom.label <- "cluster"
  } else if (is.null(bottom.label) && !cluster.cols) {
    bottom.label <- "variable"
  }

  # remove variable labels if more than 50 rows/cols
  if ((left.label == "variable") && !force.left.label) {
    if (nrow(X) > 100) {
      left.label <- "none"
    }
  }
  if (bottom.label == "variable" && !force.bottom.label) {
    if (ncol(X) > 100) {
      bottom.label <- "none"
    }
  }

  # remove the heatmap grid lines if there are more than 50 cols/rows
  # do this only when there are variable labels or no labels
  # (but we want there to be grid lines when there are more than 50
  #  rows/columns but we are grouping by cluster. In this case the grid
  #  lines correspond to the clusters rather than the variables)
  if (!cluster.cols &
      ((bottom.label == "variable") | (bottom.label == "none"))) {
    if ((ncol(X) > 50) && !force.grid.vline) {
       grid.vline <- FALSE
    }
  }
  if (!cluster.rows &
      ((left.label == "variable") | (left.label == "none"))) {
    if ((nrow(X) > 50) && !force.grid.hline) {
      grid.hline <- FALSE
    }
  }

  # if cluster.rows is TRUE and no row membership is provided,
  # then perform clustering
  if (is.null(membership.rows) && cluster.rows) {
    # identify the number of row clusters specified
    n.clusters <- n.clusters.rows
    # perform the clustering
    cluster.arg.list <- c(as.list(environment()))
    cluster.arg.list <- cluster.arg.list[names(formals(generate_cluster))]
    cluster.arg.list <- cluster.arg.list[!is.na(names(cluster.arg.list))]
    # extract the membership vector
    clustering <- do.call(generate_cluster, cluster.arg.list)

    membership.rows <- clustering$membership
    clust.rows <- clustering$clust
  }

  # if cluster.cols is TRUE and no column membership is provided,
  # then perform clustering
  if (is.null(membership.cols) && cluster.cols) {
    # identify the number of row clusters specified
    n.clusters <- n.clusters.cols
    # perform the clustering
    cluster.arg.list <- c(as.list(environment()))
    cluster.arg.list <- cluster.arg.list[names(formals(generate_cluster))]
    cluster.arg.list <- cluster.arg.list[!is.na(names(cluster.arg.list))]
    cluster.arg.list$X <- t(cluster.arg.list$X)
    # extract the membership vector
    clustering <- do.call(generate_cluster, cluster.arg.list)

    membership.cols <- clustering$membership
    clust.cols <- clustering$clust
  }


  # note that we must obtain the hierarchical clustering
  # after rearranging the order of the rows and columns
  if (yt.plot.type == "dendrogram") {
    clust.cols <- hclust(dist(t(X), method = dist.method))
  }

  if (yr.plot.type == "dendrogram") {
    clust.rows <- hclust(dist(X, method = dist.method))
  }




  # the default if clustering was not performed
  if (!cluster.cols) {
    membership.cols <- 1:ncol(X)
  }
  if (!cluster.rows) {
    membership.rows <- 1:nrow(X)
  }

  # if a specific row/col ordering is not provided,
  # define the ordering to be that given in the original matrix
  if (is.null(order.rows) && (yr.plot.type != "dendrogram")) {
    order.rows <- 1:nrow(X)
  }
  if (is.null(order.cols) && (yt.plot.type != "dendrogram")) {
    order.cols <- 1:ncol(X)
  }


  # if there is a dendrogram, order rows/cols by mean
  if (yr.plot.type == "dendrogram") {
    order.rows <- clust.rows$order
  }
  if (yt.plot.type == "dendrogram") {
    order.cols <- clust.cols$order
  }


  # re-order the rows by cluster
  if (cluster.rows) {
    order.df.rows <- data.frame(membership.rows = membership.rows[order.rows],
                                order.rows = order.rows)
    order.df.rows <- order.df.rows %>%
      dplyr::arrange(membership.rows) %>%
      dplyr::ungroup()
  } else {
    # if there is no clustering, just put all rows in the same cluster
    order.df.rows <- data.frame(membership.rows = 1,
                                order.rows = order.rows)
  }
  # re-order the columns by cluster
  if (cluster.cols) {
    order.df.cols <- data.frame(membership.cols = membership.cols[order.cols],
                                order.cols = order.cols)
    order.df.cols <- order.df.cols %>%
      dplyr::arrange(membership.cols) %>%
      dplyr::ungroup()
  } else {
    # if there is no clustering, just put all columns in the same cluster
    order.df.cols <- data.frame(membership.cols = 1, order.cols = order.cols)
  }

  # Reorder X, yr and yt based on the new ordering
  X <- X[order.df.rows$order.rows, order.df.cols$order.cols]
  if (!is.null(yr)) {
    # only rearrange within cluster if the right plot is for each
    # data point (rather than for each cluster)
    if (length(yr) == nrow(X)) {
      yr <- yr[order.df.rows$order.rows]
    }
  }
  if (!is.null(yt)) {
    # only rearrange within cluster if the top plot is for each
    # data point (rather than for each cluster)
    if (length(yt) == ncol(X)) {
      yt <- yt[order.df.cols$order.cols]
    }
  }
  membership.rows <- membership.rows[order.df.rows$order.rows]
  membership.cols <- membership.cols[order.df.cols$order.cols]











  # Extract the arguments relevant to the heatmap function
  heat.arg.list <- c(as.list(environment()))
  heat.arg.list <- heat.arg.list[names(formals(generate_heat))]
  heat.arg.list <- heat.arg.list[!is.na(names(heat.arg.list))]

  # if heatmap smoothing is specified, use the generate_smooth_heat function,
  # otherwise, use the generate_heat function
  if (smooth.heat) {
    heat <- do.call(generate_smooth_heat, heat.arg.list)
  } else {
    heat <- do.call(generate_heat, heat.arg.list)
  }
  # extract the heatmap object from the output
  gg.heat <- heat$gg.heat
  # extract the legend object from the output
  if (legend) {
    gg.legend <- heat$gg.legend
  }

  # Generate the top and right plots
  if (!is.null(yt) && (yt.plot.type != "dendrogram")) {
    # define all arguments of the top plot
    y <- yt
    y.obs.col <- yt.obs.col
    y.cluster.col <- yt.cluster.col
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

    # generate the top plot
    # identify variables defined in the environment
    plot.arg.list <- c(as.list(environment()))
    # identify the possible arguments for generate_add_on_plot
    plot.arg.list <- plot.arg.list[names(formals(generate_add_on_plot))]
    # filter the variables in the environment to those that are arguments
    # for generate_add_on_plot
    plot.arg.list <- plot.arg.list[!is.na(names(plot.arg.list))]
    gg.top <- do.call(generate_add_on_plot, plot.arg.list)
  } else if (yt.plot.type == "dendrogram") {
    suppressMessages(gg.top <- ggdendro::ggdendrogram(clust.cols) +
      ggplot2::scale_x_continuous(expand = c(1/(2 * ncol(X)), 1/(2 * ncol(X)))))
  }

  if (!is.null(yr) && (yr.plot.type != "dendrogram")) {
    # define all arguments of the right plot
    y <- yr
    y.obs.col <- yr.obs.col
    y.cluster.col <- yr.cluster.col
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

    # generate the top plot
    # identify variables defined in the environment
    plot.arg.list <- c(as.list(environment()))
    # identify the possible arguments for generate_add_on_plot
    plot.arg.list <- plot.arg.list[names(formals(generate_add_on_plot))]
    # filter the variables in the environment to those that are arguments
    # for generate_add_on_plot
    plot.arg.list <- plot.arg.list[!is.na(names(plot.arg.list))]
    gg.right <- do.call(generate_add_on_plot, plot.arg.list)
  } else if (yr.plot.type == "dendrogram") {
    suppressMessages(gg.right <- ggdendro::ggdendrogram(clust.rows, rotate = T) +
      ggplot2::scale_x_continuous(expand = c(1/(2 * nrow(X)), 1/(2 * nrow(X)))))
  }

  # Generate the bottom heatmap labels. There are two types:
  # variable: each individual column has its own label
  # cluster: all columns within a cluster are given a combined cluster name
  if (bottom.label == "variable") {
    # define the arguments for generating the bottom "variable" label
    names <- colnames(X)
    location <- "bottom"
    label.col <- bottom.label.col
    label.text.col <- bottom.label.text.col
    label.text.alignment <- bottom.label.text.alignment
    text.angle <- bottom.label.text.angle

    # generate the bottom label
    # identify variables defined in the environment
    label.arg.list <- c(as.list(environment()))
    # identify the possible arguments for generate_var_label
    label.arg.list <- label.arg.list[names(formals(generate_var_label))]
    # filter the variables in the environment to those that are arguments
    # for generate_var_label
    label.arg.list <- label.arg.list[!is.na(names(label.arg.list))]
    gg.bottom <- do.call(generate_var_label, label.arg.list)
  } else if (bottom.label == "cluster") {
    # define the arguments for generating the bottom "cluster" label
    location <- "bottom"
    membership <- membership.cols
    label.col <- bottom.label.col
    label.text.col <- bottom.label.text.col
    label.text.alignment <- bottom.label.text.alignment
    text.angle <- bottom.label.text.angle

    # generate the bottom label
    # identify variables defined in the environment
    label.arg.list <- c(as.list(environment()))
    # identify the possible arguments for generate_var_label
    label.arg.list <- label.arg.list[names(formals(generate_cluster_label))]
    # filter the variables in the environment to those that are arguments
    # for generate_var_label
    label.arg.list <- label.arg.list[!is.na(names(label.arg.list))]
    gg.bottom <- do.call(generate_cluster_label, label.arg.list)
  }

  # Generate the left heatmap labels. There are two types:
  # variable: each individual column has its own label
  # cluster: all columns within a cluster are given a combined cluster name
  if (left.label == "variable") {
    # define the arguments for generating the left "variable" label
    names <- rownames(X)
    location <- "left"
    label.col <- left.label.col
    label.text.col <- left.label.text.col
    label.text.alignment <- left.label.text.alignment
    text.angle <- left.label.text.angle

    # generate the left label
    # identify variables defined in the environment
    label.arg.list <- c(as.list(environment()))
    # identify the possible arguments for generate_var_label
    label.arg.list <- label.arg.list[names(formals(generate_var_label))]
    # filter the variables in the environment to those that are arguments
    # for generate_var_label
    label.arg.list <- label.arg.list[!is.na(names(label.arg.list))]
    gg.left <- do.call(generate_var_label, label.arg.list)
  } else if (left.label == "cluster") {
    # define the arguments for generating the left "cluster" label
    location <- "left"
    membership <- membership.rows
    label.col <- left.label.col
    label.text.col <- left.label.text.col
    label.text.alignment <- left.label.text.alignment
    text.angle <- left.label.text.angle

    # generate the left label
    # identify variables defined in the environment
    label.arg.list <- c(as.list(environment()))
    # identify the possible arguments for generate_var_label
    label.arg.list <- label.arg.list[names(formals(generate_cluster_label))]
    # filter the variables in the environment to those that are arguments
    # for generate_var_label
    label.arg.list <- label.arg.list[!is.na(names(label.arg.list))]
    gg.left <- do.call(generate_cluster_label, label.arg.list)
  }


  # Generate title
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



  # Generate desired layout
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

  to.return <- list(layout = layout,
                    plot = grob.layout,
                    membership.cols = membership.cols,
                    membership.rows = membership.rows)

  return(invisible(to.return))


}
