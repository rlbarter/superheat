
#' Generate supervised heatmaps.
#'
#' Superheat is used to generate and customize heatmaps.
#'        Scatterplots, boxplots, barplots, line plots and boxplots can
#'        be plotted adjacent to the columns and rows of the heatmap,
#'        adding an additional layer of information.
#'        For usage, see the vignette at
#'        \url{https://rlbarter.github.io/superheat/}.
#'
#' @param X a matrix whose values are to be plotted in the heatmap.
#' @param X.text a matrix containing text entries to be plotted on
#'          top of the heatmap cells. The number of rows/columns must match
#'          either the number of rows/columns of \code{X} or the number of
#'          row/column clusters of \code{X}.
#' @param yt a data frame whose columns consist of values to plot above
#'          the heatmap (the "top plot"). The length of \code{yt} must be
#'          equal to the number of columns of \code{X}.
#' @param yr a data frame whose columns consist of values to plot to the
#'          right of the heatmap (the "right plot"). The length of \code{yr}
#'          must be equal to the number of rows of \code{X}.
#' @param yt.plot.type a character specifying the \code{yt} plot type. The default is
#'          "scatter", and other options include "bar", "scattersmooth",
#'          "smooth", "boxplot", "scatterline" and "line".
#' @param yr.plot.type character specifying the \code{yr} plot type. The default is
#'          "scatter", and other options include "bar", "scattersmooth",
#'          "smooth", "boxplot", "scatterline", and "line".
#' @param membership.rows a vector specifying the cluster membership
#'          of the rows in X.
#' @param membership.cols a vector specifying the cluster membership
#'          of the columns in X.
#' @param pretty.order.cols a logical specifying whether the rows should be reordered
#'          based on hierarchical clustering. Default is TRUE.
#' @param pretty.order.rows a logical specifying whether the cols should be reordered
#'          based on hierarchical clustering. Default is TRUE.
#' @param row.dendrogram a logical specifying whether a dendrogram should be
#'          placed next to the rows. Can only be used when \code{yr} is not
#'          specified and clustering is not performed.
#' @param col.dendrogram a logical specifying whether a dendrogram should be
#'          placed next to the columns. Can only be used when \code{yt} is not
#'          specified and clustering is not performed.


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
#' @param linkage.method the linkage method to use for hierarchical clustering.
#'          This must be one of "ward.D", "ward.D2", "single", "complete",
#'          "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or
#'          "centroid" (= UPGMC).
#'
#' @param smooth.heat a logical specifying whether or not to smooth the colour
#'          of the heatmap within clusters (by taking the median/mean value
#'          defined by smooth.heat.type).
#' @param smooth.heat.type the type of smoothing used within clusters. This
#'          must be one of "median","mean".
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
#'          The default is "viridis", and other options include "red", purple",
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
#' @param heat.lim a vector of length two consisting of the maximum and minimum
#'          value for the heatmap palette.
#' @param extreme.values.na a logical describing whether values outside the range
#'          of heat.lim are presented as missing (TRUE, default) or as the
#'          max/min value of the range.
#' @param X.text.size a single number or a matrix of numbers (whose dimension
#'          matches that of \code{X.text}) that specifies the size of each text
#'          entry in \code{X.text}.
#' @param X.text.angle a single number or a matrix of numbers (whose dimension
#'          matches that of \code{X.text}) that specifies the angle of each text
#'          entry in \code{X.text}.
#' @param X.text.col a single character string or a matrix of character strings
#'          (whose dimension matches that of \code{X.text}) that specifies the
#'          colours of each text entry in \code{X.text}.
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
#' @param yt.line.col the color of the (smoothing) line in the \code{yt}
#'          plot.
#' @param yr.line.col the color of the (smoothing) line in the \code{yr}
#'          plot.
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
#' @param yt.lim a vector of length two describing the y-axis limits.
#' @param yr.lim a vector of length two describing the y-axis limits.
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
#' @param left.label.col a vector specifying the left cluster/variable label
#'          colour palette.
#' @param bottom.label.col a vector specifying the bottom cluster/variable
#'          label colour palette.
#' @param left.label.text.col a character or character vector specifying the
#'          left cluster/variable label text colour.
#' @param bottom.label.text.col a character or character vector specifying the
#'          bottom cluster/variable label text colour.
#' @param force.bottom.label a logical describing whether or not to force the
#'          bottom labels to appear (relevant only when X has more than 50
#'          columns). Note that by default there are no labels when there are
#'          more than 50 columns.
#' @param force.left.label a logical describing whether or not to force the
#'          left labels to appear (relevant only when X has more than 50
#'          rows). Note that by default there are no labels when there are
#'          more than 50 rows.
#' @param column.title a string specifying the overall column name (located
#'          below the bottom.labels).
#' @param row.title a string specifying the overall row name (located to the
#'          left of the left.labels).
#' @param column.title.size a number specifying the size of the column name. The
#'          default is 5.
#' @param row.title.size a number specifying the size of the row name. The
#'          default is 5.
#' @param legend.height a number specifying the height of the legend. The default
#'          is 0.1.
#' @param legend.width a number specifying the width of the legend. The default
#'          is 1.5.
#' @param legend.text.size a number specifying the size of the numbers on the
#'          legend axis. The default is 12.
#' @param legend.breaks a vector specifying the legend breaks.
#' @param legend.vspace a number specifying the vertical gap between the
#'          heatmap and the legend
#' @param padding the amount (in cm) of white space (padding) around the plot.
#'          The default is 1 cm.
#' @param title a character string specifying a main heading.
#' @param title.size the size of the title. The default is 5.
#' @param title.alignment the alignment of the title. The default is "center".
#'          Options are "left", "right", "center".
#' @param print.plot a logical specifying whether or not to output the plot.
#'
#' @references Barter and Yu (2018), Superheat: An R package for creating
#'        beautiful and extendable heatmaps for visualizing complex data,
#'        \url{https://www.tandfonline.com/doi/full/10.1080/10618600.2018.1473780},
#'        Journal of Computational and Graphical Statistics
#'
#' @return \code{plot} a plot with the properties specified by the above arguments.
#' @return \code{membership.cols} the column cluster membership vector
#' @return \code{membership.rows} the row cluster membership vector
#' @return \code{order.rows} the order of the rows in the heatmap
#' @return \code{order.cols} the order of the columns in the heatmap
#' @return \code{heat.pal.values} the heat.pal.values vector used
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
                      pretty.order.rows = F,
                      pretty.order.cols = F,
                      row.dendrogram = F,
                      col.dendrogram = F,

                      n.clusters.rows = NULL,
                      n.clusters.cols = NULL,
                      clustering.method = c("kmeans", "hierarchical"),
                      dist.method = c("euclidean", "maximum", "manhattan",
                                      "canberra", "binary", "minkowski"),
                      linkage.method = c("complete", "ward.D",
                                         "ward.D2", "single",
                                          "average", "mcquitty",
                                          "median", "centroid"),

                      order.cols = NULL,
                      order.rows = NULL,

                      smooth.heat = FALSE,
                      smooth.heat.type = "median",
                      scale = FALSE,

                      left.label = NULL,
                      bottom.label = NULL,

                      heat.col.scheme = c("viridis", "red", "purple", "blue",
                                          "grey", "green"),
                      heat.pal = NULL,
                      heat.pal.values = NULL,
                      heat.na.col = "grey50",
                      heat.lim = NULL,
                      extreme.values.na = TRUE,

                      X.text.size = 5,
                      X.text.col = "black",
                      X.text.angle = 0,

                      yt.plot.type = c("scatter", "bar", "boxplot",
                                       "scattersmooth", "smooth",
                                       "scatterline", "line"),
                      yr.plot.type = c("scatter", "bar", "boxplot",
                                       "scattersmooth","smooth",
                                       "scatterline", "line"),

                      legend = TRUE,
                      legend.height = 0.1,
                      legend.width = 1.5,
                      legend.text.size = 12,
                      legend.breaks = NULL,
                      legend.vspace = 0.1,

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
                      yr.line.col = NULL,
                      yt.line.col = NULL,
                      yr.line.size = NULL,
                      yt.line.size = NULL,
                      yr.lim = NULL,
                      yt.lim = NULL,

                      bottom.label.text.size = 5,
                      left.label.text.size = 5,
                      bottom.label.text.angle = NULL,
                      left.label.text.angle = NULL,
                      bottom.label.size = NULL,
                      left.label.size = NULL,
                      left.label.col = NULL,
                      bottom.label.col = NULL,
                      left.label.text.col = NULL,
                      bottom.label.text.col = NULL,
                      left.label.text.alignment = NULL,
                      bottom.label.text.alignment = NULL,
                      force.left.label = F,
                      force.bottom.label = F,

                      column.title = NULL,
                      row.title = NULL,
                      column.title.size = 5,
                      row.title.size = 5,

                      padding = 1,
                      title = NULL,
                      title.size = 5,
                      title.alignment = NULL,

                      print.plot = TRUE) {
  # The primary superheat function for plotting super heatmaps.

  # drop exess factor levels
  if (!is.null(membership.rows) && is.factor(membership.rows)) {
    membership.rows <- droplevels(membership.rows)
  }
  if (!is.null(membership.cols) && is.factor(membership.cols)) {
    membership.cols <- droplevels(membership.cols)
  }

  if (row.dendrogram) {
    pretty.order.rows = TRUE
  }
  if (col.dendrogram) {
    pretty.order.cols = TRUE
  }

  # set default title alignment
  if (is.null(title.alignment)) {
    title.alignment <- "left"
  }

  # match the arguments to those provided
  smoothing.method <- match.arg(smoothing.method)
  yt.plot.type <- match.arg(yt.plot.type)
  yr.plot.type <- match.arg(yr.plot.type)
  heat.col.scheme <- match.arg(heat.col.scheme)
  dist.method <- match.arg(dist.method)
  linkage.method <- match.arg(linkage.method)

  # clean the matrix X
  X <- clean_matrix(X, scale)

  # run error check on arguments
  stop.arg.list <- c(as.list(environment()))
  stop.arg.list <- stop.arg.list[names(formals(stopErrors))]
  stop.arg.list <- stop.arg.list[!is.na(names(stop.arg.list))]
  do.call(stopErrors, stop.arg.list)

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
      effective.col.clusters <- n.clusters.cols
    } else if (!is.null(membership.cols)) {
      effective.col.clusters <- length(unique(membership.cols))
    }
  }
  # how many row clusters
  if (cluster.rows) {
    if (!is.null(n.clusters.rows)) {
      effective.row.clusters <- n.clusters.rows
    } else if (!is.null(membership.rows)) {
      effective.row.clusters <- length(unique(membership.rows))
    }
  }



  # run error check on clustering mechanism
  cluster.stop.arg.list <- c(as.list(environment()))
  cluster.stop.arg.list <- cluster.stop.arg.list[names(formals(clusterStopErrors))]
  cluster.stop.arg.list <- cluster.stop.arg.list[!is.na(names(cluster.stop.arg.list))]
  do.call(clusterStopErrors, cluster.stop.arg.list)

  # set the type of label for each additional plot
  label.type <- setLabelType(X,
                             left.label,
                             cluster.rows,
                             bottom.label,
                             cluster.cols,
                             force.left.label,
                             force.bottom.label,
                             yr.obs.col,
                             yt.obs.col)
  bottom.label <- label.type$bottom.label
  left.label <- label.type$left.label

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

  # remove alternating color in adjacent plots if no labels
  if (!is.null(yr) && # provided a right-plot
      (nrow(X) > 100) &&  # default no labels
      !force.left.label && # nor forcing labels
      is.null(yr.obs.col) &&  # no point color provided
      (length(yr) == nrow(X)) && # right plot is at the individual-level
      !cluster.rows) { # did not cluster rows
    yr.obs.col <- rep("grey50", nrow(X))
  }
  if (!is.null(yt) && # provided a top-plot
      (ncol(X) > 100) &&  # default no labels
      is.null(yt.obs.col) &&  # no point color provided
      (length(yt) == ncol(X)) && # top plot is at the individual-level
      !cluster.cols) {  # did not cluster cols
    yt.obs.col <- rep("grey50", ncol(X))
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
    hclust.rows <- clustering$clust
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
    hclust.cols <- clustering$clust
  }

  # note that we must obtain the hierarchical clustering
  # after rearranging the order of the rows and columns
  if (pretty.order.cols) {
    hclust.cols <- hclust(dist(t(X), method = dist.method),
                          method = linkage.method)
  }

  if (pretty.order.rows) {
    hclust.rows <- hclust(dist(X, method = dist.method),
                          method = linkage.method)
  }


  # if there is a pretty.order.rows/cols, order rows/cols by hclust order
  if (pretty.order.rows && is.null(order.rows)) {
    order.rows <- hclust.rows$order
  }
  if (pretty.order.cols && is.null(order.cols)) {
    order.cols <- hclust.cols$order
  }



  # if a specific row/col ordering is not provided,
  # define the ordering to be that given in the original matrix
  if (is.null(order.rows) && (!row.dendrogram)) {
    order.rows <- 1:nrow(X)
  }
  if (is.null(order.cols) && (!col.dendrogram)) {
    order.cols <- 1:ncol(X)
  }


  # make a data frame order.df.rows/cols that contains the membership and order
  # of each row/columns.
  # if clustering was performed then re-order the rows by cluster
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

  # Reorder X matrices, yr and yt based on the new ordering
  X <- X[order.df.rows$order.rows, order.df.cols$order.cols]

  if (!is.null(X.text) && is.matrix(X.text) &&
      (nrow(X.text) == nrow(X)) &&
      (ncol(X.text) == ncol(X))) {
    X.text <- X.text[order.df.rows$order.rows, order.df.cols$order.cols]
  }
  if (!is.null(X.text.col) && is.matrix(X.text.col) &&
      (nrow(X.text.col) == nrow(X)) &&
      (ncol(X.text.col) == ncol(X))) {
    X.text.col <- X.text.col[order.df.rows$order.rows, order.df.cols$order.cols]
  }
  if (!is.null(X.text.size) && is.matrix(X.text.size) &&
      (nrow(X.text.size) == nrow(X)) &&
      (ncol(X.text.size) == ncol(X))) {
    X.text.size <- X.text.size[order.df.rows$order.rows, order.df.cols$order.cols]
  }
  if (!is.null(X.text.angle) && is.matrix(X.text.angle)) {
    X.text.angle <- X.text.angle[order.df.rows$order.rows, order.df.cols$order.cols]
  }

  if (!is.null(yr)) {
    # only rearrange within cluster if the right plot is for each
    # data point (rather than for each cluster)
    if (length(yr) == nrow(X)) {
      yr <- yr[order.df.rows$order.rows]
      yr.obs.col <- yr.obs.col[order.df.rows$order.rows]
    }
  }
  if (!is.null(yt)) {
    # only rearrange within cluster if the top plot is for each
    # data point (rather than for each cluster)
    if (length(yt) == ncol(X)) {
      yt <- yt[order.df.cols$order.cols]
      yt.obs.col <- yt.obs.col[order.df.cols$order.cols]
    }
  }
  # rearrange bar colors if needed
  if (!is.null(yt.bar.col) && (length(yt.bar.col) == ncol(order.df.cols))) {
    yt.bar.col <- yt.bar.col[order.df.cols$order.cols]
  }
  if (!is.null(yr.bar.col) && (length(yr.bar.col) == nrow(order.df.rows))) {
    yr.bar.col <- yr.bar.col[order.df.rows$order.rows]
  }
  # rearrange label colors if needed
  if (!is.null(left.label.col)) {
    left.label.col <- left.label.col[order.df.rows$order.rows]
  }
  if (!is.null(bottom.label.col)) {
    bottom.label.col <- bottom.label.col[order.df.cols$order.cols]
  }
  # rearrange label text colors if needed
  if (!is.null(left.label.text.col)) {
    left.label.text.col <- left.label.text.col[order.df.rows$order.rows]
  }
  if (!is.null(bottom.label.text.col)) {
    bottom.label.text.col <- bottom.label.text.col[order.df.cols$order.cols]
  }

  # the default if clustering was not performed
  if (!cluster.cols) {
    membership.cols <- 1:ncol(X)
  }
  if (!cluster.rows) {
    membership.rows <- 1:nrow(X)
  } else {
    membership.rows <- membership.rows[order.df.rows$order.rows]
    membership.cols <- membership.cols[order.df.cols$order.cols]
  }

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
  if (!is.null(yt) && (!col.dendrogram)) {
    # define all arguments of the top plot
    y <- yt
    y.obs.col <- yt.obs.col
    y.cluster.col <- yt.cluster.col
    y.bar.col <- yt.bar.col
    y.line.size <- yt.line.size
    y.line.col <- yt.line.col
    y.lim <- yt.lim
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
  } else if (col.dendrogram) {
    suppressMessages(gg.top <- ggdendro::ggdendrogram(hclust.cols) +
      ggplot2::scale_x_continuous(expand = c(1/(2 * ncol(X)), 1/(2 * ncol(X)))))
  }

  if (!is.null(yr) && (!row.dendrogram)) {
    # define all arguments of the right plot
    y <- yr
    y.obs.col <- yr.obs.col
    y.cluster.col <- yr.cluster.col
    y.bar.col <- yr.bar.col
    y.line.size <- yr.line.size
    y.line.col <- yr.line.col
    y.lim <- yr.lim
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
  } else if (row.dendrogram) {
    suppressMessages(gg.right <- ggdendro::ggdendrogram(hclust.rows, rotate = T) +
      ggplot2::scale_x_continuous(expand = c(1/(2 * nrow(X)), 1/(2 * nrow(X)))))
  }

  # Generate the bottom heatmap labels. There are two types:
  # variable: each individual column has its own label
  # cluster: all columns within a cluster are given a combined cluster name
  if (bottom.label == "variable") {
    # define the arguments for generating the bottom "variable" label
    names <- colnames(X)
    names[is.na(names)] <- ""
    location <- "bottom"
    label.col <- bottom.label.col
    label.text.col <- bottom.label.text.col
    label.text.alignment <- bottom.label.text.alignment
    text.angle <- bottom.label.text.angle

    # automate label size
    if (is.null(bottom.label.size)) {
      # adjust for when the bottom text is
      if (!is.null(bottom.label.text.angle) && bottom.label.text.angle == 90) {
        bottom.label.size <- max(stringr::str_length(names)) * 0.02 + 0.05
        # if rotating the bottom labels, also enforce right-alignment
        if (is.null(bottom.label.text.alignment)) {
          label.text.alignment <- "right"
        }
      } else {
        bottom.label.size <- 0.1

        if (is.null(bottom.label.text.alignment)) {
          label.text.alignment <- "center"
        }
      }
    }


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

    # automate label size
    if (is.null(bottom.label.size) && !is.null(membership)) {
      # adjust for when the bottom text is
      if (!is.null(bottom.label.text.angle) && bottom.label.text.angle == 90) {
        bottom.label.size <- max(stringr::str_length(membership)) * 0.02 + 0.05

        # automate label justification when labels are rotated
        if (is.null(bottom.label.text.alignment)) {
          label.text.alignment <- "right"
        }

      } else {
        bottom.label.size <- 0.1

        if (is.null(bottom.label.text.alignment)) {
          label.text.alignment <- "center"
        }
      }
    }



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
    names[is.na(names)] <- ""
    location <- "left"
    label.col <- left.label.col
    label.text.col <- left.label.text.col
    label.text.alignment <- left.label.text.alignment
    text.angle <- left.label.text.angle


    # automate label size
    if (is.null(left.label.size) && !is.null(names)) {
      left.label.size <- max(stringr::str_length(names)) * 0.02 + 0.05

    }

    # automate label justification
    if (is.null(left.label.text.alignment)) {
      label.text.alignment <- "right"
    }

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

    # automate label size
    if (is.null(left.label.size) && !is.null(membership.rows)) {
      left.label.size <- max(stringr::str_length(membership.rows)) * 0.02 + 0.05
    }

    # automate label justification
    if (is.null(left.label.text.alignment)) {
      label.text.alignment <- "right"
    }



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
    gg.title <- generate_title(title = title,
                               title.size = title.size,
                               title.alignment = title.alignment)
  }

  # Generate row and column titles
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
  # gtable::gtable_show_layout(layout)
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
                    membership.rows = membership.rows,
                    order.rows = order.rows,
                    order.cols = order.cols,
                    heat.pal.values = heat$heat.pal.values)

  return(invisible(to.return))
}
