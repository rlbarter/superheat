
internala <- function(variableXX) {
  namex=deparse(substitute(variableXX))
  return(namex)
}



to_df <- function(X) {
  if(!is.matrix(X)) stop("X must be a matrix")

  # converts image matrix into a data frame (one row per observation)
  X.vec <- as.vector(X) # convert the matrix to a vector
  X.mat <- cbind(value = X.vec,
                 # convert vector to matrix with columns
                 # for the x and y coordinates
                 x = rep(1:ncol(X), each = nrow(X)),
                 y = rep(1:nrow(X), times = ncol(X)))
  X.df <- as.data.frame(X.mat)

  return(X.df)
}








stop_errors <- function(X,
                        yt = NULL,
                        yr = NULL,
                        yt.plot.type = c("scatter","bar"),
                        yr.plot.type = c("scatter","bar"),
                        scale = FALSE,
                        membership.rows = NULL, # membership for rows
                        membership.cols = NULL, # membership for cols
                        n.clusters.rows = NULL,
                        n.clusters.cols = NULL,
                        cluster.rows = TRUE,
                        cluster.cols = FALSE,
                        clustering.method = c("kmeans", "hierarchical"),
                        cluster.box = TRUE,
                        legend = TRUE,
                        order.cols = NULL, # how to order within clusters (must be an integer vector e..g c(1,3,2) means the ordering is the first, third then second observation
                        order.rows = NULL,
                        left.heat.label = NULL,
                        bottom.heat.label = NULL,
                        yt.axis = T,
                        yr.axis = T,
                        yt.plot.size = 0.3,
                        yr.plot.size = 0.3,
                        yt.axis.name = NULL,
                        yr.axis.name = NULL,
                        yr.axis.size = 10,
                        yt.axis.size = 10,
                        yr.axis.name.size = 10,
                        yt.axis.name.size = 10,
                        bottom.text.size = 5,
                        left.text.size = 5,
                        bottom.text.angle = NULL,
                        left.text.angle = NULL,
                        bottom.label.size = 0.1,
                        left.label.size = 0.1,
                        heat.col.scheme = c("red", "purple", "blue", "grey", "green"),
                        heat.pal = NULL,
                        heat.pal.values = NULL,
                        left.label.pal = NULL,
                        bottom.label.pal = NULL,
                        left.label.text.col = NULL,
                        bottom.label.text.col = NULL,
                        yt.col = NULL,
                        yr.col = NULL,
                        yt.point.size = 2,
                        yr.point.size = 2,
                        legend.size = 2,
                        padding = 1,
                        title = NULL,
                        title.size = 5,
                        print.plot = TRUE) {


  if (!is.null(membership.rows) && !cluster.rows) {
    warning("'cluster.rows = FALSE' is overridden by the supplied 'membership.rows' argument.")
  }

  if (!cluster.rows && !is.null(n.clusters.rows)) {
    warning("'cluster.rows = FALSE' is overridden by the supplied 'n.clusters.rows' argument.")
  }


  if (is.matrix(X) && sum(!(apply(X, 2, class) %in% c("numeric", "integer")) > 0)) {
    stop("'X' must contain numeric entries only")
  }

  if (is.data.frame(X) && sum(!(sapply(X, class) %in% c("integer","numeric")) > 0)) {
    stop("'X' must contain numeric entries only")
  }


  if (!is.null(left.heat.label)) {
    possible.methods <- c("variable", "cluster", "none")
    i.meth <- pmatch(left.heat.label, possible.methods)
    if (is.na(i.meth))
      stop("invalid left.heat.label", paste("", left.heat.label))
  }

  if (!is.null(bottom.heat.label)) {
    possible.methods <- c("variable", "cluster", "none")
    i.meth <- pmatch(bottom.heat.label, possible.methods)
    if (is.na(i.meth))
      stop("invalid bottom.heat.label", paste("", bottom.heat.label))
  }

  clustering.method <- match.arg(clustering.method)
  possible.methods <- c("hierarchical", "kmeans")
  i.meth <- pmatch(clustering.method, possible.methods)
  if (is.na(i.meth))
    stop("invalid clustering method", paste("", clustering.method))

  if (is.null(membership.cols) && cluster.cols && is.null(n.clusters.cols)) {
    stop("Please supply either a 'membership.cols' vector or the number of column clusters 'n.clusters.cols'.")
  }

  if (is.null(membership.rows) && cluster.rows && is.null(n.clusters.rows)) {
    stop("Please supply either a 'membership.rows' vector or the number of row clusters 'n.clusters.rows'.")
  }

  if (!is.null(left.heat.label) && (left.heat.label == "cluster") && (is.null(membership.rows)) && is.null(n.clusters.rows)) {
    stop("Cannot have 'left.heat.label = 'cluster'' if we have not clustered the rows")
  }

  if (!is.null(bottom.heat.label) && (bottom.heat.label == "cluster") && (is.null(membership.cols)) && is.null(n.clusters.cols)) {
    stop("Cannot have 'bottom.heat.label = 'cluster'' if we have not clustered the columns")
  }

  if (!is.null(n.clusters.cols) && (n.clusters.cols > ncol(X))) {
    stop("The number of column clusters ('n.clusters.cols') is larger than the number of columns of X.")
  }

  if (!is.null(n.clusters.rows) && (n.clusters.rows > nrow(X))) {
    stop("The number of row clusters ('n.clusters.rows') is larger than the number of rows of X.")
  }

  if (!is.null(membership.rows) && (nrow(X) != length(membership.rows))) {
    stop("The length of 'membership.rows' must be equal to the number of rows in 'X'.")
  }

  if (!is.null(membership.cols) && (ncol(X) != length(membership.cols))) {
    stop("The length of 'membership.cols' must be equal to the number of columns in 'X'.")
  }

  if (!is.null(order.cols) && (ncol(X) != length(order.cols))) {
    stop("The length of 'order.cols' must be equal to the number of columns in 'X'.")
  }

  if (sum(duplicated(order.cols)) > 0) {
    stop("'order.cols' contains duplicated columns.")
  }

  if (sum(duplicated(order.rows)) > 0) {
    stop("'order.cols' contains duplicated columns.")
  }
  if (!is.null(order.rows) && (nrow(X) != length(order.rows))) {
    stop("The length of 'order.rows' must be equal to the number of rows in 'X'.")
  }

  if (!is.null(n.clusters.cols) && (length(n.clusters.cols) > 1)) {
    stop("'n.clusters.cols' must be a single integer.")
  }

  if (!is.null(n.clusters.rows) && (length(n.clusters.rows) > 1)) {
    stop("'n.clusters.rows' must be a single integer.")
  }

  if (!is.null(yt) && (length(yt) != ncol(X))) {
    stop("'yt' must have length equal to the number of columns of 'X'.")
  }

  if (!is.null(yr) && (length(yr) != nrow(X))) {
    stop("'yr' must have length equal to the number of rows of 'X'.")
  }

  if (!is.null(order.cols) && (!identical(sort(order.cols), 1:ncol(X)))) {
    stop("'order.cols' must be a vector containing the column indexes of 'X'.")
  }

  if (!is.null(order.rows) && (!identical(sort(order.rows), 1:nrow(X)))) {
    stop("'order.rows' must be a vector containing the row indexes of 'X'.")
  }



}
