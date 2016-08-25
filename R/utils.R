
internala <- function(variable) {
  # extract the name of a variable assigned to an argument
  name <- deparse(substitute(variable))
  return(name)
}

to_df <- function(X) {
  # convert a matrix to a data data frame with x-y coordinates
  if(!is.matrix(X)) {
    stop("X must be a matrix")
  }

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

to_cluster_df <- function(X.text, smooth.heat,
                          membership.cols, membership.rows) {
  # X.text is a text matrix

  # wil be used in conjunction with generate_text_heat
  if(!is.matrix(X.text)) {
    stop("X.text must be a matrix")
  }

  # converts matrix into a x-y data frame
  # need to have xmin, xmax, ymin, ymax
  X.vec <- as.vector(X.text) # convert the matrix to a vector

  # hacky way of dealing with repeated text entries
  duplicates <- X.vec[duplicated(X.vec)]

  if (length(duplicates) > 0) {


    # the location of the duplicated values
    duplicated.index <- c()
    # the number of times each was duplicated
    # we need this for when we remove the numbers at the end
    # e.g. if the number has double digits, we need to remove 2 characters
    duplicated.number.len <- c()
    for (text in duplicates) {
      duplicated.text <- X.vec[X.vec == text]
      duplicated.index <- c(duplicated.index, which(X.vec == text))

      # the number of entires we will have to remove at the end:
      duplicated.number.len <- c(duplicated.number.len,
                                 rep(nchar(length(duplicated.text)),
                                     length(duplicated.text)))
      # add a number after each duplicate
      X.vec[X.vec == text] <- paste0(duplicated.text, "...!...",
                                     1:length(duplicated.text))
    }
  }

  X.text <- matrix(X.vec, ncol = ncol(X.text))

  # expand matrix into full matrix
  membership.rows.numeric <- as.numeric(factor(membership.rows,
                                               levels = unique(membership.rows)))
  membership.cols.numeric <- as.numeric(factor(membership.cols,
                                               levels = unique(membership.cols)))
  membership.matrix <- X.text[membership.rows.numeric,
                              membership.cols.numeric]

  row.matrix <- matrix(rep(1:nrow(membership.matrix),
                           ncol(membership.matrix)),
                       ncol = ncol(membership.matrix),
                       byrow = F)

  col.matrix <- matrix(rep(1:ncol(membership.matrix),
                           nrow(membership.matrix)),
                       ncol = ncol(membership.matrix),
                       byrow = T)


  x <- c()
  y <- c()
  for (text in unique(as.vector(membership.matrix))) {
    x <- c(x, mean(col.matrix[membership.matrix == text]))
    y <- c(y, mean(row.matrix[membership.matrix == text]))
  }

  if (smooth.heat) {
    # fix position for smoothed option
    x <- x - 0.5
    y <- y - 0.5
  }


  # remove the hacky addition to the duplicated text entries
  if (length(duplicates) > 0) {
    duplicated_split_words <- strsplit(X.vec[duplicated.index], "...!...")
    X.vec[duplicated.index] <- sapply(duplicated_split_words, function(x) {
        x[-length(x)]
      }
    )
  }

  X.text <- matrix(X.vec, ncol = ncol(X.text))

  X.mat <- cbind(value = X.vec,
                 # convert vector to matrix with columns
                 # for the x and y coordinates
                 x = x,
                 y = y)
  X.df <- as.data.frame(X.mat)
  X.df$x <- as.numeric(x)
  X.df$y <- as.numeric(y)

  return(X.df)
}



stop_errors <- function(X,
                        X.text = NULL,
                        X.text.size = NULL,
                        X.text.col = NULL,
                        yt = NULL,
                        yr = NULL,
                        yt.plot.type = c("scatter", "bar"),
                        yr.plot.type = c("scatter", "bar"),
                        scale = FALSE,
                        membership.rows = NULL, # membership for rows
                        membership.cols = NULL, # membership for cols
                        n.clusters.rows = NULL,
                        n.clusters.cols = NULL,
                        clustering.method = c("kmeans", "hierarchical"),
                        cluster.box = TRUE,
                        legend = TRUE,
                        order.cols = NULL,
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
                        heat.col.scheme = c("red", "purple", "blue",
                                            "grey", "green"),
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

  if (is.matrix(X) &&
      sum(!(apply(X, 2, class) %in% c("numeric", "integer")) > 0)) {
    stop("'X' must contain numeric entries only")
  }

  if (is.data.frame(X) &&
      sum(!(sapply(X, class) %in% c("integer","numeric")) > 0)) {
    stop("'X' must contain numeric entries only")
  }

  if (!is.null(X.text) && !is.matrix(X.text)) {
    stop("'X.text' must be a matrix")
  }

  if (!is.null(left.heat.label)) {
    possible.methods <- c("variable", "cluster", "none")
    i.meth <- pmatch(left.heat.label, possible.methods)
    if (is.na(i.meth)) {
      stop("invalid left.heat.label", paste("", left.heat.label))
    }
  }

  if (!is.null(bottom.heat.label)) {
    possible.methods <- c("variable", "cluster", "none")
    i.meth <- pmatch(bottom.heat.label, possible.methods)
    if (is.na(i.meth)) {
      stop("invalid bottom.heat.label", paste("", bottom.heat.label))
    }
  }

  clustering.method <- match.arg(clustering.method)
  possible.methods <- c("hierarchical", "kmeans")
  i.meth <- pmatch(clustering.method, possible.methods)
  if (is.na(i.meth))
    stop("invalid clustering method", paste("", clustering.method))


  if (!is.null(left.heat.label) &&
      (left.heat.label == "cluster") &&
      is.null(membership.rows) &&
      is.null(n.clusters.rows)) {
    stop(paste("Cannot have 'left.heat.label = 'cluster''",
               "if we have not clustered the rows"))
  }

  if (!is.null(bottom.heat.label) &&
      (bottom.heat.label == "cluster") &&
      is.null(membership.cols) &&
      is.null(n.clusters.cols)) {
    stop(paste("Cannot have 'bottom.heat.label = 'cluster'' if we",
               "have not clustered the columns"))
  }

  if (!is.null(n.clusters.cols) &&
      (n.clusters.cols > ncol(X))) {
    stop(paste("The number of column clusters ('n.clusters.cols')",
               "is larger than the number of columns of X."))
  }

  if (!is.null(n.clusters.rows) &&
      (n.clusters.rows > nrow(X))) {
    stop(paste("The number of row clusters ('n.clusters.rows')",
               "is larger than the number of rows of X."))
  }

  if (!is.null(membership.rows) && (nrow(X) != length(membership.rows))) {
    stop(paste("The length of 'membership.rows' must be equal to",
               "the number of rows in 'X'."))
  }

  if (!is.null(membership.cols) && (ncol(X) != length(membership.cols))) {
    stop(paste("The length of 'membership.cols' must be equal to",
               "the number of columns in 'X'."))
  }

  if (!is.null(order.cols) && (ncol(X) != length(order.cols))) {
    stop(paste("The length of 'order.cols' must be equal to",
               "the number of columns in 'X'."))
  }

  if (sum(duplicated(order.cols)) > 0) {
    stop("'order.cols' contains duplicated columns.")
  }

  if (sum(duplicated(order.rows)) > 0) {
    stop("'order.cols' contains duplicated columns.")
  }
  if (!is.null(order.rows) && (nrow(X) != length(order.rows))) {
    stop(paste("The length of 'order.rows' must be equal to",
               "the number of rows in 'X'."))
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
