
internala <- function(variable) {
  # extract the name of a variable assigned to an argument
  name <- deparse(substitute(variable))
  return(name)
}

matrixToDataFrame <- function(X) {
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

getClusterDf <- function(X.text, smooth.heat,
                          membership.cols, membership.rows) {
  # X.text is a text matrix
  
  if (length(unique(membership.rows)) != nrow(X.text)) {
    membership.rows <- 1:nrow(X.text)
  }
  if (length(unique(membership.cols)) != ncol(X.text)) {
    membership.cols <- 1:ncol(X.text)
  }

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



stopErrors <- function(X,
                        X.text = NULL,
                        yt = NULL,
                        yr = NULL,
                        heat.lim = NULL,
                        membership.rows = NULL, # membership for rows
                        membership.cols = NULL, # membership for cols
                        pretty.order.rows = T,
                        pretty.order.cols = T,
                        row.dendrogram = F,
                        col.dendrogram = F,
                        
                        n.clusters.rows = NULL,
                        n.clusters.cols = NULL,
                        clustering.method = c("kmeans", "hierarchical"),
                        dist.method = c("euclidean", "maximum", "manhattan",
                                        "canberra", "binary", "minkowski"),
                        
                        order.cols = NULL,
                        order.rows = NULL,
                        
                        smooth.heat = FALSE,
                        scale = FALSE,
                        
                        heat.col.scheme = c("viridis", "red", "purple", "blue",
                                            "grey", "green"),
                        heat.pal = NULL,
                        heat.pal.values = NULL,
                        heat.na.col = "grey50",
                        
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
                        
                        bottom.label = NULL,
                        left.label = NULL,
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
                        title.size = 5) {

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
  
  # make sure a single value or a matrix
  if (!is.null(X.text.size) &&  
      (length(X.text.size) != 1) && 
      !is.matrix(X.text.size)) {
    stop("'X.text.size' must be either a single value or a matrix")
  }
  if (!is.null(X.text.size) &&  
      (length(X.text.size) != 1) && 
      !is.matrix(X.text.size)) {
    stop("'X.text.size' must be either a single value or a matrix")
  }
  if (!is.null(X.text.angle) &&  
      (length(X.text.angle) != 1) && 
      !is.matrix(X.text.angle)) {
    stop("'X.text.angle' must be either a single value or a matrix")
  }
  
  # if matrix, make sure the dimension matches
  if (!is.null(X.text.size) &&  
      is.matrix(X.text.size) &&
      (ncol(X.text.size) != ncol(X.text))) {
    stop(paste("'X.text.size' must be either a single value or a matrix",
               "whose dimension matches 'X.text'"))
  }
  if (!is.null(X.text.col) &&  
      is.matrix(X.text.col) &&
      (ncol(X.text.col) != ncol(X.text))) {
    stop(paste("'X.text.col' must be either a single value or a matrix",
               "whose dimension matches 'X.text'"))
  }
  if (!is.null(X.text.angle) &&  
      is.matrix(X.text.angle) &&
      (ncol(X.text.angle) != ncol(X.text))) {
    stop(paste("'X.text.angle' must be either a single value or a matrix",
               "whose dimension matches 'X.text'"))
  }
  
  
  
  if (!is.null(X.text.col) && 
      (length(X.text.col) != 1) && 
      !is.matrix(X.text.col)) {
    stop("'X.text.col' must be either a single value or a matrix")
  } 
  
  
  if (!is.null(heat.lim)) {
    if (!is.vector(heat.lim)) {
      stop("`heat.lim` must be a vector.")
    }
    if (length(heat.lim) != 2) {
      stop("`heat.lim` must be a vector of length 2.")
    }
    if (!is.numeric(heat.lim)) {
      stop("`heat.lim` must contain numeric values.")
    }
  }
  
  
  # cannot set dendrogram = TRUE if pretty.order = FALSE
  if (row.dendrogram && !pretty.order.rows) {
    stop("Cannot set 'row.dendrogram = TRUE' if 'pretty.order.rows = FALSE'")
  }
  
  if (col.dendrogram && !pretty.order.cols) {
    stop("Cannot set 'col.dendrogram = TRUE' if 'pretty.order.cols = FALSE'")
  }
  

  if (!is.null(left.label)) {
    possible.methods <- c("variable", "cluster", "none")
    i.meth <- pmatch(left.label, possible.methods)
    if (is.na(i.meth)) {
      stop("invalid left.label", paste("", left.label))
    }
  }

  if (!is.null(bottom.label)) {
    possible.methods <- c("variable", "cluster", "none")
    i.meth <- pmatch(bottom.label, possible.methods)
    if (is.na(i.meth)) {
      stop("invalid bottom.label", paste("", bottom.label))
    }
  }

  clustering.method <- match.arg(clustering.method)
  possible.methods <- c("hierarchical", "kmeans")
  i.meth <- pmatch(clustering.method, possible.methods)
  if (is.na(i.meth))
    stop("invalid clustering method", paste("", clustering.method))


  if (!is.null(left.label) &&
      (left.label == "cluster") &&
      is.null(membership.rows) &&
      is.null(n.clusters.rows)) {
    stop(paste("Cannot have 'left.label = 'cluster''",
               "if we have not clustered the rows"))
  }

  if (!is.null(bottom.label) &&
      (bottom.label == "cluster") &&
      is.null(membership.cols) &&
      is.null(n.clusters.cols)) {
    stop(paste("Cannot have 'bottom.label = 'cluster'' if we",
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


  if (!is.null(order.cols) && (!identical(sort(order.cols), 1:ncol(X)))) {
    stop("'order.cols' must be a vector containing the column indexes of 'X'.")
  }

  if (!is.null(order.rows) && (!identical(sort(order.rows), 1:nrow(X)))) {
    stop("'order.rows' must be a vector containing the row indexes of 'X'.")
  }

  
  if (!is.null(yr.obs.col) && (yr.plot.type == "line")) {
    stop(paste("Cannot set `yr.obs.col` when `yr.plot.type == 'line'`.", 
               "Use `yr.line.col` instead."))
  }
  if (!is.null(yt.obs.col) && (yt.plot.type == "line")) {
    stop(paste("Cannot set `yt.obs.col` when `yt.plot.type == 'line'`.", 
               "Use `yt.line.col` instead."))
  }
  if (!is.null(yt.obs.col) && (yt.plot.type == "smooth")) {
    stop(paste("Cannot set `yt.obs.col` when `yt.plot.type == 'smooth'`.", 
               "Use `yt.line.col` instead."))
  }
  if ((length(yt) != ncol(X)) && (yt.plot.type == "line")) {
    stop(paste("`yt` must have same length as `ncol(X)` when",
               "`yt.plot.type == 'line'`."))
  }
  if ((length(yr) != nrow(X)) && (yr.plot.type == "line")) {
    stop(paste("`yr` must have same length as `nrow(X)` when",
               "`yr.plot.type == 'line'`."))
  }
  if ((length(yt) != ncol(X)) && (yt.plot.type == "smooth")) {
    stop(paste("`yt` must have same length as `ncol(X)` when",
               "`yt.plot.type == 'smooth'`."))
  }
  if ((length(yr) != nrow(X)) && (yr.plot.type == "smooth")) {
    stop(paste("`yr` must have same length as `nrow(X)` when",
               "`yr.plot.type == 'smooth'`."))
  }
  if (!is.null(yr.obs.col) && 
      ((length(yr.obs.col) != length(yr)) && (length(yr.obs.col) != 1))) {
    stop(paste("`yr.obs.col` must have same length as `yr`"))
  }
  if (!is.null(yt.obs.col) && 
      ((length(yt.obs.col) != length(yt)) && (length(yt.obs.col) != 1)))  {
    stop(paste("`yt.obs.col` must have same length as `yt`"))
  }
  

}




clusterStopErrors <- function(X,
                              pretty.order.rows = T,
                              pretty.order.cols = T,
                              row.dendrogram = F,
                              col.dendrogram = F,
                              cluster.cols = NULL,
                              cluster.rows = NULL,
                              yt = NULL,
                              yr = NULL,
                              yt.plot.type = NULL,
                              yr.plot.type = NULL,
                              effective.col.clusters = NULL,
                              effective.row.clusters = NULL) {
  
  
  if ((!cluster.cols && !is.null(yt) && (length(yt) != ncol(X))) |
      ((cluster.cols && !is.null(yt) && (length(yt) != effective.col.clusters)) &&
       (cluster.cols && !is.null(yt) && (length(yt) != ncol(X))))) {
    stop(paste("'yt' must have length equal to either the number of columns",
               "of 'X' or the number of column clusters of 'X'."))
  }
  
  if ((!cluster.rows && !is.null(yr) && (length(yr) != nrow(X))) |
      ((cluster.rows && !is.null(yr) && (length(yr) != effective.row.clusters)) &&
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
  if (cluster.cols && col.dendrogram) {
    stop("Cannot perform column clustering while placing a dendrogram")
  }
  if (cluster.rows && row.dendrogram) {
    stop("Cannot perform row clustering while placing a dendrogram")
  }
  
  if (!is.null(yr) && row.dendrogram) {
    stop("Cannot set 'yr' when placing a dendrogram")
  }
  if (!is.null(yt) && col.dendrogram) {
    stop("Cannot set 'yt' when placing a dendrogram")
  }
  
  
}

clean_matrix <- function(X, scale) {
  
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
  
  return(X)
}
  

setLabelType <- function(X,
                         left.label, 
                         cluster.rows, 
                         bottom.label, 
                         cluster.cols,
                         force.left.label,
                         force.bottom.label,
                         yr.obs.col,
                         yt.obs.col) {
  
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
  
  
  return(list(left.label = left.label, bottom.label = bottom.label))
}