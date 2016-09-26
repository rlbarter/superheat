generate_cluster <- function(X,
                             n.clusters,
                             clustering.method = c("kmeans", "hierarchical"),
                             dist.method = c("euclidean", "maximum", "manhattan",
                                             "canberra", "binary", "minkowski"),
                             ...) {

  # calculate dissimilarity matrix
  if (is.null(dist.method)) {
    # identify numeric vars
    if (is.data.frame(X)) {
        vars <- sapply(X, is.numeric)
    }
    if (is.matrix(X)) {
        vars <- apply(X, 2, is.numeric)
    }
    # restrict to numeric variables
    X.vars <- X[, vars]
    # calculate the correlation matrix
    cor.mat <- cor(t(X.vars))
    dissim.mat <- 1 - cor.mat # calculate dissimilarity matrix
  } else {
    # identify numeric vars
    if (is.data.frame(X)) {
      vars <- sapply(X, is.numeric)
    }
    if (is.matrix(X)) {
      vars <- apply(X, 2, is.numeric)
    }
    # restrict to numeric variables
    X.vars <- X[, vars]
    # calculate the correlation matrix
    cor.mat <- cor(t(X.vars))
    # calculate the distance matrix using the specified distance method
    dissim.mat <- dist(X.vars, method = dist.method, ...)
  }

  clustering.method <- match.arg(clustering.method)


  # perform clustering and generate membership vector:
  if (clustering.method == "hierarchical") {
      clust <- hclust(as.dist(dissim.mat))
      membership <- cutree(clust, k = n.clusters)
  } else if (clustering.method == "kmeans") {
      clust <- kmeans(as.dist(dissim.mat), centers = n.clusters)
      membership <- clust$cluster
  }

  return(list(clust = clust, membership = membership))
}
