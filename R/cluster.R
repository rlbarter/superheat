

generate_cluster <- function(X,
                             n.clusters,
                             clustering.method = c("kmeans", "hierarchical"),
                             dist.method = NULL,
                             ...) {

#     # give warning if user does not specify number of clusters
#     if (is.null(n.clusters)) {
#         warning("Number of clusters defaulted to obs/50. Use 'n.clusters = x' to adjust this.")
#     }

    # calculate dissimilarity matrix
  if (is.null(dist.method)) {
    if (is.data.frame(X))
        vars <- sapply(X, is.numeric)  #identify numeric vars
    if (is.matrix(X))
        vars <- apply(X, 2, is.numeric)  #identify numeric vars
    X.vars <- X[, vars]  # restrict to numeric variables
    cor.mat <- cor(t(X.vars))  #calculate cor matrix
    dissim.mat <- 1 - cor.mat  #calculate dissimilarity matrix
  } else {
    if (is.data.frame(X))
      vars <- sapply(X, is.numeric)  #identify numeric vars
    if (is.matrix(X))
      vars <- apply(X, 2, is.numeric)  #identify numeric vars
    X.vars <- X[, vars]  # restrict to numeric variables
    cor.mat <- cor(t(X.vars))
    dissim.mat <- dist(X.vars, method = dist.method, ...)
  }
    # should I have abs(cor.mat)?

    clustering.method <- match.arg(clustering.method)


    # perform clustering and generate membership vector:
    if (clustering.method == "hierarchical") {
        clust <- hclust(as.dist(dissim.mat))  #hierarchical cluster
        if (!is.null(n.clusters)) {
            membership <- cutree(clust, k = n.clusters)  # generate n.clusters
        } #else if (is.null(n.clusters)) {
          #  n.clusters <- clusGap(dissim.mat, function(x) cutree(hclust(x)), K.max = K.max)  # current default value
          #  membership <- cutree(clust, k = n.clusters)
        #}
    } else if (clustering.method == "kmeans") {
        if (!is.null(n.clusters)) {
            clust <- kmeans(as.dist(dissim.mat), centers = n.clusters)
            membership <- clust$cluster
        } #else if (is.null(n.clusters)) {
          #  n.clusters <- clusGap(dissim.mat, kmeans, K.max = K.max)  # current default value
          #  clust <- kmeans(as.dist(dissim.mat), centers = n.clusters)
          #  membership <- clust$cluster
        #}
    }




    return(membership)

}






