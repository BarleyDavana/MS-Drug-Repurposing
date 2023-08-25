# @Author: Yitan Lu
# @Timestamp for Creation: 2022-11-04 15:26:50

# ===============================================
# Analysis of drug mechanisms of action via SOM
# ===============================================
# Description:
# This script defines a function named 'analyzeMoA' that is designed to analyze the 
# Mechanism of Action (MoA) of drugs using Self-Organizing Maps (SOM). This method 
# provides an unsupervised approach to identify patterns in the MoA data and cluster 
# them accordingly. The function preprocesses the data, trains a SOM, classifies the 
# data points based on the SOM model, and determines the optimal number of clusters 
# using the Davies-Bouldin Index (DBI).
#
# To use this function, save the script as a .R file,
# and load it in your R session using the 'source' function,
# for example: source("moa_analysis.R")
# ===============================================
# Parameters:
# data_path: String. Path to the input data file in a tab-separated format with headers and row names.
# grid_dim: Numeric Vector. Dimensions of the SOM grid.
# rlen: Numeric. The number of times the complete data set will be presented to the SOM. Default is 200.
# ===============================================
# Returns:
# A list containing:
# - som_model: The trained SOM model.
# - som_cluster: The cluster assignments for the data points.
# - som_model_code_class_cluster: The data combined with the SOM codes and cluster assignments.
# - dbi_values: Davies-Bouldin Index values for each tested number of clusters.
# ===============================================
# Required packages: kohonen
# If the package is not installed,
# please run install.packages("kohonen")
# ===============================================
# Example usage:
# result <- analyzeMoA(data_path = "data.txt", grid_dim = c(5, 5), rlen = 150)
# som_model <- result$som_model
# -----------------------------------------------
# Display the training process,
# observe the trend of decreasing distance with iterations
# to assess if the iterations are sufficient;
# a stable trend towards the end is preferable.
# plot(som_model, type = "changes")
# ===============================================

analyzeMoA <- function(data_path, grid_dim, rlen = 200) {
  # Load kohonen library
  library(kohonen)
  
  # Read and preprocess the scoring matrix
  data <- read.table(data_path, sep = "\t", header = T, row.names = 1)
  data_train_matrix <- as.matrix(t(scale(data)))
  
  # Train the SOM model
  grid <- somgrid(xdim = grid_dim[1], ydim = grid_dim[2], topo = "hexagonal")
  som_model <- som(X = data_train_matrix, grid = grid, rlen = rlen)
  
  # Classify data points based on the SOM model
  som_model_code_class <- data.frame(name = rownames(data_train_matrix), code_class = som_model$unit.classif)
  
  # Determine optimal clustering based on DBI
  mydata <- as.matrix(as.data.frame(som_model$codes))
  dbi <- vector()
  for (i in 2:11) {
    k <- kmeans(mydata, centers = i)$cluster
    dbi[i - 1] <- calDBI(mydata, k)
  }
  
  # Find the number of clusters that gives the minimum DBI
  optimal_clusters <- which.min(dbi)
  
  # Perform hierarchical clustering
  som_cluster <- cutree(hclust(dist(mydata)), optimal_clusters)
  
  # Assign the new classes based on the clustering
  som_model_code_class_cluster <- som_model_code_class
  som_model_code_class_cluster$cluster <- som_cluster[som_model_code_class$code_class]
  
  return(list(som_model = som_model, som_cluster = som_cluster, som_model_code_class_cluster = som_model_code_class_cluster, dbi_values = dbi))
}


calDBI <- function(x, labels) {
  # Determine the number of unique clusters
  clusters_n <- length(unique(labels))
  
  # Split the data by cluster assignments
  cluster_k <- list()
  for (i in 1:clusters_n) {
    cluster_k[[i]] <- x[which(labels == i), ]
  }
  
  # Calculate the centroids for each cluster
  centroids <- list()
  for (i in 1:clusters_n) {
    centroids[[i]] <- apply(cluster_k[[i]], 2, mean)
  }
  
  # Calculate the average distance from each point in a cluster to its centroid
  s <- list()
  for (i in 1:clusters_n) {
    a <- c()
    for (j in 1:nrow(cluster_k[[i]])) {
      b <- dist(rbind(cluster_k[[i]][j, ], centroids[[i]]), method = "euclidean")
      a <- c(a, b)
    }
    s[[i]] <- mean(a)
  }
  
  # Compute the Ri value for each cluster, which measures its separation from other clusters
  Ri <- list()
  for (i in 1:clusters_n) {
    r <- c()
    for (j in 1:clusters_n) {
      if (j != i) {
        h <- (s[[i]] + s[[j]]) / dist(rbind(centroids[[i]], centroids[[j]]), method = "euclidean")
        r <- c(r, h)
      }
    }
    Ri[[i]] <- max(r)
  }
  
  # Calculate the Davies-Bouldin Index (DBI) by averaging the Ri values
  dbi <- mean(unlist(Ri))
  
  return(dbi)
}
