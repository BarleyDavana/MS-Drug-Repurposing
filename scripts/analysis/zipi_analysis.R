# @Author: Yitan Lu
# @Timestamp for Creation: 2022-07-15 12:29:06

# ==============================================================
# Analysis of within-module degree and participation coefficient
# ==============================================================
# Description:
# This script performs an analysis of within-module degree
# and participation coefficient for each node in a network.
# It then classifies nodes into four different types based on
# specific thresholds and saves the results to separate output files.
#
# To use this function, save the script as a .R file,
# and load it in your R session using the 'source' function,
# for example: source("zipi_analysis.R")
# ===============================================
# Parameters:
# edge_file: Path to the edge data file.
# node_module_file: Path to the node-module mapping file,
# which can be obtained using the 'getCommunities' function from the DTANetPerturbeR package.
# ===============================================
# Example usage:
# zi_pi <- analyzeZiPi("edges.txt", "node_Module.txt")
# ===============================================

analyzeZiPi <- function(edge_file, node_module_file) {
  # Load necessary libraries
  library(igraph)
  library(dplyr)

  # Load the edge data and the node module data
  edges <- read.table(edge_file, header = TRUE, sep = "\t")
  node_module <- read.table(node_module_file, header = TRUE, sep = "\t")

  # create a graph
  g <- graph_from_data_frame(edges, directed = FALSE)

  # Add module information to the graph
  vex <- vertex_attr(g)
  vex <- as.data.frame(vex)
  modu <- left_join(vex, node_module, by = "name")
  V(g)$module <- modu$module

  # Calculate within-module degree z-score and participation coefficient
  result1 <- within_module_deg_z_score(g)
  result2 <- part_coeff(g)
  result2 <- as.data.frame(result2)
  colnames(result2) <- c("pi")

  # Get and join the zi-pi results with module data
  zi_pi <- result1
  zi_pi$pi <- result2$pi
  zi_pi <- na.omit(zi_pi)
  zi_pi$name <- rownames(zi_pi)
  zi_pi_module <- left_join(zi_pi, node_module)

  # Classify nodes into four types based on thresholds
  zi_pi[which(zi_pi$z < 2.5 & zi_pi$pi < 0.62), 'type'] <- 'Peripherals'
  zi_pi[which(zi_pi$z < 2.5 & zi_pi$pi > 0.62), 'type'] <- 'Connectors'
  zi_pi[which(zi_pi$z > 2.5 & zi_pi$pi < 0.62), 'type'] <- 'Module hubs'
  zi_pi[which(zi_pi$z > 2.5 & zi_pi$pi > 0.62), 'type'] <- 'Network hubs'

  # Writing the results to files
  write.table(result1, "within-module.txt", sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE)
  write.table(result2, "among-module.txt", sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE)
  write.table(zi_pi, "zi_pi.txt", sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE)
  write.table(zi_pi_module, "zi_pi_module.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
}


# Function to calculate within-module degree z-score
within_module_deg_z_score <- function(g, A = NULL, weighted = FALSE) {
  stopifnot(is_igraph(g))
  if (is.null(A)) {
    if (isTRUE(weighted)) {
      A <- as_adj(g, sparse = FALSE, names = TRUE, attr = 'weight')
    } else {
      A <- as_adj(g, sparse = FALSE, names = TRUE)
    }
  }
  memb <- vertex_attr(g, "module")
  N <- max(memb)
  nS <- tabulate(memb)
  z <- Ki <- rep.int(0, dim(A)[1L])
  Ksi <- sigKsi <- rep.int(0, N)
  names(z) <- names(Ki) <- rownames(A)
  for (S in seq_len(N)) {
    x <- rowSums(A[memb == S, memb == S])
    Ki[memb == S] <- x
    Ksi[S] <- sum(x) / nS[S]
    sigKsi[S] <- sqrt(sum((x - Ksi[S])^2) / (nS[S] - 1))
  }
  z <- (Ki - Ksi[memb]) / sigKsi[memb]
  z[is.infinite(z)] <- 0
  df <- data.frame(Ki, z, row.names = names(Ki))
  return(df)
}


# Function to calculate participation coefficient
part_coeff <- function(g, A = NULL, weighted = FALSE) {
  stopifnot(is_igraph(g))
  if (is.null(A)) {
    if (isTRUE(weighted)) {
      A <- as_adj(g, sparse = FALSE, attr = 'weight')
    } else {
      A <- as_adj(g, sparse = FALSE)
    }
  }
  memb <- vertex_attr(g, "module")
  Ki <- colSums(A)
  N <- max(memb)
  Kis <- t(rowsum(A, memb))
  pi <- 1 - ((1 / Ki^2) * rowSums(Kis^2))
  names(pi) <- rownames(A)
  return(pi)
}
