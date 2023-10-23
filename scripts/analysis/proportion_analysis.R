# @Author: Yitan Lu
# @Timestamp for Creation: 2023-10-22 13:10:07

# ===============================================
# Analysis of target proportion in each module
# ===============================================
# Description:
# This script performs an analysis of target proportion within each module.
#
# To use this function, save the script as a .R file,
# and load it in your R session using the 'source' function,
# for example: source("proportion_analysis.R")
# ===============================================
# Parameters:
# edge_module_file: Path to the edge-module mapping file,
# which can be obtained using the 'getCommunities' function from the DTANetPerturbeR package.
# net_tars_file: Path to the network target file,
# which can be obtained using the 'mapTargets' function from the DTANetPerturbeR package.
# ===============================================
# Example usage:
# tar_proportion <- analyzeProportion("edge_Module.txt", "Net_Tars.txt")
# ===============================================

analyzeProportion <- function(edge_module_file, net_tars_file) {
  # Load the necessary library
  library(igraph)

  # Load the edge-module mapping data and the network target data
  edges <- read.table(edge_module_file, header = TRUE, sep = "\t")
  net_tars <- read.table(net_tars_file, header = TRUE, sep = "\t")

  # Store module-specific target proportions
  all_modules <- sort(unique(edges$module))
  module_perc_targets <- numeric(length(all_modules))

  for (m in all_modules) {
    module_edges <- edges[edges$module == m, ]
    module_edges <- data.frame(from = module_edges[, 1], to = module_edges[, 2])
    module_net <- graph_from_data_frame(module_edges, directed = FALSE)
    module_nodes <- get.vertex.attribute(module_net)[[1]]

    # Get all the drug targets that are in the current module
    all_tars <- net_tars[, 1]
    module_tars <- intersect(all_tars, module_nodes)

    # Calculate the percentage of targets in the module
    perc_targets <- length(module_tars) / length(module_nodes)
    module_perc_targets[m] <- perc_targets
  }

  # Write the result data frame to a text file
  tar_proportion <- data.frame(module = all_modules, proportion = module_perc_targets)
  write.table(tar_proportion, "target_proportion.txt", sep = "\t", quote = FALSE, row.names = FALSE)

  # Return the calculation result
  return(tar_proportion)
}
