# @Author: Yitan Lu
# @Timestamp for Creation: 2023-03-09 12:23:46

# ===============================================
# Degree Distribution Visualization Script
# ===============================================
# Description:
# This script draws the degree distribution of the input graph.
# -----------------------------------------------
# The 'Seed_Net.txt' file contains the MS Seed PPIN,
# where the first two columns represent the gene symbols
# of the source and target nodes, respectively.
# ===============================================

# Load necessary libraries
library(igraph)
library(ggplot2)

# Read data from the text file
Gene_net <- read.table("Seed_Net.txt", header = TRUE, sep = "\t")

# Create a data frame for edges from the input data
edges <- data.frame(from = Gene_net[, 1], to = Gene_net[, 2])

# Create an undirected graph from the edge data
Gene_net <- graph.data.frame(edges, directed = FALSE)

# Calculate the degree of each node in the graph
d_Gene_net <- degree(Gene_net)

# Create a data frame for degree frequency
degree_freq <- data.frame(table(d_Gene_net))

# Rename the columns
colnames(degree_freq) <- c("Degree", "Frequency")

# Convert factor type columns to numeric
degree_freq$Degree <- as.numeric(as.character(degree_freq$Degree))
degree_freq$Frequency <- as.numeric(as.character(degree_freq$Frequency))

# Create the degree distribution plot
ggplot(degree_freq, aes(x = Degree, y = Frequency)) +
  geom_point(color = "deepskyblue4", size = 3) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  xlab("Number of interaction partners") +
  ylab("Number of proteins") +
  theme_classic() +
  ggtitle("Degree distribution") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(size = 1.2),
        axis.ticks = element_line(size = 1.2))
