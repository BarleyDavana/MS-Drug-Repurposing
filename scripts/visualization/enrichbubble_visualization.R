# @Author: Yitan Lu
# @Timestamp for Creation: 2023-08-02 19:45:07

# ===============================================
# Enrichment Bubble Visualization Script
# ===============================================
# Description:
# This script creates the bubble plot for
# visualizing the enrichment analysis results of
# the Seed and MS-related PPIN by ontology categories.
# -----------------------------------------------
# The 'enrich80.txt' and 'enrich3680.txt' files contain the results of
# enrichment analysis generated using the clusterProfiler package.
# ===============================================

# Read the enrichment result from the text file
seed_net <- read.table("enrich80.txt", header = TRUE, sep = "\t")
enlarged_net <- read.table("enrich3680.txt", header = TRUE, sep = "\t")

# Load the ggplot2 library
library(ggplot2)

# Create the enrichment bubble plot for the Seed PPIN
ggplot(seed_net, aes(x = reorder(Description, GeneRatio), y = GeneRatio, color = p.adjust, size = Count)) +
  geom_point() +
  scale_color_gradient(low = "red", high = "blue") +
  coord_flip() +
  labs(x = "", y = "GeneRatio", color = "p.adjust", size = "Count") +
  theme_bw() +
  facet_grid(ONTOLOGY~., scale = "free_y", space = "free_y") +
  scale_size_continuous(breaks = seq(0, max(seed_net$Count), by = 5), labels = seq(0, max(seed_net$Count), by = 5)) +
  theme(axis.title.x = element_text(size = rel(1.2), face = "bold"),
        axis.text.y = element_text(size = rel(1.5), face = "bold"),
        axis.text.x = element_text(size = rel(1.2), face = "bold"),
        legend.title = element_text(size = rel(1.2), face = "bold"),
        legend.text = element_text(size = rel(1.2), face = "bold"))

# Create the enrichment bubble plot for the MS-related PPIN
ggplot(enlarged_net, aes(x = reorder(Description, GeneRatio), y = GeneRatio, color = p.adjust, size = Count)) +
  geom_point() +
  scale_color_gradient(low = "red", high = "blue", labels = function(x) sprintf("%.1e", x)) +
  coord_flip() +
  labs(x = "", y = "GeneRatio", color = "p.adjust", size = "Count") +
  theme_bw() +
  facet_grid(ONTOLOGY~., scale = "free_y", space = "free_y") +
  scale_size_continuous(breaks = seq(0, max(enlarged_net$Count), by = 100), labels = seq(0, max(enlarged_net$Count), by = 100)) +
  theme(axis.title.x = element_text(size = rel(1.2), face = "bold"),
        axis.text.y = element_text(size = rel(1.5), face = "bold"),
        axis.text.x = element_text(size = rel(1.2), face = "bold"),
        legend.title = element_text(size = rel(1.2), face = "bold"),
        legend.text = element_text(size = rel(1.2), face = "bold"))
