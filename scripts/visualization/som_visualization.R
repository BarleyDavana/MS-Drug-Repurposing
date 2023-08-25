# @Author: Yitan Lu
# @Timestamp for Creation: 2023-01-03 13:54:59

# ===============================================
# SOM Visualization Script
# ===============================================
# Description:
# This script provides visualizations for a trained SOM model.
# It assumes that the SOM model is obtained using the `analyzeMoA` function.
# -----------------------------------------------
# Example of obtaining the som_model, som_cluster, and dbi_values:
# result <- analyzeMoA(data_path = "data.txt", grid_dim = c(5, 5), rlen = 150)
# som_model <- result$som_model
# som_cluster <- result$som_cluster
# dbi_values <- result$dbi_values
# ===============================================

# Load necessary libraries
library(kohonen)
library(ggplot2)

# U-matrix: Data vector distance
pdf("som_distance.pdf", width = 6, height = 4)
plot(som_model, shape = c("straight"),
     palette.name = hcl.colors, ncolors = 30,
     heatkeywidth = .3,
     border = "white", type = "dist.neighbours",
     main = "")
dev.off()

# Cluster codebook vectors using hierarchical clustering
som_hc <- cutree(hclust(object.distances(som_model, "codes")), 5)
add.cluster.boundaries(som_model, som_hc)
bgcol <- c("#6e9ece", "#8d6ab8", "#4e9595", "#76ba80", "#EFC000")[som_cluster]

# Matrix plot after clustering
pdf("som_cluster.pdf", width = 6, height = 4)
plot(som_model, shape = c("straight"),
     heatkeywidth = .3, bgcol = bgcol,
     border = "white", type = "codes",
     main = "",
     codeRendering = "None")
dev.off()

# DBI index line plot
df <- data.frame(cluster = c(1:10), dbi = dbi_values, labels = round(dbi_values, 2))
ggplot(df, aes(x = cluster, y = dbi)) +
  geom_line(size = 1, color = "#78A9CE", linetype = 1) +
  geom_point(size = 3, color = "#5786C5") +
  labs(x = "Number of Clusters", y = "DBI index") +
  scale_x_continuous(limits = c(1, 10), breaks = 1:10) +
  scale_y_continuous(limits = c(0.9, 1.7), breaks = seq(0.9, 1.7, by = 0.2)) +
  theme_classic() +
  theme(text = element_text(family = "sans", colour = "gray30", size = 18),
        axis.line = element_line(size = 0.6, colour = "gray30"),
        axis.ticks = element_line(size = 0.6, colour = "gray30"),
        axis.ticks.length = unit(1.5, units = "mm"),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = "inches")) +
  geom_vline(xintercept = 5, size = 0.9, color = "#E37449", lty = "dashed") +
  geom_point(aes(5, 0.94), color = "#5786C5") +
  geom_text(aes(5, 0.94, label = "0.94"), color = "#3E859D", size = 5, hjust = 1.5, vjust = 1)
