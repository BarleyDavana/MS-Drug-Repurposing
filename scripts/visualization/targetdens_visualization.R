# @Author: Yitan Lu
# @Timestamp for Creation: 2023-03-13 10:01:42

# ===========================================================
# Target Density Distribution and Zscore Visualization Script
# ===========================================================
# Description:
# This script plots the change in target density with
# the number of genes in seed and randomly sampled networks
# and the Z-score curve under different ratios of network sizes.
# It uses the results calculated from the `targetdens_analysis.R` script.
# ===========================================================

# Read data from the text file
random_density <- read.table("Random_100_Target_Density.txt", header = TRUE, sep = "\t")
zscore_pvalue <- read.table("Des_Mat_Distribution.txt", header = TRUE, sep = "\t")
seedcount <- 80

# Load necessary libraries
library(reshape2)
library(ComplexHeatmap)
library(ggplot2)

# Prepare data for the first plot
sample_cols <- ncol(random_density)
random_density$GeneNum <- 1:nrow(random_density)
random_density$Seed <- zscore_pvalue$Seed
random_density$rowmeans <- rowMeans(random_density[, 1:sample_cols])

# Melt data for plotting
density_data <- melt(random_density, id = "GeneNum")
colnames(density_data) <- c("GeneNum", "sample", "value")
cols <- c(rep("gray", sample_cols), "red", "black")

# Create a legend
lgd <- Legend(labels = c("random", "seed"), type = "lines",
              legend_gp = gpar(col = 1:2, fontsize = 16),
              grid_width = unit(0.5, "cm"), background = c("gray", "white"),
              labels_gp = gpar(col = "black", fontsize = 12))

xlim <- c(seedcount, nrow(random_density))

# Create the first plot
ggplot(density_data, aes(x = GeneNum, y = value, color = sample)) +
  geom_line() +
  theme_classic() +
  scale_color_manual(values = cols) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = xlim) +
  labs(x = "Number of genes in network", y = "Density of targets in network") +
  theme(axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
draw(lgd, x = unit(0.75, "npc"), y = unit(0.8, "npc"), just = c("right", "top"))

# Prepare data for the second plot
zscore_pvalue$GeneNum <- 1:nrow(zscore_pvalue)
df <- data.frame(zscore = zscore_pvalue$zscore, GeneNum = zscore_pvalue$GeneNum)
start <- seedcount * 2
zscore_data <- df[seq(start, nrow(df), by = seedcount), ]
zscore_data$Fold <- zscore_data$GeneNum / seedcount

# Create the second plot
ggplot(zscore_data, aes(x = Fold, y = zscore)) +
  geom_line() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 70)) +
  scale_y_continuous(limits = c(0, 2.5)) +
  labs(x = "Ratio of network size", y = "Zscore of target density") +
  theme(axis.text.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) +
  geom_hline(yintercept = c(0, 0.5), linetype = 3, color = 2:3, linewidth = 1)
