# @Author: Yitan Lu
# @Timestamp for Creation: 2023-03-19 10:50:26

# ===============================================
# Reachability Visualization Script
# ===============================================
# Description:
# This script generates a density plot of
# fold change values for Background and MS-related genes.
# -----------------------------------------------
# The 'Reachability_Score.txt' file can be obtained using
# the 'getEnlargedNet' function from the DTANetPerturbeR package.
# ===============================================

# Read data from the text file
Reachability <- read.table("Reachability_Score.txt", header = TRUE, sep = "\t")

# Set parameters
seedcount <- 80
size_ratio <- 45
genenum_cutoff <- seedcount * size_ratio

# Extract the Background and MS-related data
Reachability1 <- Reachability[1:nrow(Reachability), ]
Reachability2 <- Reachability[1:genenum_cutoff, ]

# Extract the Fold_Change column data
background_data <- Reachability1$Fold_Change
selectgenes_data <- Reachability2$Fold_Change

# Set the threshold
quantile_cutoff <- 0.95
selectgenes_data_trimmed <- selectgenes_data[selectgenes_data <= quantile(selectgenes_data, quantile_cutoff)]
background_data_trimmed <- background_data[background_data <= quantile(background_data, quantile_cutoff)]

# Create a data frame
data <- data.frame(
  value = c(selectgenes_data_trimmed, background_data_trimmed),
  group = c(rep("selectgenes", length(selectgenes_data_trimmed)),
            rep("Background", length(background_data_trimmed)))
)

# Load the ggplot2 library and create the plot
library(ggplot2)
ggplot(data, aes(x = value, color = group)) +
  geom_density(size = 1.2, alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 8, by = 2)) +
  scale_color_manual(values = c("selectgenes" = "#FB6467",
                                "Background" = "#5C88DA")) +
  labs(x = "Fold Change of Network Reachability",
       y = "Density") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 14),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )
