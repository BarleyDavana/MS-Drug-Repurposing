# @Author: Yitan Lu
# @Timestamp for Creation: 2022-07-22 10:22:20

# =======================================================================
# Within-module Degree and Participation Coefficient Visualization Script
# =======================================================================
# Description:
# This script draws the scatter plot of within-module degree
# and participation coefficient scores of nodes in the modules.
# It uses the result calculated from the `zipi_analysis.R` script.
# =======================================================================

# Read the zi-pi result from the text file
zi_pi <- read.table("zi_pi.txt", header = TRUE, sep = "\t")

# Load the ggplot2 library
library(ggplot2)

# Create a scatter plot of Within-module Degree vs. Participation Coefficient
ggplot(zi_pi, aes(pi, z)) +
  geom_point(aes(color = type), alpha = 0.5, size = 2) +
  scale_color_manual(values = c('gray', 'red', 'blue', 'purple'),
                     limits = c('Peripherals', 'Connectors', 'Module hubs', 'Network hubs')) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = 'black'),
        panel.background = element_blank(), legend.key = element_blank()) +
  labs(x = 'Among-module connectivities', y = 'Within-module connectivities', color = '') +
  geom_vline(xintercept = 0.62) +
  geom_hline(yintercept = 2.5) +
  theme(axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 14))
