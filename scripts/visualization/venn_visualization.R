# @Author: Yitan Lu
# @Timestamp for Creation: 2022-03-24 23:06:05

# ===============================================
# Venn Visualization Script
# ===============================================
# Description:
# This script uses disease genes associated with MS collected
# from five different databases and creates a Venn diagram
# to visualize the intersection of these gene sets.
# ===============================================

# Load the necessary library
library(VennDiagram)

# Read data from CSV files
MS_DISEASES_Genes <- read.csv("MS_DISEASES_Gene.csv", header = FALSE)
DISEASES <- MS_DISEASES_Genes[, 1]

MS_DisGeNET_Gene <- read.csv("MS_DisGeNET_Gene.csv", header = FALSE)
DisGeNET <- MS_DisGeNET_Gene[, 1]

MS_PGKB_Gene <- read.csv("MS_PGKB_Gene.csv", header = FALSE)
PGKB <- MS_PGKB_Gene[, 1]

MS_PheGenl_Gene <- read.csv("MS_PheGenl_Gene.csv", header = FALSE)
PheGenl <- MS_PheGenl_Gene[, 1]

MS_OMIM_Gene <- read.csv("MS_OMIM_Gene.csv", header = FALSE)
OMIM <- MS_OMIM_Gene[, 1]

# Create a list of gene sets for Venn diagram
vennlist <- list(DISEASES, DisGeNET, PGKB, PheGenl, OMIM)

# Generate a Venn diagram to visualize the intersection of gene sets
venn.diagram(vennlist,
             fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
             category.names = c("DISEASES", "DisGeNET", "PGKB", "PheGenl", "OMIM"),
             alpha = 0.50, cat.col = rep("black", 5), col = "grey",
             cex = 1.5, fontfamily = "sans",
             cat.cex = 1.5, cat.fontfamily = "sans", cat.fontface = "bold",
             margin = 0.2, filename = "MS_Genes.png",
             output = TRUE)
