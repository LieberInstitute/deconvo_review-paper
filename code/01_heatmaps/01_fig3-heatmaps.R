#!/usr/bin/env R

# Author: Sean Maden
#
# Heatmaps for main figure summarizing deconvolution.
#
#

library(ComplexHeatmap)

set.seed(0)

#-----------------
# simulate markers
#-----------------
# simulate cells by type
# neuron data
num.neuron <- 20
neuron.data <- unlist(lapply(seq(num.neuron), function(index){
  c(rpois(n = 50, lambda = sample(c(1, 5, 20), 100, replace = T)),
    rpois(n = 5, lambda = 1),
    rpois(n = 5, lambda = 1),
    rpois(n = 5, lambda = 10))
}))
# oligo data
num.oligo <- 11
oligo.data <- unlist(lapply(seq(num.oligo), function(index){
  c(rpois(n = 50, lambda = sample(c(1, 2, 12), 100, replace = T)),
    rpois(n = 5, lambda = 1),
    rpois(n = 5, lambda = 10),
    rpois(n = 5, lambda = 1))
}))
# astro data
num.astro <- 4
astro.data <- unlist(lapply(seq(num.astro), function(index){
  c(rpois(n = 50, lambda = sample(c(2, 5, 7), 100, replace = T)),
    rpois(n = 5, lambda = 10),
    rpois(n = 5, lambda = 1),
    rpois(n = 5, lambda = 1))
}))

# make main heatmap table
hm.data <- as.numeric(c(neuron.data, oligo.data, astro.data))
hm.table <- matrix(hm.data, nrow = 65)
colnames(hm.table) <- c(paste0("Neuron", seq(num.neuron)),
                        paste0("Oligodendrocyte", seq(num.oligo)),
                        paste0("Astrocyte", seq(num.astro)))
rownames(hm.table) <- paste0("gene", seq(nrow(hm.table)))

# make marker heatmap table
cell.types <- c("Neuron", "Oligodendrocyte", "Astrocyte")
which.markers <- seq(51, 65)
hm.marker.list <- lapply(cell.types, function(type){
  rowMeans(hm.table[which.markers,
                    grepl(type, colnames(hm.table))])
})
hm.marker.table <- do.call(cbind, hm.marker.list)
colnames(hm.marker.table) <- cell.types
rownames(hm.marker.table) <- paste0("marker", seq(nrow(hm.marker.table)))

#------------------
# make new heatmaps
#------------------
# gene expression across cells

jpeg("hm_genes-cells.jpg", width = 5.5, height = 3.5, units = "in", res = 1000)

Heatmap(hm.table,
        cluster_rows = F,
        cluster_columns = F,
        show_row_dend = F,
        show_column_dend = F,
        column_labels = colnames(hm.table),
        show_row_names = F,
        row_title = "Genes",
        column_title = "Cells",
        show_heatmap_legend = F)

dev.off()

# marker expression across cells
which.markers <- seq(51, 65)

jpeg("hm_markers-cells.jpg", width = 5.5, height = 3.5, units = "in", res = 1000)

Heatmap(hm.table[which.markers,],
        cluster_rows = F,
        cluster_columns = F,
        show_row_dend = F,
        show_column_dend = F,
        column_labels = colnames(hm.table),
        show_row_names = F,
        row_title = "Markers (G)",
        column_title = "Cells",
        show_heatmap_legend = F)

dev.off()

# marker expression by cell types
jpeg("hm_markers-types.jpg", width = 1.5, height = 3.5, units = "in", res = 1000)
Heatmap(hm.marker.table,
        cluster_rows = F,
        cluster_columns = F,
        show_row_dend = F,
        show_column_dend = F,
        show_row_names = F,
        column_labels = cell.types,
        row_title = "Markers (G)",
        column_title = "Cell Types (K)",
        show_heatmap_legend = F,
        border = "black")

dev.off()
