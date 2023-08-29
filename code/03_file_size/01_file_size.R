
library(spatialLIBD)

sce_path_zip <- fetch_data("spatialDLPFC_snRNAseq")
sce_path <- unzip(sce_path_zip, exdir = tempdir())
sce <- HDF5Array::loadHDF5SummarizedExperiment(
  file.path(tempdir(), "sce_DLPFC_annotated"))

sce
# class: SingleCellExperiment 
# dim: 36601 77604 
# metadata(3): Samples cell_type_colors cell_type_colors_broad
# assays(2): counts logcounts
# rownames(36601): MIR1302-2HG FAM138A ... AC007325.4 AC007325.2
# rowData names(7): source type ... gene_type binomial_deviance
# colnames(77604): 1_AAACCCAAGTTCTCTT-1 1_AAACCCACAAGGTCTT-1 ... 19_TTTGTTGTCTCATTGT-1 19_TTTGTTGTCTTAAGGC-1
# colData names(32): Sample Barcode ... cellType_layer layer_annotation
# reducedDimNames(4): GLMPCA_approx TSNE UMAP HARMONY
# mainExpName: NULL
# altExpNames(0):

lobstr::obj_size(sce)
# 172.28 MB

class(sce)
# [1] "SingleCellExperiment"

class(logcounts(sce))
# [1] "DelayedMatrix"
# attr(,"package")
# [1] "DelayedArray"

## how large are the assays?
lobstr::obj_size(counts(sce_hdf5))
## how large are the assays?
lobstr::obj_size(logcounts(sce_hdf5))
# 9.32 MB

## How large are the assays as a matrix?
lobstr::obj_size(as.matrix(logcounts(sce_hdf5)))
# 22.72 GB

logcounts(sce_hdf5) <- as.matrix(logcounts(sce_hdf5))
counts(sce_hdf5) <- as.matrix(counts(sce_hdf5))
lobstr::obj_size(sce_hdf5)
# 45.62 GB
                 
