# HDF5DataFrame

HDF5-backed DataFrame objects and methods.

## Installation

You can install **{HDF5DataFrame}** from Bioconductor using **BiocManager**:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}
BiocManager::install("HDF5DataFrame")
```

Or you can install the development version of **{HDF5DataFrame}** from GitHub like so:

``` r
if (!require("devtools", quietly = TRUE))
    install.packages("devtools")
devtools::install_github("BIMSBbioinfo/HDF5DataFrame")
```