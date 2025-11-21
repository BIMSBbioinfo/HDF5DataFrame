# library
library(rhdf5)
library(HDF5Array)
library(HDF5DataFrame)

# h5
output_h5ad <- tempfile(fileext = ".h5")

# data
data("chickwts")
metadata <- chickwts

test_that("create metadata", {
  
  # set metadata
  metadata_large <- writeHDF5DataFrame(metadata, 
                                       filepath = output_h5ad, 
                                       name = "metadata", 
                                       replace = TRUE)
  
  # get group metadata
  group_metadata <- h5lsgroup(output_h5ad, "metadata")
  expect_contains(group_metadata$name, colnames(metadata))
  group_metadata <- h5lsgroup(output_h5ad, "/metadata")
  expect_contains(group_metadata$name, colnames(metadata))
  
  # set metadata at subgroup
  metadata_large <- writeHDF5DataFrame(metadata, 
                                       filepath = output_h5ad, 
                                       name = "metadata/submeta", 
                                       replace = FALSE)
  group_metadata <- h5lsgroup(output_h5ad, "metadata/submeta")
  expect_contains(group_metadata$name, colnames(metadata))
  group_metadata <- h5lsgroup(output_h5ad, "/metadata/submeta")
  expect_contains(group_metadata$name, colnames(metadata))
  
  # refresh
  file.remove(output_h5ad)
})