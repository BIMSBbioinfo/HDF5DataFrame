# library
library(rhdf5)
library(HDF5Array)
library(HDF5DataFrame)

# h5
output_h5ad <- tempfile(fileext = ".h5")

# data
data("chickwts")
metadata <- chickwts

test_that("hdf5columnseed", {
  
  # open h5ad
  h5createFile(output_h5ad)
  
  # set metadata
  h5createGroup(output_h5ad, group = "metadata")
  cur_column <- as.vector(subset(metadata, 
                                 select = colnames(metadata)[1]))[[1]]
  cur_column <- as.character(cur_column)
  cur_column <- as.array(cur_column)
  hdf5_column <-  writeHDF5Array(cur_column, 
                                 output_h5ad, 
                                 name = paste0("metadata", "/", 
                                               colnames(metadata)[1]), 
                                 with.dimnames = FALSE)
  
  # define hd5columnseed
  columnseed <- HDF5ColumnSeed(path = path(hdf5_column), 
                               name = "metadata", 
                               column = colnames(metadata)[1], 
                               type = type(hdf5_column))
  
  # dim
  expect_equal(dim(columnseed), nrow(metadata))
  
  # path
  expect_equal(path(columnseed), path(hdf5_column))
  
  # type
  expect_equal(type(columnseed), type(hdf5_column))
  
  # refresh
  file.remove(output_h5ad)
})