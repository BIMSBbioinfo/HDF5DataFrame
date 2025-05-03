# library
library(rhdf5)
library(HDF5Array)
library(HDF5DataFrame)

# h5
output_h5ad <- tempfile(fileext = ".h5")

# data
data("chickwts")
metadata <- chickwts
metadata2 <- chickwts
names(metadata2) <- paste0("new", names(metadata2))

test_that("hdf5columnseed", {
  
  # open h5ad
  h5createFile(output_h5ad)
  
  # set metadata
  meta.data_list <- list()
  h5createGroup(output_h5ad, group = "metadata")
  for(i in 1:ncol(metadata)){
    cur_column <- as.vector(subset(metadata, 
                                   select = colnames(metadata)[i]))[[1]]
    if(is.character(cur_column) || is.factor(cur_column))
      cur_column <- as.character(cur_column)
    cur_column <- as.array(cur_column)
    meta.data_list[[colnames(metadata)[i]]] <- 
      writeHDF5Array(cur_column, 
                     output_h5ad, 
                     name = paste0("metadata", "/", 
                                   colnames(metadata)[i]), 
                     with.dimnames = FALSE)
  }
  
  # define hd5columnseed
  columnseed <- HDF5ColumnSeed(path = path(meta.data_list[[1]]), 
                               name = "metadata", 
                               column = colnames(metadata)[i], 
                               type = type(meta.data_list[[1]]))
  
  # dim
  expect_equal(dim(columnseed), nrow(metadata))
  
  # path
  expect_equal(path(columnseed), path(meta.data_list[[1]]))
  
  # type
  expect_equal(type(columnseed), type(meta.data_list[[1]]))
  
  # refresh
  file.remove(output_h5ad)
})