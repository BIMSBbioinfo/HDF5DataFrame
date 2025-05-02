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

test_that("create metadata", {
  
  # open h5ad
  h5createFile(output_h5ad)

  # set metadata
  meta.data_list <- list()
  h5createGroup(output_h5ad, group = "assay")
  for(i in 1:ncol(metadata)){
    cur_column <- as.vector(subset(metadata, 
                                   select = colnames(metadata)[i]))[[1]]
    if(is.character(cur_column) || is.factor(cur_column))
      cur_column <- as.character(cur_column)
    cur_column <- as.array(cur_column)
    meta.data_list[[colnames(metadata)[i]]] <- 
      writeHDF5Array(cur_column, 
                     output_h5ad, 
                     name = paste0("assay", "/", 
                                   colnames(metadata)[i]), 
                     with.dimnames = FALSE)
  }
  metadata_large <- 
    HDF5DataFrame(meta.data_list, 
                  name = "assay", 
                  columns = names(meta.data_list))
  
  # check functions
  expect_equal(dim(metadata_large), dim(metadata))
  expect_equal(ncol(metadata_large), ncol(metadata))
  expect_equal(nrow(metadata_large), nrow(metadata))
  expect_equal(names(metadata_large), names(metadata))
  
  # random access/subset
  metadata_subset <- metadata[c(1,4,8),]
  metadata_large_subset <- metadata_large[c(1,4,8),]
  expect_equal(dim(metadata_large_subset), dim(metadata_subset))
  expect_equal(ncol(metadata_large_subset), ncol(metadata_subset))
  expect_equal(nrow(metadata_large_subset), nrow(metadata_subset))
  expect_equal(names(metadata_large_subset), names(metadata_subset))

  # conversion
  metadata_large_local <- as.data.frame(metadata_large)
  expect_equal(dim(metadata_large_local), dim(metadata))
  expect_equal(ncol(metadata_large_local), ncol(metadata))
  expect_equal(nrow(metadata_large_local), nrow(metadata))
  expect_equal(names(metadata_large_local), names(metadata))
  
  # merge with in memory metadata
  metadata3 <- cbind(metadata_large, metadata2)
  
  # add new column
  metadata_large$weightnew <- metadata$weight
  metadata_large[["weightnew2"]] <- metadata$weight
  
  # set new metadata
  meta.data_list <- list()
  h5createGroup(output_h5ad, group = "assay2")
  for(i in 1:ncol(metadata2)){
    cur_column <- as.vector(subset(metadata2, 
                                   select = colnames(metadata2)[i]))[[1]]
    if(is.character(cur_column) || is.factor(cur_column))
      cur_column <- as.character(cur_column)
    cur_column <- as.array(cur_column)
    meta.data_list[[colnames(metadata)[i]]] <- 
      writeHDF5Array(cur_column, 
                     output_h5ad, 
                     name = paste0("assay2", "/", 
                                   colnames(metadata)[i]), 
                     with.dimnames = FALSE) 
  }
  metadata2_large <- 
    HDF5DataFrame(meta.data_list, 
                  name = "assay2", 
                  columns = names(meta.data_list))

  # merge with in memory metadata
  metadata3 <- cbind(metadata_large, metadata2_large)
  
  # refresh
  file.remove(output_h5ad)
})