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
# names(metadata2) <- paste0("new", names(metadata2))

test_that("create metadata", {
  
  # set metadata
  metadata_large <- writeHDF5DataFrame(metadata, 
                                       filepath = output_h5ad, 
                                       name = "metadata", 
                                       replace = TRUE)
  
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
  
  # rownames
  rownames(metadata_large) <- paste0("row", 1:nrow(metadata_large))
  expect_equal(rownames(metadata_large), paste0("row", 1:nrow(metadata_large)))
  
  # merge with in memory metadata
  metadata3 <- cbind(metadata_large, metadata2)
  
  # add new column
  metadata_large$weightnew <- metadata$weight
  metadata_large[["weightnew2"]] <- metadata$weight
  
  # set new metadata
  metadata2_large <- writeHDF5DataFrame(metadata2, 
                                        filepath = output_h5ad, 
                                        name = "metadata2", 
                                        replace = FALSE)

  # merge with in memory metadata
  metadata3 <- cbind(metadata_large, metadata2_large)
  
  # refresh
  file.remove(output_h5ad)
})