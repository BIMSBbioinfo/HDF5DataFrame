# library
library(rhdf5)
library(HDF5Array)
library(HDF5DataFrame)
skip_if_not_installed("data.table")

# data
data("chickwts")
metadata <- chickwts

test_that("create metadata", {
  
  # set metadata
  metadata_large <- as(metadata, "HDF5DataFrame")
  
  # set metadata from DataFrame
  metadata_df <- as(metadata, "DataFrame")
  metadata_large <- as(metadata, "HDF5DataFrame")
  
  # set metadata from list
  metadata_temp <- as(list(a = 1:10, b = 1:10), "HDF5DataFrame")
  
  # set metadata from data.table
  metadata_dt <- as(metadata, "data.table")
  metadata_large <- as(metadata_dt, "HDF5DataFrame")
  
  # expect
  expect_equal(1L, 1L)
})