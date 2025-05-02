#' HDF5DataFrame Class
#' 
#' The HDF5DataFrame class is a DataFrame subclass for representing datasets 
#' with arbitrary collections of columns stored in HDF5.
#'
#' @param path The path (as a single string or H5File object) to the HDF5 file 
#' (.h5 or .h5ad) where the dataset is located. 
#' @param name The name of the group in the HDF5 file.
#' @param columns the names of the columns, 
#' see \link[HDF5ColumnVector-class]{HDF5ColumnVector}
#' @param nrows the number of rows of the DataFrame.
#'
#' @exportClass HDF5DataFrame
setClass("HDF5DataFrame", 
         contains="DataFrame",
         slots=c(path="character", 
                 name = "character", 
                 columns="character", 
                 nrows="integer"))

#' HDF5ColumnSeed Class
#' 
#' The HDF5ColumnSeed class for \link[HDF5ColumnVector-class]{HDF5ColumnVector}.
#'
#' @param path The path (as a single string or H5File object) to the HDF5 file 
#' (.h5 or .h5ad) where the dataset is located. 
#' @param name The name of the dataset in the HDF5 file.
#' @param column the names of the columns, 
#' see \link[HDF5ColumnVector-class]{HDF5ColumnVector}
#' @param length the length of the \link[HDF5Array]{HDF5Array}.
#' 
#' @exportClass HDF5ColumnSeed
setClass("HDF5ColumnSeed", 
         slots=c(path="character", 
                 name = "character", 
                 column="character", 
                 length="integer", 
                 type="character"))

#' HDF5ColumnVector Class
#' 
#' The HDF5ColumnVector class for each column of a 
#' \link[HDF5DataFrame-class]{HDF5DataFrame} class
#'
#' @param seed An \link[HDF5ColumnSeed-class]{HDF5ColumnSeed} object
#'
#' @exportClass HDF5ColumnVector
setClass("HDF5ColumnVector", 
         contains="DelayedArray", 
         slots=c(seed="HDF5ColumnSeed"))