#' Column of an HDF5-based data frame
#'
#' Represent a column of a HDF5-based data frame as a 1-dimensional \linkS4class{DelayedArray}.
#' This allows us to use HDF5-backed data inside \linkS4class{DataFrame}s without loading them into memory.
#'
#' @param path String containing a path to a HDF5-based data frame.
#' @param name String containing the HDF5 group of the h5 file.
#' @param column String containing the name of the column inside the file.
#' @param length Integer containing the number of rows.
#' If \code{NULL}, this is determined by inspecting the file.
#' This should only be supplied for efficiency purposes, to avoid a file look-up on construction.
#' @param type String specifying the type of the data.
#' If \code{NULL}, this is determined by inspecting the file.
#' Users may specify this to avoid a look-up, or to coerce the output into a different type.
#' @param x Either a string containing the path to an HDF5-based data frame file (to be used as \code{path}),
#' or an existing HDF5ColumnSeed object.
#' @param ... Further arguments to be passed to the \code{HDF5ColumnSeed} constructor.
#'
#' @return For \code{HDF5ColumnSeed}, a HDF5ColumnSeed is returned, obviously.
#' 
#' For \code{HDF5ColumnVector}, a HDF5ColumnVector is returned.
#'
#' @author Artür Manukyan
#'
#' @aliases
#' HDF5ColumnSeed-class
#' dim,HDF5ColumnSeed-method
#' type,HDF5ColumnSeed-method
#' path,HDF5ColumnSeed-method
#' extract_array,HDF5ColumnSeed-method
#' HDF5ColumnVector-class
#' DelayedArray,HDF5ColumnSeed-method
#'
#' @name HDF5ColumnSeed
NULL

#' @export
#' @import methods
setClass("HDF5ColumnSeed", slots=c(path="character", name = "character", column="character", length="integer", type="character"))

#' @export
setMethod("dim", "HDF5ColumnSeed", function(x) x@length)

#' @export
#' @importFrom DelayedArray type
setMethod("type", "HDF5ColumnSeed", function(x) x@type)

#' @export
#' @importFrom BiocGenerics path
setMethod("path", "HDF5ColumnSeed", function(object) object@path)

#' @export
#' @importFrom DelayedArray extract_array
#' @importFrom HDF5Array h5mread
setMethod("extract_array", "HDF5ColumnSeed", function(x, index) {
    slice <- index[[1]]

    if (is.null(slice)) {
        output <- HDF5Array::h5mread(filepath = x@path, name = paste0(x@name, "/", x@column))
    } else if (length(slice) == 0) {
        output <- logical()
    } else {
        original <- slice
        modified <- FALSE

        if (anyDuplicated(slice)) {
            slice <- unique(slice)
            modified <- TRUE
        }

        if (is.unsorted(slice)) {
            slice <- sort(slice)
            modified <- TRUE
        }

        output <- HDF5Array::h5mread(filepath = x@path, name = paste0(x@name, "/", x@column), starts = list(slice))
        if (modified) {
            m <- match(original, slice)
            output <- output[m]
        }
    }
    
    if (!is(output, x@type)) {
        output <- as(output, x@type)
    }

    array(output)
})

#' @export
#' @rdname HDF5ColumnSeed
#' @importFrom DelayedArray type
HDF5ColumnSeed <- function(path, name, column, type=NULL, length=NULL) {
    if (is.null(type) || is.null(length)) {
        if (is.null(type)){ 
            type <- DelayedArray::type(HDF5Array::h5mread(filepath = path, name = paste0(name, "/", column), starts = list(1)))
        }
        if (is.null(length)) {
          length <- length(HDF5Array::h5mread(filepath = path, name = paste0(name, "/", column)))
        }
    } 
    # print(type)
    new("HDF5ColumnSeed", path=path, name=name, column=column, length=length, type=type)
}

#' @export
setClass("HDF5ColumnVector", contains="DelayedArray", slots=c(seed="HDF5ColumnSeed"))

#' @export
#' @importFrom DelayedArray DelayedArray
setMethod("DelayedArray", "HDF5ColumnSeed", function(seed) HDF5ColumnVector(seed))

#' @export
#' @rdname HDF5ColumnSeed
HDF5ColumnVector <- function(x, ...) {
    if (!is(x, "HDF5ColumnSeed")) {
        x <- HDF5ColumnSeed(x, ...)
    }
    new("HDF5ColumnVector", seed=x)
}
