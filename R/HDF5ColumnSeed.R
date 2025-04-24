#' Column of an HDF5-based data frame
#'
#' Represent a column of a HDF5-based data frame as a 1-dimensional 
#' \linkS4class{DelayedArray}. This allows us to use HDF5-backed data 
#' inside \linkS4class{DataFrame} without loading them into memory.
#'
#' @param path String containing a path to a HDF5-based data frame.
#' @param name String containing the HDF5 group of the h5 file.
#' @param column String containing the name of the column inside the file.
#' @param length Integer containing the number of rows.
#' If \code{NULL}, this is determined by inspecting the file.
#' This should only be supplied for efficiency purposes, to avoid a 
#' file look-up on construction.
#' @param type String specifying the type of the data.
#' If \code{NULL}, this is determined by inspecting the file.
#' Users may specify this to avoid a look-up, or to coerce the output 
#' into a different type.
#' @param x Either a string containing the path to an HDF5-based data frame 
#' file (to be used as \code{path}), or an existing HDF5ColumnSeed object.
#' @param seed A HDF5ColumnSeed object
#' @param ... Further arguments to be passed to the \code{HDF5ColumnSeed} 
#' constructor.
#'
#' @importFrom methods new is as
#' @return For \code{HDF5ColumnSeed}, a HDF5ColumnSeed is returned, obviously.
#' 
#' For \code{HDF5ColumnVector}, a HDF5ColumnVector is returned.
#'
#' @author Artür Manukyan
#'
#' @aliases 
#' DelayedArray, HDF5ColumnSeed-method
#' 
#' @name HDF5ColumnSeed
NULL

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
#' @importFrom h5mread h5mread
setMethod("extract_array", "HDF5ColumnSeed", function(x, index) {
    slice <- index[[1]]
   
    if (is.null(slice)) {
        output <- h5mread::h5mread(filepath = x@path, 
                                   name = paste0(x@name, "/", x@column))
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

        output <- h5mread::h5mread(filepath = x@path, 
                                   name = paste0(x@name, "/", x@column), 
                                   starts = list(slice))
        if (modified) {
            m <- match(original, slice)
            output <- output[m]
        }
    }
    
    if (!methods::is(output, x@type)) {
        output <- methods::as(output, x@type)
    }

    array(output)
})

#' @export
#' @rdname HDF5ColumnSeed
#' @importFrom h5mread h5mread
#' @importFrom DelayedArray type
HDF5ColumnSeed <- function(path, name, column, type=NULL, length=NULL) {
    if (is.null(type) || is.null(length)) {
        if (is.null(type)){ 
            type <- DelayedArray::type(
              h5mread::h5mread(filepath = path, 
                               name = paste0(name, "/", column), 
                               starts = list(1)))
        }
        if (is.null(length)) {
          length <- length(h5mread::h5mread(filepath = path, 
                                            name = paste0(name, "/", column)))
        }
    } 
    methods::new("HDF5ColumnSeed", 
        path=path, 
        name=name, 
        column=column, 
        length=length, 
        type=type)
}

#' @export
#' @rdname HDF5ColumnSeed
HDF5ColumnVector <- function(x, ...) {
    if (!methods::is(x, "HDF5ColumnSeed")) {
        x <- HDF5ColumnSeed(x, ...)
    }
    methods::new("HDF5ColumnVector", seed=x)
}

#' @export
#' @rdname HDF5ColumnSeed
#' @importFrom DelayedArray DelayedArray
setMethod("DelayedArray", 
          "HDF5ColumnSeed", 
          function(seed) HDF5ColumnVector(seed))
