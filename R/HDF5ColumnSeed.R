#' HDF5ColumnSeed
#'
#' Represent a column of a HDF5-based data frame as a 1-dimensional 
#' \link[DelayedArray]{DelayedArray}. This allows us to use HDF5-backed data 
#' inside \link[S4Vectors]{DataFrame} without loading them into memory.
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
#' @param seed,object  A HDF5ColumnSeed object
#' @param index An unnamed list of integer vectors, one per dimension in x. 
#' See \link[S4Arrays]{extract_array}
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
#' DelayedArray,HDF5ColumnSeed-method
#' 
#' @name HDF5ColumnSeed
#' 
#' @examples
#' # libraries
#' library(rhdf5)
#' library(HDF5Array)
#' library(HDF5DataFrame)
#' 
#' # h5
#' output_h5ad <- tempfile(fileext = ".h5")
#' h5createFile(output_h5ad)
#' h5createGroup(output_h5ad, group = "assay")
#' 
#' # data
#' data("chickwts")
#' metadata <- chickwts
#' 
#' # set metadata
#' meta.data_list <- list()
#' for(i in 1:ncol(metadata)){
#'   cur_column <- as.vector(subset(metadata, 
#'                                  select = colnames(metadata)[i]))[[1]]
#'   if(is.character(cur_column) || is.factor(cur_column))
#'     cur_column <- as.character(cur_column)
#'   cur_column <- as.array(cur_column)
#'   meta.data_list[[colnames(metadata)[i]]] <- 
#'     writeHDF5Array(cur_column, 
#'                    output_h5ad, 
#'                    name = paste0("assay", "/", 
#'                                  colnames(metadata)[i]), 
#'                    with.dimnames = FALSE)
#' }
#' 
#' # define hd5columnseed
#' columnseed <- HDF5ColumnSeed(path = path(meta.data_list[[1]]), 
#'                              name = "metadata", 
#'                              column = colnames(metadata)[i], 
#'                              type = type(meta.data_list[[1]]))
#' 
#' # methods
#' dim(columnseed)
#' path(columnseed)
#' type(columnseed)                              
#'                        
NULL

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

#' @rdname HDF5ColumnSeed  
#' @export
setMethod("dim", "HDF5ColumnSeed", function(x) x@length)

#' @rdname HDF5ColumnSeed
#' @export
#' @importFrom DelayedArray type
setMethod("type", "HDF5ColumnSeed", function(x) x@type)

#' @rdname HDF5ColumnSeed
#' @export
#' @importFrom BiocGenerics path
setMethod("path", "HDF5ColumnSeed", function(object) object@path)

#' @rdname HDF5ColumnSeed
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
