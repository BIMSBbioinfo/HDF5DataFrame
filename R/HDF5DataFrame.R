#' HDF5-backed DataFrame
#'
#' Create a HDF5-backed \link[S4Vectors]{DataFrame}, where the data are 
#' kept on disk until requested.
#' 
#' @param x,object A set of HDF5Arrays that are the columns of the 
#' HDF5DataFrame object.
#' @param i Depends on the usage
#' @param j Depends on the usage
#' @param ... arguments passed to other methods
#' @param name String containing the HDF5 group of the h5 file.
#' @param columns Character vector containing the names of columns in a  
#' HDF5-based data frame. If \code{NULL}, this is determined from \code{path}.
#' @param nrows Integer scalar specifying the number of rows in a  HDF5-based 
#' data frame. If \code{NULL}, this is determined from \code{path}.
#' @param row.names,optional See ?base::\link[base]{as.data.frame} for a 
#' description of these arguments.
#' @param deparse.level See ?base::\link[base]{cbind} for a description of
#' description of these arguments.
#' @param value rownames, names or new columns for 
#' \link[HDF5DataFrame]{HDF5DataFrame} object
#'
#' @importFrom methods new as is callNextMethod
#' @importFrom DelayedArray path
#' @return A HDF5DataFrame object where each column is a 
#' \link[HDF5DataFrame]{HDF5ColumnVector}.
#'
#' @author Artür Manukyan
#'
#' @name HDF5DataFrame
#' 
#' @aliases 
#' as.data.frame,HDF5DataFrame-method
#' length,HDF5DataFrame-method
#' path,HDF5DataFrame-method
#' cbind,HDF5DataFrame-method
#' 
#' rownames,HDF5DataFrame-method
#' rownames<-,HDF5DataFrame-method
#' 
#' names,HDF5DataFrame-method
#' names<-,HDF5DataFrame-method
#' 
#' [[,HDF5DataFrame-method
#' [[<-,HDF5DataFrame-method
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
#' h5createGroup(output_h5ad, group = "metadata")
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
#'                    name = paste0("metadata", "/", 
#'                                  colnames(metadata)[i]), 
#'                    with.dimnames = FALSE)
#' }
#' metadata_large <- 
#'   HDF5DataFrame(meta.data_list,
#'                 name = "metadata", 
#'                 columns = names(meta.data_list))
#' 
#' # coerce to data.frame
#' metadata_large <- as.data.frame(metadata_large)
#' 
#' # cbind
#' metadata_large <- cbind(metadata_large, metadata)
#' 
#' @export
#' @return HDF5DataFrame object
HDF5DataFrame <- function(x, name, columns=NULL, nrows=NULL) {
    if (is.null(columns) || is.null(nrows)) {
        if (is.null(columns)) {
            columns <- names(x)
        }
        if (is.null(nrows)) {
            nrows <- length(x[[1]])
        }
    } 
    path <- DelayedArray::path(x[[1]])
    name <- dirname(x[[1]]@seed@name)
    methods::new("HDF5DataFrame", 
                 path=path, 
                 name = name, 
                 columns=columns, 
                 nrows=nrows)
}

.DollarNames.HDF5DataFrame <- function(x, pattern = "")
  grep(pattern, x@columns, value=TRUE)

#' @rdname HDF5DataFrame
#' @export
#' @return number of rows of HDF5DataFrame object
setMethod("nrow", "HDF5DataFrame", function(x) x@nrows)

#' @rdname HDF5DataFrame
#' @export
#' @return length of HDF5DataFrame object
setMethod("length", "HDF5DataFrame", function(x) length(x@columns))

#' @rdname HDF5DataFrame
#' @export
#' @return path to hdf5 file of HDF5DataFrame object
setMethod("path", "HDF5DataFrame", function(object) object@path)

#' @rdname HDF5DataFrame
#' @export
#' @return rownames of HDF5DataFrame object
setMethod("rownames", "HDF5DataFrame", function(x) NULL)

#' @rdname HDF5DataFrame
#' @export
#' @return names of columns of HDF5DataFrame object
setMethod("names", "HDF5DataFrame", function(x) x@columns)

#' @rdname HDF5DataFrame
#' @export
setReplaceMethod("rownames", "HDF5DataFrame", function(x, value) {
    if (!is.null(value)) {
        x <- .collapse_to_df(x)
        rownames(x) <- value
    }
    x
})

#' @rdname HDF5DataFrame
#' @export
setReplaceMethod("names", "HDF5DataFrame", function(x, value) {
    if (!identical(value, names(x))) {
        x <- .collapse_to_df(x)
        names(x) <- value
    }
    x
})

#' subsetting-utils 
#' 
#' Low-level utility functions and classes to support subsetting of 
#' vector-like objects. They are not intended to be used directly. 
#' See \link[S4Vectors]{extractROWS}.
#' 
#' @name subsetting-utils
#'
#' @param x HDF5DataFrame object
#' @param i row/column index or name
#' @param value vector to be replaced
#' @aliases 
#' extractROWS,HDF5DataFrame,ANY-method
#' extractCOLS,HDF5DataFrame,ANY-method
#' replaceROWS,HDF5DataFrame,ANY-method
#' replaceCOLS,HDF5DataFrame,ANY-method

#' @rdname subsetting-utils
#' @export
#' @importFrom S4Vectors extractROWS
#' @return HDF5DataFrame object
setMethod("extractROWS", "HDF5DataFrame", function(x, i) {
    if (!missing(i)) {
        collapsed <- .collapse_to_df(x)
        extractROWS(collapsed, i)
    } else {
        x
    }
})

#' @rdname subsetting-utils
#' @export
#' @importFrom stats setNames
#' @importFrom S4Vectors extractCOLS normalizeSingleBracketSubscript
#' @return HDF5DataFrame object
setMethod("extractCOLS", "HDF5DataFrame", function(x, i) {
    if (!missing(i)) {
        xstub <- setNames(seq_along(x), names(x))
        i <- normalizeSingleBracketSubscript(i, xstub)
        x@columns <- x@columns[i]
        x@elementMetadata <- extractROWS(x@elementMetadata, i)
    }
    x
})

#' @rdname HDF5DataFrame
#' @export
#' @importFrom S4Vectors normalizeDoubleBracketSubscript
setMethod("[[", "HDF5DataFrame", function(x, i, j, ...) {
    if (!missing(j)) {
        stop("list-style indexing of a HDF5DataFrame", 
             " with non-missing 'j' is not supported")
    }

    if (missing(i) || length(i) != 1L) {
        stop("expected a length-1 'i' for list-style ", 
             "indexing of a HDF5DataFrame")
    }

    i <- normalizeDoubleBracketSubscript(i, x)
    HDF5ColumnVector(x@path, column=x@columns[i], name = x@name)
})

#' @rdname subsetting-utils
#' @export
#' @importFrom S4Vectors replaceROWS
setMethod("replaceROWS", "HDF5DataFrame", function(x, i, value) {
    x <- .collapse_to_df(x)
    replaceROWS(x, i, value)
})

#' @rdname subsetting-utils
#' @export
#' @importFrom stats setNames
#' @importFrom S4Vectors replaceCOLS normalizeSingleBracketSubscript
setMethod("replaceCOLS", "HDF5DataFrame", function(x, i, value) {
    xstub <- setNames(seq_along(x), names(x))
    i2 <- normalizeSingleBracketSubscript(i, xstub, allow.NAs=TRUE)
    if (length(i2) == 1L && !is.na(i2)) {
        if (methods::is(value, "HDF5DataFrame")) {
            if (x@path == value@path && 
                identical(x@columns[i2], value@columns)) {
                return(x)
            }
        }
    }

    # In theory, it is tempting to return a HDF5DataFrame; the problem is
    # that assignment will change the mapping of column names to their
    # contents, so it is no longer a pure representation of a HDF5DataFrame.
    x <- .collapse_to_df(x)
    replaceCOLS(x, i, value)
})

#' @rdname HDF5DataFrame
#' @importFrom S4Vectors normalizeDoubleBracketSubscript
setMethod("[[<-", "HDF5DataFrame", function(x, i, j, ..., value) {
    i2 <- normalizeDoubleBracketSubscript(i, x, allow.nomatch=TRUE)
    if (length(i2) == 1L && !is.na(i2)) {
        if (methods::is(value, "HDF5ColumnVector")) {
            if (x@path == value@seed@path && 
                x@columns[i2] == value@seed@column) {
                return(x)
            }
        }
    }

    x <- .collapse_to_df(x)
    x[[i]] <- value
    x
})

#' @export
#' @importFrom S4Vectors mcols make_zero_col_DFrame combineRows
cbind.HDF5DataFrame <- function(..., deparse.level=1) {
    preserved <- TRUE
    all_columns <- character(0)
    objects <- list(...)
    xpath <- NULL

    for (i in seq_along(objects)) {
        obj <- objects[[i]]
        if (methods::is(obj, "HDF5DataFrame")) {
            if (is.null(xpath)) {
                xpath <- obj@path
            } else if (obj@path != xpath) {
                preserved <- FALSE
                break
            } 
            all_columns <- c(all_columns, obj@columns)

        } else if (methods::is(obj, "HDF5ColumnVector")) {
            if (is.null(xpath)) {
                xpath <- obj@seed@path
            } else if (obj@seed@path != xpath || 
                       !identical(names(objects)[i], 
                                  obj@seed@column)) {
                preserved <- FALSE
                break
            } 
            all_columns <- c(all_columns, obj@seed@column)

        } else {
            preserved <- FALSE
            break
        }
    }

    if (!preserved) {
        for (i in seq_along(objects)) {
            obj <- objects[[i]]
            print(obj)
            if (methods::is(obj, "HDF5DataFrame")) {
                objects[[i]] <- .collapse_to_df(obj)
            }
        }
        do.call(cbind, objects)

    } else {
        all_mcols <- list()
        has_mcols <- FALSE
        all_metadata <- list()

        for (i in seq_along(objects)) {
            obj <- objects[[i]]

            mc <- NULL
            md <- list()
            if (methods::is(obj, "DataFrame")) {
                mc <- mcols(obj, use.names=FALSE)
                md <- metadata(obj)
                if (is.null(mc)) {
                    mc <- make_zero_col_DFrame(length(obj))
                } else {
                    has_mcols <- TRUE
                }
            } else {
                mc <- make_zero_col_DFrame(1)
            }

            all_mcols[[i]] <- mc
            all_metadata[[i]] <- md
        }

        if (has_mcols) {
            all_mcols <- do.call(combineRows, all_mcols)
        } else {
            all_mcols <- NULL
        }

        # make column un
        methods::new("HDF5DataFrame", 
            path=xpath,
            columns=all_columns,
            nrows=NROW(objects[[1]]),
            elementMetadata=all_mcols,
            metadata=do.call(c, all_metadata)
        )
    }
}

#' @rdname HDF5DataFrame
#' @export
#' @importFrom S4Vectors bindCOLS
#' @return HDF5DataFrame object
setMethod("cbind", "HDF5DataFrame", cbind.HDF5DataFrame)

#' @importFrom S4Vectors make_zero_col_DFrame 
#' @importFrom S4Vectors mcols mcols<- metadata metadata<-
.collapse_to_df <- function(x) {
    df <- make_zero_col_DFrame(x@nrows)
    for (i in seq_along(x@columns)) {
        df[[as.character(i)]] <- 
          HDF5ColumnVector(x@path, 
                           x@name, 
                           column=x@columns[i], 
                           length = x@nrows)
    }
    colnames(df) <- x@columns
    mcols(df) <- mcols(x, use.names=FALSE)
    metadata(df) <- metadata(x)
    df
}

#' @rdname HDF5DataFrame
#' @importFrom h5mread h5mread
#' @export
#' @return data.frame object
setMethod("as.data.frame", 
          "HDF5DataFrame", 
          function(x, row.names = NULL, optional = FALSE, ...) {
  df <- make_zero_col_DFrame(x@nrows)
  for (i in seq_along(x@columns)) {
    df[[as.character(i)]] <- 
      h5mread::h5mread(filepath = x@path, 
                       name = paste0(x@name, "/", x@columns[i]))
  }
  colnames(df) <- x@columns
  mcols(df) <- mcols(x, use.names=FALSE)
  metadata(df) <- metadata(x)
  as.data.frame(df)
})

#' @export
setAs("HDF5DataFrame", "DFrame", function(from) .collapse_to_df(from))