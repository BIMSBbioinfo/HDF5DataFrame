#' HDF5-backed DataFrame
#'
#' Create a HDF5-backed \linkS4class{DataFrame}, where the data are kept on disk until requested.
#' 
#' @param path String containing a path to a Parquet file.
#' @param columns Character vector containing the names of columns in a Parquet file.
#' If \code{NULL}, this is determined from \code{path}.
#' @param nrows Integer scalar specifying the number of rows in a Parquet file.
#' If \code{NULL}, this is determined from \code{path}.
#'
#' @return A ParquetDataFrame where each column is a \linkS4class{HDF5ColumnVector}.
#'
#' @details
#' The HDF5DataFrame is essentially just a \linkS4class{DataFrame} of \linkS4class{HDF5ColumnVector} objects.
#' It is primarily useful for indicating that the in-memory representation is consistent with the underlying Parquet file
#' (e.g., no delayed filter/mutate operations have been applied, no data has been added from other files).
#' Thus, users can specialize code paths for a HDF5DataFrame to operate directly on the underlying Parquet file.
#' 
#' In that vein, operations on a HDF5DataFrame may return another HDF5DataFrame if the operation does not introduce inconsistencies with the file-backed data.
#' For example, slicing or combining by column will return a HDF5DataFrame as the contents of the retained columns are unchanged.
#' In other cases, the HDF5DataFrame will collapse to a regular \linkS4class{DFrame} of \linkS4class{HDF5ColumnVector} objects before applying the operation;
#' these are still file-backed but lack the guarantee of file consistency.
#'
#' @author Aaron Lun
#' @examples
#' # Mocking up a file:
#' tf <- tempfile()
#' on.exit(unlink(tf))
#' arrow::write_parquet(mtcars, tf)
#'
#' # Creating our HDF5-backed data frame:
#' df <- HDF5DataFrame(tf)
#' df
#'
#' # Extraction yields a HDF5ColumnVector:
#' df$carb
#'
#' # Some operations preserve the HDF5DataFrame:
#' df[,1:5]
#' combined <- cbind(df, df)
#' class(combined)
#'
#' # ... but most operations collapse to a regular DFrame:
#' df[1:5,]
#' combined2 <- cbind(df, some_new_name=df[,1])
#' class(combined2)
#'
#' @aliases
#' HDF5DataFrame-class
#'
#' nrow,HDF5DataFrame-method
#' ncol,HDF5DataFrame-method
#' length,HDF5DataFrame-method
#' path,HDF5DataFrame-method
#'
#' rownames,HDF5DataFrame-method
#' names,HDF5DataFrame-method
#' rownames<-,HDF5DataFrame-method
#' names<-,HDF5DataFrame-method
#'
#' extractROWS,HDF5DataFrame,ANY-method
#' extractCOLS,HDF5DataFrame-method
#' [[,HDF5DataFrame-method
#'
#' replaceROWS,HDF5DataFrame-method
#' replaceCOLS,HDF5DataFrame-method
#' normalizeSingleBracketReplacementValue,HDF5DataFrame-method
#' [[<-,HDF5DataFrame-method
#'
#' cbind,HDF5DataFrame-method
#' cbind.HDF5DataFrame
#'
#' as.data.frame,HDF5DataFrame-method
#' coerce,HDF5DataFrame,DFrame-method
#'
#' @export
HDF5DataFrame <- function(tab, name, columns=NULL, nrows=NULL) {
    if (is.null(columns) || is.null(nrows)) {
        # tab <- acquireTable(path)
        if (is.null(columns)) {
            columns <- names(tab)
        }
        if (is.null(nrows)) {
            nrows <- length(tab[[1]])
        }
    } 
    path <- DelayedArray::path(tab[[1]])
    name <- dirname(tab[[1]]@seed@name)
    new("HDF5DataFrame", path=path, name = name, columns=columns, nrows=nrows)
}

#' @export
setClass("HDF5DataFrame", contains="DataFrame", slots=c(path="character", name = "character", columns="character", nrows="integer"))

#' @export
setMethod("nrow", "HDF5DataFrame", function(x) x@nrows)

#' @export
setMethod("length", "HDF5DataFrame", function(x) length(x@columns))

#' @export
setMethod("path", "HDF5DataFrame", function(object) object@path)

#' @export
setMethod("rownames", "HDF5DataFrame", function(x) NULL)

#' @export
setMethod("names", "HDF5DataFrame", function(x) x@columns)

#' @export
setReplaceMethod("rownames", "HDF5DataFrame", function(x, value) {
    if (!is.null(value)) {
        x <- .collapse_to_df(x)
        rownames(x) <- value
    }
    x
})

#' @export
setReplaceMethod("names", "HDF5DataFrame", function(x, value) {
    if (!identical(value, names(x))) {
        x <- .collapse_to_df(x)
        names(x) <- value
    }
    x
})

#' @export
#' @importFrom S4Vectors extractROWS
setMethod("extractROWS", "HDF5DataFrame", function(x, i) {
    if (!missing(i)) {
        collapsed <- .collapse_to_df(x)
        extractROWS(collapsed, i)
    } else {
        x
    }
})

#' @export
#' @importFrom stats setNames
#' @importFrom S4Vectors extractCOLS normalizeSingleBracketSubscript
setMethod("extractCOLS", "HDF5DataFrame", function(x, i) {
    if (!missing(i)) {
        xstub <- setNames(seq_along(x), names(x))
        i <- normalizeSingleBracketSubscript(i, xstub)
        x@columns <- x@columns[i]
        x@elementMetadata <- extractROWS(x@elementMetadata, i)
    }
    x
})

#' @export
#' @importFrom S4Vectors normalizeDoubleBracketSubscript
setMethod("[[", "HDF5DataFrame", function(x, i, j, ...) {
    if (!missing(j)) {
        stop("list-style indexing of a HDF5DataFrame with non-missing 'j' is not supported")
    }

    if (missing(i) || length(i) != 1L) {
        stop("expected a length-1 'i' for list-style indexing of a HDF5DataFrame")
    }

    i <- normalizeDoubleBracketSubscript(i, x)
    HDF5ColumnVector(x@path, column=x@columns[i], name = x@name)
})

#' @export
#' @importFrom S4Vectors replaceROWS
setMethod("replaceROWS", "HDF5DataFrame", function(x, i, value) {
    x <- .collapse_to_df(x)
    replaceROWS(x, i, value)
})

#' @export
#' @importFrom S4Vectors normalizeSingleBracketReplacementValue
setMethod("normalizeSingleBracketReplacementValue", "HDF5DataFrame", function(value, x) {
    if (is(value, "HDF5ColumnVector")) {
        return(new("HDF5DataFrame", path=value@seed@path, columns=value@seed@column, nrows=length(value)))
    }
    callNextMethod()
})

#' @export
#' @importFrom stats setNames
#' @importFrom S4Vectors replaceCOLS normalizeSingleBracketSubscript
setMethod("replaceCOLS", "HDF5DataFrame", function(x, i, value) {
    xstub <- setNames(seq_along(x), names(x))
    i2 <- normalizeSingleBracketSubscript(i, xstub, allow.NAs=TRUE)
    if (length(i2) == 1L && !is.na(i2)) {
        if (is(value, "HDF5DataFrame")) {
            if (x@path == value@path && identical(x@columns[i2], value@columns)) {
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

#' @export
#' @importFrom S4Vectors normalizeDoubleBracketSubscript
setMethod("[[<-", "HDF5DataFrame", function(x, i, j, ..., value) {
    i2 <- normalizeDoubleBracketSubscript(i, x, allow.nomatch=TRUE)
    if (length(i2) == 1L && !is.na(i2)) {
        if (is(value, "HDF5ColumnVector")) {
            if (x@path == value@seed@path && x@columns[i2] == value@seed@column) {
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
        if (is(obj, "HDF5DataFrame")) {
            if (is.null(xpath)) {
                xpath <- obj@path
            } else if (obj@path != xpath) {
                preserved <- FALSE
                break
            } 
            all_columns <- c(all_columns, obj@columns)

        } else if (is(obj, "HDF5ColumnVector")) {
            if (is.null(xpath)) {
                xpath <- obj@seed@path
            } else if (obj@seed@path != xpath || !identical(names(objects)[i], obj@seed@column)) {
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
            if (is(obj, "HDF5DataFrame")) {
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
            if (is(obj, "DataFrame")) {
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

        new("HDF5DataFrame", 
            path=xpath,
            columns=all_columns,
            nrows=NROW(objects[[1]]),
            elementMetadata=all_mcols,
            metadata=do.call(c, all_metadata)
        )
    }
}

#' @export
#' @importFrom S4Vectors bindCOLS
setMethod("cbind", "HDF5DataFrame", cbind.HDF5DataFrame)

#' @importFrom S4Vectors make_zero_col_DFrame mcols mcols<- metadata metadata<-
.collapse_to_df <- function(x) {
    df <- make_zero_col_DFrame(x@nrows)
    for (i in seq_along(x@columns)) {
        df[[as.character(i)]] <- HDF5ColumnVector(x@path, x@name, column=x@columns[i], length = x@nrows)
    }
    colnames(df) <- x@columns
    mcols(df) <- mcols(x, use.names=FALSE)
    metadata(df) <- metadata(x)
    df
}

#' @export
setMethod("as.data.frame", "HDF5DataFrame", function(x, row.names = NULL, optional = FALSE, ...) {
    tab <- acquireTable(x@path)

    ucol <- unique(x@columns)
    is.same <- identical(x@columns, ucol)
    tab <- tab[,ucol]

    output <- as.data.frame(tab, row.names=row.names, optional=optional, ...)
    output <- output[,match(x@columns,colnames(output)),drop=FALSE]

    output
})

#' @export
setAs("HDF5DataFrame", "DFrame", function(from) .collapse_to_df(from))

