% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HDF5DataFrame.R
\name{HDF5DataFrame}
\alias{HDF5DataFrame}
\alias{as.data.frame,HDF5DataFrame-method}
\alias{length,HDF5DataFrame-method}
\alias{path,HDF5DataFrame-method}
\alias{cbind,HDF5DataFrame-method}
\alias{rownames,HDF5DataFrame-method}
\alias{rownames<-,HDF5DataFrame-method}
\alias{names,HDF5DataFrame-method}
\alias{names<-,HDF5DataFrame-method}
\alias{[[,HDF5DataFrame-method}
\alias{[[<-,HDF5DataFrame-method}
\alias{nrow,HDF5DataFrame-method}
\title{HDF5-backed DataFrame}
\usage{
HDF5DataFrame(x, name, columns = NULL, nrows = NULL)

\S4method{nrow}{HDF5DataFrame}(x)

\S4method{length}{HDF5DataFrame}(x)

\S4method{path}{HDF5DataFrame}(object)

\S4method{rownames}{HDF5DataFrame}(x)

\S4method{names}{HDF5DataFrame}(x)

\S4method{rownames}{HDF5DataFrame}(x) <- value
}
\arguments{
\item{x, object}{A set of HDF5Arrays that are the columns of the 
HDF5DataFrame object.}

\item{name}{String containing the HDF5 group of the h5 file.}

\item{columns}{Character vector containing the names of columns in a
HDF5-based data frame. If \code{NULL}, this is determined from \code{path}.}

\item{nrows}{Integer scalar specifying the number of rows in a  HDF5-based 
data frame. If \code{NULL}, this is determined from \code{path}.}

\item{value}{rownames, names or new columns for 
\link[HDF5DataFrame]{HDF5DataFrame} object}
}
\value{
A HDF5DataFrame object where each column is a 
\link[HDF5DataFrame]{HDF5ColumnVector}.
}
\description{
Create a HDF5-backed \link[S4Vectors]{DataFrame}, where the data are 
kept on disk until requested.
}
\examples{
# libraries
library(rhdf5)
library(HDF5Array)
library(HDF5DataFrame)

# h5
output_h5ad <- tempfile(fileext = ".h5")
h5createFile(output_h5ad)
h5createGroup(output_h5ad, group = "assay")

# data
data("chickwts")
metadata <- chickwts

# set metadata
meta.data_list <- list()
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

# coerce to data.frame
metadata_large <- as.data.frame(metadata_large)

# cbind
metadata_large <- cbind(metadata_large, metadata)

}
\author{
Artür Manukyan
}
