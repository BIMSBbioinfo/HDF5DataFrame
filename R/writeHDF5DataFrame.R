#' writeHDF5DataFrame
#' 
#' A function for writing an data frames to an HDF5 file.
#'
#' @param x data.frame
#' @param filepath NULL or the path (as a single string) to the 
#' (new or existing) HDF5 file where to write the dataset. 
#' See \link[HDF5Array]{writeHDF5Array}
#' @param name NULL or the name of the HDF5 group to write columns of the 
#' dataset.
#' @param replace replace
#'
#' @importFrom rhdf5 h5createFile h5createGroup
#' 
#' @export
#'
#' @examples
writeHDF5DataFrame <- function(x, 
                               filepath, 
                               name, 
                               replace){
  
  # create or replace output folder
  if (!.isTRUEorFALSE(replace)) {
    stop("'replace' must be TRUE or FALSE")
  }
  if (replace) {
    if (file.exists(filepath)) {
      file.remove(filepath)
    }
  }
  
  # create HDF5 file
  if (!file.exists(filepath)) {
    rhdf5::h5createFile(filepath)
  }
  
  # create group if needed
  if(!name %in% c("", "/"))
    rhdf5::h5createGroup(filepath, group = name)
  
  # write data frame
  meta.data_list <- list()
  columns <- colnames(x)
  for(i in 1:ncol(x)){
    cur_column <- as.vector(subset(x, select = colnames(x)[i]))[[1]]
    if(is.character(cur_column) || is.factor(cur_column))
      cur_column <- as.character(cur_column)
    cur_column <- as.array(cur_column)
    meta.data_list[[colnames(x)[i]]] <- 
      writeHDF5Array(cur_column, 
                     filepath, 
                     name = file.path(name, colnames(x)[i]), 
                     with.dimnames = FALSE)
  }
  
  # return data frame
  # HDF5DataFrame(meta.data_list, 
  #               name = name, 
  #               columns = names(meta.data_list))
  HDF5DataFrame2(filepath, name, columns = columns)
}