#' @noRd
.isTRUEorFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
h5lsgroup <- function(filepath, name) {
  
  # list all objects
  all_objs <- rhdf5::h5ls(filepath, recursive = TRUE)
  
  # adjust name
  if(!grepl("^/", name))
    name <- gsub(name, pattern="^", replacement="/", fixed=TRUE)
  
  # check groups
  groups <- unique(all_objs$group)
  if(!any(grepl(name, groups)))
    stop("Group '", name, "' not found in HDF5 file '", filepath, "'.")
  
  # subset group
  all_objs_group <- all_objs[grepl(name, all_objs$group), ]

  # return
  return(all_objs_group)
}

.check_dataframe_dim <- function(filepath, name){
  group_metadata <- h5lsgroup(filepath, name)
  dim_ds <- lapply(1:nrow(group_metadata), function(i){
    dim(HDF5Array(filepath = filepath, 
                  name = file.path(name, group_metadata$name[i])))
  })
  if(length(unique(dim_ds)) > 1 || all(lapply(dim_ds, length) > 1))
    stop("Inconsistent dimensions among columns in HDF5DataFrame at '", 
         name, "' in file '", filepath, "'.")
}