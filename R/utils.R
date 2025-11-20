#' @noRd
.isTRUEorFALSE <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}