#' fill rows with sampled values (upsampling)
#' If the data frame has more than n rows, then sample through the data frame without replacement
#' If 
sample_n_if_needed <- function(tbl, size, fill) {
  if (nrow(tbl) >= size) {
    res <- dplyr::sample_n(tbl, size, replace = FALSE)
    return(res)
  }
  fill <- dplyr::select(fill, tidyselect::all_of(names(tbl)))
  res <- rbind(
    dplyr::sample_n(fill, size - nrow(tbl), replace = TRUE),
    tbl
  )
  dplyr::sample_n(res, size, replace = FALSE)
}
