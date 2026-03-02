#' This function change the values in the raster to convert values higher than 0
#' into 1 and NA values into 0
#'
#' @param r a raster layer with values >= 0
#' @return a binary raster with values 1 and 0
#' @export
one.cero <- function(r) {
  r[r > 0] <- 1
  r[is.na(r)] <- 0
  return(r)
}
