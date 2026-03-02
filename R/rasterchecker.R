#' This function read and resample the rasters and convert all the raster data
#' into a stack
#'
#' @param ... one or more than two rasters or a raster stack.
#' @param base base layer that contains the extent and resolution desired.
#' @return A standardize raster, or rasters with the same extent and resolution,
#' and projection
#' @export
#' @examples
#' library(raster)
#' @examples
#' library(raster)
#'
#' pop.b <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.b <- raster::raster(pop.b)
#'
#' pop.2030 <- system.file("extdata", "POP_2030.tif", package = "UPtooltest")
#' pop.2030 <- raster::raster(pop.2030)
#'
#' pop.corrected <- rasterchecker(pop.2030, base = pop.b)
rasterchecker <- function(..., base) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]])) {
    r <- x[[1]]
  } else {
    r <- x
  }
  y <- base # r[[1]]
  adjust <- function(p) {
    if (!compareCRS(y, p)) {
      p <- projectRaster(p, y)
    }
    if (!extent(p) == extent(y) ||
      res(p)[1] != res(y)[1] || res(p)[2] != res(y)[2]) {
      p <- resample(p, y, method = "ngb")
    }
    return(p)
  }
  s <- lapply(r, adjust)
  f <- stack(s)
  return(f)
}
