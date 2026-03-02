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
#' pop_b <- system.file("extdata", "POP_2025.tif", package = "urbanperformance")
#' pop_b <- raster::raster(pop_b)
#'
#' pop_2030 <- system.file("extdata", "POP_2030.tif",
#'   package = "urbanperformance"
#' )
#' pop_2030 <- raster::raster(pop_2030)
#'
#' pop_corrected <- rasterchecker(pop_2030, base = pop_b)
rasterchecker <- function(..., base) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]])) {
    r <- x[[1]]
  } else {
    r <- x
  }
  y <- base
  adjust <- function(p) {
    if (!raster::compareCRS(y, p)) {
      p <- raster::projectRaster(p, y)
    }
    if (!raster::extent(p) == raster::extent(y) ||
      raster::res(p)[1] != raster::res(y)[1] ||
      raster::res(p)[2] != raster::res(y)[2]) {
      p <- raster::resample(p, y, method = "ngb")
    }
    p
  }
  s <- lapply(r, adjust)
  f <- raster::stack(s)
  f
}
