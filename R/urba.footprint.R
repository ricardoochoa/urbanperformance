#' This function calculates the urban footprint for a specific scenario/year
#'
#' METHOD:
#' Urban footprint (urban_footprint) of the urban area in a specific year or
#' scenario is calculated
#' by adding up the area of each pixel with buildup (r), then divide it by 1e6
#' to convert the value to square kilometer.
#'
#' @param ras one raster that contains buildup information
#' @return a data frame with information about the urban footprint area in
#' square kilometers
#' @export
#' @examples
#' library(raster)
#'
#' footprint_b <- system.file("extdata", "Build_up_2025.tif",
#'   package = "urbanperformance"
#' )
#' footprint_b <- raster::raster(footprint_b)
#'
#' u_footprint25 <- urban_footprint(footprint_b)
urban_footprint <- function(ras) {
  ras <- raster::stack(ras)
  x <- lapply(1:raster::nlayers(ras), function(i) {
    r <- ras[[i]]
    r[r != 0] <- 1
    r <- raster::projectRaster(r, crs = sp::CRS("+init=EPSG:3857"))
    t <- data.frame(
      indicator = "Urban footprint",
      fclass = names(r),
      value = paste(
        round(((raster::cellStats(r, sum) *
          (raster::res(r)[1] * raster::res(r)[2])) / 1e6), 2)
      ),
      units = "km2"
    )
    t
  }) |> dplyr::bind_rows()
  x
}
