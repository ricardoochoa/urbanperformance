#' This function calculates the urban footprint for a specific scenario/year
#'
#' METHOD:
#' Urban footprint (urban.footprint) of the urban area in a specific year or
#' scenario is calculated
#' by adding up the area of each pixel with buildup (r), then divide it by 1e6
#' to convert the value to square kilometer.
#'
#' @param ras one raster that contains buildup information
#' @return a data frame with information about the urban footprint area in
#' square kilometers
#' @import raster
#' @export
#' @examples
#' library(raster)
#'
#' footprint.b <- system.file("extdata", "Build_up_2025.tif",
#'   package =
#'     "UPtooltest"
#' )
#' footprint.b <- raster::raster(footprint.b)
#'
#' u.footprint25 <- urban.footprint(footprint.b)
urban.footprint <- function(ras) {
  ras <- stack(ras)
  x <- lapply(1:nlayers(ras), function(i) {
    r <- ras[[i]]
    r[r != 0] <- 1
    r <- projectRaster(r, crs = sp::CRS("+init=EPSG:3857"))
    t <- data.frame(
      indicator = "Urban footprint",
      fclass = names(r),
      value = paste(round(((cellStats(r, sum) * (res(r)[1] * res(r)[2])) / 1e6), 2)),
      units = "km2"
    )
  }) %>% bind_rows()
  return(x)
}
