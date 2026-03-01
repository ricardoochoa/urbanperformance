#' This function calculates the roads kilometers per square kilometer of urban area.
#'
#' METHOD:
#' The road density in the urban area (roads.dens) is calculated by adding up the
#' length of each road (r.lengthm) in the urban area and dividing this total (r.length) by
#' the urban area’s footprint (u.footprint).
#'
#' @param roads  shapefile that contains  roads
#' @param footprint raster that contains the urban footprint in the base year
#' @return a data frame with information about the total km of roads per square kilometer
#' @importFrom sf st_length
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(roads.cun)
#'
#' footprint.b <- system.file("extdata", "Build_up_2025.tif", package = "UPtooltest")
#' footprint.b <- raster::raster(footprint.b)
#'
#' roads.d <- roads.density(roads.cun, footprint.b)
roads.density <- function(roads, footprint){
  r <- roads
  r$lengthm <- st_length(r)
  r.length <- round(as.numeric(sum(r$lengthm))/1e3,2)

  u.footprint <- urban.footprint(footprint)$value
  roads.dens <- data.frame(indicator = "Roads density",
                           fclass = "road density",
                           value = round(r.length/as.numeric(u.footprint),2),
                           units = "km/km2")
  return(roads.dens)

}
