#' This function calculates the roads kilometers per square kilometer of urban
#' area.
#'
#' METHOD:
#' The road density in the urban area (roads.dens) is calculated by adding up
#' the
#' length of each road (r.lengthm) in the urban area and dividing this total
#' (r.length) by
#' the urban area’s footprint (u_footprint).
#'
#' @param roads  shapefile that contains  roads
#' @param footprint raster that contains the urban footprint in the base year
#' @return a data frame with information about the total km of roads per square
#' kilometer
#' @importFrom sf st_length
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(roads.cun)
#'
#' footprint_b <- system.file("extdata", "Build_up_2025.tif",
#'   package = "urbanperformance"
#' )
#' footprint_b <- raster::raster(footprint_b)
#'
#' roads_d <- roads_density(roads.cun, footprint_b)
roads_density <- function(roads, footprint) {
  r <- roads
  r$lengthm <- sf::st_length(r)
  r_length <- round(as.numeric(sum(r$lengthm)) / 1e3, 2)

  u_footprint <- urban_footprint(footprint)$value
  roads_dens <- data.frame(
    indicator = "Roads density",
    fclass = "road density",
    value = round(r_length / as.numeric(u_footprint), 2),
    units = "km/km2"
  )
  roads_dens
}
