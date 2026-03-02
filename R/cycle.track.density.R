#' This function calculates the cycle track kilometers per square kilometer of
#' urban area.
#'
#' METHOD:
#' The cycling infrastructure coverage (cycle.dens) is calculated by adding up
#' the
#' length of each cycle path (c.lengthm) in the urban area and dividing this
#' total (c.length) by
#' the urban area’s footprint (u_footprint).
#'
#' @param cycle  shapefile that contains cycle tracks
#' @param footprint raster that contains the urban footprint in the base year
#' @return a data frame with information about the total km of cycle tracks per
#' square kilometer
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(cycle.cun)
#'
#' footprint.b <- system.file("extdata", "Build_up_2025.tif",
#'   package =
#'     "urbanperformance"
#' )
#' footprint.b <- raster::raster(footprint.b)
#'
#' cycle.dens <- cycle_track_density(cycle.cun, footprint.b)
cycle_track_density <- function(cycle, footprint) {
  c <- cycle
  c$lengthm <- sf::st_length(c)
  c_length <- round(as.numeric(sum(c$lengthm)) / 1e3, 2)

  u_footprint <- urban_footprint(footprint)$value
  cycle_dens <- data.frame(
    indicator = "Cycle density",
    fclass = "cycle density",
    value = round(c_length / as.numeric(u_footprint), 2),
    units = "km/km2"
  )
  cycle_dens
}
