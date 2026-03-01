#' This function calculates the cycle track kilometers per square kilometer of
#' urban area.
#'
#' METHOD:
#' The cycling infrastructure coverage (cycle.dens) is calculated by adding up
#' the
#' length of each cycle path (c.lengthm) in the urban area and dividing this
#' total (c.length) by
#' the urban area’s footprint (u.footprint).
#'
#' @param cycle  shapefile that contains cycle tracks
#' @param footprint raster that contains the urban footprint in the base year
#' @return a data frame with information about the total km of cycle tracks per
#' square kilometer
#' @import raster
#' @import dplyr
#' @import sf
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(cycle.cun)
#'
#' footprint.b <- system.file("extdata", "Build_up_2025.tif",
#'   package =
#'     "UPtooltest"
#' )
#' footprint.b <- raster::raster(footprint.b)
#'
#' cycle.dens <- cycle_track_density(cycle.cun, footprint.b)
cycle_track_density <- function(cycle, footprint) {
  c <- cycle
  c$lengthm <- st_length(c)
  c.length <- round(as.numeric(sum(c$lengthm)) / 1e3, 2)

  u.footprint <- urban.footprint(footprint)$value
  cycle.dens <- data.frame(
    indicator = "Cycle density",
    fclass = "cycle density",
    value = round(c.length / as.numeric(u.footprint), 2),
    units = "km/km2"
  )
  return(cycle.dens)
}
