#' This function calculates the total and the percentage of the population that
#' lives within a maximum recommended distance from a cycle track
#'
#' METHOD:
#' Cycle proximity (cycle_proximity) is calculated by dividing the population
#' (pop_prox_cycle) that
#' lives within the service radio of cycling infrastructure by the total
#' population (pop).
#' First, a raster distance is calculated from the cycle tracks buffer. Next, a
#' selection of pixels
#' that contain values lower or equal to the maximum recommended distance from a
#' cycle track. General maximum recommended
#' distance for cycle track can be found in the ‘p.distances’ table of this
#' package.
#' Next, the population (pop) of all the pixels contained in the raster
#' selection is added up to obtain the population that
#' has access to the cycle track (pop_prox_cycle). Finally, this population is
#' divided by the total population of the
#' urban area (pop) to obtain the percentage of the population that lives within
#' the recommended distance for that
#' type of amenity (c.proximity).
#' @param cycle a shapefile that contains the lines or tracks of cycle
#' infrastructure. The shapefile must contain the "fclass" column.
#' @param pop raster with information about population distribution
#' @param parameters base variable is null, but the indicator can use the
#' p.distances tables that contains information about the maximum distance
#' recommended for each
#' fclass to calculate the proximities
#' @param save select TRUE for saving distance raster or FALSE to skip this
#' process
#' @return a data frame with the results of the indicator with
#' the total and percentage population with proximity to cycle tracks
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(cycle.cun)
#'
#' pop_base <- system.file("extdata", "POP_2025.tif",
#'   package = "urbanperformance"
#' )
#' pop_base <- raster::raster(pop_base)
#'
#' cycle_prox <- cycle_proximity(cycle.cun, pop = pop_base)
cycle_proximity <- function(cycle, pop, parameters = NULL, save = TRUE) {
  if (is.null(parameters)) {
    p <- p.distances
  } else {
    p <- parameters
  }
  category <- unique(stats::na.omit(cycle$fclass))
  param <- p[p$fclass == category, ]
  param <- as.numeric(param$value)

  cycle_r <- raster::rasterize(cycle, pop)
  cycle_d <- raster::distance(cycle_r)
  cycle_d <- raster::resample(cycle_d, pop, method = "ngb")

  cycle_reclass <- cycle_d
  cycle_reclass[cycle_reclass <= param] <- 1
  cycle_reclass[cycle_reclass > param] <- 0
  cycle_reclass[is.na(cycle_reclass)] <- 0

  pop_prox_cycle <- cycle_reclass * pop

  if (save == TRUE) {
    warning("The 'save' argument is deprecated. Rasters are no longer automatically saved to the global environment to comply with CRAN policies.")
  }

  c_proximity <- data.frame(
    indicator = "Cycle proximity",
    fclass = "cycle",
    value = c(
      round(raster::cellStats(pop_prox_cycle, sum, na.rm = TRUE), 0),
      round(
        (raster::cellStats(pop_prox_cycle, sum, na.rm = TRUE) /
          raster::cellStats(pop, sum, na.rm = TRUE)) * 100, 2
      )
    ),
    units = c("inhabitants", "%")
  )
  c_proximity
}
