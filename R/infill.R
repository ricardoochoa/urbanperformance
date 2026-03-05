#' This function calculates the amount of land that was located within the
#' boundaries
#' of the urban area in the base scenario and that at least doubled its
#' population
#' between the base year and the horizon year. It is also known as
#' intensification area.
#'
#' METHOD:
#' The infill area (infill_a) is calculated as the sum of the area (area_in) of
#' all analysis pixels that
#' had population since the base scenario (base > 0) and their population in the
#' scenario of analysis for the horizon year (p_scenario)
#' is at least twice the population in the base scenario (p_base).
#' Vacant lands (p_base = 0) in which population will settle in the future are
#' not considered in the
#' calculation of the infill area, since they will use infrastructure networks
#' within
#' the capacity with which they were first conceived.
#'
#' @param p_base  raster that contains population distribution in the base year
#' @param p_scenario raster that contains population distribution in the horizon
#' year
#' @return a data frame with information about the infill areas in square
#' kilometers
#' @export
#' @examples
#' library(raster)
#'
#' pop2025 <- system.file("extdata", "POP_2025.tif",
#'   package = "urbanperformance"
#' )
#' pop2025 <- raster::raster(pop2025)
#' pop2030 <- system.file("extdata", "POP_2030.tif",
#'   package = "urbanperformance"
#' )
#' pop2030 <- raster::raster(pop2030)
#'
#' infill.area <- infill(pop2025, pop2030)
infill <- function(p_base, p_scenario) {
  p_scenario <- rasterchecker(p_scenario,
    base = p_base
  )
  base <- p_base
  base[base <= 0] <- NA
  ue <- p_scenario / base
  ue[ue < 2] <- NA
  area_in <- raster::projectRaster(ue, crs = sp::CRS("+init=EPSG:3857"))
  area_in <- (raster::cellStats(ue, sum) * (raster::res(area_in)[1] *

    raster::res(area_in)[2])) / 1e6

  infill_a <- data.frame(
    indicator = "Infill",
    fclass = "infill",
    value = round(area_in, 2),
    units = "km2"
  )
  infill_a
}
