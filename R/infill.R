#' This function calculates the amount of land that was located within the
#' boundaries
#' of the urban area in the base scenario and that at least doubled its
#' population
#' between the base year and the horizon year. It is also known as
#' intensification area.
#'
#' METHOD:
#' The infill area (infill.a) is calculated as the sum of the area (area.in) of
#' all analysis pixels that
#' had population since the base scenario (base > 0) and their population in the
#' scenario of analysis for the horizon year (p.scenario)
#' is at least twice the population in the base scenario (p.base).
#' Vacant lands (p.base = 0) in which population will settle in the future are
#' not considered in the
#' calculation of the infill area, since they will use infrastructure networks
#' within
#' the capacity with which they were first conceived.
#'
#' @param p.base  raster that contains population distribution in the base year
#' @param p.scenario raster that contains population distribution in the horizon
#' year
#' @return a data frame with information about the infill areas in square
#' kilometers
#' @export
#' @examples
#' library(raster)
#'
#' pop2025 <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop2025 <- raster::raster(pop2025)
#' pop2030 <- system.file("extdata", "POP_2030.tif", package = "UPtooltest")
#' pop2030 <- raster::raster(pop2030)
#'
#' infill.area <- infill(pop2025, pop2030)
infill <- function(p.base, p.scenario) {
  p.scenario <- rasterchecker(p.scenario, base = p.base)
  base <- p.base
  base[base <= 0] <- NA
  ue <- p.scenario / base
  ue[ue < 2] <- NA
  area.in <- projectRaster(ue, crs = sp::CRS("+init=EPSG:3857"))
  area.in <- (cellStats(ue, sum) * (res(area.in)[1] * res(area.in)[2])) / 1e6
  assign("infill_area", ue, envir = .GlobalEnv)
  infill.a <- data.frame(
    indicator = "Infill",
    fclass = "infill",
    value = round(area.in, 2),
    units = "km2"
  )
  return(infill.a)
}
