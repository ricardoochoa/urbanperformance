#' This function calculates the total green area within the urban area
#' (forests, parks, gardens, etc.) per inhabitant.
#'
#' METHOD:
#' The park and public space area available per person (green_area.p) is
#' estimated
#' by adding up the area with vegetation (green_area), in square meters, in the
#' parks
#' and public open spaces of the urban area, and dividing this result
#' (green_area.t) by the total population (t.pop).
#'
#' @param ga  raster layer or shapefile that contains the polygons for green
#' areas
#' @param pop raster of population distribution or numeric value with
#' information of total population
#' @return a data frame with information about the infill areas in square
#' kilometers
#' @export
#' @examples
#' library(raster)
#'
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package =
#'     "urbanperformance"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' pop_base <- system.file("extdata", "POP_2025.tif",
#'   package = "urbanperformance"
#' )
#' pop_base <- raster::raster(pop_base)
#'
#' green.land <- land.cover2025
#' green.land[green.land != 8] <- 0
#' green_a_capita <- green_area_pcapita(green.land, pop_base)
green_area_pcapita <- function(ga, pop) {
  if (is.numeric(pop) == TRUE) {
    t_pop <- pop
  } else {
    if (inherits(pop, "RasterLayer")) {
      t_pop <- as.numeric(tot_pop(pop)$value)
    }
  }

  if (inherits(ga, "RasterLayer")) {
    green_area <- one_cero(ga)
    green_area_r <- raster::projectRaster(ga, crs = sp::CRS("+init=EPSG:3857"))
    green_area_t <- raster::cellStats(ga, sum) * raster::res(green_area_r)[1] *
      raster::res(green_area_r)[2]
  } else {
    green_area <- sf::st_transform(ga, crs = 3857)
    green_area$aream <- sf::st_area(green_area)
    green_area_t <- as.numeric(sum(green_area$aream))
  }

  green_area_p <- data.frame(
    indicator = "Green area percapita",
    fclass = "green area capita",
    value = round(green_area_t / t_pop, 2),
    units = "m2/inh"
  )
  green_area_p
}
