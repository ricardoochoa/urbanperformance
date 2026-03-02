#' This function calculates the total green area within the urban area
#' (forests, parks, gardens, etc.) per inhabitant.
#'
#' METHOD:
#' The park and public space area available per person (green.area.p) is
#' estimated
#' by adding up the area with vegetation (green.area), in square meters, in the
#' parks
#' and public open spaces of the urban area, and dividing this result
#' (green.area.t) by the total population (t.pop).
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
#'     "UPtooltest"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' pop.base <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.base <- raster::raster(pop.base)
#'
#' green.land <- land.cover2025
#' green.land[green.land != 8] <- 0
#' green.a.capita <- green.area.pcapita(green.land, pop.base)
green.area.pcapita <- function(ga, pop) {
  if (is.numeric(pop) == TRUE) {
    t.pop <- pop
  } else {
    if (inherits(pop, "RasterLayer")) {
      t.pop <- as.numeric(tot.pop(pop)$value)
    }
  }

  if (inherits(ga, "RasterLayer")) {
    green.area <- one.cero(ga)
    green.area.r <- projectRaster(ga, crs = sp::CRS("+init=EPSG:3857"))
    green.area.t <- cellStats(ga, sum) * res(green.area.r)[1] *
      res(green.area.r)[2]
  } else {
    green.area <- st_transform(ga, crs = 3857)
    green.area$aream <- st_area(green.area)
    green.area.t <- as.numeric(sum(green.area$aream))
  }

  green.area.p <- data.frame(
    indicator = "Green area percapita",
    fclass = "green area capita",
    value = round(green.area.t / t.pop, 2),
    units = "m2/inh"
  )
  return(green.area.p)
}
