#' This function calculates the amount of high-value agricultural land that is
#' estimated
#' to become urban settlements, between the base year and the horizon year.
#'
#' METHOD:
#' Agricultural land consumption (agricultural.consumption) is calculated as the
#' area of the
#' urban footprint (buildup) that was land with high agricultural value in the
#' base year.
#' The first step is to define the polygon or the area that acknowledges
#' high-value agricultural land.
#' The agricultural land lost to urbanization (agri.c) is calculated by adding
#' up the hectares of urban area (buildup)
#' located within the agricultural polygon (agri.b).
#'
#' @param agri  polygon or raster that contains the area of high-value
#' agricultural land
#' @param buildup raster that contains the urban footprint in the base year
#' @return a data frame with information about the losses in agricultural land
#' in square kilometers
#' @import raster
#' @import dplyr
#' @import sf
#' @export
#' @examples
#' library(raster)
#'
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package =
#'     "UPtooltest"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif",
#'   package =
#'     "UPtooltest"
#' )
#' footprint.2030 <- raster::raster(footprint.2030)
#' agri.land <- land.cover2025
#' agri.land[agri.land != 1] <- 0
#'
#' agri.consumption <- agricultural.land.consumption(agri.land, footprint.2030)
agricultural.land.consumption <- function(agri, buildup) {
  if (!inherits(agri, "RasterLayer")) {
    agri.b <- rasterize(agri, buildup, field = 1)
    agri.b <- one.cero(agri.b)
  } else {
    agri.b <- one.cero(agri)
  }

  buildup.b <- one.cero(buildup)

  agri.c <- agri.b + buildup.b
  agri.c[agri.c != 2] <- NA
  agri.r <- projectRaster(agri.c, crs = sp::CRS("+init=EPSG:3857"))

  agricultural.consumption <- data.frame(
    indicator = "Agricultural land consumption",
    fclass = "agricultural loss",
    value = round(((cellStats(agri.c, sum) * res(agri.r)[1] * res(agri.r)[2])) / 1e6, 2),
    units = "km2"
  )
  return(agricultural.consumption)
}
