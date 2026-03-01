#' This function calculates the amount of valuable natural land that is
#' estimated to become urban settlements, between the base year and the horizon
#' year.
#'
#' METHOD:
#' Biodiversity land consumption (bl.consumption) is calculated as the area
#' of the urban footprint (buildup) that was land with valuable natural land
#' in the base year. The first step is to define the polygon or the area that
#' acknowledges valuable natural land. The biodiversity land lost to
#' urbanization (bl.c) is calculated by adding up the hectares of urban area
#' (buildup) located within the green land polygon (bl.b).
#'
#' @param bl polygon or raster that contains the area of valuable natural land
#' @param buildup raster that contains the urban footprint in the base year
#' @return a data frame with information about the losses in biodiversity
#'   land in square kilometers
#' @import raster
#' @import dplyr
#' @import sf
#' @export
#' @examples
#' library(raster)
#'
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package = "UPtooltest"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif",
#'   package = "UPtooltest"
#' )
#' footprint.2030 <- raster::raster(footprint.2030)
#' bio.land <- land.cover2025
#' bio.land[bio.land < 7] <- 0
#'
#' bio.consumption <- biodiversity.land.consumption(bio.land, footprint.2030)
biodiversity.land.consumption <- function(bl, buildup) {
  if (!inherits(bl, "RasterLayer")) {
    bl.b <- rasterize(bl, buildup, field = 1)
    bl.b <- one.cero(bl.b)
  } else {
    bl.b <- one.cero(bl)
  }

  buildup.b <- one.cero(buildup)

  bl.c <- bl.b + buildup.b
  bl.c[bl.c != 2] <- NA
  bl.r <- projectRaster(bl.c, crs = CRS("+init=EPSG:3857"))

  bl.consumption <- data.frame(
    indicator = "Biodiversity land consumption",
    fclass = "biodiversity land loss",
    value = round(
      ((cellStats(bl.c, sum) * res(bl.r)[1] * res(bl.r)[2])) / 1e6,
      2
    ),
    units = "km2"
  )
  return(bl.consumption)
}
