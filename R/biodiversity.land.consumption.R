#' This function calculates the amount of valuable natural land that is
#' estimated to become urban settlements, between the base year and the horizon
#' year.
#'
#' METHOD:
#' Biodiversity land consumption (bl.consumption) is calculated as the area
#' of the urban footprint (buildup) that was land with valuable natural land
#' in the base year. The first step is to define the polygon or the area that
#' acknowledges valuable natural land. The biodiversity land lost to
#' urbanization (bl_c) is calculated by adding up the hectares of urban area
#' (buildup) located within the green land polygon (bl_b).
#'
#' @param bl polygon or raster that contains the area of valuable natural land
#' @param buildup raster that contains the urban footprint in the base year
#' @return a data frame with information about the losses in biodiversity
#'   land in square kilometers
#' @export
#' @examples
#' library(raster)
#'
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package = "urbanperformance"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif",
#'   package = "urbanperformance"
#' )
#' footprint.2030 <- raster::raster(footprint.2030)
#' bio.land <- land.cover2025
#' bio.land[bio.land < 7] <- 0
#'
#' bio.consumption <- biodiversity_land_consumption(bio.land, footprint.2030)
biodiversity_land_consumption <- function(bl, buildup) {
  if (!inherits(bl, "RasterLayer")) {
    bl_b <- raster::rasterize(bl, buildup, field = 1)
    bl_b <- one_cero(bl_b)
  } else {
    bl_b <- one_cero(bl)
  }

  buildup_b <- one_cero(buildup)

  bl_c <- bl_b + buildup_b
  bl_c[bl_c != 2] <- NA
  bl_r <- raster::projectRaster(bl_c, crs = sp::CRS("+init=EPSG:3857"))

  bl_consumption <- data.frame(
    indicator = "Biodiversity land consumption",
    fclass = "biodiversity land loss",
    value = round(
      ((raster::cellStats(bl_c, sum) * raster::res(bl_r)[1] *
        raster::res(bl_r)[2])) / 1e6,
      2
    ),
    units = "km2"
  )
  bl_consumption
}
