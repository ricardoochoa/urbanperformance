#' This function calculates the amount of valuable natural land that is
#' estimated
#' to become urban settlements, between the base year and the horizon year.
#'
#' METHOD:
#' Green land consumption (greenl.consumption) is calculated as the area of the
#' urban footprint (buildup) that was land with valuable green land in the base
#' year.
#' The first step is to define the polygon or the area that acknowledges
#' valueable land.
#' The green land lost to urbanization (greenl_c) is calculated by adding up the
#' hectares of urban area (buildup)
#' located within the green land polygon (greenl_b).
#'
#' @param greenl  polygon or raster that contains the area of valuable green
#' land
#' @param buildup raster that contains the urban footprint in the base year
#' @return a data frame with information about the losses in green land in
#' square kilometers
#' @export
#' @examples
#' library(raster)
#'
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package =
#'     "urbanperformance"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif",
#'   package =
#'     "urbanperformance"
#' )
#' footprint.2030 <- raster::raster(footprint.2030)
#' green.land <- land.cover2025
#' green.land[green.land != 8] <- 0
#'
#' green_consumption <- green_land_consumption(green.land, footprint.2030)
green_land_consumption <- function(greenl, buildup) {
  if (!inherits(greenl, "RasterLayer")) {
    greenl_b <- raster::rasterize(greenl, buildup, field = 1)
    greenl_b <- one_cero(greenl_b)
  } else {
    greenl_b <- one_cero(greenl)
  }

  buildup_b <- one_cero(buildup)

  greenl_c <- greenl_b + buildup_b
  greenl_c[greenl_c != 2] <- NA
  greenl_r <- raster::projectRaster(greenl_c, crs = sp::CRS("+init=EPSG:3857"))

  greenl_consumption <- data.frame(
    indicator = "Green land consumption",
    fclass = "green land loss",
    value = round(
      ((raster::cellStats(greenl_c, sum) * raster::res(greenl_r)[1] *
          raster::res(greenl_r)[2])) / 1e6, 2
    ),
    units = "km2"
  )
  greenl_consumption
}
