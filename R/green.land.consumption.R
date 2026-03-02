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
#' The green land lost to urbanization (greenl.c) is calculated by adding up the
#' hectares of urban area (buildup)
#' located within the green land polygon (greenl.b).
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
#'     "UPtooltest"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif",
#'   package =
#'     "UPtooltest"
#' )
#' footprint.2030 <- raster::raster(footprint.2030)
#' green.land <- land.cover2025
#' green.land[green.land != 8] <- 0
#'
#' green.consumption <- green.land.consumption(green.land, footprint.2030)
green.land.consumption <- function(greenl, buildup) {
  if (!inherits(greenl, "RasterLayer")) {
    greenl.b <- rasterize(greenl, buildup, field = 1)
    greenl.b <- one.cero(greenl.b)
  } else {
    greenl.b <- one.cero(greenl)
  }

  buildup.b <- one.cero(buildup)

  greenl.c <- greenl.b + buildup.b
  greenl.c[greenl.c != 2] <- NA
  greenl.r <- projectRaster(greenl.c, crs = sp::CRS("+init=EPSG:3857"))

  greenl.consumption <- data.frame(
    indicator = "Green land consumption",
    fclass = "green land loss",
    value = round(
        ((cellStats(greenl.c, sum) * res(greenl.r)[1] * res(greenl.r)[2])) / 1e6, 2
      ),
    units = "km2"
  )
  return(greenl.consumption)
}
