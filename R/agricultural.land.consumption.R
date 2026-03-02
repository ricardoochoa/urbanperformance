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
#' The agricultural land lost to urbanization (agri_c) is calculated by adding
#' up the hectares of urban area (buildup)
#' located within the agricultural polygon (agri_b).
#'
#' @param agr_land_base a raster that contains agricultural land base information
#' @param agr_land_horizon a raster that contains buildup informationt in the base year
#' @return a data frame with information about the losses in agricultural land
#' in square kilometers
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
#' agri.land <- land.cover2025
#' agri.land[agri.land != 1] <- 0
#'
#' agri.consumption <- agricultural_land_consumption(agri.land, footprint.2030)
agricultural_land_consumption <- function(agr_land_base, agr_land_horizon) {
  if (!inherits(agr_land_base, "RasterLayer")) {
    agri_b <- raster::rasterize(agr_land_base, agr_land_horizon, field = 1)
    agri_b <- one_cero(agri_b)
  } else {
    agri_b <- one_cero(agr_land_base)
  }

  buildup_b <- one_cero(agr_land_horizon)


  agr_1 <- agri_b + buildup_b
  agr_1[agr_1 != 2] <- NA
  agr_1 <- raster::projectRaster(agr_1,
    crs = sp::CRS("+init=EPSG:3857")
  )

  agricultural_consumption <- data.frame(
    indicator = "Agricultural land consumption",
    fclass = "agricultural loss",
    value = round(((raster::cellStats(agr_1, sum) *
      raster::res(agr_1)[1] * raster::res(agr_1)[2])) / 1e6, 2),
    units = "km2"
  )
  agricultural_consumption
}
