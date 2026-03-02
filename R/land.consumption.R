#' This function calculates land consumption between two urban growth scenarios.
#' Land consumption is the amount of land predicted to change from natural
#' habitats or
#' agricultural uses into urban human settlements between the base year and the
#' horizon year.
#'
#' METHOD:
#' Land consumption (land_consumption) is calculated as the difference between
#' the city footprint in the horizon year (fp_horizon) and the footprint in the
#' base
#' year (fp_base). The city footprint refers to the total built-up area of a
#' city,
#' including streets, open space, and inner vacant land.
#'
#' @param fp_base a raster that contains the footprint in the base year
#' @param fp_horizon a raster that contains the footprint in the horizon year
#' @return a data frame with information about the land consumption indicator
#' calculated in square kilometers
#' @export
#' @examples
#' library(raster)
#'
#' footprint.base <- system.file("extdata", "Build_up_2025.tif",
#'   package =
#'     "urbanperformance"
#' )
#' footprint.base <- raster::raster(footprint.base)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif",
#'   package =
#'     "urbanperformance"
#' )
#' footprint.2030 <- raster::raster(footprint.2030)
#'
#' l_consumption <- land_consumption(footprint.base, footprint.2030)
land_consumption <- function(fp_base, fp_horizon) {
  if (is.null(fp_horizon)) {
    print("Please provide a raster layer for the footprint in the horizon year")
  } else {
    p <- rasterchecker(fp_horizon, base = fp_base)
  }
  base <- one_cero(fp_base)
  x <- lapply(1:raster::nlayers(p), function(i) {
    horizon <- p[[i]]
    horizon <- one_cero(horizon)
    l_consumption <- horizon - base
    l_consumption[l_consumption != 1] <- NA
    r_consumption <- raster::projectRaster(l_consumption,
      crs = sp::CRS("+init=EPSG:3857")
    )
    y <- data.frame(
      indicator = "Land consumption",
      fclass = paste(names(horizon), "-", names(base)),
      value = round(
        (raster::cellStats(l_consumption, sum) *
          (raster::res(r_consumption)[1] *
            raster::res(r_consumption)[2])) / 1e6,
        2
      ),
      units = "km2"
    )
    assign(paste("urban.expansion_", names(horizon)), l_consumption,
      envir = .GlobalEnv
    )
    y
  }) |> dplyr::bind_rows()
  x
}
