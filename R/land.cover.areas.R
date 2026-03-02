#' This function calculates areas for each land cover in a specific scenario
#'
#' METHOD:
#' The first step is to identify the each land cover category (i) included in
#' the raster.
#' Then, the total area for each category is calculated by adding up the square
#' kilometers (areai)
#' of each land cover.
#'
#' @param ras a raster that contains different land covers
#' @param class base parameter is NULL, user can load a data frame that contains
#' land cover classification
#' @return a data frame with information about the land consumption in square
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
#'
#' lc_areas_25 <- land_cover_areas(land.cover2025, class = land.cover.classes)
land_cover_areas <- function(ras, class = NULL) {
  ras <- raster::stack(ras)
  x <- lapply(1:raster::nlayers(ras), function(i) {
    r <- ras[[i]]
    w <- as.data.frame(raster::freq(r))
    names(w) <- c("cat", "count")
    if (!is.null(class)) {
      y <- class |>
        dplyr::left_join(w)
    } else {
      y <- w |>
        dplyr::mutate(fclass = cat)
    }
    y$layer <- names(r)
    r <- raster::projectRaster(r, crs = sp::CRS("+init=EPSG:3857"))
    y$value <- round(((raster::res(r)[1] * raster::res(r)[2]) *
      ifelse(is.na(y$count), 0, y$count) / 1e6), 2)
    y$units <- "km2"
    y
  }) |>
    dplyr::bind_rows()

  x
}
