#' This function calculates the amount of a specific land coverage that is
#' become
#' to a different land cover, between different scenarios. This indicator can be
#' calculate for
#' a specific land coverage loss or different coverages including in the same
#' raster.
#'
#' METHOD:
#' Land cover loss (land.cover.loss) is calculated as the difference in areas of
#' a specific land cover in two different periods.
#' First the area of each coverage is calculated for the base year using the
#' function land.cover.areas included in the
#' package. Next, the areas of each coverage is calculated for the horizon year
#' or for the scenario or period selected.
#' Finally, the difference between both scenarios is calculated (loss).
#'
#' @param ...  raster layers or a raster stack that contains land cover areas
#' @param areas base parameter is NULL, user can load a data frame with the land
#' categories
#' @return a data frame with information about the losses in land coverage in
#' square kilometers
#' @export
#' @examples
#' library(raster)
#' library(reshape2)
#'
#' land.cover2018 <- system.file("extdata", "Land_cover_2018.tif",
#'   package =
#'     "UPtooltest"
#' )
#' land.cover2018 <- raster::raster(land.cover2018)
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package =
#'     "UPtooltest"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#'
#' land.cover <- stack(land.cover2018, land.cover2025)
#'
#' land.loss <- land.cover.loss(land.cover)
land.cover.loss <- function(..., areas = NULL) {
  ras <- stack(...)
  if (!is.null(areas)) {
    y <- areas
  } else {
    y <- land.cover.areas(ras)
  }
  if (nlayers(ras) > 1) {
    y <- dcast(y, fclass ~ layer, value.var = "value")
    base <- names(y)[2]
    loss <- y %>%
      mutate(across(-c(1, 2), ~ .x - y[[2]], .names = "value"))
    loss <- loss[, c(1, 4:ncol(loss))]
    loss$indicator <- "Land cover loss"
    loss$units <- "km2"
    loss <- loss[c("indicator", "fclass", "value", "units")]
  } else {
    y$indicator <- "Land cover loss"
    loss <- y[, c(7, 3, 5, 6)]

    names(loss) <- c("indicator", "fclass", "value", "units")
  }
  return(loss)
}
