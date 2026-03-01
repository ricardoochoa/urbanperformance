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
#' @import raster
#' @export
#' @examples
#' library(raster)
#'
#' land.cover2025 <- system.file("extdata", "Land_cover.tif",
#'   package =
#'     "UPtooltest"
#' )
#' land.cover2025 <- raster::raster(land.cover2025)
#'
#' lc.areas.2025 <- land.cover.areas(land.cover2025, class = land.cover.classes)
land.cover.areas <- function(ras, class = NULL) {
  ras <- stack(ras)
  x <- lapply(1:nlayers(ras), function(i) {
    r <- ras[[i]]
    w <- as.data.frame(freq(r))
    names(w) <- c("cat", "count")
    if (!is.null(class)) {
      y <- class %>%
        left_join(w)
    } else {
      y <- w %>%
        mutate(fclass = cat)
    }
    y$layer <- names(r)
    r <- projectRaster(r, crs = sp::CRS("+init=EPSG:3857"))
    y$value <- round(((res(r)[1] * res(r)[2]) * ifelse(is.na(y$count), 0, y$count) / 1e6), 2)
    y$units <- "km2"
    return(y)
  }) %>% bind_rows()

  return(x)
}
