#' This function calculates the average number of urban crossroads per unit of
#' area.
#' This indicator is a proxy for pedestrian accessibility across the urban area.
#'
#' METHOD:
#' An intersection is the point where two or more roads cross each other in an
#' urban area.
#' The first step is to calculate the roads intersection (inter) and calculate
#' the total number
#' of unique points. Then, the total number of identified intersections
#' (n_inter) is then divided by the
#' total built-up area (u_footprint) which is calculated using the urban
#' footprint function included in the
#' package, to obtain the intersection density (inter_dens).
#'
#' @param roads  shapefile that contains  roads
#' @param footprint raster that contains the urban footprint in the base year
#' @return a data frame with information about the intersection density per
#' square kilometer
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(roads.cun)
#'
#' footprint.b <- system.file("extdata", "Build_up_2025.tif",
#'   package =
#'     "urbanperformance"
#' )
#' footprint.b <- raster::raster(footprint.b)
#'
#' inter_dens <- intersection_density(roads.cun, footprint.b)
intersection_density <- function(roads, footprint) {
  inter <- sf::st_intersection(roads)

  inter <- inter |>
    dplyr::filter(sf::st_is(.data$geometry, c("POINT", "MULTIPOINT")))
  inter <- unique(sf::st_geometry(inter))

  n_inter <- as.numeric(length(inter))

  u_footprint <- urban_footprint(footprint)$value

  inter_dens <- data.frame(
    indicator = "Intersection density",
    fclass = "intersection density",
    value = round(n_inter / as.numeric(u_footprint), 2),
    units = "int/km2"
  )

  inter_dens
}
