#' This function calculates the average number of urban crossroads per unit of area.
#' This indicator is a proxy for pedestrian accessibility across the urban area.
#'
#' METHOD:
#' An intersection is the point where two or more roads cross each other in an urban area.
#' The first step is to calculate the roads intersection (inter) and calculate the total number
#' of unique points. Then, the total number of identified intersections (n.inter) is then divided by the
#' total built-up area (u.footprint) which is calculated using the urban footprint function included in the
#' package, to obtain the intersection density (inter.dens).
#'
#' @param roads  shapefile that contains  roads
#' @param footprint raster that contains the urban footprint in the base year
#' @return a data frame with information about the intersection density per square kilometer
#' @import raster
#' @import dplyr
#' @import sf
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(roads.cun)
#'
#' footprint.b <- system.file("extdata", "Build_up_2025.tif", package = "UPtooltest")
#' footprint.b <- raster::raster(footprint.b)
#'
#' inter.dens <- intersection.density(roads.cun, footprint.b)
intersection.density <- function(roads, footprint){
  inter <- st_intersection(roads)

  inter <- inter %>%
    filter(st_is(geometry, c("POINT", "MULTIPOINT")))
  inter <- unique(st_geometry(inter))

  n.inter <- as.numeric(length(inter))

  u.footprint <- urban.footprint(footprint)$value

  inter.dens <- data.frame(indicator = "Intersection density",
                           fclass = "intersection density",
                           value = round(n.inter/as.numeric(u.footprint),2),
                           units = "int/km2")

  return(inter.dens)
}
