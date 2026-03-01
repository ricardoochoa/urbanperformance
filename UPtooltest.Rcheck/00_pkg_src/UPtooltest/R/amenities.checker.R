#' This function adjust the vector layer that cointains information about urban amenities,
#' standardize the CRS and ensure the points and polygons are inside the area of interest
#'
#' @param ... a shapefile or list of shapefile that contains polygons or points with the localition of urban amenities.
#' The shapefile must contain the "fclass" column.
#' @param aoi a shapefile that contains the delimitation of the study area
#' @param proj CRS coordinates
#' @return a standardize shapefile that contains the amenities points that are inside the study area.
#' @import raster
#' @export
#' @examples
#' library(sf)
#' data(amenities.cun)
#' data(aoi.cun)
#'
#'
#' amenities <- amenities.checker(amenities.cun, aoi = aoi.cun)
amenities.checker <- function(..., aoi, proj = 4326){
  x <- list(...)
  if (sf::st_crs(aoi) != sf::st_crs(proj)) {
    aoi <- st_transform(aoi, crs = proj)
  }
  flatten <- function(x) {
    if (inherits(x, "sf")) return(list(x))
    if (is.list(x)) return(unlist(lapply(x, flatten), recursive = FALSE))
  }
  x <- flatten(x)
  check <- function(s){
    if (sf::st_crs(s) != sf::st_crs(proj)) {
      s <- sf::st_transform(s, crs = proj)
    }
    points <- sf::st_intersection(s,aoi)
    points.c <- st_centroid(points)
    points.c$X <- st_coordinates(points.c)[,1]
    points.c$Y <- st_coordinates(points.c)[,2]
    return(points.c)
  }
  l <- lapply(x, check)
  p <- do.call(rbind, l)
  p <- p[c("fclass", "X", "Y")]
  return(p)

}
