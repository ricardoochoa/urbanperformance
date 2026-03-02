#' @keywords internal
#' @aliases UPtooltest-package
#' @importFrom dplyr bind_rows filter mutate across select left_join %>% group_by summarize
#' @importFrom raster cellStats res nlayers extent distance projectRaster raster rasterize distanceFromPoints freq stack resample values<- calc compareCRS
#' @importFrom sf st_length st_centroid st_coordinates st_transform st_geometry_type st_area st_intersection st_is st_geometry
#' @importFrom reshape2 dcast
#' @importFrom stats sd
"_PACKAGE"

utils::globalVariables(c(
  "fclass", "geometry", "lengthkm", "p.distances", ".data", "value", "layer", "units", "cat"
))
