#' @keywords internal
#' @aliases urbanperformance-package
#' @importFrom dplyr bind_rows filter mutate across select left_join
#' @importFrom dplyr group_by summarize
#' @importFrom raster cellStats res nlayers extent distance projectRaster raster
#' @importFrom raster rasterize distanceFromPoints freq stack resample values<-
#' @importFrom raster calc compareCRS
#' @importFrom sf st_length st_centroid st_coordinates st_transform
#' @importFrom sf st_geometry_type st_area st_intersection st_is st_geometry
#' @importFrom reshape2 dcast
#' @importFrom stats sd
"_PACKAGE"

utils::globalVariables(c(
    "fclass", "geometry", "lengthkm", "p.distances",
    ".data", "value", "layer", "units", "cat",
    "one_cero", "urban_footprint", "tot_pop",
    "rasterchecker", "land_cover_areas"
))
