#' This function calculates the total and the percentage of the population that
#' lives within the maximum recommended distance to each of the following urban
#' public services and amenities: schools, health facilities (clinics and hospitals),
#' cultural facilities (community centers, libraries, theaters),
#' worship places, markets, sport facilities, public spaces (parks, green areas, squares, playgrounds),
#' and any other provided.
#'
#' METHOD:
#' One indicator is calculated for each of the provided classes of amenities.
#' The proximity (proxi) is calculated for each amenity class (fclassi) by dividing the
#' population (pop_prox_ami) that lives within the maximum distance recommended for that type of amenity (max_disti),
#' by the total population (tot_pop).
#'
#' The first step is to calculate a raster that contains the linear distance from the center of each amenity.
#' Next, a selection of pixels that contain values lower or equal to the maximum recommended distance for each amenity.
#' Maximum recommended distance values for base amenities can be found in the ‘p.distances’ table of this package.
#' Next, the population (pop) of all the pixels contained in the raster selection is added up to obtain the population that
#' has access to a particular amenity (proxi). Finally, this population is divided by the total population of the
#' urban area (tot.pop) to obtain the percentage of the population that lives within the recommended distance for that
#' type of amenity (r.proximity).
#' @param ... a shapefile or list of shapefiles that contains polygons or points with the location of urban amenities.
#' The shapefile must contain the "fclass" column.
#' @param pop raster with information about population distribution
#' @param parameters table with the maximum distance values for each fclass. Base variable is null, and the indicator use the
#' p.distances table included in the package that contains general standards related to the maximum distance recommended for each
#' fclass to calculate the proximities.
#' @param save specify if the raster distance will be saved in the global environment
#' @return a data frame with the results of the indicator with
#' the total and percentage of population with proximity to urban amenities
#' @import raster
#' @import sf
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(amenities.cun)
#'
#' pop.b <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.b <- raster::raster(pop.b)
#'
#' amen.proximity <- amenities.proximity(amenities.cun, pop = pop.b)
amenities.proximity <- function(..., pop, parameters = NULL, save = TRUE){
  if (is.null(parameters)) {
    p <- p.distances
  }else{
    p <- parameters
  }

  amen.args <- list(...)

  if (length(amen.args) == 1 && is.list(amen.args[[1]]) && !inherits(amen.args[[1]], "sf")) {
    amen <- amen.args[[1]]
    amen <- do.call(rbind, amen)
  } else{
    amen <- do.call(rbind, amen.args)
  }

  category <- unique(stats::na.omit(amen$fclass))

  dist.rasters.list <- lapply(category, function(cat) {

    amen.s <- amen[which(amen$fclass == cat), ]

    if(all(st_geometry_type(amen.s)!= "POINT")){
      amen.p <- sf::st_centroid(amen.s)
    }else{
      amen.p <- amen.s
    }


    coords <- sf::st_coordinates(amen.p)

    dist.r <- raster::distanceFromPoints(pop, coords)

    param.row <- p[p$fclass == cat, ]

    if(nrow(param.row) != 1) {
      warning(paste("There is not a parameter for :", cat, ". Indicator will not be calculated, please adjust the parameters."))
      dist.reclass <- raster(pop)
      values(dist.reclass) <- 0
      names(dist.reclass) <- cat
      return(dist.reclass)
    }

    param.value <- as.numeric(param.row$value)

    dist.reclass <- dist.r
    dist.reclass[dist.reclass <= param.value] <- 1
    dist.reclass[dist.reclass > param.value] <- 0
    dist.reclass[is.na(dist.reclass)] <- 0

    names(dist.reclass) <- cat
    if(save == TRUE){
      assign(paste("amenities.distances.",cat), dist.reclass, envir = .GlobalEnv)
    }
    return(dist.reclass)
  })

  distance.stack <- raster::stack(dist.rasters.list)

  r.proximity <- distance.stack * pop
  names(r.proximity) <- category

  proxi <- lapply(1:nlayers(r.proximity), function(i){
    r <- r.proximity[[i]]
    y <- data.frame(indicator = "Amenities proximity",
                    fclass = names(r),
                    value = c(round(cellStats(r, sum, na.rm = TRUE),0),round((cellStats(r, sum, na.rm = TRUE)/cellStats(pop, sum, na.rm= TRUE))*100,2)),
                    units = c("inhabitants", "%"))
    return(y)
  })
  proximities <- do.call(rbind, proxi)
  return(proximities)
}
