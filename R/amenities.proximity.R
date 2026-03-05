#' This function calculates the total and the percentage of the population that
#' lives within the maximum recommended distance to each of the following urban
#' public services and amenities: schools, health facilities (clinics and
#' hospitals),
#' cultural facilities (community centers, libraries, theaters),
#' worship places, markets, sport facilities, public spaces (parks, green areas,
#' squares, playgrounds),
#' and any other provided.
#'
#' METHOD:
#' One indicator is calculated for each of the provided classes of amenities.
#' The proximity (proxi) is calculated for each amenity class (fclassi) by
#' dividing the
#' population (pop_prox_ami) that lives within the maximum distance recommended
#' for that type of amenity (max_disti),
#' by the total population (tot_pop).
#'
#' The first step is to calculate a raster that contains the linear distance
#' from the center of each amenity.
#' Next, a selection of pixels that contain values lower or equal to the maximum
#' recommended distance for each amenity.
#' Maximum recommended distance values for base amenities can be found in the
#' ‘p.distances’ table of this package.
#' Next, the population (pop) of all the pixels contained in the raster
#' selection is added up to obtain the population that
#' has access to a particular amenity (proxi). Finally, this population is
#' divided by the total population of the
#' urban area (tot_pop) to obtain the percentage of the population that lives
#' within the recommended distance for that
#' type of amenity (r_proximity).
#' @param ... a shapefile or list of shapefiles that contains polygons or points
#' with the location of urban amenities.
#' The shapefile must contain the "fclass" column.
#' @param pop raster with information about population distribution
#' @param parameters table with the maximum distance values for each fclass.
#' Base variable is null, and the indicator use the
#' p.distances table included in the package that contains general standards
#' related to the maximum distance recommended for each
#' fclass to calculate the proximities.
#' @param save specify if the raster distance will be saved in the global
#' environment
#' @return a data frame with the results of the indicator with
#' the total and percentage of population with proximity to urban amenities
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(amenities.cun)
#'
#' pop.b <- system.file("extdata", "POP_2025.tif", package = "urbanperformance")
#' pop.b <- raster::raster(pop.b)
#'
#' amen.proximity <- amenities_proximity(amenities.cun, pop = pop.b)
amenities_proximity <- function(..., pop, parameters = NULL, save = TRUE) {
  if (is.null(parameters)) {
    p <- p.distances
  } else {
    p <- parameters
  }

  amen_args <- list(...)

  if (length(amen_args) == 1 && is.list(amen_args[[1]]) &&
    !inherits(amen_args[[1]], "sf")) {
    amen <- amen_args[[1]]
    amen <- do.call(rbind, amen)
  } else {
    amen <- do.call(rbind, amen_args)
  }

  category <- unique(stats::na.omit(amen$fclass))

  dist_rasters_list <- lapply(category, function(cat) {
    amen_s <- amen[which(amen$fclass == cat), ]

    if (all(sf::st_geometry_type(amen_s) != "POINT")) {
      amen_p <- sf::st_centroid(amen_s)
    } else {
      amen_p <- amen_s
    }


    coords <- sf::st_coordinates(amen_p)

    dist_r <- raster::distanceFromPoints(pop, coords)

    param_row <- p[p$fclass == cat, ]

    if (nrow(param_row) != 1) {
      warning(paste(
        "There is not a parameter for :", cat,
        ". Indicator will not be calculated, please adjust the parameters."
      ))
      dist_reclass <- raster::raster(pop)
      raster::values(dist_reclass) <- 0
      names(dist_reclass) <- cat
      return(dist_reclass)
    }

    param_value <- as.numeric(param_row$value)

    dist_reclass <- dist_r
    dist_reclass[dist_reclass <= param_value] <- 1
    dist_reclass[dist_reclass > param_value] <- 0
    dist_reclass[is.na(dist_reclass)] <- 0

    names(dist_reclass) <- cat
    if (save == TRUE) {
      warning(
        "The 'save' argument is deprecated. Rasters are no longer ",
        "automatically saved to the global environment to comply ",
        "with CRAN policies."
      )
    }
    dist_reclass
  })

  distance_stack <- raster::stack(dist_rasters_list)

  r_proximity <- distance_stack * pop
  names(r_proximity) <- category

  proxi <- lapply(1:raster::nlayers(r_proximity), function(i) {
    r <- r_proximity[[i]]

    sum_r <- raster::cellStats(r, sum, na.rm = TRUE)
    sum_pop <- raster::cellStats(pop, sum, na.rm = TRUE)

    y <- data.frame(
      indicator = "Amenities proximity",
      fclass = names(r),
      value = c(
        round(sum_r, 0),
        round((sum_r / sum_pop) * 100, 2)
      ),
      units = c("inhabitants", "%")
    )
    y
  })
  do.call(rbind, proxi)
}
