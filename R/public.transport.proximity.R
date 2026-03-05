#' This function calculates the total and the percentage of the population that
#' lives within a maximum recommended distance from a public transport station,
#' according
#' to the transport classification.
#'
#' METHOD:
#' Public transport proximity (transport.proximity) is calculated by dividing
#' the population
#' (pop_prox_transit) that lives within the maximum recommended distance to a
#' public transport route or stop,
#' by the total population (pop).
#' First, a raster that contains the linear distance from bus stops location is
#' calculated (dist_r),
#' according to the type of transportation system (fclass). The maximum
#' recommended distance varies according
#' to the type of transportation.
#' Second, the population (pop) of all the analysis points contained in the
#' buffer is added up
#' to obtain the population that lives close to public transport
#' (pop_prox_transit).
#' Third, this population is divided by the total population of the urban area
#' (tot_pop) to
#' obtain the percentage of the population that lives close to public transport
#' (transport.proximity).
#'
#' @param ... a shapefile or list of shapefiles that contains polygons or points
#' with the location of public
#' transportation stops or lines. The shapefile must contain the "fclass"
#' column.
#' @param pop raster with information about population distribution
#' @param parameters base variable is null, but the indicator can use the
#' p.distances tables that contains information about the maximum distance
#' recommended for each
#' fclass to calculate the proximities
#' @return a data frame with the results of the indicator with
#' the total and percentage population with proximity to transport stops or
#' services
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(transport.cun)
#'
#' pop_base <- system.file("extdata", "POP_2025.tif",
#'   package = "urbanperformance"
#' )
#' pop_base <- raster::raster(pop_base)
#'
#' pt_prox <- public_transport_proximity(transport.cun, pop = pop_base)
public_transport_proximity <- function(..., pop, parameters = NULL) {
  if (is.null(parameters)) {
    p <- p.distances
  } else {
    p <- parameters
  }

  t_args <- list(...)

  if (length(t_args) == 1 && is.list(t_args[[1]])) {
    transport <- t_args[[1]]
  } else {
    transport <- do.call(rbind, t_args)
  }

  if (all(sf::st_geometry_type(transport) != "POINT")) {
    transport_p <- sf::st_centroid(transport)
  } else {
    transport_p <- transport
  }
  category <- unique(stats::na.omit(transport_p$fclass))

  dist_rasters_list <- lapply(category, function(cat) {
    transport_s <- transport_p[which(transport_p$fclass == cat), ]

    coords <- sf::st_coordinates(transport_s)

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
    dist_reclass
  })

  distance_stack <- raster::stack(dist_rasters_list)

  if (raster::nlayers(distance_stack) > 1) {
    distance_stack <- raster::calc(distance_stack, fun = sum, na.rm = TRUE)
    distance_stack <- one_cero(distance_stack)
  }


  pop_prox_transit <- distance_stack * pop

  transport_proximity <- data.frame(
    indicator = "Public transport proximity",
    fclass = "public transport",
    value = c(
      round(raster::cellStats(pop_prox_transit, sum, na.rm = TRUE), 0),
      round(
        (raster::cellStats(pop_prox_transit, sum, na.rm = TRUE) /
          raster::cellStats(pop, sum, na.rm = TRUE)) * 100, 2
      )
    ),
    units = c("inhabitants", "%")
  )
  transport_proximity
}
