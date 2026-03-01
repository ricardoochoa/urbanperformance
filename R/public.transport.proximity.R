#' This function calculates the total and the percentage of the population that
#' lives within a maximum recommended distance from a public transport station,
#' according
#' to the transport classification.
#'
#' METHOD:
#' Public transport proximity (transport.proximity) is calculated by dividing
#' the population
#' (pop.prox.transit) that lives within the maximum recommended distance to a
#' public transport route or stop,
#' by the total population (pop).
#' First, a raster that contains the linear distance from bus stops location is
#' calculated (dist.r),
#' according to the type of transportation system (fclass). The maximum
#' recommended distance varies according
#' to the type of transportation.
#' Second, the population (pop) of all the analysis points contained in the
#' buffer is added up
#' to obtain the population that lives close to public transport
#' (pop.prox.transit).
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
#' @import raster
#' @import sf
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(transport.cun)
#'
#' pop.base <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.base <- raster::raster(pop.base)
#'
#' pt.prox <- public.transport.proximity(transport.cun, pop = pop.base)
public.transport.proximity <- function(..., pop, parameters = NULL) {
  if (is.null(parameters)) {
    p <- p.distances
  } else {
    p <- parameters
  }

  t.args <- list(...)

  if (length(t.args) == 1 && is.list(t.args[[1]])) {
    transport <- t.args[[1]]
  } else {
    transport <- do.call(rbind, t.args)
  }

  if (all(st_geometry_type(transport) != "POINT")) {
    transport.p <- sf::st_centroid(transport)
  } else {
    transport.p <- transport
  }
  category <- unique(stats::na.omit(transport.p$fclass))

  dist.rasters.list <- lapply(category, function(cat) {
    transport.s <- transport.p[which(transport.p$fclass == cat), ]

    coords <- sf::st_coordinates(transport.s)

    dist.r <- raster::distanceFromPoints(pop, coords)

    param.row <- p[p$fclass == cat, ]

    if (nrow(param.row) != 1) {
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
    return(dist.reclass)
  })

  distance.stack <- raster::stack(dist.rasters.list)

  if (nlayers(distance.stack) > 1) {
    distance.stack <- calc(distance.stack, fun = sum, na.rm = TRUE)
    distance.stack <- one.cero(distance.stack)
  }

  assign("transport.distances", distance.stack, envir = .GlobalEnv)
  pop.prox.transit <- distance.stack * pop

  transport.proximity <- data.frame(
    indicator = "Public transport proximity",
    fclass = "public transport",
    value = c(
      round(cellStats(pop.prox.transit, sum, na.rm = TRUE), 0),
      round((cellStats(pop.prox.transit, sum, na.rm = TRUE) / cellStats(pop, sum, na.rm = TRUE)) * 100, 2)
    ),
    units = c("inhabitants", "%")
  )
  return(transport.proximity)
}
