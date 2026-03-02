#' This function calculates the number and percentage of the population that is
#' exposed to hazards because they live within the zone of influence
#' of natural and anthropogenic sources.
#'
#' METHOD:
#' Exposure (hazard.exposure) is calculated by dividing the number of
#' inhabitants (r.pop)
#' that live within the zone of influence of a hazard zone by the total
#' population (p).
#' First, clean raster process in apply to convert the layer into a binary
#' raster. Each type of
#' hazard is identified by its layer name. Second, the population (r.pop) of all
#' the analysis
#' points contained in the hazard area is added up to obtain the population that
#' lives within the affectation zone (r.pop).
#' Finally, the population is divided by the total population of the urban area
#' (p) to obtain the exposure
#' (hazard.exposure) expressed as a percentage.
#'
#' @param r a raster or raster stack with delimitation of risk areas
#' @param pop a raster o raster stack with information about population
#' distribution
#' @return a data frame with information about total population in inhabitants
#' and percentage
#' @export
#' @examples
#' library(raster)
#'
#' pop.b <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.b <- raster::raster(pop.b)
#'
#' hazard <- system.file("extdata", "Hazard.tif", package = "UPtooltest")
#' hazard <- raster::raster(hazard)
#'
#' pop.in.hazard <- hazard.exposure(hazard, pop = pop.b)
hazard.exposure <- function(r, pop) {
  h <- stack(r)
  p <- pop
  x <- lapply(1:nlayers(h), function(i) {
    r <- h[[i]]
    r <- one.cero(r)
    r.pop <- r * p
    y <- data.frame(
      indicator = "Population exposed to risk",
      fclass = names(r),
      value = c(
        round(cellStats(r.pop, sum), 0),
        round((cellStats(r.pop, sum) / cellStats(p, sum)) * 100, 2)
      ),
      units = c("inhabitants", "%")
    )
    return(y)
  }) %>% bind_rows()
  return(x)
}
