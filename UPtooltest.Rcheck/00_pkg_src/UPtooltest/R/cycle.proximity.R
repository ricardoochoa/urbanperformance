#' This function calculates the total and the percentage of the population that
#' lives within a maximum recommended distance from a cycle track
#'
#' METHOD:
#' Cycle proximity (cycle.proximity) is calculated by dividing the population (pop.prox.cycle) that
#' lives within the service radio of cycling infrastructure by the total population (pop).
#' First, a raster distance is calculated from the cycle tracks buffer. Next, a selection of pixels
#' that contain values lower or equal to the maximum recommended distance from a cycle track. General maximum recommended
#' distance for cycle track can be found in the ‘p.distances’ table of this package.
#' Next, the population (pop) of all the pixels contained in the raster selection is added up to obtain the population that
#' has access to the cycle track (pop.prox.cycle). Finally, this population is divided by the total population of the
#' urban area (pop) to obtain the percentage of the population that lives within the recommended distance for that
#' type of amenity (c.proximity).
#' @param cycle a shapefile that contains the lines or tracks of cycle infrastructure. The shapefile must contain the "fclass" column.
#' @param pop raster with information about population distribution
#' @param parameters base variable is null, but the indicator can use the
#' p.distances tables that contains information about the maximum distance recommended for each
#' fclass to calculate the proximities
#' @param save select TRUE for saving distance raster or FALSE to skip this process
#' @return a data frame with the results of the indicator with
#' the total and percentage population with proximity to cycle tracks
#' @import raster
#' @import sf
#' @export
#' @examples
#' library(sf)
#' library(raster)
#' data(cycle.cun)
#'
#' pop.base <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.base <- raster::raster(pop.base)
#'
#' cycle.prox <- cycle_proximity(cycle.cun, pop = pop.base)
cycle_proximity <- function(cycle, pop, parameters = NULL, save = TRUE){
  if (is.null(parameters)) {
    p <- p.distances
  }else{
    p <- parameters
  }
  category <- unique(stats::na.omit(cycle$fclass))
  param <- p[p$fclass == category, ]
  param <- as.numeric(param$value)

  cycle.r <- rasterize(cycle, pop)
  cycle.d <- distance(cycle.r)
  cycle.d <- resample(cycle.d, pop, method = 'ngb')

  cycle.reclass <- cycle.d
  cycle.reclass[cycle.reclass <= param] <- 1
  cycle.reclass[cycle.reclass > param] <- 0
  cycle.reclass[is.na(cycle.reclass)] <- 0

  pop.prox.cycle <- cycle.reclass * pop

  if(save == TRUE){
    assign("cycle.distances", cycle.reclass, envir = .GlobalEnv)
  }

  c.proximity <- data.frame(indicator = "Cycle proximity",
                                fclass = "cycle",
                                value = c(round(cellStats(pop.prox.cycle, sum,na.rm = TRUE),0),
                                          round((cellStats(pop.prox.cycle, sum, na.rm= TRUE)/cellStats(pop, sum, na.rm=TRUE))*100,2)),
                                units = c("inhabitants", "%"))
  return(c.proximity)
}
