#' This function calculates the percentage of the population that lives within
#' 1,000 m from the areas with high job density in the urban area.
#'
#' METHOD:
#' This indicator identifies the areas of the urban area that concentrate employment and
#' then quantifies the population that lives close to these areas as a percentage of the total population.
#'
#' @param jobs a raster that contains information about jobs distribution
#' @param pop a raster that contains information about population distribution
#' @return a data frame with information about the number and percentage of population with proximity to job hubs
#' @import raster
#' @export
#' @examples
#' library(raster)
#'
#' pop.b <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.b <- raster::raster(pop.b)
#'
#' jobs <- system.file("extdata", "Jobs.tif", package = "UPtooltest")
#' jobs <- raster::raster(jobs)
#'
#' jobs.prox <- jobs.proximity(jobs, pop = pop.b)
jobs.proximity <- function(jobs, pop){
  j <- jobs
  p <- pop
  j.mean <-cellStats(j, mean)
  j.sd <-cellStats(j, sd)
  j.value <-(2*j.sd)+j.mean
  j[j <= j.value] <- NA
  j[j > j.value] <- 1
  j.dist <- distance(j)
  j.dist[j.dist > 1000] <- NA
  j.dist[!is.na(j.dist)] <- 1
  j.prox <- j.dist*p
  jobs.proximity <- data.frame(indicator = "Jobs proximity",
                               fclass = "jobs",
                               value = c(round(cellStats(j.prox, sum, na.rm = TRUE),0),round((cellStats(j.prox, sum, na.rm = TRUE)/cellStats(pop, sum, na.rm= TRUE))*100,2)),
                               units = c("inhabitants", "%"))
  return(jobs.proximity)
}
