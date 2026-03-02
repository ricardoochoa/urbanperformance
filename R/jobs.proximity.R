#' This function calculates the percentage of the population that lives within
#' 1,000 m from the areas with high job density in the urban area.
#'
#' METHOD:
#' This indicator identifies the areas of the urban area that concentrate
#' employment and
#' then quantifies the population that lives close to these areas as a
#' percentage of the total population.
#'
#' @param jobs a raster that contains information about jobs distribution
#' @param pop a raster that contains information about population distribution
#' @return a data frame with information about the number and percentage of
#' population with proximity to job hubs
#' @export
#' @examples
#' library(raster)
#'
#' pop.b <- system.file("extdata", "POP_2025.tif", package = "urbanperformance")
#' pop.b <- raster::raster(pop.b)
#'
#' jobs <- system.file("extdata", "Jobs.tif", package = "urbanperformance")
#' jobs <- raster::raster(jobs)
#'
#' jobs_prox <- jobs_proximity(jobs, pop = pop.b)
jobs_proximity <- function(jobs, pop) {
  j <- jobs
  p <- pop
  j_mean <- raster::cellStats(j, mean)
  j_sd <- raster::cellStats(j, stats::sd)
  j_value <- (2 * j_sd) + j_mean
  j[j <= j_value] <- NA
  j[j > j_value] <- 1
  j_dist <- raster::distance(j)
  j_dist[j_dist > 1000] <- NA
  j_dist[!is.na(j_dist)] <- 1
  j_prox <- j_dist * p
  jobs_prox <- data.frame(
    indicator = "Jobs proximity",
    fclass = "jobs",
    value = c(
      round(raster::cellStats(j_prox, sum, na.rm = TRUE), 0),
      round((raster::cellStats(j_prox, sum, na.rm = TRUE) /
        raster::cellStats(pop, sum, na.rm = TRUE)) * 100, 2)
    ),
    units = c("inhabitants", "%")
  )
  jobs_prox
}
