#' This function calculates population density in a scenario.
#' Population density is the amount of inhabitants per built-up area
#' in a scenario
#'
#' METHOD:
#' The population density (population density) is calculated by dividing the
#' total number of inhabitants
#' (pop_tot) by the total built-up area (urban_fp).
#' The total population of the urban area (pop_tot) is calculated as the sum
#' of the population of each pixel, this calculation uses the total population
#' function (tot_pop), while the
#' total build-up area is calculated as the sum of build-up area of each pixel,
#' using the urban footprint function (urban_footprint)
#' The total population and urban footprint functions are included in this
#' package.
#' NOTE: If user provide a stack in any of the parameters, the should must have
#' the same number of layers for population
#' and footprint stack.
#'
#' @param pop a raster layer or raster stack that contains the population
#' distribution in the base or horizon year
#' @param fp a raster layer or raster stack that contains the footprint in the
#' base or horizon year
#' @return a data frame with information about the population density indicator
#' calculated in inhabitants per square kilometer,
#' total population, and the total footprint calculated in square kilometers per
#' each scenario.
#' @export
#' @examples
#' library(raster)
#' population.b <- system.file("extdata", "POP_2025.tif",
#'   package =
#'     "urbanperformance"
#' )
#' population.b <- raster::raster(population.b)
#' footprint.base <- system.file("extdata", "Build_up_2025.tif",
#'   package =
#'     "urbanperformance"
#' )
#' footprint.base <- raster::raster(footprint.base)
#'
#' pop_dens <- population_density(population.b, footprint.base)
population_density <- function(pop, fp) {
  pop <- raster::stack(pop)
  fp <- raster::stack(fp)
  if (raster::nlayers(pop) != raster::nlayers(fp)) {
    stop(
      paste0(
        "Please provide the same number of population ",
        "and urban footprint layers"
      )
    )
  }
  fp <- rasterchecker(fp, base = pop)
  nl <- as.numeric(max(raster::nlayers(pop), raster::nlayers(fp)))
  r <- lapply(1:nl, function(i) {
    p <- pop[[i]]
    b <- fp[[i]]
    pop_tot <- as.numeric(tot_pop(p)$value)
    urban_fp <- as.numeric(urban_footprint(b)$value)
    pop_dens <- round(pop_tot / urban_fp, 2)
    density <- data.frame(
      indicator = c("Total population", "Footprint area", "Population density"),
      fclass = c(names(p), names(b), "density"),
      value = c(pop_tot, urban_fp, pop_dens),
      units = c("inhabitants", "km2", "inh/km2")
    )
    density
  }) |> dplyr::bind_rows()
  r
}
