#' This function calculates the total population of a specific scenario or year
#'
#' @param ras a raster that contains population information
#' @return a data frame with information about the total population in
#' inhabitants
#' @import raster
#' @export
#' @examples
#' library(raster)
#'
#' pop.base <- system.file("extdata", "POP_2025.tif", package = "UPtooltest")
#' pop.base <- raster::raster(pop.base)
#'
#' total.population <- tot.pop(pop.base)
tot.pop <- function(ras) {
  ras <- stack(ras)
  pop <- lapply(1:nlayers(ras), function(i) {
    r <- ras[[i]]
    t <- data.frame(
      indicator = "Total population",
      fclass = names(r),
      value = paste(round(cellStats(r, sum), 0)),
      units = "inhabitants"
    )
  }) %>% bind_rows()
  return(pop)
}
