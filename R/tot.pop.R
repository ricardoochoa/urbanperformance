#' This function calculates the total population of a specific scenario or year
#'
#' @param ras a raster that contains population information
#' @return a data frame with information about the total population in
#' inhabitants
#' @export
#' @examples
#' library(raster)
#'
#' pop_base <- system.file("extdata", "POP_2025.tif",
#'   package = "urbanperformance"
#' )
#' pop_base <- raster::raster(pop_base)
#'
#' total_population <- tot_pop(pop_base)
tot_pop <- function(ras) {
  ras <- raster::stack(ras)
  pop <- lapply(1:raster::nlayers(ras), function(i) {
    r <- ras[[i]]
    t <- data.frame(
      indicator = "Total population",
      fclass = names(r),
      value = paste(round(raster::cellStats(r, sum), 0)),
      units = "inhabitants"
    )
    t
  }) |> dplyr::bind_rows()
  pop
}
