#' This function calculates the roads kilometers per each fclass in the study
#' area
#'
#' METHOD:
#' The roads length (roads.length) is calculated by adding up the
#' length of road roads in kilometers (lengthkm) for each category (fclass)
#' included.
#'
#' @param roads  shapefile that contains roads. This layer must contain the
#' "fclass" colum
#' @return a data frame with information about the total km of roads of each
#' category in kilometers
#' @importFrom sf st_length
#' @export
#' @examples
#' library(sf)
#' data(roads.cun)
#'
#' r.length <- roads.length(roads.cun)
roads.length <- function(roads) {
  r <- roads
  r$lengthkm <- as.numeric(st_length(r)) / 1e3
  rl <- data.frame(
    indicator = "Roads length",
    units = "km"
  )
  r2 <- as.data.frame(r) %>%
    group_by(fclass) %>%
    summarize(value = sum(lengthkm))

  rl <- cbind(rl, r2)
  return(rl)
}
