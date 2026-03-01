#' This function calculates land consumption between two urban growth scenarios.
#' Land consumption is the amount of land predicted to change from natural habitats or
#' agricultural uses into urban human settlements between the base year and the horizon year.
#'
#' METHOD:
#' Land consumption (land.consumption) is calculated as the difference between
#' the city footprint in the horizon year (fp.horizon) and the footprint in the base
#' year (fp.base). The city footprint refers to the total built-up area of a city,
#' including streets, open space, and inner vacant land.
#'
#' @param fp.base a raster that contains the footprint in the base year
#' @param fp.horizon a raster that contains the footprint in the horizon year
#' @return a data frame with information about the land consumption indicator calculated in square kilometers
#' @import raster
#' @import dplyr
#' @export
#' @examples
#' library(raster)
#'
#' footprint.base <- system.file("extdata", "Build_up_2025.tif", package = "UPtooltest")
#' footprint.base <- raster::raster(footprint.base)
#' footprint.2030 <- system.file("extdata", "Build_up_2030.tif", package = "UPtooltest")
#' footprint.2030 <- raster::raster(footprint.2030)
#'
#' l.consumption <- land.consumption(footprint.base, footprint.2030)
land.consumption <- function(fp.base, fp.horizon){
  if(is.null(fp.horizon)){
    print("Please provide a raster layer for the footprint in the horizon year")
  }else{
    p <- rasterchecker(fp.horizon, base = fp.base)
  }
  base <- fp.base%>%
    one.cero
  x <- lapply(1:nlayers(p), function(i){
    horizon <- p[[i]]
    horizon <- one.cero(horizon)
    l.consumption <- horizon - base
    l.consumption[l.consumption !=1 ] <- NA
    r.consumption <- projectRaster(l.consumption, crs = CRS("+init=EPSG:3857"))
    y <- data.frame(indicator= "Land consumption",
                    fclass = paste(names(horizon),"-",names(base)),
                    value = round((cellStats(l.consumption, sum)*(res(r.consumption)[1]*res(r.consumption)[2]))/1e6,2),
                    units = "km2")
    assign(paste("urban.expansion_", names(horizon)), l.consumption, envir = .GlobalEnv)
    return(y)
  })%>% bind_rows()
  return(x)
}
