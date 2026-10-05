#' Urban Heat Island (UHI) Intensity
#'
#' This function quantifies the surface temperature differential between built-up urban
#' areas and the surrounding natural/rural environment.
#'
#' METHOD:
#' Urban structures and impervious surfaces trap solar radiation, creating localized thermal
#' microclimates. The algorithm masks Land Surface Temperature (LST) thermal grids by land-cover
#' classifications. Crucially, water bodies (such as lagoons, cenotes, and marine waters) are
#' excluded from the baseline reference because their distinct thermal inertia skews rural
#' comparisons. The rural baseline is evaluated within a perimeter buffer zone (default: 5 km)
#' around the urban footprint to maintain regional climatic consistency:
#' \deqn{UHI = \mu_{urban} - \mu_{rural}}
#'
#' @param lst `RasterLayer` of Land Surface Temperature (LST) in degrees Celsius or Kelvin.
#' @param land_cover `RasterLayer` of land cover classifications.
#' @param rural_buffer_km Numeric. Outreach distance in kilometers around the urban footprint
#'   to define the rural reference zone (default: 5). If 0, uses all non-urban, non-water pixels.
#' @param water_class Numeric vector of land cover categories representing water (default: 3).
#' @param urban_class Numeric vector of land cover categories representing urban built-up (default: c(2, 11, 12, 13, 14, 15)).
#' @param spatial Logical. If TRUE, returns a raster of temperature anomalies relative to rural baseline.
#'
#' @return A tidy `data.frame` containing mean urban temperature, mean rural reference temperature,
#'   and the UHI intensity differential in Celsius.
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' # uhi_intensity(lst_raster, land_cover_raster)
#' }
uhi_intensity <- function(lst,
                          land_cover,
                          rural_buffer_km = 5,
                          water_class = 3,
                          urban_class = c(2, 11, 12, 13, 14, 15),
                          spatial = FALSE) {
  if (!inherits(lst, "RasterLayer")) {
    stop("Input 'lst' must be a 'RasterLayer'.")
  }

  if (!inherits(land_cover, "RasterLayer")) {
    stop("Input 'land_cover' must be a 'RasterLayer'.")
  }

  # Ensure spatial alignment
  if (!raster::compareCRS(lst, land_cover) ||
      !raster::extent(lst) == raster::extent(land_cover) ||
      raster::res(lst)[1] != raster::res(land_cover)[1]) {
    land_cover <- rasterchecker(land_cover, base = lst)[[1]]
  }

  # 1. Urban Mask
  urban_r <- raster::calc(land_cover, fun = function(x) {
    ifelse(x %in% urban_class, 1, 0)
  })

  urban_pixels <- raster::cellStats(urban_r, stat = "sum", na.rm = TRUE)
  if (urban_pixels == 0) {
    stop("No urban pixels identified matching 'urban_class' parameters.")
  }

  lst_urban <- raster::mask(lst, urban_r, maskvalue = 0)
  t_urban <- raster::cellStats(lst_urban, stat = "mean", na.rm = TRUE)

  # 2. Rural Reference Mask
  if (rural_buffer_km > 0) {
    # Create buffered polygon around urban pixels
    urban_poly <- raster::rasterToPolygons(urban_r, fun = function(x){x == 1}, dissolve = TRUE)
    if (!is.null(urban_poly) && length(urban_poly) > 0) {
      urban_sf <- sf::st_as_sf(urban_poly)
      urban_sf_metric <- sf::st_transform(urban_sf, crs = 3857)
      buff_sf_metric <- sf::st_buffer(urban_sf_metric, dist = rural_buffer_km * 1000)
      buff_sf_orig <- sf::st_transform(buff_sf_metric, crs = sf::st_crs(raster::crs(lst)))

      buff_r <- raster::rasterize(buff_sf_orig, lst, field = 1, background = 0)

      # Rural mask: inside buffer, NOT urban, and NOT water
      rural_r <- raster::overlay(land_cover, buff_r, urban_r, fun = function(lc, buf, urb) {
        ifelse(buf == 1 & urb == 0 & !lc %in% water_class & !is.na(lc), 1, 0)
      })
    } else {
      rural_r <- raster::overlay(land_cover, urban_r, fun = function(lc, urb) {
        ifelse(urb == 0 & !lc %in% water_class & !is.na(lc), 1, 0)
      })
    }
  } else {
    rural_r <- raster::overlay(land_cover, urban_r, fun = function(lc, urb) {
      ifelse(urb == 0 & !lc %in% water_class & !is.na(lc), 1, 0)
    })
  }

  rural_pixels <- raster::cellStats(rural_r, stat = "sum", na.rm = TRUE)
  if (rural_pixels == 0) {
    # Fallback to all non-urban pixels if water-exclusion left no pixels
    warning("No rural reference pixels remained after buffer and water masking. Using all non-urban pixels.")
    rural_r <- raster::calc(urban_r, fun = function(u) ifelse(u == 0, 1, 0))
  }

  lst_rural <- raster::mask(lst, rural_r, maskvalue = 0)
  t_rural <- raster::cellStats(lst_rural, stat = "mean", na.rm = TRUE)

  uhi_diff <- t_urban - t_rural

  res <- data.frame(
    indicator = rep("Urban heat island intensity", 3),
    fclass = c("urban_mean_temperature", "rural_reference_temperature", "uhi_differential"),
    value = c(round(t_urban, 2), round(t_rural, 2), round(uhi_diff, 2)),
    units = rep("Celsius", 3),
    stringsAsFactors = FALSE
  )

  if (spatial) {
    anomaly_r <- lst - t_rural
    names(anomaly_r) <- "uhi_thermal_anomaly"
    return(anomaly_r)
  }

  res
}
