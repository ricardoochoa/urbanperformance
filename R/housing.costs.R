#' Housing Costs
#'
#' This function calculates the average cost per housing unit across residential
#' zones using land values, construction costs, and socioeconomic deciles.
#'
#' METHOD:
#' Housing costs are estimated by intersecting residential block geometries with
#' a socioeconomic income decile raster. For each block, the modal decile is
#' determined, and corresponding land and construction parameters are assigned.
#' The unit cost factors in both the pro-rata land share per unit and the
#' physical construction cost based on average unit size per decile:
#' \deqn{HC_j = \frac{Area_j \times LandCost_{sqm}}{NumUnits_j} + (AvgUnit_{sqm} \times ConstrCost_{sqm})}
#' The city-wide indicator is calculated as the unit-weighted average across all
#' residential zones.
#'
#' @param residential_zones `sf` polygon collection representing residential blocks or parcels.
#' @param socioeco_raster `RasterLayer` containing socioeconomic deciles (values 1 to 10).
#' @param cost_params `data.frame` containing cost multipliers per decile with columns:
#'   \describe{
#'     \item{decile}{Integer decile (1 to 10)}
#'     \item{land_cost_sqm}{Land value per square meter (or `land_cost_sqm_usd`)}
#'     \item{constr_cost_sqm}{Construction cost per square meter (or `constr_cost_sqm_usd`)}
#'     \item{avg_unit_sqm}{Average unit size in square meters}
#'   }
#' @param num_units_col Character. Column name in `residential_zones` representing the
#'   number of housing units. If missing or NULL, units are estimated from polygon area.
#' @param currency Character. Currency label for output table (default: "USD").
#' @param spatial Logical. If TRUE, returns the `sf` object with calculated unit costs attached.
#'
#' @return A tidy `data.frame` with indicator name, classification, value, and units.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' cost_params <- data.frame(
#'   decile = 1:10,
#'   land_cost_sqm = c(45, 60, 85, 300, 350, 400, 450, 800, 1200, 2500),
#'   constr_cost_sqm = c(324, 380, 450, 810, 900, 980, 1135, 1350, 1800, 2432),
#'   avg_unit_sqm = c(42, 45, 55, 68, 80, 95, 120, 180, 220, 350)
#' )
#' # housing_costs(residential_zones, socioeco_raster, cost_params)
#' }
housing_costs <- function(residential_zones,
                          socioeco_raster,
                          cost_params,
                          num_units_col = "num_units",
                          currency = "USD",
                          spatial = FALSE) {
  if (!inherits(residential_zones, "sf")) {
    stop("Input 'residential_zones' must be an 'sf' object.")
  }

  if (!inherits(socioeco_raster, "RasterLayer")) {
    stop("Input 'socioeco_raster' must be a 'RasterLayer'.")
  }

  if (nrow(residential_zones) == 0) {
    stop("'residential_zones' cannot be empty.")
  }

  # Standardize cost_params columns
  col_map <- list(
    decile = c("decile"),
    land_cost_sqm = c("land_cost_sqm", "land_cost_sqm_usd", "land_cost"),
    constr_cost_sqm = c("constr_cost_sqm", "constr_cost_sqm_usd", "constr_cost"),
    avg_unit_sqm = c("avg_unit_sqm", "unit_sqm", "unit_size_sqm")
  )

  params_clean <- cost_params
  for (std_name in names(col_map)) {
    found <- intersect(col_map[[std_name]], names(params_clean))
    if (length(found) > 0) {
      names(params_clean)[names(params_clean) == found[1]] <- std_name
    } else if (!std_name %in% names(params_clean)) {
      stop(paste0("Required column '", std_name, "' not found in 'cost_params' data frame."))
    }
  }

  # Align coordinate systems
  zones <- residential_zones
  raster_crs <- raster::crs(socioeco_raster)
  if (!sf::st_crs(zones) == sf::st_crs(raster_crs)) {
    zones <- sf::st_transform(zones, crs = sf::st_crs(raster_crs))
  }

  # Compute metric area in square meters (transform to EPSG:3857 for metric area if not planar)
  zones_metric <- sf::st_transform(zones, crs = 3857)
  area_sqm <- as.numeric(sf::st_area(zones_metric))

  # Extract modal decile per polygon
  extracted_vals <- raster::extract(socioeco_raster, zones)

  calc_mode <- function(v) {
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NA_integer_)
    tbl <- table(round(v))
    as.integer(names(tbl)[which.max(tbl)])
  }

  modal_deciles <- vapply(extracted_vals, calc_mode, integer(1))

  # Centroid fallback for NA deciles
  na_indices <- which(is.na(modal_deciles))
  if (length(na_indices) > 0) {
    centroids <- sf::st_centroid(zones[na_indices, ])
    centroid_vals <- raster::extract(socioeco_raster, centroids)
    modal_deciles[na_indices] <- as.integer(round(centroid_vals))
  }

  # Default fallback for any remaining NAs (median decile 5)
  modal_deciles[is.na(modal_deciles)] <- 5L
  modal_deciles <- pmin(10L, pmax(1L, modal_deciles))

  # Resolve unit counts
  if (!is.null(num_units_col) && num_units_col %in% names(zones)) {
    units <- as.numeric(zones[[num_units_col]])
    units[is.na(units) | units <= 0] <- 1
  } else {
    # Fallback estimation using FAR = 0.6 and decile average unit size
    match_idx <- match(modal_deciles, params_clean$decile)
    avg_sizes <- params_clean$avg_unit_sqm[match_idx]
    units <- pmax(1, floor((area_sqm * 0.6) / avg_sizes))
  }

  # Compute per-block unit costs
  match_idx <- match(modal_deciles, params_clean$decile)
  land_costs <- params_clean$land_cost_sqm[match_idx]
  constr_costs <- params_clean$constr_cost_sqm[match_idx]
  avg_units <- params_clean$avg_unit_sqm[match_idx]

  land_share <- area_sqm / units
  hc_per_unit <- (land_share * land_costs) + (avg_units * constr_costs)

  # City-wide weighted average
  total_units <- sum(units)
  city_avg_cost <- sum(hc_per_unit * units) / total_units

  res <- data.frame(
    indicator = "Housing cost",
    fclass = "average housing cost",
    value = round(city_avg_cost, 2),
    units = paste0(currency, "/unit"),
    stringsAsFactors = FALSE
  )

  if (spatial) {
    zones_out <- residential_zones
    zones_out$modal_decile <- modal_deciles
    zones_out$estimated_units <- units
    zones_out$housing_cost_unit <- round(hc_per_unit, 2)
    return(zones_out)
  }

  res
}
