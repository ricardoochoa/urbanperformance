#' Vacant Housing Rate
#'
#' This function calculates the total number and percentage of vacant housing units
#' across an urban area using spatial rasters or vector polygon blocks.
#'
#' METHOD:
#' In urban planning, a healthy housing market maintains a frictional vacancy
#' rate of approximately 3% to 5% to support household mobility. Vacancy rates
#' below 3% signal acute supply shortages, while rates exceeding 8% to 10%
#' indicate real estate speculation, economic decline, or neighborhood blight.
#' The vacancy rate is calculated as:
#' \deqn{Rate = \frac{\sum Housing_{vacant}}{\sum Housing_{total}} \times 100}
#'
#' @param housing_total `RasterLayer` or `sf` object containing total housing units.
#'   If `sf`, must contain an attribute representing unit counts (e.g., `total_units`).
#' @param housing_vacant `RasterLayer` or `sf` object containing vacant housing units.
#'   If omitted and `housing_total` is an `sf` object containing both total and vacant columns,
#'   vacant units are extracted directly.
#' @param total_col Character. Column name in `housing_total` if using `sf` (default: "total_units").
#' @param vacant_col Character. Column name in `housing_vacant` (or `housing_total`) if using `sf` (default: "vacant_units").
#' @param spatial Logical. If TRUE, returns the spatial layer with calculated vacancy rates.
#'
#' @return A tidy `data.frame` containing total vacant units and the vacancy rate percentage.
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' # vacant_housing(r_total_hu, r_vacant_hu)
#' }
vacant_housing <- function(housing_total,
                           housing_vacant = NULL,
                           total_col = "total_units",
                           vacant_col = "vacant_units",
                           spatial = FALSE) {
  # Raster processing branch
  if (inherits(housing_total, "RasterLayer")) {
    if (is.null(housing_vacant) || !inherits(housing_vacant, "RasterLayer")) {
      stop("When 'housing_total' is a RasterLayer, 'housing_vacant' must also be a RasterLayer.")
    }

    # Align rasters if needed
    if (!raster::compareCRS(housing_total, housing_vacant) ||
        !raster::extent(housing_total) == raster::extent(housing_vacant)) {
      housing_vacant <- rasterchecker(housing_vacant, base = housing_total)[[1]]
    }

    tot_units <- raster::cellStats(housing_total, stat = "sum", na.rm = TRUE)
    vac_units <- raster::cellStats(housing_vacant, stat = "sum", na.rm = TRUE)

    tot_units <- ifelse(is.na(tot_units), 0, tot_units)
    vac_units <- ifelse(is.na(vac_units), 0, vac_units)

    rate <- if (tot_units > 0) (vac_units / tot_units) * 100 else 0

    if (spatial) {
      r_rate <- (housing_vacant / housing_total) * 100
      r_rate[housing_total == 0] <- 0
      names(r_rate) <- "vacancy_rate_pct"
      return(r_rate)
    }

  # Vector sf branch
  } else if (inherits(housing_total, "sf")) {
    sf_tot <- housing_total

    if (!is.null(housing_vacant) && inherits(housing_vacant, "sf")) {
      # Merge or match if two separate sf objects
      tot_name <- intersect(c(total_col, "total", "units", "hu_tot"), names(sf_tot))[1]
      vac_name <- intersect(c(vacant_col, "vacant", "vhu"), names(housing_vacant))[1]

      if (is.na(tot_name) || is.na(vac_name)) {
        stop("Could not identify total/vacant units columns in sf inputs.")
      }

      tot_units <- sum(as.numeric(sf_tot[[tot_name]]), na.rm = TRUE)
      vac_units <- sum(as.numeric(housing_vacant[[vac_name]]), na.rm = TRUE)
      rate <- if (tot_units > 0) (vac_units / tot_units) * 100 else 0

      if (spatial) {
        sf_tot$vacant_units <- as.numeric(housing_vacant[[vac_name]])
        sf_tot$vacancy_rate_pct <- ifelse(sf_tot[[tot_name]] > 0,
                                          (sf_tot$vacant_units / sf_tot[[tot_name]]) * 100, 0)
        return(sf_tot)
      }

    } else {
      # Columns exist within single sf object
      tot_name <- intersect(c(total_col, "total", "units", "hu_tot"), names(sf_tot))[1]
      vac_name <- intersect(c(vacant_col, "vacant", "vhu"), names(sf_tot))[1]

      if (is.na(tot_name) || is.na(vac_name)) {
        stop("Could not find both total and vacant unit columns in 'housing_total' sf object.")
      }

      tot_units <- sum(as.numeric(sf_tot[[tot_name]]), na.rm = TRUE)
      vac_units <- sum(as.numeric(sf_tot[[vac_name]]), na.rm = TRUE)
      rate <- if (tot_units > 0) (vac_units / tot_units) * 100 else 0

      if (spatial) {
        sf_tot$vacancy_rate_pct <- ifelse(sf_tot[[tot_name]] > 0,
                                          (as.numeric(sf_tot[[vac_name]]) / as.numeric(sf_tot[[tot_name]])) * 100, 0)
        return(sf_tot)
      }
    }

  } else {
    stop("Input 'housing_total' must be either a 'RasterLayer' or an 'sf' object.")
  }

  res <- data.frame(
    indicator = rep("Vacant housing", 2),
    fclass = c("vacant_units", "vacancy_rate"),
    value = c(round(vac_units, 0), round(rate, 2)),
    units = c("units", "%"),
    stringsAsFactors = FALSE
  )

  res
}
