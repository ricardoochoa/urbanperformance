#' Walkability Index
#'
#' This function calculates a composite spatial walkability index reflecting pedestrian
#' accessibility based on intersection density, transit stop density, and land-use mix diversity.
#'
#' METHOD:
#' Walkability is evaluated by discretizing the urban footprint into a regular spatial grid
#' (default 500m x 500m). For each cell, three pillars of pedestrian environments are quantified:
#' 1. Street connectivity: density of street intersections per square kilometer.
#' 2. Transit access: density of public transit boarding points per square kilometer.
#' 3. Land use mix: Shannon entropy of active land-use categories or destination types:
#'    \deqn{LUE = \frac{-\sum_{k=1}^n P_k \ln(P_k)}{\ln(n)}}
#' Each pillar is standardized into a z-score across all cells in the urban area, and the
#' final index is generated using policy weights (default: 0.33 intersection, 0.33 transit,
#' 0.34 land-use mix):
#'    \deqn{Score_j = w_1 z_{ID, j} + w_2 z_{TD, j} + w_3 z_{LUE, j}}
#'
#' @param roads `sf` lines collection representing the street network.
#' @param land_cover `RasterLayer` representing land-use / land-cover classifications.
#'   Can be NULL if `amenities` point collection is provided.
#' @param transit_stops `sf` points representing public transit stations and bus stops.
#' @param footprint `RasterLayer` or `sf` polygon representing the urban footprint boundary.
#' @param grid_size_m Numeric. Resolution of spatial analysis grid in meters (default: 500).
#' @param weights Numeric vector of length 3 for intersection density, transit density,
#'   and land use entropy weights (default: c(0.33, 0.33, 0.34)).
#' @param amenities Optional `sf` points of urban destinations/amenities with column `fclass`.
#'   Used to calculate destination entropy if `land_cover` is NULL or lacks urban subcategories.
#' @param spatial Logical. If TRUE, returns the spatial grid `sf` collection with computed metrics.
#'
#' @return A tidy `data.frame` containing the city-wide mean walkability z-score and the
#'   percentage of urban area with above-average walkability.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' # walkability_index(roads.cun, land_cover, transport_stops, footprint)
#' }
walkability_index <- function(roads,
                              land_cover = NULL,
                              transit_stops,
                              footprint,
                              grid_size_m = 500,
                              weights = c(0.33, 0.33, 0.34),
                              amenities = NULL,
                              spatial = FALSE) {
  if (!inherits(roads, "sf")) {
    stop("Input 'roads' must be an 'sf' object.")
  }

  if (!inherits(transit_stops, "sf")) {
    stop("Input 'transit_stops' must be an 'sf' object.")
  }

  if (length(weights) != 3) {
    stop("'weights' must be a numeric vector of length 3.")
  }

  w <- weights / sum(weights)

  # Standardize CRS to Web Mercator (EPSG:3857) for metric grid operations
  roads_metric <- sf::st_transform(roads, crs = 3857)
  transit_metric <- sf::st_transform(transit_stops, crs = 3857)

  # Generate analysis grid over urban footprint
  if (inherits(footprint, "RasterLayer")) {
    fp_poly <- sf::st_as_sfc(sf::st_bbox(raster::projectExtent(footprint, crs = sp::CRS("+init=epsg:3857"))))
    grid_all <- sf::st_make_grid(fp_poly, cellsize = grid_size_m)
    grid_sf <- sf::st_sf(grid_id = seq_along(grid_all), geometry = grid_all, crs = 3857)

    # Filter cells containing urban footprint
    centroids <- sf::st_centroid(grid_sf)
    centroids_orig <- sf::st_transform(centroids, crs = sf::st_crs(raster::crs(footprint)))
    fp_vals <- raster::extract(footprint, centroids_orig)
    keep_idx <- which(!is.na(fp_vals) & fp_vals > 0)

    if (length(keep_idx) == 0) {
      # Fallback to bbox if extract yielded no matching centroids
      keep_idx <- seq_len(min(100, nrow(grid_sf)))
    }
    grid_cells <- grid_sf[keep_idx, ]
  } else if (inherits(footprint, "sf")) {
    fp_metric <- sf::st_transform(footprint, crs = 3857)
    fp_union <- sf::st_union(fp_metric)
    grid_all <- sf::st_make_grid(fp_union, cellsize = grid_size_m)
    grid_sf <- sf::st_sf(grid_id = seq_along(grid_all), geometry = grid_all, crs = 3857)
    intersects <- sf::st_intersects(grid_sf, fp_union, sparse = FALSE)
    grid_cells <- grid_sf[intersects[, 1], ]
  } else {
    stop("Input 'footprint' must be either a 'RasterLayer' or an 'sf' object.")
  }

  n_cells <- nrow(grid_cells)
  if (n_cells == 0) {
    stop("No analysis grid cells intersect the provided footprint.")
  }

  grid_cells$area_km2 <- as.numeric(sf::st_area(grid_cells)) / 1e6

  # 1. Intersection Density
  # Extract unique junction points
  inter <- sf::st_intersection(roads_metric)
  inter <- inter[sf::st_is(inter, c("POINT", "MULTIPOINT")), ]
  if (nrow(inter) > 0) {
    inter_pts <- unique(sf::st_geometry(inter))
    pts_in_cell <- lengths(sf::st_intersects(grid_cells, inter_pts))
  } else {
    pts_in_cell <- rep(0, n_cells)
  }
  grid_cells$intersection_density <- pts_in_cell / grid_cells$area_km2

  # 2. Transit Density
  stops_in_cell <- lengths(sf::st_intersects(grid_cells, transit_metric))
  grid_cells$transit_density <- stops_in_cell / grid_cells$area_km2

  # 3. Land Use / Amenity Entropy
  calc_entropy <- function(counts) {
    counts <- counts[!is.na(counts) & counts > 0]
    n_cat <- length(counts)
    if (n_cat <= 1) return(0)
    p <- counts / sum(counts)
    -sum(p * log(p)) / log(n_cat)
  }

  entropy_vals <- numeric(n_cells)

  if (!is.null(land_cover) && inherits(land_cover, "RasterLayer")) {
    grid_orig <- sf::st_transform(grid_cells, crs = sf::st_crs(raster::crs(land_cover)))
    lc_extracted <- raster::extract(land_cover, grid_orig)
    entropy_vals <- vapply(lc_extracted, function(vals) {
      if (length(vals) == 0 || all(is.na(vals))) return(0)
      tbl <- table(vals)
      calc_entropy(as.numeric(tbl))
    }, numeric(1))
  } else if (!is.null(amenities) && inherits(amenities, "sf")) {
    amen_metric <- sf::st_transform(amenities, crs = 3857)
    fclass_col <- intersect(c("fclass", "type", "category"), names(amen_metric))[1]
    if (!is.na(fclass_col)) {
      inter_amen <- sf::st_join(amen_metric, grid_cells["grid_id"])
      tbl_list <- split(inter_amen[[fclass_col]], inter_amen$grid_id)
      for (gid in names(tbl_list)) {
        idx <- which(grid_cells$grid_id == as.integer(gid))
        if (length(idx) > 0) {
          entropy_vals[idx] <- calc_entropy(as.numeric(table(tbl_list[[gid]])))
        }
      }
    }
  }

  grid_cells$land_use_entropy <- entropy_vals

  # Standardization to Z-scores across spatial units
  calc_z <- function(vec) {
    s <- stats::sd(vec, na.rm = TRUE)
    if (is.na(s) || s == 0) return(rep(0, length(vec)))
    (vec - mean(vec, na.rm = TRUE)) / s
  }

  grid_cells$z_id <- calc_z(grid_cells$intersection_density)
  grid_cells$z_td <- calc_z(grid_cells$transit_density)
  grid_cells$z_lue <- calc_z(grid_cells$land_use_entropy)

  grid_cells$walkability_score <- (w[1] * grid_cells$z_id) +
    (w[2] * grid_cells$z_td) +
    (w[3] * grid_cells$z_lue)

  mean_score <- mean(grid_cells$walkability_score, na.rm = TRUE)
  high_walk_pct <- (sum(grid_cells$walkability_score > 0, na.rm = TRUE) / n_cells) * 100

  res <- data.frame(
    indicator = rep("Walkability Index", 2),
    fclass = c("mean_score", "high_walkability_area"),
    value = c(round(mean_score, 2), round(high_walk_pct, 2)),
    units = c("z-score", "%"),
    stringsAsFactors = FALSE
  )

  if (spatial) {
    return(grid_cells)
  }

  res
}
