#' Anthropogenic Hazard Exposure
#'
#' This function calculates the population exposed to localized anthropogenic and
#' industrial hazards by generating outreach buffers and intersecting them with
#' population distribution grids.
#'
#' METHOD:
#' Outreach perimeters are determined based on hazard type (`fclass`) using
#' regulated buffer distances from `p.distances` (such as aquifer protection in
#' karst regions or industrial safety zones). To prevent double-counting
#' individuals living within overlapping plumes, buffers are dissolved via
#' spatial union operations before rasterization and population extraction.
#'
#' @param pollution_sources `sf` points or polygons containing a column named `fclass`.
#' @param pop `RasterLayer` containing population distribution.
#' @param p_distances Optional data frame containing threshold distances with columns
#'   `fclass` and `value` (or `buffer_distance_m`). If NULL, package data `p.distances` is used.
#' @param spatial Logical. If TRUE, returns the unioned buffer geometries with exposure metrics.
#'
#' @return A tidy `data.frame` containing exposed inhabitants and percentages per hazard class
#'   and for the combined hazard footprint.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(raster)
#' pop_b <- raster(system.file("extdata", "POP_2025.tif", package = "urbanperformance"))
#' # anthropogenic_hazard(pollution_sources, pop_b)
#' }
anthropogenic_hazard <- function(pollution_sources,
                                 pop,
                                 p_distances = NULL,
                                 spatial = FALSE) {
  if (!inherits(pollution_sources, "sf")) {
    stop("Input 'pollution_sources' must be an 'sf' object.")
  }

  if (!inherits(pop, "RasterLayer")) {
    stop("Input 'pop' must be a 'RasterLayer'.")
  }

  if (!"fclass" %in% names(pollution_sources)) {
    stop("'pollution_sources' must contain an 'fclass' attribute column.")
  }

  # Load lookup table if not supplied
  if (is.null(p_distances)) {
    utils::data("p.distances", package = "urbanperformance", envir = environment())
    p_dist_df <- get("p.distances", envir = environment())
  } else {
    p_dist_df <- p_distances
  }

  # Normalize column names in lookup table
  if ("buffer_distance_m" %in% names(p_dist_df) && !"value" %in% names(p_dist_df)) {
    names(p_dist_df)[names(p_dist_df) == "buffer_distance_m"] <- "value"
  }

  if (!all(c("fclass", "value") %in% names(p_dist_df))) {
    stop("Lookup table must contain 'fclass' and 'value' columns.")
  }

  # Transform pollution sources to match pop raster CRS
  pop_crs <- raster::crs(pop)
  if (!sf::st_crs(pollution_sources) == sf::st_crs(pop_crs)) {
    pollution_sources <- sf::st_transform(pollution_sources, crs = sf::st_crs(pop_crs))
  }

  # Total population calculation
  total_pop <- raster::cellStats(pop, stat = "sum", na.rm = TRUE)
  if (is.na(total_pop) || total_pop <= 0) {
    stop("Total population in 'pop' raster must be greater than zero.")
  }

  hazard_classes <- unique(pollution_sources$fclass)
  hazard_classes <- hazard_classes[!is.na(hazard_classes)]

  results_list <- list()
  unioned_buffers <- list()

  for (h_class in hazard_classes) {
    src_sub <- pollution_sources[pollution_sources$fclass == h_class, ]
    dist_val <- p_dist_df$value[p_dist_df$fclass == h_class]

    if (length(dist_val) == 0 || is.na(dist_val[1])) {
      warning(paste0("Hazard class '", h_class, "' not found in lookup table. Using default 500m."))
      dist_val <- 500
    } else {
      dist_val <- dist_val[1]
    }

    # Buffer and dissolve/union to eliminate internal overlaps
    buff <- sf::st_buffer(src_sub, dist = dist_val)
    buff_union <- sf::st_union(buff)

    buff_sf <- sf::st_sf(fclass = h_class, geometry = sf::st_sfc(buff_union), crs = sf::st_crs(src_sub))
    unioned_buffers[[h_class]] <- buff_sf

    # Rasterize buffer onto pop grid
    r_mask <- raster::rasterize(buff_sf, pop, field = 1, background = 0)
    pop_exposed <- raster::cellStats(r_mask * pop, stat = "sum", na.rm = TRUE)
    exp_pct <- (pop_exposed / total_pop) * 100

    results_list[[h_class]] <- data.frame(
      indicator = rep("Population exposed to risk", 2),
      fclass = rep(h_class, 2),
      value = c(round(pop_exposed, 0), round(exp_pct, 2)),
      units = c("inhabitants", "%"),
      stringsAsFactors = FALSE
    )
  }

  # Combined multi-hazard footprint
  if (length(unioned_buffers) > 0) {
    all_buffs <- do.call(rbind, unioned_buffers)
    total_union <- sf::st_union(all_buffs)
    total_sf <- sf::st_sf(fclass = "total_anthropogenic", geometry = sf::st_sfc(total_union), crs = sf::st_crs(all_buffs))

    r_mask_tot <- raster::rasterize(total_sf, pop, field = 1, background = 0)
    pop_tot_exposed <- raster::cellStats(r_mask_tot * pop, stat = "sum", na.rm = TRUE)
    exp_tot_pct <- (pop_tot_exposed / total_pop) * 100

    results_list[["total"]] <- data.frame(
      indicator = rep("Population exposed to risk", 2),
      fclass = rep("total_anthropogenic", 2),
      value = c(round(pop_tot_exposed, 0), round(exp_tot_pct, 2)),
      units = c("inhabitants", "%"),
      stringsAsFactors = FALSE
    )
  }

  out_df <- do.call(rbind, results_list)
  rownames(out_df) <- NULL

  if (spatial) {
    combined_sf <- do.call(rbind, unioned_buffers)
    return(combined_sf)
  }

  out_df
}
