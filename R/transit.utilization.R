#' Transit Utilization Rate
#'
#' This function assesses public transit network efficiency by comparing daily ridership
#' against operational vehicle capacity.
#'
#' METHOD:
#' Public transit performance requires balancing infrastructure supply with passenger demand.
#' The algorithm calculates route and network-level utilization by evaluating daily trips
#' against vehicle capacity:
#' \deqn{Utilization = \frac{\sum DailyRiders}{\sum (DailyTrips \times VehicleCapacity)} \times 100}
#' To reflect real-world operating conditions, both nominal capacity (seated plus comfortable standees)
#' and crush capacity (maximum physical vehicle load) are supported.
#' Outputs are classified according to municipal mobility benchmarks (e.g. IMOVEQROO):
#' \itemize{
#'   \item \bold{Underutilized}: < 30%
#'   \item \bold{Moderate}: 30% to 59%
#'   \item \bold{Optimal}: 60% to 80%
#'   \item \bold{Overcrowded}: > 85%
#' }
#'
#' @param transit_routes `sf` lines or `data.frame` representing transit routes with operational attributes:
#'   `daily_trips` (or `avg_daily_trips_per_route`), and `vehicle_capacity` (or `nominal_vehicle_capacity`).
#' @param ridership_data Optional `data.frame` mapping route IDs to `daily_riders` (or `avg_daily_ridership_per_route`).
#'   If NULL, ridership columns are extracted directly from `transit_routes`.
#' @param route_id_col Character. Column name identifying unique routes (default: "route_id").
#' @param capacity_type Character. Capacity model to evaluate: `"both"` (default), `"nominal"`, or `"crush"`.
#' @param spatial Logical. If TRUE, returns the `sf` route network with attached utilization metrics.
#'
#' @return A tidy `data.frame` containing total daily ridership, capacity, and utilization percentage.
#' @export
#'
#' @examples
#' routes <- data.frame(
#'   route_id = c("R1_BRT", "R2_BUS"),
#'   daily_trips = c(120, 80),
#'   nominal_vehicle_capacity = c(100, 45),
#'   crush_vehicle_capacity = c(150, 70),
#'   daily_riders = c(9800, 3200)
#' )
#' transit_utilization(routes)
transit_utilization <- function(transit_routes,
                                ridership_data = NULL,
                                route_id_col = "route_id",
                                capacity_type = c("both", "nominal", "crush"),
                                spatial = FALSE) {
  capacity_type <- match.arg(capacity_type)

  df_routes <- transit_routes

  # Resolve route ID column
  id_col <- intersect(c(route_id_col, "route_id", "id", "transit_mode", "route"), names(df_routes))[1]
  if (is.na(id_col)) {
    df_routes$route_id <- seq_len(nrow(df_routes))
    id_col <- "route_id"
  }

  # Join ridership data if provided separately
  if (!is.null(ridership_data) && is.data.frame(ridership_data)) {
    rider_id <- intersect(c(route_id_col, "route_id", "id", "route"), names(ridership_data))[1]
    rider_val_col <- intersect(c("daily_riders", "ridership", "avg_daily_ridership_per_route", "riders"), names(ridership_data))[1]

    if (is.na(rider_id) || is.na(rider_val_col)) {
      stop("Could not identify route ID and ridership columns in ridership_data.")
    }

    match_idx <- match(df_routes[[id_col]], ridership_data[[rider_id]])
    df_routes$daily_riders <- as.numeric(ridership_data[[rider_val_col]][match_idx])
  } else {
    rider_col <- intersect(c("daily_riders", "ridership", "avg_daily_ridership_per_route", "riders"), names(df_routes))[1]
    if (is.na(rider_col)) {
      stop("Ridership column not found in transit_routes. Supply 'ridership_data' or include 'daily_riders'.")
    }
    df_routes$daily_riders <- as.numeric(df_routes[[rider_col]])
  }

  # Resolve operational trip count
  trips_col <- intersect(c("daily_trips", "trips", "avg_daily_trips_per_route"), names(df_routes))[1]
  if (is.na(trips_col)) {
    stop("Trips column not found in transit_routes (expected 'daily_trips').")
  }
  daily_trips <- as.numeric(df_routes[[trips_col]])

  # Resolve vehicle capacities
  nom_col <- intersect(c("nominal_vehicle_capacity", "vehicle_capacity", "capacity", "nom_capacity"), names(df_routes))[1]
  crush_col <- intersect(c("crush_vehicle_capacity", "crush_capacity", "max_capacity"), names(df_routes))[1]

  if (is.na(nom_col) && is.na(crush_col)) {
    stop("Vehicle capacity columns not found in transit_routes.")
  }

  cap_nom <- if (!is.na(nom_col)) as.numeric(df_routes[[nom_col]]) else as.numeric(df_routes[[crush_col]]) * 0.7
  cap_crush <- if (!is.na(crush_col)) as.numeric(df_routes[[crush_col]]) else cap_nom * 1.4

  # Calculate route-level capacities
  route_cap_nom <- daily_trips * cap_nom
  route_cap_crush <- daily_trips * cap_crush

  df_routes$route_capacity_nominal <- route_cap_nom
  df_routes$route_capacity_crush <- route_cap_crush

  df_routes$utilization_nominal_pct <- round((df_routes$daily_riders / route_cap_nom) * 100, 2)
  df_routes$utilization_crush_pct <- round((df_routes$daily_riders / route_cap_crush) * 100, 2)

  # System-wide sums
  tot_riders <- sum(df_routes$daily_riders, na.rm = TRUE)
  tot_cap_nom <- sum(route_cap_nom, na.rm = TRUE)
  tot_cap_crush <- sum(route_cap_crush, na.rm = TRUE)

  sys_util_nom <- if (tot_cap_nom > 0) (tot_riders / tot_cap_nom) * 100 else 0
  sys_util_crush <- if (tot_cap_crush > 0) (tot_riders / tot_cap_crush) * 100 else 0

  tag_util <- function(u) {
    if (u < 30) "Underutilized"
    else if (u < 60) "Moderate"
    else if (u <= 85) "Optimal"
    else "Overcrowded"
  }

  df_routes$nominal_status <- vapply(df_routes$utilization_nominal_pct, tag_util, character(1))
  df_routes$crush_status <- vapply(df_routes$utilization_crush_pct, tag_util, character(1))

  # Build return table based on capacity_type
  rows_list <- list()
  rows_list[[1]] <- data.frame(
    indicator = "Transit utilization",
    fclass = "total_ridership",
    value = round(tot_riders, 0),
    units = "passengers/day",
    stringsAsFactors = FALSE
  )

  if (capacity_type %in% c("both", "nominal")) {
    rows_list[[length(rows_list) + 1]] <- data.frame(
      indicator = "Transit utilization",
      fclass = "nominal_capacity",
      value = round(tot_cap_nom, 0),
      units = "passengers/day",
      stringsAsFactors = FALSE
    )
    rows_list[[length(rows_list) + 1]] <- data.frame(
      indicator = "Transit utilization",
      fclass = paste0("nominal_utilization (", tag_util(sys_util_nom), ")"),
      value = round(sys_util_nom, 2),
      units = "%",
      stringsAsFactors = FALSE
    )
  }

  if (capacity_type %in% c("both", "crush")) {
    rows_list[[length(rows_list) + 1]] <- data.frame(
      indicator = "Transit utilization",
      fclass = "crush_capacity",
      value = round(tot_cap_crush, 0),
      units = "passengers/day",
      stringsAsFactors = FALSE
    )
    rows_list[[length(rows_list) + 1]] <- data.frame(
      indicator = "Transit utilization",
      fclass = paste0("crush_utilization (", tag_util(sys_util_crush), ")"),
      value = round(sys_util_crush, 2),
      units = "%",
      stringsAsFactors = FALSE
    )
  }

  out_df <- do.call(rbind, rows_list)

  if (spatial && inherits(transit_routes, "sf")) {
    transit_routes$nominal_utilization_pct <- df_routes$utilization_nominal_pct
    transit_routes$crush_utilization_pct <- df_routes$utilization_crush_pct
    transit_routes$nominal_status <- df_routes$nominal_status
    transit_routes$crush_status <- df_routes$crush_status
    return(transit_routes)
  }

  attr(out_df, "route_breakdown") <- df_routes[, c(id_col, "daily_riders", "route_capacity_nominal", "utilization_nominal_pct", "nominal_status")]
  out_df
}
