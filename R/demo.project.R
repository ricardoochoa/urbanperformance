#' Create or Load Built-in Cancún Demo Project
#'
#' Sets up a complete demo project using the Cancún spatial datasets
#' bundled with `urbanperformance`.
#'
#' @param target_dir Optional character path. If NULL, creates a directory in `tempdir()`.
#'
#' @return Normalized path to the initialized demo project directory.
#' @export
create_demo_project <- function(target_dir = NULL) {
  if (is.null(target_dir) || !nzchar(target_dir)) {
    target_dir <- file.path(tempdir(), "cancun_urban_assessment")
  }

  init_project(target_dir, project_name = "Cancun_Urban_Assessment", overwrite = TRUE)

  raw_dir <- file.path(target_dir, "data", "raw")

  # 1. Copy raster datasets from inst/extdata
  extdata_files <- c(
    "POP_2025.tif",
    "POP_2030.tif",
    "Build_up_2025.tif",
    "Build_up_2030.tif",
    "Land_cover.tif",
    "Hazard.tif",
    "Jobs.tif"
  )

  for (f in extdata_files) {
    src <- system.file("extdata", f, package = "urbanperformance")
    if (file.exists(src)) {
      file.copy(src, file.path(raw_dir, f), overwrite = TRUE)
    }
  }

  # 2. Export vector datasets bundled in package data/ as GeoJSON
  export_vector <- function(dataset_name, out_name) {
    if (exists(dataset_name, envir = asNamespace("urbanperformance"))) {
      obj <- get(dataset_name, envir = asNamespace("urbanperformance"))
    } else {
      # Try loading via utils::data
      e <- new.env()
      utils::data(list = dataset_name, package = "urbanperformance", envir = e)
      obj <- e[[dataset_name]]
    }

    if (!is.null(obj) && inherits(obj, "sf")) {
      sf::st_write(obj, file.path(raw_dir, out_name), delete_dsn = TRUE, quiet = TRUE)
    }
  }

  export_vector("roads.cun", "roads.geojson")
  export_vector("cycle.cun", "cycle_tracks.geojson")
  export_vector("transport.cun", "transit_stops.geojson")
  export_vector("amenities.cun", "amenities.geojson")
  export_vector("aoi.cun", "aoi.geojson")

  # 3. Build comprehensive initial project state with layer metadata
  state <- load_project_state(target_dir)

  state$layers <- list(
    pop_base = list(
      id = "pop_base",
      file_name = "POP_2025.tif",
      file_path = file.path(raw_dir, "POP_2025.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    pop_horizon = list(
      id = "pop_horizon",
      file_name = "POP_2030.tif",
      file_path = file.path(raw_dir, "POP_2030.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    buildup_base = list(
      id = "buildup_base",
      file_name = "Build_up_2025.tif",
      file_path = file.path(raw_dir, "Build_up_2025.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    buildup_horizon = list(
      id = "buildup_horizon",
      file_name = "Build_up_2030.tif",
      file_path = file.path(raw_dir, "Build_up_2030.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    landcover_base = list(
      id = "landcover_base",
      file_name = "Land_cover.tif",
      file_path = file.path(raw_dir, "Land_cover.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    hazard = list(
      id = "hazard",
      file_name = "Hazard.tif",
      file_path = file.path(raw_dir, "Hazard.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    jobs = list(
      id = "jobs",
      file_name = "Jobs.tif",
      file_path = file.path(raw_dir, "Jobs.tif"),
      type = "raster",
      status = "ready",
      missing_attributes = list()
    ),
    roads = list(
      id = "roads",
      file_name = "roads.geojson",
      file_path = file.path(raw_dir, "roads.geojson"),
      type = "vector",
      status = "ready",
      missing_attributes = list()
    ),
    cycle_tracks = list(
      id = "cycle_tracks",
      file_name = "cycle_tracks.geojson",
      file_path = file.path(raw_dir, "cycle_tracks.geojson"),
      type = "vector",
      status = "ready",
      missing_attributes = list()
    ),
    transit_stops = list(
      id = "transit_stops",
      file_name = "transit_stops.geojson",
      file_path = file.path(raw_dir, "transit_stops.geojson"),
      type = "vector",
      status = "ready",
      missing_attributes = list()
    ),
    amenities = list(
      id = "amenities",
      file_name = "amenities.geojson",
      file_path = file.path(raw_dir, "amenities.geojson"),
      type = "vector",
      status = "ready",
      missing_attributes = list()
    )
  )

  save_project_state(target_dir, state)
  message(sprintf("[UrbanPerformance] Demo project ready at: %s", target_dir))

  invisible(target_dir)
}
