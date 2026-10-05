#' Initialize a Standardized Urban Performance Project Directory
#'
#' Scaffolds a reproducible project directory structure for spatial data
#' analysis, Scoping Wizard validation, and automated reporting.
#'
#' @param path Character. File path where the project directory should be initialized.
#' @param project_name Optional character string. If NULL, inferred from `basename(path)`.
#' @param overwrite Logical. If TRUE, re-initializes `project_state.json` if it already exists.
#'
#' @return The normalized absolute path to the initialized project directory (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' init_project("projects/Cancun_PokTaPok")
#' }
init_project <- function(path, project_name = NULL, overwrite = FALSE) {
  if (missing(path) || is.null(path) || nchar(trimws(path)) == 0) {
    stop("Please specify a valid path to initialize the project.")
  }

  dir_path <- normalizePath(path, mustWork = FALSE)
  if (is.null(project_name)) {
    project_name <- basename(dir_path)
  }

  # Required subdirectories
  dirs_to_create <- c(
    file.path(dir_path, "data", "raw"),
    file.path(dir_path, "data", "processed"),
    file.path(dir_path, "exports"),
    file.path(dir_path, "metadata")
  )

  for (d in dirs_to_create) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
    }
  }

  metadata_file <- file.path(dir_path, "metadata", "project_state.json")
  up_file <- file.path(dir_path, paste0(project_name, ".up"))

  if (!file.exists(metadata_file) || overwrite) {
    # Load default indicator schema if available
    schema_path <- system.file("schema", "indicator_dependencies.json", package = "urbanperformance")
    all_indicators <- character(0)
    if (file.exists(schema_path)) {
      schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
      all_indicators <- sapply(schema$indicators, function(x) x$id)
    }

    initial_state <- list(
      project_name = project_name,
      project_path = dir_path,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      target_indicators = all_indicators,
      layers = list(),
      parameters = list(
        transit_buffer_m = 800,
        amenities_buffer_m = 500,
        cycle_buffer_m = 500,
        jobs_buffer_m = 1000,
        rural_buffer_km = 5.0,
        grid_size_m = 500
      ),
      scenarios = list(
        base = list(name = "Baseline", year = 2025),
        horizon = list(name = "Horizon Scenario", year = 2030)
      ),
      metadata = list(
        version = "0.2.0",
        crs = "EPSG:32616",
        notes = ""
      )
    )

    json_content <- jsonlite::toJSON(initial_state, pretty = TRUE, auto_unbox = TRUE)
    writeLines(json_content, con = metadata_file)
    writeLines(json_content, con = up_file)
    message(sprintf("[UrbanPerformance] Project initialized successfully at: %s", dir_path))
  } else {
    message(sprintf("[UrbanPerformance] Project already exists at: %s (use overwrite = TRUE to reset)", dir_path))
  }

  invisible(dir_path)
}

#' Load Project State from Directory or .up File
#'
#' @param project Character path to project directory or `.up` file.
#' @return A list containing the deserialized project state.
#' @export
load_project_state <- function(project) {
  if (missing(project) || is.null(project)) {
    stop("Please specify a project directory or .up file to load.")
  }

  if (file.exists(project) && !dir.exists(project)) {
    # Direct .up or .json file
    state_file <- project
    project_dir <- dirname(project)
    if (basename(project_dir) == "metadata") {
      project_dir <- dirname(project_dir)
    }
  } else if (dir.exists(project)) {
    project_dir <- project
    state_file <- file.path(project, "metadata", "project_state.json")
    if (!file.exists(state_file)) {
      up_files <- list.files(project, pattern = "\\.up$", full.names = TRUE)
      if (length(up_files) > 0) {
        state_file <- up_files[1]
      } else {
        stop("No project_state.json or .up file found in project directory.")
      }
    }
  } else {
    stop(sprintf("Project path does not exist: %s", project))
  }

  state <- jsonlite::fromJSON(state_file, simplifyVector = FALSE)
  state$project_path <- normalizePath(project_dir, mustWork = FALSE)
  state
}

#' Save Project State to Directory and .up File
#'
#' @param project_dir Character. Root path of the project.
#' @param state List. Reactive state object to serialize.
#' @return Normalized path to saved metadata file.
#' @export
save_project_state <- function(project_dir, state) {
  if (!dir.exists(project_dir)) {
    dir.create(file.path(project_dir, "metadata"), recursive = TRUE, showWarnings = FALSE)
  }

  state$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  metadata_file <- file.path(project_dir, "metadata", "project_state.json")
  proj_n <- if (!is.null(state$project_name) && nzchar(state$project_name)) state$project_name else "project"
  up_file <- file.path(project_dir, paste0(proj_n, ".up"))

  json_content <- jsonlite::toJSON(state, pretty = TRUE, auto_unbox = TRUE)
  writeLines(json_content, con = metadata_file)
  writeLines(json_content, con = up_file)

  invisible(metadata_file)
}
