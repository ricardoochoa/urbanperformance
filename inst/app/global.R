# ==============================================================================
# Urban Performance Explorer - global.R
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(leaflet.extras2)
  library(visNetwork)
  library(DT)
  library(plotly)
  library(terra)
  library(sf)
  library(jsonlite)
  library(urbanperformance)
})

# ------------------------------------------------------------------------------
# Color Token Palette & Accessible Neutrals
# ------------------------------------------------------------------------------
COLOR_MIGHTY_SLATE <- "#556270"  # Master brand & typography
COLOR_PACIFICA     <- "#4ECDC4"  # Positive / Satisfied / Infill
COLOR_APPLE_CHIC   <- "#C7F464"  # Partial fulfillment / Missing attributes
COLOR_CHEERY_PINK  <- "#FF6B6B"  # Missing data / Warning
COLOR_GRANDMA      <- "#C44D58"  # High hazard / Risk / Degradation
COLOR_BG_GRAY      <- "#F8F9FA"  # Grayscale base background
COLOR_BORDER       <- "#DCE1E3"  # Subtle slate borders
COLOR_TEXT_DARK    <- "#212529"  # High contrast text (>6:1 on Pacifica & Apple Chic)
COLOR_TEXT_MUTED   <- "#6C757D"  # Muted secondary text

# ------------------------------------------------------------------------------
# Load Master Dependency Schema
# ------------------------------------------------------------------------------
schema_file <- system.file("schema", "indicator_dependencies.json", package = "urbanperformance")
if (!file.exists(schema_file)) {
  schema_file <- file.path("..", "schema", "indicator_dependencies.json")
}

if (file.exists(schema_file)) {
  INDICATOR_SCHEMA <- jsonlite::fromJSON(schema_file, simplifyVector = FALSE)
} else {
  INDICATOR_SCHEMA <- list(layers = list(), indicators = list())
}

# ------------------------------------------------------------------------------
# Helper: Granular Layer Inspector (terra & sf)
# ------------------------------------------------------------------------------
inspect_layer_metadata <- function(file_path, expected_layer_def) {
  res <- list(
    file_path = file_path,
    status = "missing",
    missing_attributes = list(),
    crs = NA_character_,
    extent = NULL,
    geom_type = NULL,
    attributes = character(0)
  )

  if (!file.exists(file_path)) {
    return(res)
  }

  is_raster <- identical(expected_layer_def$type, "raster")

  tryCatch({
    if (is_raster) {
      r <- terra::rast(file_path)
      res$crs <- terra::crs(r, proj = TRUE)
      ext <- as.vector(terra::ext(r))
      res$extent <- ext
      res$geom_type <- "raster"
      res$status <- "ready"
      res$missing_attributes <- list()
    } else {
      # Vector file via sf
      layers_info <- sf::st_layers(file_path)
      v <- sf::st_read(file_path, quiet = TRUE)
      res$crs <- sf::st_crs(v)$input %||% as.character(sf::st_crs(v)$epsg)
      res$extent <- as.vector(sf::st_bbox(v))
      res$geom_type <- as.character(sf::st_geometry_type(v, by_geometry = FALSE))
      res$attributes <- names(v)

      required_attrs <- expected_layer_def$required_attributes
      missing_attrs <- character(0)
      if (length(required_attrs) > 0) {
        cols_lower <- tolower(names(v))
        for (req in required_attrs) {
          if (!tolower(req) %in% cols_lower) {
            missing_attrs <- c(missing_attrs, req)
          }
        }
      }

      if (length(missing_attrs) == 0) {
        res$status <- "ready"
        res$missing_attributes <- list()
      } else {
        res$status <- "partial"  # Apple Chic: geometry present, attributes missing
        res$missing_attributes <- as.list(missing_attrs)
      }
    }
  }, error = function(e) {
    res$status <- "missing"
    res$error <- e$message
  })

  res
}

# ------------------------------------------------------------------------------
# Helper: Generate LLM Scoping Prompt for Client Email Bridge
# ------------------------------------------------------------------------------
generate_scoping_prompt <- function(project_state, schema = INDICATOR_SCHEMA) {
  missing_layers <- list()
  partial_layers <- list()

  for (l_def in schema$layers) {
    layer_id <- l_def$id
    l_state <- project_state$layers[[layer_id]]

    if (is.null(l_state) || identical(l_state$status, "missing")) {
      missing_layers[[length(missing_layers) + 1]] <- list(
        id = layer_id,
        label = l_def$label,
        type = l_def$type,
        format = paste(l_def$format, collapse = "/"),
        description = l_def$description,
        required_attributes = l_def$required_attributes
      )
    } else if (identical(l_state$status, "partial")) {
      partial_layers[[length(partial_layers) + 1]] <- list(
        id = layer_id,
        label = l_def$label,
        missing_attrs = unlist(l_state$missing_attributes),
        description = l_def$description
      )
    }
  }

  prompt <- paste0(
    "You are a Senior Urban Planner and Spatial Data Lead for the Urban Performance project '",
    project_state$project_name %||% "Urban Assessment", "'.\n\n",
    "Please draft a polite, highly professional, and technically precise data request email addressed to the municipal client / GIS counterpart.\n\n",
    "CONTEXT:\n",
    "We have reviewed the spatial repository for the study area. To proceed with the calculation of the contractual urban sustainability indicators, we require the following items:\n\n"
  )

  if (length(missing_layers) > 0) {
    prompt <- paste0(prompt, "### 1. Completely Missing Spatial Layers:\n")
    for (m in missing_layers) {
      req_str <- if (length(m$required_attributes) > 0) {
        paste0(" (Must include columns: ", paste(m$required_attributes, collapse = ", "), ")")
      } else ""
      prompt <- paste0(prompt, "- **", m$label, "** [Format: ", m$format, "]: ", m$description, req_str, "\n")
    }
    prompt <- paste0(prompt, "\n")
  }

  if (length(partial_layers) > 0) {
    prompt <- paste0(prompt, "### 2. Layers Received with Missing Attribute Fields (Partial Fulfillment):\n")
    for (p in partial_layers) {
      prompt <- paste0(prompt, "- **", p$label, "**: File received, but missing required tabular field(s): `",
                       paste(p$missing_attrs, collapse = "`, `"), "`. Please provide an updated shapefile or crosswalk table containing these classifications.\n")
    }
    prompt <- paste0(prompt, "\n")
  }

  prompt <- paste0(
    prompt,
    "TONE & INSTRUCTIONS:\n",
    "- Thank the client for the materials delivered so far.\n",
    "- Emphasize that providing these specific geometries and attribute classifications enables the scenario evaluation (active mobility, ecological protection, housing affordability, and risk exposure).\n",
    "- Offer to schedule a brief 15-minute technical check-in if data conversions or projection coordinate systems require coordination."
  )

  prompt
}

# ------------------------------------------------------------------------------
# Helper: Safe operator for fallback
# ------------------------------------------------------------------------------
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (is.character(a) && !nzchar(a[1]))) b else a
}

# ------------------------------------------------------------------------------
# Source Application Modules
# ------------------------------------------------------------------------------
module_files <- list.files("modules", pattern = "\\.R$", full.names = TRUE)
for (f in module_files) {
  source(f, local = FALSE)
}
