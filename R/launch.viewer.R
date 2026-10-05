#' Launch the Urban Performance Explorer Shiny Application
#'
#' Initiates the interactive, map-centric Shiny application for spatial scoping,
#' scenario comparison, and parameterized reporting.
#'
#' @param project Optional character string specifying the path to a project directory
#'   or a `.up` project configuration file. If NULL (default), the app launches with
#'   an onboarding modal to create, open, or explore a demo project.
#' @param host Character string for IP address to listen on (default: "127.0.0.1").
#' @param port Optional integer port number.
#' @param launch.browser Logical. Whether to launch the browser automatically (default: TRUE).
#' @param ... Additional arguments passed to \code{shiny::runApp()}.
#'
#' @return Runs the Shiny application (returns invisible NULL upon exit).
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch with onboarding dialog
#' launch_viewer()
#'
#' # Launch directly with an existing project
#' launch_viewer("projects/Cancun_PokTaPok")
#' }
launch_viewer <- function(project = NULL,
                          host = "127.0.0.1",
                          port = NULL,
                          launch.browser = TRUE,
                          ...) {
  # Verify required suggested packages
  required_pkgs <- c("shiny", "bslib", "leaflet", "visNetwork", "DT", "terra", "sf", "jsonlite")
  missing_pkgs <- character(0)
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg)
    }
  }

  if (length(missing_pkgs) > 0) {
    stop(
      sprintf(
        "The Urban Performance Explorer requires the following packages: %s.\nPlease install them via: install.packages(c(%s))",
        paste(missing_pkgs, collapse = ", "),
        paste(sprintf('"%s"', missing_pkgs), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  app_dir <- system.file("app", package = "urbanperformance")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    # Fallback to local dev path if run from source package
    app_dir <- file.path(getwd(), "inst", "app")
    if (!dir.exists(app_dir)) {
      stop("Could not locate the Shiny app directory. Please verify that urbanperformance is properly installed.", call. = FALSE)
    }
  }

  if (!is.null(project) && nzchar(trimws(project))) {
    options(urbanperformance.active_project = normalizePath(project, mustWork = FALSE))
  } else {
    options(urbanperformance.active_project = NULL)
  }

  shiny::runApp(
    appDir = app_dir,
    host = host,
    port = port,
    launch.browser = launch.browser,
    ...
  )
}
