# ==============================================================================
# Module: Launch & Project Selection Modal
# ==============================================================================

launchModalUI <- function(id) {
  ns <- NS(id)
  tagList()
}

launchModalServer <- function(id, project_state, active_project_path) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Check if a project was passed via options
    initial_path <- getOption("urbanperformance.active_project", default = NULL)

    observe({
      if (!is.null(initial_path) && dir.exists(initial_path)) {
        tryCatch({
          st <- load_project_state(initial_path)
          project_state(st)
          active_project_path(initial_path)
        }, error = function(e) {
          showLaunchModal()
        })
      } else {
        showLaunchModal()
      }
    })

    showLaunchModal <- function() {
      showModal(
        modalDialog(
          title = div(
            style = "display: flex; align-items: center; gap: 10px;",
            tags$span(style = "font-size: 24px; color: #556270; font-weight: 700;", "Urban Performance"),
            tags$span(style = "background: #4ECDC4; color: white; padding: 3px 8px; border-radius: 4px; font-size: 12px; font-weight: 600;", "EXPLORER")
          ),
          size = "l",
          easyClose = FALSE,
          footer = NULL,
          div(
            style = "padding: 10px 0;",
            p(
              style = "color: #556270; font-size: 15px; margin-bottom: 25px;",
              "Welcome to the Urban Performance Explorer. Select a workflow to begin analyzing urban scenarios, validating spatial layers, and generating parameterized briefs:"
            ),
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 18px;",
              # Option C: Demo Dataset (Highlighted)
              div(
                style = "border: 2px solid #4ECDC4; border-radius: 8px; padding: 20px; background: #FAFDFD; display: flex; flex-direction: column; justify-content: space-between;",
                div(
                  div(
                    style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                    tags$span(style = "font-weight: 700; color: #556270; font-size: 16px;", "Cancún Demo"),
                    tags$span(style = "background: #C7F464; color: #333; font-size: 10px; font-weight: 700; padding: 2px 6px; border-radius: 3px;", "READY")
                  ),
                  p(
                    style = "font-size: 13px; color: #666; line-height: 1.4;",
                    "Instantly load pre-configured spatial data for Cancún (Pok Ta Pok), including 2025/2030 footprints, transit stops, cycle tracks, and hazard zones."
                  )
                ),
                actionButton(
                  ns("btn_load_demo"),
                  "Explore Cancún Demo",
                  class = "btn-primary",
                  style = "background-color: #4ECDC4; border-color: #4ECDC4; color: white; font-weight: 600; width: 100%; margin-top: 15px;"
                )
              ),
              # Option A: Create New Project
              div(
                style = "border: 1px solid #DCE1E3; border-radius: 8px; padding: 20px; background: white; display: flex; flex-direction: column; justify-content: space-between;",
                div(
                  tags$span(style = "font-weight: 700; color: #556270; font-size: 16px; margin-bottom: 10px; display: block;", "New Project"),
                  p(
                    style = "font-size: 13px; color: #666; line-height: 1.4;",
                    "Scaffold a standardized project directory with /data/raw, /data/processed, /exports, and a metadata tracker on your local system."
                  ),
                  textInput(ns("new_proj_name"), "Project Name", value = "Monterrey_Transit", width = "100%"),
                  textInput(ns("new_proj_path"), "Directory Path", value = file.path(tempdir(), "Monterrey_Transit"), width = "100%")
                ),
                actionButton(
                  ns("btn_create_project"),
                  "Initialize Workspace",
                  class = "btn-outline-secondary",
                  style = "color: #556270; border-color: #556270; font-weight: 600; width: 100%; margin-top: 15px;"
                )
              ),
              # Option B: Open Existing Project
              div(
                style = "border: 1px solid #DCE1E3; border-radius: 8px; padding: 20px; background: white; display: flex; flex-direction: column; justify-content: space-between;",
                div(
                  tags$span(style = "font-weight: 700; color: #556270; font-size: 16px; margin-bottom: 10px; display: block;", "Existing Project"),
                  p(
                    style = "font-size: 13px; color: #666; line-height: 1.4;",
                    "Open a saved project directory or resume an incomplete session from a serialized .up / project_state.json configuration."
                  ),
                  textInput(ns("open_proj_path"), "Existing Path or .up File", placeholder = "/path/to/project", width = "100%")
                ),
                actionButton(
                  ns("btn_open_project"),
                  "Open Project",
                  class = "btn-outline-secondary",
                  style = "color: #556270; border-color: #556270; font-weight: 600; width: 100%; margin-top: 15px;"
                )
              )
            )
          )
        )
      )
    }

    # Handler: Load Demo Project
    observeEvent(input$btn_load_demo, {
      removeModal()
      showNotification("Configuring Cancún Pok Ta Pok demo dataset...", type = "message", duration = 3)
      demo_dir <- create_demo_project()
      st <- load_project_state(demo_dir)
      project_state(st)
      active_project_path(demo_dir)
      showNotification("Cancún demo loaded successfully!", type = "default", duration = 4)
    })

    # Handler: Create New Project
    observeEvent(input$btn_create_project, {
      req(input$new_proj_path)
      path <- trimws(input$new_proj_path)
      pname <- trimws(input$new_proj_name)
      if (nzchar(path)) {
        tryCatch({
          dir_init <- init_project(path, project_name = pname)
          st <- load_project_state(dir_init)
          project_state(st)
          active_project_path(dir_init)
          removeModal()
          showNotification(sprintf("Initialized project: %s", pname), type = "message", duration = 4)
        }, error = function(e) {
          showNotification(paste("Error creating project:", e$message), type = "error")
        })
      }
    })

    # Handler: Open Existing Project
    observeEvent(input$btn_open_project, {
      req(input$open_proj_path)
      path <- trimws(input$open_proj_path)
      if (nzchar(path)) {
        tryCatch({
          st <- load_project_state(path)
          project_state(st)
          active_project_path(st$project_path %||% path)
          removeModal()
          showNotification(sprintf("Loaded project: %s", st$project_name %||% "Urban Project"), type = "message", duration = 4)
        }, error = function(e) {
          showNotification(paste("Error loading project:", e$message), type = "error")
        })
      }
    })
  })
}
