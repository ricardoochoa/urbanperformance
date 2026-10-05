# ==============================================================================
# Urban Performance Explorer - server.R
# ==============================================================================

server <- function(input, output, session) {
  # Master Reactive State Objects
  project_state <- reactiveVal(NULL)
  active_project_path <- reactiveVal(NULL)
  simulation_results <- reactiveVal(NULL)

  # Module Server Activations
  launchModalServer("launch", project_state, active_project_path)
  scopingWizardServer("scoping", project_state, active_project_path, session)
  workshopViewServer("workshop", project_state, active_project_path, simulation_results)
  deepDiveServer("matrix", project_state, simulation_results)
  reportingServer("reporting", project_state, simulation_results)
}
