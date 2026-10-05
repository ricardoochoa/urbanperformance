# ==============================================================================
# Module: Scoping Wizard & Data Procurement Tracker
# ==============================================================================

scopingWizardUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "container-fluid",
      style = "padding: 20px 25px;",
      # Top Header Bar
      div(
        style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #DCE1E3; padding-bottom: 15px; margin-bottom: 20px;",
        div(
          h3(style = "color: #556270; margin: 0; font-weight: 700;", textOutput(ns("proj_title"))),
          p(style = "color: #88929A; margin: 4px 0 0 0; font-size: 13px;", "Bipartite Dependency Network & Attribute Gap Tracker")
        ),
        div(
          style = "display: flex; align-items: center; gap: 15px;",
          # Autosave status badge
          div(
            style = "display: flex; align-items: center; gap: 8px; background: white; border: 1px solid #DCE1E3; padding: 6px 14px; border-radius: 20px;",
            tags$span(style = "display: inline-block; width: 9px; height: 9px; border-radius: 50%; background-color: #4ECDC4;"),
            tags$span(style = "font-size: 12px; color: #556270;", textOutput(ns("save_status_text"), inline = TRUE))
          ),
          actionButton(
            ns("btn_force_save"),
            "Force Save",
            icon = icon("save"),
            class = "btn-sm",
            style = "border-color: #556270; color: #556270; font-weight: 600;"
          ),
          actionButton(
            ns("btn_goto_workshop"),
            "Open Workshop View",
            icon = icon("map"),
            class = "btn-sm btn-primary",
            style = "background-color: #4ECDC4; border-color: #4ECDC4; color: white; font-weight: 600;"
          )
        )
      ),

      # Main Content Grid: Bipartite Graph (Left 8 cols) + Data Ingestion & Scoping Brief (Right 4 cols)
      div(
        class = "row",
        div(
          class = "col-lg-8",
          div(
            style = "background: white; border: 1px solid #DCE1E3; border-radius: 8px; padding: 15px; box-shadow: 0 2px 4px rgba(0,0,0,0.02);",
            div(
              style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
              h5(style = "color: #556270; margin: 0; font-weight: 700;", "Prerequisite Spatial Dependency Graph"),
              # Legend
              div(
                style = "display: flex; gap: 12px; font-size: 11px; font-weight: 600;",
                tags$span(tags$span(style = "color: #4ECDC4;", "● "), "Pacifica (Ready)"),
                tags$span(tags$span(style = "color: #C7F464;", "● "), "Apple Chic (Missing Attributes)"),
                tags$span(tags$span(style = "color: #FF6B6B;", "● "), "Cheery Pink (Missing Layer)")
              )
            ),
            visNetworkOutput(ns("network_graph"), height = "520px")
          )
        ),

        div(
          class = "col-lg-4",
          # Layer Ingestion & Attribute Inspector
          div(
            style = "background: white; border: 1px solid #DCE1E3; border-radius: 8px; padding: 18px; margin-bottom: 20px;",
            h5(style = "color: #556270; font-weight: 700; margin-top: 0;", "Spatial Layer Ingestion"),
            selectInput(
              ns("select_layer_target"),
              "Target Layer Concept",
              choices = NULL,
              width = "100%"
            ),
            uiOutput(ns("layer_requirement_info")),
            fileInput(
              ns("upload_spatial_file"),
              "Upload Spatial File (.tif, .geojson, .shp, .gpkg)",
              accept = c(".tif", ".geojson", ".shp", ".gpkg", ".zip"),
              width = "100%"
            ),
            uiOutput(ns("upload_feedback_badge"))
          ),

          # The Client Communication Bridge Card
          div(
            style = "background: #FDFBFA; border: 1px solid #F5D3C8; border-radius: 8px; padding: 18px;",
            div(
              style = "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;",
              tags$span(style = "color: #FF6B6B; font-size: 18px;", icon("envelope-open-text")),
              h5(style = "color: #556270; font-weight: 700; margin: 0;", "Client Scoping Bridge")
            ),
            p(
              style = "font-size: 12px; color: #666; line-height: 1.4;",
              "Automatically serialize remaining Cheery Pink missing layers and Apple Chic attribute gaps into a structured LLM prompt to draft an instant data procurement email to the municipal client."
            ),
            actionButton(
              ns("btn_generate_brief"),
              "Generate Scoping Brief",
              icon = icon("robot"),
              class = "btn-danger",
              style = "background-color: #FF6B6B; border-color: #FF6B6B; color: white; font-weight: 600; width: 100%; margin-top: 5px;"
            )
          )
        )
      )
    )
  )
}

scopingWizardServer <- function(id, project_state, active_project_path, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_saved_time <- reactiveVal(format(Sys.time(), "%H:%M:%S"))
    is_dirty <- reactiveVal(FALSE)

    # Populate layer target choices
    observe({
      req(INDICATOR_SCHEMA$layers)
      choices <- setNames(
        sapply(INDICATOR_SCHEMA$layers, function(x) x$id),
        sapply(INDICATOR_SCHEMA$layers, function(x) paste0(x$label, " [", x$type, "]"))
      )
      updateSelectInput(session, "select_layer_target", choices = choices)
    })

    # Title display
    output$proj_title <- renderText({
      st <- project_state()
      if (is.null(st)) "Urban Performance Project" else (st$project_name %||% "Urban Performance Project")
    })

    # Autosave text
    output$save_status_text <- renderText({
      paste("Saved at", last_saved_time())
    })

    # Requirement info UI
    output$layer_requirement_info <- renderUI({
      req(input$select_layer_target)
      target_id <- input$select_layer_target
      layer_def <- NULL
      for (l in INDICATOR_SCHEMA$layers) {
        if (identical(l$id, target_id)) { layer_def <- l; break }
      }
      req(layer_def)

      st <- project_state()
      curr_state <- st$layers[[target_id]]
      status_val <- if (!is.null(curr_state)) curr_state$status else "missing"

      badge_color <- if (identical(status_val, "ready")) "#4ECDC4" else if (identical(status_val, "partial")) "#C7F464" else "#FF6B6B"
      badge_text  <- if (identical(status_val, "ready")) "READY (Pacifica)" else if (identical(status_val, "partial")) "PARTIAL (Missing Attr)" else "MISSING (Cheery Pink)"

      req_attrs <- if (length(layer_def$required_attributes) > 0) {
        paste(layer_def$required_attributes, collapse = ", ")
      } else "None required (geometry only)"

      div(
        style = "font-size: 12px; margin-bottom: 12px; padding: 10px; background: #F8F9FA; border-radius: 6px;",
        div(style = "display: flex; justify-content: space-between; margin-bottom: 6px;",
            tags$strong("Current Status:"),
            tags$span(style = paste0("background: ", badge_color, "; color: #333; font-weight: 700; padding: 2px 6px; border-radius: 4px; font-size: 10px;"), badge_text)
        ),
        div(tags$strong("Required Type: "), paste0(layer_def$type, " (", paste(layer_def$format, collapse = "/"), ")")),
        div(tags$strong("Mandatory Attributes: "), tags$code(req_attrs)),
        if (!is.null(curr_state) && length(curr_state$missing_attributes) > 0)
          div(style = "color: #C44D58; margin-top: 4px;",
              tags$strong("Missing Columns: "), tags$code(paste(unlist(curr_state$missing_attributes), collapse = ", ")))
      )
    })

    # Handle Uploaded Spatial File
    observeEvent(input$upload_spatial_file, {
      req(input$upload_spatial_file)
      req(active_project_path())
      req(input$select_layer_target)

      upload_info <- input$upload_spatial_file
      target_id <- input$select_layer_target

      # Find target definition
      layer_def <- NULL
      for (l in INDICATOR_SCHEMA$layers) {
        if (identical(l$id, target_id)) { layer_def <- l; break }
      }
      req(layer_def)

      raw_dir <- file.path(active_project_path(), "data", "raw")
      if (!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

      dest_file <- file.path(raw_dir, upload_info$name)
      file.copy(upload_info$datapath, dest_file, overwrite = TRUE)

      # Inspect metadata
      meta <- inspect_layer_metadata(dest_file, layer_def)

      # Update project state
      st <- project_state()
      st$layers[[target_id]] <- list(
        id = target_id,
        file_name = upload_info$name,
        file_path = dest_file,
        type = layer_def$type,
        status = meta$status,
        missing_attributes = meta$missing_attributes,
        crs = meta$crs,
        geom_type = meta$geom_type
      )
      project_state(st)
      is_dirty(TRUE)

      status_msg <- if (identical(meta$status, "ready")) "Layer validated successfully (Pacifica)!" else "Layer loaded with missing attributes (Apple Chic)."
      showNotification(status_msg, type = if (identical(meta$status, "ready")) "message" else "warning")
    })

    # Render Bipartite Network Graph
    output$network_graph <- renderVisNetwork({
      req(INDICATOR_SCHEMA$layers, INDICATOR_SCHEMA$indicators)
      st <- project_state()

      # Node lists
      nodes_df <- data.frame(
        id = character(0),
        label = character(0),
        group = character(0),
        color = character(0),
        shape = character(0),
        size = numeric(0),
        font.color = character(0),
        title = character(0),
        stringsAsFactors = FALSE
      )

      edges_df <- data.frame(
        from = character(0),
        to = character(0),
        color = character(0),
        arrows = character(0),
        stringsAsFactors = FALSE
      )

      # 1. Layer Nodes (Left)
      layer_status_map <- list()
      for (l in INDICATOR_SCHEMA$layers) {
        curr_l <- st$layers[[l$id]]
        status_val <- if (!is.null(curr_l)) curr_l$status else "missing"
        layer_status_map[[l$id]] <- status_val

        col <- if (identical(status_val, "ready")) "#4ECDC4" else if (identical(status_val, "partial")) "#C7F464" else "#FF6B6B"
        tooltip <- paste0("<b>", l$label, "</b><br>Status: ", status_val, "<br>Type: ", l$type)

        nodes_df <- rbind(nodes_df, data.frame(
          id = l$id,
          label = l$label,
          group = "layer",
          color = col,
          shape = "box",
          size = 20,
          font.color = if (identical(status_val, "partial")) "#333333" else "#ffffff",
          title = tooltip,
          stringsAsFactors = FALSE
        ))
      }

      # 2. Indicator Nodes (Right) & Edges
      for (ind in INDICATOR_SCHEMA$indicators) {
        dep_statuses <- sapply(ind$dependencies, function(d) layer_status_map[[d]] %||% "missing")

        ind_color <- if (all(dep_statuses == "ready")) {
          "#4ECDC4"
        } else if (any(dep_statuses == "partial") && !any(dep_statuses == "missing")) {
          "#C7F464"
        } else {
          "#FF6B6B"
        }

        nodes_df <- rbind(nodes_df, data.frame(
          id = ind$id,
          label = ind$label,
          group = "indicator",
          color = ind_color,
          shape = "ellipse",
          size = 24,
          font.color = if (identical(ind_color, "#C7F464")) "#333333" else "#ffffff",
          title = paste0("<b>", ind$label, "</b><br>Category: ", ind$category),
          stringsAsFactors = FALSE
        ))

        for (dep in ind$dependencies) {
          edges_df <- rbind(edges_df, data.frame(
            from = dep,
            to = ind$id,
            color = "#DCE1E3",
            arrows = "to",
            stringsAsFactors = FALSE
          ))
        }
      }

      visNetwork(nodes_df, edges_df) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = FALSE) %>%
        visLayout(randomSeed = 42) %>%
        visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -40, springLength = 80))
    })

    # Debounced autosave observer
    debounced_dirty <- debounce(reactive(is_dirty()), 2000)
    observe({
      if (debounced_dirty() && !is.null(active_project_path())) {
        st <- project_state()
        if (!is.null(st)) {
          save_project_state(active_project_path(), st)
          last_saved_time(format(Sys.time(), "%H:%M:%S"))
          is_dirty(FALSE)
        }
      }
    })

    # Force save button
    observeEvent(input$btn_force_save, {
      req(active_project_path())
      st <- project_state()
      if (!is.null(st)) {
        save_project_state(active_project_path(), st)
        last_saved_time(format(Sys.time(), "%H:%M:%S"))
        is_dirty(FALSE)
        showNotification("Project state saved to disk.", type = "default", duration = 3)
      }
    })

    # Generate Scoping Brief (LLM Prompt Bridge)
    observeEvent(input$btn_generate_brief, {
      st <- project_state()
      prompt_text <- generate_scoping_prompt(st, INDICATOR_SCHEMA)

      # Attempt clipboard copy via clipr if supported
      tryCatch({
        if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
          clipr::write_clip(prompt_text)
        }
      }, error = function(e) {})

      showModal(modalDialog(
        title = div(
          style = "display: flex; align-items: center; gap: 8px;",
          tags$span(style = "color: #FF6B6B;", icon("copy")),
          "Client Scoping Brief (Ready for LLM)"
        ),
        size = "l",
        easyClose = TRUE,
        div(
          p(style = "color: #556270; font-size: 14px;",
            "This prompt has been serialized from current Cheery Pink and Apple Chic nodes. Copy and paste it into your AI assistant to generate the client data request email:"),
          tags$textarea(
            id = ns("prompt_box"),
            style = "width: 100%; height: 320px; font-family: monospace; font-size: 12px; padding: 10px; border: 1px solid #DCE1E3; border-radius: 6px; resize: vertical;",
            readonly = "readonly",
            prompt_text
          )
        ),
        footer = tagList(
          modalButton("Close")
        )
      ))
    })

    # Switch to workshop tab
    observeEvent(input$btn_goto_workshop, {
      updateNavbarPage(parent_session, "main_navbar", selected = "tab_workshop")
    })
  })
}
