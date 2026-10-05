# ==============================================================================
# Module: Deep Dive Matrix View (27 Indicators)
# ==============================================================================

deepDiveUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "container-fluid",
      style = "padding: 20px 25px;",
      # Header
      div(
        style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #DCE1E3; padding-bottom: 15px; margin-bottom: 20px;",
        div(
          h3(style = "color: #556270; margin: 0; font-weight: 700;", "Complete 27-Indicator Master Matrix"),
          p(style = "color: #88929A; margin: 4px 0 0 0; font-size: 13px;", "Comparative baseline vs. horizon scenario evaluation across all urban sustainability pillars")
        ),
        div(
          downloadButton(
            ns("btn_download_csv"),
            "Export CSV Matrix",
            class = "btn-sm btn-outline-secondary",
            style = "color: #556270; border-color: #556270; font-weight: 600;"
          )
        )
      ),

      # Delta Bar Chart Visualizations (Top Panel)
      div(
        style = "background: white; border: 1px solid #DCE1E3; border-radius: 8px; padding: 18px; margin-bottom: 25px;",
        h5(style = "color: #556270; font-weight: 700; margin-top: 0;", "Key Indicator Variations (% Change)"),
        plotlyOutput(ns("plot_delta_bars"), height = "320px")
      ),

      # Master Data Table (DT)
      div(
        style = "background: white; border: 1px solid #DCE1E3; border-radius: 8px; padding: 18px;",
        DTOutput(ns("matrix_datatable"))
      )
    )
  )
}

deepDiveServer <- function(id, project_state, simulation_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Compute full 27-indicator data frame
    matrix_data <- reactive({
      req(INDICATOR_SCHEMA$indicators)
      res <- simulation_results()

      # Benchmark numbers based on Cancún baseline & simulation
      # Realistic default values modeled after urbanperformance vignette
      base_metrics <- c(
        tot_pop = 911500,
        urban_footprint = 142.5,
        population_density = 6396,
        land_consumption = 0,
        infill = 0,
        biodiversity_land_consumption = 0,
        agricultural_land_consumption = 0,
        green_land_consumption = 0,
        green_area_pcapita = 12.4,
        land_cover_loss = 0,
        roads_length = 1250.4,
        roads_density = 8.77,
        intersection_density = 48.2,
        cycle_track_density = 0.85,
        cycle_proximity = 24.5,
        public_transport_proximity = 64.2,
        amenities_proximity = 52.8,
        jobs_proximity = 68.1,
        hazard_exposure = 18.4,
        tree_canopy_carbon = 3450,
        housing_costs = 780,
        anthropogenic_hazard = 12.3,
        vacant_housing = 9.8,
        walkability_index = 46.2,
        housing_affordability = 4.8,
        uhi_intensity = 3.2,
        transit_utilization = 54.0
      )

      horiz_metrics <- c(
        tot_pop = 1065000,
        urban_footprint = 158.2,
        population_density = 6732,
        land_consumption = 15.7,
        infill = 6.2,
        biodiversity_land_consumption = 0.8,
        agricultural_land_consumption = 3.4,
        green_land_consumption = 1.1,
        green_area_pcapita = 14.8,
        land_cover_loss = 15.7,
        roads_length = 1380.0,
        roads_density = 8.72,
        intersection_density = 52.1,
        cycle_track_density = 1.45,
        cycle_proximity = 41.2,
        public_transport_proximity = 78.5,
        amenities_proximity = 66.4,
        jobs_proximity = 74.3,
        hazard_exposure = 14.1,
        tree_canopy_carbon = 4820,
        housing_costs = 820,
        anthropogenic_hazard = 9.5,
        vacant_housing = 7.2,
        walkability_index = 58.7,
        housing_affordability = 4.2,
        uhi_intensity = 2.8,
        transit_utilization = 68.5
      )

      # If live simulation results exist, overwrite corresponding macro keys
      if (!is.null(res)) {
        base_metrics["tot_pop"] <- res$pop$base
        horiz_metrics["tot_pop"] <- res$pop$horiz
        base_metrics["urban_footprint"] <- res$fp$base
        horiz_metrics["urban_footprint"] <- res$fp$horiz
        base_metrics["public_transport_proximity"] <- res$transit$base
        horiz_metrics["public_transport_proximity"] <- res$transit$horiz
        base_metrics["hazard_exposure"] <- res$hazard$base
        horiz_metrics["hazard_exposure"] <- res$hazard$horiz
      }

      rows <- lapply(seq_along(INDICATOR_SCHEMA$indicators), function(i) {
        ind <- INDICATOR_SCHEMA$indicators[[i]]
        b_val <- unname(base_metrics[ind$id] %||% 0)
        h_val <- unname(horiz_metrics[ind$id] %||% 0)
        delta_abs <- round(h_val - b_val, 2)
        delta_pct <- if (b_val != 0) round((delta_abs / b_val) * 100, 1) else 0

        data.frame(
          No = i,
          Indicator = ind$label,
          Category = ind$category,
          Baseline = round(b_val, 2),
          Horizon = round(h_val, 2),
          Abs_Change = delta_abs,
          Pct_Change = delta_pct,
          Units = ind$output_unit,
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, rows)
    })

    # Render Horizontal Delta Bar Chart via Plotly
    output$plot_delta_bars <- renderPlotly({
      df <- matrix_data()
      req(nrow(df) > 0)

      # Pick top indicators with highest percentage change (positive or negative)
      df_sub <- df[df$Pct_Change != 0, ]
      if (nrow(df_sub) > 12) {
        df_sub <- df_sub[order(abs(df_sub$Pct_Change), decreasing = TRUE)[1:12], ]
      }
      df_sub <- df_sub[order(df_sub$Pct_Change), ]
      df_sub$Indicator <- factor(df_sub$Indicator, levels = df_sub$Indicator)
      df_sub$BarColor <- ifelse(df_sub$Pct_Change >= 0, "#4ECDC4", "#FF6B6B")

      plot_ly(
        data = df_sub,
        x = ~Pct_Change,
        y = ~Indicator,
        type = "bar",
        orientation = "h",
        marker = list(
          color = df_sub$BarColor,
          line = list(color = "rgba(0,0,0,0.06)", width = 1)
        ),
        hoverinfo = "text",
        text = ~paste0(
          "<b>", Indicator, "</b> (", Category, ")<br>",
          "Baseline: ", format(Baseline, big.mark = ","), " ", Units, "<br>",
          "Horizon: ", format(Horizon, big.mark = ","), " ", Units, "<br>",
          "Delta: ", ifelse(Abs_Change > 0, "+", ""), Abs_Change, " ", Units, " (",
          ifelse(Pct_Change > 0, "+", ""), Pct_Change, "%)"
        )
      ) %>%
      layout(
        xaxis = list(
          title = list(text = "Percentage Shift (%)", font = list(size = 12, color = "#556270")),
          zeroline = TRUE,
          zerolinecolor = "#556270",
          zerolinewidth = 1.5,
          gridcolor = "#ECEFF1",
          tickfont = list(size = 11, color = "#556270")
        ),
        yaxis = list(
          title = "",
          automargin = TRUE,
          tickfont = list(size = 11, color = "#556270")
        ),
        margin = list(l = 10, r = 20, t = 10, b = 40),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    })

    # Render DT Datatable with Right-Aligned Numbers and Bidirectional Formatting
    output$matrix_datatable <- renderDT({
      df <- matrix_data()
      req(nrow(df) > 0)

      datatable(
        df,
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          dom = "ftip",
          order = list(list(0, "asc")),
          columnDefs = list(
            list(className = "dt-right", targets = 3:6),
            list(className = "dt-center", targets = c(0, 7)),
            list(className = "dt-left", targets = c(1, 2))
          )
        ),
        rownames = FALSE,
        class = "display compact hover"
      ) %>%
        formatRound(columns = c("Baseline", "Horizon", "Abs_Change"), digits = 1) %>%
        formatString(columns = "Pct_Change", suffix = "%") %>%
        formatStyle(
          "Pct_Change",
          background = styleInterval(
            c(0),
            c("rgba(255, 107, 107, 0.12)", "rgba(78, 205, 196, 0.15)")
          ),
          color = styleInterval(
            c(0),
            c("#C44D58", "#007A72")
          ),
          fontWeight = "700",
          borderRadius = "4px"
        )
    })

    # Download Handler for CSV
    output$btn_download_csv <- downloadHandler(
      filename = function() {
        st <- project_state()
        pname <- if (!is.null(st)) st$project_name else "urbanperformance"
        paste0(pname, "_27_indicator_matrix.csv")
      },
      content = function(file) {
        write.csv(matrix_data(), file, row.names = FALSE)
      }
    )
  })
}
