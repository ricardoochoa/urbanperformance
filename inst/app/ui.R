# ==============================================================================
# Urban Performance Explorer - ui.R
# ==============================================================================

app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "litera",
  primary = "#4ECDC4",
  secondary = "#556270",
  base_font = bslib::font_google("Inter")
)

ui <- tagList(
  # Include Head Styles
  tags$head(
    tags$style(HTML("
      /* Master Theme Tweaks */
      .navbar {
        background-color: #556270 !important;
        border-bottom: 2px solid #4ECDC4;
      }
      .navbar-brand {
        color: white !important;
        font-weight: 700;
        font-size: 1.15rem;
        letter-spacing: 0.5px;
      }
      .nav-link {
        color: #DCE1E3 !important;
        font-weight: 500;
        transition: color 0.2s ease;
      }
      .nav-link:hover, .nav-link.active {
        color: #FFFFFF !important;
        font-weight: 600;
      }
      .nav-link.active {
        border-bottom: 2px solid #4ECDC4 !important;
      }
      /* Leaflet map full-height adjustments & loading states */
      .leaflet-container {
        background-color: #F8F9FA;
      }
      .leaflet.recalculating {
        opacity: 0.5;
        transition: opacity 0.25s ease-in-out;
      }
      /* Buttons styling with accessible contrast */
      .btn-primary {
        background-color: #4ECDC4 !important;
        border-color: #4ECDC4 !important;
        color: #212529 !important;
        font-weight: 600 !important;
      }
      .btn-primary:hover {
        background-color: #3EB7AE !important;
        border-color: #3EB7AE !important;
        color: #111827 !important;
      }
      /* Accordion custom styling */
      .accordion-button:not(.collapsed) {
        background-color: #F8F9FA;
        color: #556270;
        font-weight: 600;
      }
      .accordion-button {
        padding: 0.65rem 0.9rem;
        font-size: 0.82rem;
        font-weight: 600;
      }
      .accordion-body {
        padding: 0.75rem 0.9rem;
      }
    ")),
    tags$script(HTML("
      $(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"], button[data-bs-toggle=\"tab\"]', function(e) {
        window.dispatchEvent(new Event('resize'));
        setTimeout(function() {
          if (window.HTMLWidgets) {
            $('.leaflet').each(function() {
              var mapObj = HTMLWidgets.find('#' + this.id);
              if (mapObj && mapObj.getMap) {
                mapObj.getMap().invalidateSize();
              }
            });
          }
        }, 150);
      });
    "))
  ),

  launchModalUI("launch"),

  page_navbar(
    id = "main_navbar",
    title = div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span(style = "color: white; font-weight: 800; letter-spacing: 0.5px;", "URBAN PERFORMANCE"),
      tags$span(style = "color: #4ECDC4; font-weight: 300; font-size: 0.85em;", "EXPLORER")
    ),
    theme = app_theme,
    selected = "tab_scoping",

    # Tab 1: Scoping Wizard
    nav_panel(
      title = "Scoping Wizard",
      value = "tab_scoping",
      icon = icon("project-diagram"),
      scopingWizardUI("scoping")
    ),

    # Tab 2: Workshop View
    nav_panel(
      title = "Workshop View",
      value = "tab_workshop",
      icon = icon("map-marked-alt"),
      workshopViewUI("workshop")
    ),

    # Tab 3: Deep Dive Matrix
    nav_panel(
      title = "Deep Dive Matrix",
      value = "tab_matrix",
      icon = icon("table"),
      deepDiveUI("matrix")
    ),

    # Right side: Report Export Button
    nav_spacer(),
    nav_item(
      reportingUI("reporting")
    )
  )
)
