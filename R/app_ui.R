#' @import shiny shinydashboard leaflet dplyr dashboardthemes plotly
#' @import flexdashboard DT
#' @import formattable leaflet.extras sp bdutilities.app
#' 
app_ui <- function() {
  dashboardPage(
    skin = "yellow",
    dashboardHeader(title = "bdvis Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        id = "sideBar",
        menuItem(
          "Data Input",
          tabName = "dataInputTab",
          icon = icon("database")
        ),
        menuItem(
          "Data Overview",
          icon = icon("database"),
          menuSubItem(
            "Data Summary",
            tabName = "dataSummary"
          ),
          menuSubItem(
            "Missing Data Overview",
            tabName = "missing_overview"
          )
        ),
        menuItem(
          "Spatial Visualization",
          tabName = "spatialTab",
          icon = icon("map-marked")
        ),
        menuItem(
          "Taxonomic Visualization",
          tabName = "taxonomicTab",
          icon = icon("connectdevelop")
        ),
        menuItem(
          "Temporal Visualization",
          tabName = "temporalTab",
          icon = icon("clock")
        ),
        menuItem("Cite Us",
                 tabName = "cite",
                 icon = icon("copyright")
        )
      )
    ),
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      golem_add_external_resources(),
      tabItems(
        tabItem(
          tabName = "dataInputTab",
          bdutilities.app::mod_add_data_ui("bdFileInput"),
          bdutilities.app::mod_darwinize_ui("darwinize")
        ),
        tabItem(
          tabName = "dataSummary",
          mod_dataSummary_ui("dataSummary_ui")
        ),
        tabItem(
          tabName = "missing_overview",
          mod_missing_data_ui("missing_data_ui")
        ),
        tabItem(
          tabName = "spatialTab",
          mod_spatial_ui("spatial_ui")
        ),
        tabItem(
          tabName = "taxonomicTab",
          mod_taxonomic_ui("taxonomic_ui")
        ),
        tabItem(
          tabName = "temporalTab",
          mod_temporal_ui("temporal_ui")
        ),
        tabItem("cite",
          fluidRow(
            div(
              bdutilities.app::mod_citation_ui("bdcite", "bdchecks.app")
            )
          )
        )
      )
    )
  )
}

#' @import shiny
#Function to attach Files such as css to shiny.
golem_add_external_resources <- function() {
  addResourcePath('www', system.file('app/www', package = 'dashboard.demo'))
  
  tags$head(
    golem::activate_js(),
    #golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}
