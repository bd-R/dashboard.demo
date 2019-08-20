#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import dplyr
#' @import dashboardthemes
#' @import ggpubr
#' @import plotly
#' @import circlepackeR
#' @import data.tree
#' @import flexdashboard
#' @import bdvis
#' @import ggplot2
#' @import tidyr
#' @import treemap
#' @import sunburstR
#' @import formattable
#' @import summarytools
#' @import leaflet.extras
#' @import sp
#' @import bddwc.app
app_ui <- function() {
  dashboardPage(
    skin = "green",
    dashboardHeader(title = "bdvisashboard"),
    #----------------------SideBar Start-------------------------------------
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
        )
      )#Sidebar menu ends here
    ),
    #sidebar Dashboard ends here
    #----------------------SideBar End-------------------------------------
    
    #----------------------Body Start-------------------------------------
    dashboardBody(
      shinyDashboardThemes(theme = "grey_dark"),
      golem_add_external_resources(),
      tabItems(
        
        tabItem(tabName = "dataInputTab",
                # -------------------------------
                bddwc.app::mod_add_data_ui("bdFileInput")
                # -------------------------------
        ),
        tabItem(tabName = "dataSummary",
                # -------------------------------
                mod_dataSummary_ui("dataSummary_ui")
                # -------------------------------
                ),
        tabItem(tabName = "missing_overview",
                #--------------------------------
                mod_missing_data_ui("missing_data_ui")
                #--------------------------------
                
        ),
        tabItem(tabName = "spatialTab",
                # -------------------------------

                mod_spatial_ui("spatial_ui")

                # -------------------------------
        ),
        tabItem(tabName = "taxonomicTab",
                # -------------------------------

                mod_taxonomic_ui("taxonomic_ui")

                # -------------------------------
        ),
        tabItem(tabName = "temporalTab",
                # -------------------------------

                mod_temporal_ui("temporal_ui")

                # -------------------------------
        )
      )
    )#Dashboard Body ends here
    #-------------------------Body Ends Here-------------------------------
  )#End of dashboard page
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
