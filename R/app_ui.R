#' @import shiny
#' @import shinydashboard
#' @import dashboardthemes

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
          "Data Summary",
          tabName = "dataSummary",
          icon = icon("database")
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
        
        
      )
    )#Dashboard Body ends here
    
    #-------------------------Body Ends Here-------------------------------
    
  )#End of dashboard page
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath('www', system.file('app/www', package = 'bdvisShiny'))
  
  tags$head(
    golem::activate_js(),
    golem::favicon()
    
    
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css")
  )
}
