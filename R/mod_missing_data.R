# Module UI
  
#' @title   mod_missing_data_ui and mod_missing_data_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_missing_data
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_missing_data_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tabsetPanel(
        tabPanel(
          "Spatial",
          htmlOutput(ns("table_spatial"))
        ),
        tabPanel(
          "Temporal",
          htmlOutput(ns("table_temporal"))
        ),
        tabPanel(
          "Taxonomic",
          htmlOutput(ns("table_taxonomic"))
        )
      )
    )
  )
}
    
# Module Server
    
#' @rdname mod_missing_data
#' @export
#' @keywords internal
    
mod_missing_data_server <- function(input, output, session, dataset_missing){
  ns <- session$ns
  output$table_spatial <- renderUI({
    spatial <- dataset_missing()[c("countryCode",
                              "locality",
                              "coordinatePrecision",
                              "coordinateUncertaintyInMeters",
                              "decimalLatitude",
                              "decimalLongitude")
                            ]
    print(dfSummary(spatial),  max.tbl.height = 500, method = "render")
  })
  
  output$table_temporal <- renderUI({
    temporal <- dataset_missing()[c("eventDate",
                                    "day",
                                    "month",
                                    "year",
                                    "dateIdentified",
                                    "lastInterpreted")]
    print(dfSummary(temporal),  max.tbl.height = 500, method = "render")
  })
  
  output$table_taxonomic <- renderUI({
    taxonomic <- dataset_missing()[c("kingdom",
                                     "phylum",
                                     "order",
                                     "family",
                                     "genus",
                                     "species")]
    print(dfSummary(taxonomic),  max.tbl.height = 500, method = "render")
  })
}
