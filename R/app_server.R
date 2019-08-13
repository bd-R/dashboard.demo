#' @import shiny
options(shiny.maxRequestSize = 5000 * 1024 ^ 2)

app_server <- function(input, output, session) {

  inputDataset <-
    callModule(mod_dataInput_server, "dataInput_ui", session)
  
  callModule(mod_dataSummary_server, "dataSummary_ui", inputDataset)
  
  callModule(mod_spatial_server, "spatial_ui", inputDataset)
  
  callModule(mod_taxonomic_server, "taxonomic_ui", inputDataset)
  
  callModule(mod_temporal_server, "temporal_ui", inputDataset)
}
