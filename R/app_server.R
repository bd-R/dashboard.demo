#' @import shiny
options(shiny.maxRequestSize = 5000 * 1024 ^ 2)

app_server <- function(input, output, session) {
  ##################Input tabs#################################
  
  inputDataset <-
    callModule(mod_dataInput_server, "dataInput_ui_1", session)
  
  ##############################################################
  
  
  ##################DataSummary tabs############################
  
  callModule(mod_dataSummary_server, "dataSummary_ui_1", inputDataset)
  
  ##############################################################
  
  
  
  
  
  ##################Spatial tabs###############################
  
  callModule(mod_spatial_server, "spatial_ui_1", inputDataset)
  
  ################End of Input Tab#############################
  
  
  
  ##################Spatial tabs###############################
  
  callModule(mod_taxonomic_server, "taxonomic_ui_1", inputDataset)
  
  ################End of Input Tab#############################
  
  
  ##################Spatial tabs###############################
  
  callModule(mod_temporal_server, "temporal_ui_1", inputDataset)
  
  ################End of Input Tab#############################
  
  
}
