# Module UI

#' @title   mod_dataInput_ui and mod_dataInput_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dataInput
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_dataInput_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    div(
      tagList(
        column(
          12,
          h1("Add Occurrence Data"),
          column(
            3,
            # ------------- DB Module -------------------
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Existing Data",
                selectizeInput(
                  ns("dataSet"),
                  "Select Sample Datasets",
                  choices = c("Mammals", "Hyena", "Puma Concolor"),
                  selected = "Puma Concolor"
                ),
                div(
                  class = "activeButton",
                  actionButton(
                    ns("loadexisting"),
                    "Load New Dataset",
                    icon("upload")
                  )
                )
              ),
              tabPanel(
                "Download Data",
                div(
                  class = "secondaryHeaders",
                  h3("Option 01: From Online Database")
                ),
                textInput(
                  ns("scientificName"),
                  label = h3("Scientific Name:"),
                  value = "Puma concolor"
                ),
                
                numericInput(
                  ns("recordSize"),
                  label = h3("Record Size:"),
                  value = 500
                ),
                
                selectInput(
                  ns("hasCoords"),
                  label = h3("Records Filter:"),
                  choices = list(
                    "With Coordinates" = "1",
                    "Without Coordinates" = "2",
                    "No Filter" = "3"
                  ),
                  selected = 3
                ),
                
                radioButtons(
                  ns("queryDB"),
                  label = h3("Online Database:"),
                  choices = list(
                    "GBIF (Global Biodiversity Information Facility)" = "gbif",
                    "iDigBio (Integrated Digitized Biocollections)" = "idigbio",
                    "EcoEngine (Berkeley Ecoinformatics Engine)" = "ecoengine",
                    "Vertnet (Vertebrate Network)" = "vertnet",
                    "BISON (Biodiversity Information Serving Our Nation)" = "bison",
                    "iNaturalist" = "inat",
                    "ALA (Atlas of Living Australia)" = "ala"
                    # "OBIS (Ocean Biogeographic Information System)" = "obis",
                    # "AntWeb" = "antweb"
                  ),
                  selected = "gbif"
                ),
                
                br(),
                div(
                  id = ns("queryDatabaseDiv"),
                  class = "activeButton",
                  actionButton(
                    ns("queryDatabase"),
                    "Query Database",
                    icon("download")
                  )
                ),
                br()
              ),
              
              # ------------- End of DB Module -------------------
              
              # ------------- Local Disk Module -------------------
              tabPanel(
                "Upload Data",
                div(
                  class = "secondaryHeaders",
                  h3("Option 02: From Local Disk")
                ),
                div(
                  id = ns("inputFileDiv"),
                  class = "activeButton",
                  fileInput(
                    ns("inputFile"),
                    label = h3("CSV / DWCA ZIP file input"),
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv",
                      ".zip",
                      "application/zip"
                    )
                  )
                )
              ),
              
              helpText(
                "To manually edit or clean headers, use ",
                a("bdDwC", href = "https://github.com/bd-R/bdDwC"),
                " package."
              )
              
              
              # ------------- End of Local Disk Module -------------------
              
              
            ),
            div(
              id = "nextButton",
              actionButton(
                ns("nextTab"),
                "Next:: Data Summary",
                icon("forward")
              )
            )
          ),
          
          # ------------- Map / Table Module -------------------
          column(9,
                 tabsetPanel(
                   type = "tabs",
                   tabPanel(
                     "Map View",
                     leafletOutput(ns("mymap"), height = "700"),
                     absolutePanel(
                       top = 60,
                       right = 20,
                       selectInput(
                         ns("mapTexture"),
                         "Map Texture",
                         choices = list(
                           "OpenStreetMap.Mapnik" = "OpenStreetMap.Mapnik",
                           "OpenStreetMap.BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
                           "Stamen.Toner" = "Stamen.Toner",
                           "CartoDB.Positron" = "CartoDB.Positron",
                           "Esri.NatGeoWorldMap" = "Esri.NatGeoWorldMap",
                           "Stamen.Watercolor" = "Stamen.Watercolor",
                           "Stamen.Terrain" = "Stamen.Terrain",
                           "Esri.WorldImagery" = "Esri.WorldImagery",
                           "Esri.WorldTerrain" = "Esri.WorldTerrain"
                         ),
                         selected = "Stamen.Toner"
                       ),
                       selectInput(
                         ns("mapColor"),
                         "Points Color",
                         choices = list(
                           "Red" = 'red',
                           "Green" = "green",
                           "Blue" = "blue",
                           "Black" = "black"
                         )
                       )
                     )
                   ),
                   tabPanel("Table View",
                            DT::dataTableOutput(
                              ns("inputDataTable")
                            )
                   )
                 )
          )
          
          # ------------- End of Map/Table Module -------------------
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_dataInput
#' @export
#' @keywords internal

mod_dataInput_server <-
  function(input, output, session, parentSession) {
    ns <- session$ns
    #load the rda file
    load(file = "data/mammals.rda")
    load(file = "data/pumaConcolor.rda")
    load(file = "data/hyena.rda")
    
    #Initialize returnData Value
    returnData <- pumaConcolor
    
    map <- leafletProxy(ns("mymap"))
    
    # ----------------
    #when user click on button called load existing dataset
    observeEvent(
      input$loadexisting, {
        if (input$dataSet == "Mammals") {
          returnData <<- mammals
        } else if (input$dataSet == "Hyena") {
          returnData <<- hyena
        } else if (input$dataSet == "Puma Concolor") {
          returnData <<- pumaConcolor
        }
        dataLoadedTask(returnData)
      }
    )
    
    #Button to navigate to DataSummary Tab
    observeEvent(
      input$nextTab, {
        updateTabItems(parentSession,
                       "sideBar", 
                       "dataSummary"
        )
      }
    )
    
    #Download Database function
    observeEvent(input$queryDatabase, {
      withProgress(
        message = paste("Querying", input$queryDB, "..."), {
          if (input$queryDB == "gbif") {
            data <-
              rgbif::occ_search(
                scientificName = input$scientificName,
                limit = input$recordSize,
                hasCoordinate = switch(
                  input$hasCoords,
                  "1" = TRUE,
                  "2" = FALSE,
                  "3" = NULL
                )
              )
            returnData <<- data$data
            
          } else {
            warnings <- capture.output(
              data <-
                spocc::occ(
                  query = input$scientificName,
                  from = input$queryDB,
                  limit = input$recordSize,
                  has_coords = switch(
                    input$hasCoords,
                    "1" = TRUE,
                    "2" = FALSE,
                    "3" = NULL
                  )
                ),
              type = "message"
            )
            
            if (length(warnings) > 0) {
              showNotification(paste(warnings, collapse = " "),
                               duration = 6)
            }
            
            tempData <- data[[input$queryDB]]$data[[1]]
            returnData <<- tempData
          }
        })
      dataLoadedTask(returnData)
    })
    
    
    #Upload Dataset Function
    observeEvent(input$inputFile, {
      withProgress(message = paste("Reading", 
                                   input$inputFile$name, "..."), {
                                     if (is.null(input$inputFile))
                                       return("No data to view")
                                     
                                     if (grepl("zip", tolower(input$inputFile$type))) {
                                       message("Reading DWCA ZIP...")
                                       finchRead <-
                                         finch::dwca_read(input$inputFile$datapath, read = T)
                                       returnData <<- finchRead$data[[1]]
                                       
                                     } else {
                                       returnData <<-
                                         data.table::fread(input$inputFile$datapath)
                                     }
                                   })
      dataLoadedTask(returnData)
    })
    
    #Map Texture
    observeEvent(input$mapTexture, {
      if (length(returnData) == 0) {
        return(NULL)
      }
      leafletProxy(ns("mymap"), data = returnData) %>%
        clearShapes() %>%
        addCircles( ~ decimalLongitude, 
                    ~ decimalLatitude, 
                    color = input$mapColor
        )
    })
    
    #Map Color
    observeEvent(input$mapColor, {
      if (length(returnData) == 0) {
        return(NULL)
      }
      leafletProxy(ns("mymap"), data = returnData) %>%
        clearShapes() %>%
        addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    })
    
    #Draw Map
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(input$mapTexture) %>%
        setView(0, 0, zoom = 2)
    })
    
    
    
    
    #Draw Table
    output$inputDataTable <- DT::renderDataTable(DT::datatable({
      returnData
    }, options = list(scrollX = TRUE)))
    
    
    
    dataLoadedTask <- function(data) {
      if (length(data) == 0) {
        showNotification("Empty data returned! Try different setting.",
                         duration = 2)
        return()
      }
      
      
      if ("decimalLatitude" %in% colnames(returnData)) {
        returnData$decimalLatitude <<-
          as.numeric(returnData$decimalLatitude)
        returnData$decimalLongitude <<-
          as.numeric(returnData$decimalLongitude)
      }
      
      # ------------ End of Darwinizing Data -------------
      
      try(leafletProxy(ns("mymap"), data = returnData) %>%
            clearShapes() %>%
            addCircles( ~ decimalLongitude,
                        ~ decimalLatitude, 
                        color = input$mapColor
            )
      )
      
      output$inputDataTable <- DT::renderDataTable(DT::datatable({
        returnData
      }, options = list(scrollX = TRUE)))
      
      
      shinyjs::runjs(code = paste(
        '$("#',
        ns("queryDatabaseDiv"),
        '").addClass("readyButton");',
        sep = ""
      ))
      shinyjs::runjs(code = paste(
        '$("#',
        ns("queryDatabaseDiv"),
        '").removeClass("activeButton");',
        sep = ""
      ))
      shinyjs::runjs(code = paste(
        '$("#',
        ns("inputFileDiv"),
        '").addClass("readyButton");',
        sep = ""
      ))
      shinyjs::runjs(code = paste(
        '$("#',
        ns("inputFileDiv"),
        '").removeClass("activeButton");',
        sep = ""
      ))
      shinyjs::runjs(
        code = paste(
          '$("#',
          "dataToConfigureDiv",
          '").addClass("completedButton");',
          sep = ""
        )
      )
      shinyjs::runjs(
        code = paste(
          '$("#',
          "dataToConfigureDiv",
          '").removeClass("activeButton");',
          sep = ""
        )
      )
      
      
      
      showNotification("Read Data Successfully", duration = 2)
      
      
      # --------- Setting flag tab statistic boxes -------
      # TODO
      
    }
    
    #Reactive function to change all plots when new dataset is selected
    returnDataReact <- reactive({
      # Input actions that need to trigger new dataframe return
      input$loadexisting
      input$inputFile
      input$queryDatabase
      
      returnData
    })
    
    
    return(returnDataReact)
  }

## To be copied in the UI
# mod_dataInput_ui("dataInput_ui_1")

## To be copied in the server
# callModule(mod_dataInput_server, "dataInput_ui_1")
