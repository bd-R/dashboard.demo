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
mod_missing_data_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tabsetPanel(
        id = ns("first_tabset"),
        tabPanel(
          "Spatial",
          value = "spatial",
          formattable::formattableOutput(
            ns("spatial_table")
          )
        ),
        tabPanel(
          "Temporal",
          value = "temporal",
          formattable::formattableOutput(
            ns("temporal_table")
          )
        ),
        tabPanel(
          "Taxonomic",
          value = "taxonomic",
          formattable::formattableOutput(
            ns("taxonomic_table" )
          )
        )
      )
    ),
    tags$br(),
    tags$br(),
    h4("List of Important Columns Not Present in Dataset"),
    fluidRow(
      tabsetPanel(
        id = ns("second_tabset"),
        tabPanel(
          "Spatial",
          value = "spatial",
          formattable::formattableOutput(ns("spatial_missing"))
        ),
        tabPanel(
          "Temporal",
          value = "temporal",
          formattable::formattableOutput(ns("temporal_missing"))
        ),
        tabPanel(
          "Taxonomic",
          value = "taxonomic",
          formattable::formattableOutput(ns("taxonomic_missing"))
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_missing_data
#' @export
#' @keywords internal

mod_missing_data_server <- function(input, output, session, dataset_missing) {
  ns <- session$ns
  

  #Calculating missing data and create the table for spatial Tab
  output$spatial_table <- formattable::renderFormattable({
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    missing_name <- vector()
    names <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    spatial_column <- c(
      "countryCode",
      "locality",
      "decimalLatitude",
      "decimalLongitude",
      "verbatimLatitude",
      "verbatimLongitude",
      "coordinateUncertaintyInMeters",
      "coordinatePrecision",
      "elevation",
      "elevationAccuracy",
      "depth",
      "depthAccuracy",
      "establishmentMeans"
      )
    
    for(i in spatial_column){
      if(i %in% colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      }else {
        missing_name <- c(missing_name,i)
      }
    }
    
    
    output$spatial_missing <- formattable::renderFormattable({
      table <- data.frame(missing_name)
      formattable::formattable(
        table,
        align = c(
          "c",
          rep(
            "l",
            NCOL(
              table
            ) - 1
          )
        )
      )
    })
    
    table <- data.frame(
      names,
      total_records,
      missing_records,
      records_percentage
    )
      
    customRed <- "#ff7f7f"
      
    unit.scale = function(x){
      x/100
    }
    formattable::formattable(
      table,
      align = c(
        "l",
        rep(
          "r",
          NCOL(
            table
          ) - 1
        )
      ),
      list(
        records_percentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
  
  #Calculating missing data and create the table for Temporal Tab  
  output$temporal_table <- formattable::renderFormattable({
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    names <- vector()
    missing_name <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    temporal_column <-
      c(
        "eventDate",
        "day",
        "month",
        "year",
        "dateIdentified",
        "lastInterpreted",
        "dateModified",
        "datecollected",
        "begin_date",
        "observed_on",
        "date"
      )
    
    for(i in temporal_column){
      if(i %in% colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      }else {
        missing_name <- c(missing_name,i)
      }
    }
    
    output$temporal_missing <- formattable::renderFormattable({
      table <- data.frame(missing_name)
      formattable::formattable(
        table,
        align = c(
          "c",
          rep(
            "l",
            NCOL(
              table
            ) - 1
          )
        )
      )
    })
      
    
    table <- data.frame(
      names,
      total_records,
      missing_records,
      records_percentage
    )
      
    customRed <- "#ff7f7f"
      
    unit.scale = function(x)
      x/100
      
    formattable::formattable(
      table,
      align = c(
        "l",
        rep(
          "r",
          NCOL(
            table
          ) - 1
        )
      ),
      list(
        records_percentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
    
  #Calculating missing data and create the table for Taxonomic Tab
  output$taxonomic_table <- formattable::renderFormattable({
    validate(
      need(length(dataset_missing())>0, 'Please upload/download a dataset first')
    )
    df <- dataset_missing()
    names <- vector()
    missing_name <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    taxonomic_columns <-
      c(
        "kingdom",
        "phylum",
        "order",
        "family",
        "genus",
        "species",
        "name",
        "taxonRank",
        "scientificName",
        "taxonKey",
        "speciesKey",
        "identifiedBy",
        "dateIdentified",
        "recordedBy",
        "recordNumber",
        "typeStatus"
      )
      
    for(i in taxonomic_columns){
      if(i %in% colnames(df)){
        names <- c(names,i)
        total_records <- c(
          total_records,
          nrow(df[i])
        )
        missing_records <- c(
          missing_records,
          sum(
            is.na(
              df[i]
            )
          )
        )
        records_percentage <- c(
          records_percentage,
          round(
            (
              (
                nrow(
                  df[i]
                ) - sum(
                  is.na(
                    df[i]
                  )
                )
              ) /
                nrow(
                  df[i]
                )
            ),
            2
          ) * 100
        )
      } else {
        missing_name <- c(missing_name,i)
      }
    }
    
    output$taxonomic_missing <- formattable::renderFormattable({
      table <- data.frame(missing_name)
      formattable::formattable(
        table,
        align = c(
          "c",
          rep(
            "l",
            NCOL(
              table
            ) - 1
          )
        )
      )
    })

    table <- data.frame(
      names,
      total_records,
      missing_records,
      records_percentage
    )
      
    customRed <- "#ff7f7f"
      
    unit.scale = function(x)
      x/100
      
    formattable::formattable(
      table,
      align = c(
        "l",
        rep(
          "r",
          NCOL(
            table
          ) - 1
        )
      ),
      list(
        records_percentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
  
  #Missing Data Record
  
  output$temporal_missing <- renderText(missing_temporal)
  output$taxonomic <- renderText(missing_taxonomic)
  
  
  
  observeEvent(input$first_tabset, {
    updateTabsetPanel(session, "second_tabset",
                      selected = input$first_tabset
    )
  })
  
  observeEvent(input$second_tabset, {
    updateTabsetPanel(session, "first_tabset",
                      selected = input$second_tabset
    )
  })
  
}
