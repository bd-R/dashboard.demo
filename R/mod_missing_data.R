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
        tabPanel(
          "Spatial",
          formattable::formattableOutput(
            ns("spatial_table")
          )
        ),
        tabPanel(
          "Temporal",
          formattable::formattableOutput(
            ns("temporal_table")
          )
        ),
        tabPanel(
          "Taxonomic",
          formattable::formattableOutput(
            ns("taxonomic_table" )
          )
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
    names <- vector()
    total_records <- vector()
    missing_records <- vector()
    records_percentage <- vector()
    spatial_column <- c(
      "countryCode",
      "locality",
      "decimalLatitude",
      "decimalLongitude",
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
      }
    }
    
    
      
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
        "lastInterpreted"
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
      }
    }
      
    
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
      }
    }
      
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
  
}
