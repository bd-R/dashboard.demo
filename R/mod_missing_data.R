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
  fluidPage(fluidRow(tabsetPanel(
    tabPanel("Spatial",
             formattable::formattableOutput(ns("spatial_table"))),
    tabPanel("Temporal",
             formattable::formattableOutput(ns("temporal_table"))),
    tabPanel("Taxonomic",
             formattable::formattableOutput(ns(
               "taxonomic_table"
             )))
  )))
}

# Module Server

#' @rdname mod_missing_data
#' @export
#' @keywords internal

mod_missing_data_server <-
  function(input, output, session, dataset_missing) {
    ns <- session$ns
    output$spatial_table <- formattable::renderFormattable({
      df <- dataset_missing()
      names <-
        c(
          "decimalLatitude",
          "decimalLongitude",
          "coordinateUncertaintyInMeters",
          "countryCode",
          "locality"
        )
      
      TotalRecords <-
        c(nrow(df["decimalLatitude"]),
          nrow(df["decimalLongitude"]),
          nrow(df["coordinateUncertaintyInMeters"]),
          nrow(df["countryCode"]),
          nrow(df["locality"]))
      
      MissingRecords <-
        c(sum(is.na(df["decimalLatitude"])),
          sum(is.na(df["decimalLongitude"])),
          sum(is.na(df["coordinateUncertaintyInMeters"])),
          sum(is.na(df["countryCode"])),
          sum(is.na(df["locality"])))
      RecordsPercentage <-
        c(
          round(((
            nrow(df["decimalLatitude"]) - sum(is.na(df["decimalLatitude"]))
          ) /
            nrow(df["decimalLatitude"])), 2) * 100,
          round(((
            nrow(df["decimalLongitude"]) -
              sum(is.na(df["decimalLongitude"]))
          ) /
            nrow(df["decimalLongitude"])), 2) * 100,
          round(((
            nrow(df["coordinateUncertaintyInMeters"]) -
              sum(is.na(df["coordinateUncertaintyInMeters"]))
          ) /
            nrow(df["coordinateUncertaintyInMeters"])), 2) * 100,
          round(((
            nrow(df["countryCode"]) - sum(is.na(df["countryCode"]))
          ) /
            nrow(df["countryCode"])), 2) * 100,
          round(((
            nrow(df["locality"]) - sum(is.na(df["locality"]))
          ) /
            nrow(df["locality"])), 2) * 100
        )
      
      table <-
        data.frame(names, TotalRecords, MissingRecords, RecordsPercentage)
      customRed <- "#ff7f7f"
      unit.scale = function(x)
        (x - min(x)) / (max(x) - min(x))
      formattable::formattable(table,
                               align = c("l", rep("r", NCOL(table) - 1)),
                               list(RecordsPercentage = color_bar(customRed,
                                                                  fun = unit.scale)))
    })
    
    output$temporal_table <- formattable::renderFormattable({
      df <- dataset_missing()
      names <-
        c(
          "eventDate",
          "day",
          "month",
          "year",
          "dateIdentified",
          "lastInterpreted"
        )
      
      TotalRecords <-
        c(nrow(df["eventDate"]),
          nrow(df["day"]),
          nrow(df["month"]),
          nrow(df["year"]),
          nrow(df["dateIdentified"]),
          nrow(df["lastInterpreted"]))
      
      MissingRecords <-
        c(sum(is.na(df["eventDate"])),
          sum(is.na(df["day"])),
          sum(is.na(df["month"])),
          sum(is.na(df["year"])),
          sum(is.na(df["dateIdentified"])),
          sum(is.na(df["lastInterpreted"])))
      RecordsPercentage <-
        c(
          round(((
            nrow(df["eventDate"]) - sum(is.na(df["eventDate"]))
          ) /
            nrow(df["eventDate"])), 2) * 100,
          round(((
            nrow(df["day"]) -
              sum(is.na(df["day"]))
          ) /
            nrow(df["day"])), 2) * 100,
          round(((
            nrow(df["month"]) -
              sum(is.na(df["month"]))
          ) /
            nrow(df["month"])), 2) * 100,
          round(((
            nrow(df["year"]) -
              sum(is.na(df["year"]))
          ) /
            nrow(df["year"])), 2) * 100,
          round(((
            nrow(df["dateIdentified"]) - sum(is.na(df["dateIdentified"]))
          ) /
            nrow(df["dateIdentified"])), 2) * 100,
          round(((
            nrow(df["lastInterpreted"]) - sum(is.na(df["lastInterpreted"]))
          ) /
            nrow(df["lastInterpreted"])), 2) * 100
        )
      
      table <-
        data.frame(names, TotalRecords, MissingRecords, RecordsPercentage)
      customRed <- "#ff7f7f"
      unit.scale = function(x)
        x/100
      formattable::formattable(table,
                               align = c("l", rep("r", NCOL(table) - 1)),
                               list(RecordsPercentage = color_bar(customRed,
                                                                  fun = unit.scale)))
    })
    
    
    output$taxonomic_table <- formattable::renderFormattable({
      df <- dataset_missing()
      names <-
        c(
          "kingdom",
          "phylum",
          "order",
          "family",
          "genus",
          "species",
          "identifiedBy"
        )
      
      TotalRecords <-
        c(nrow(df["kingdom"]),
          nrow(df["phylum"]),
          nrow(df["order"]),
          nrow(df["family"]),
          nrow(df["genus"]),
          nrow(df["species"]),
          nrow(df["identifiedBy"]))
      
      MissingRecords <-
        c(sum(is.na(df["kingdom"])),
          sum(is.na(df["phylum"])),
          sum(is.na(df["order"])),
          sum(is.na(df["family"])),
          sum(is.na(df["genus"])),
          sum(is.na(df["species"])),
          sum(is.na(df["identifiedBy"])))
      RecordsPercentage <-
        c(
          round(((
            nrow(df["kingdom"]) - sum(is.na(df["kingdom"]))
          ) /
            nrow(df["kingdom"])), 2) * 100,
          round(((
            nrow(df["phylum"]) -
              sum(is.na(df["phylum"]))
          ) /
            nrow(df["phylum"])), 2) * 100,
          round(((
            nrow(df["order"]) -
              sum(is.na(df["order"]))
          ) /
            nrow(df["order"])), 2) * 100,
          round(((
            nrow(df["family"]) -
              sum(is.na(df["family"]))
          ) /
            nrow(df["family"])), 2) * 100,
          round(((
            nrow(df["genus"]) - sum(is.na(df["genus"]))
          ) /
            nrow(df["genus"])), 2) * 100,
          round(((
            nrow(df["species"]) - sum(is.na(df["species"]))
          ) /
            nrow(df["species"])), 2) * 100,
          round(((
            nrow(df["identifiedBy"]) - sum(is.na(df["identifiedBy"]))
          ) /
            nrow(df["identifiedBy"])), 2) * 100
        )
      
      table <-
        data.frame(names, TotalRecords, MissingRecords, RecordsPercentage)
      customRed <- "#ff7f7f"
      unit.scale = function(x)
        x/100
      formattable::formattable(table,
                               align = c("l", rep("r", NCOL(table) - 1)),
                               list(RecordsPercentage = color_bar(customRed,
                                                                  fun = unit.scale)))
    })
    
    
    
    
    
  }
