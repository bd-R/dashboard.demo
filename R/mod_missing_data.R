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
    df <- dataset_missing()
    names <-
      c(
        "decimal_latitude",
        "decimal_longitude",
        "coordinate_uncertainty_in_meters",
        "country_code",
        "locality",
        "coordinate_precision",
        "elevation",
        "elevation_accuracy",
        "depth",
        "depth_accuracy",
        "establishment_means"
      )
    
    TotalRecords <-
      c(nrow(df["decimalLatitude"]),
        nrow(df["decimalLongitude"]),
        nrow(df["coordinateUncertaintyInMeters"]),
        nrow(df["countryCode"]),
        nrow(df["locality"]),
        nrow(df["coordinatePrecision"]),
        nrow(df["elevation"]),
        nrow(df["elevationAccuracy"]),
        nrow(df["depth"]),
        nrow(df["depthAccuracy"]),
        nrow(df["establishmentMeans"])
      )
    
    MissingRecords <-
      c(sum(is.na(df["decimalLatitude"])),
        sum(is.na(df["decimalLongitude"])),
        sum(is.na(df["coordinateUncertaintyInMeters"])),
        sum(is.na(df["countryCode"])),
        sum(is.na(df["locality"])),
        sum(is.na(df["coordinatePrecision"])),
        sum(is.na(df["elevation"])),
        sum(is.na(df["elevationAccuracy"])),
        sum(is.na(df["depth"])),
        sum(is.na(df["depthAccuracy"])),
        sum(is.na(df["establishmentMeans"]))
      )
  
    RecordsPercentage <-
      c(
        round(
          (
            (
              nrow(
                df["decimalLatitude"]
                ) - sum(
                  is.na(
                    df["decimalLatitude"]
                  )
                )
              ) /
              nrow(
                df["decimalLatitude"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["decimalLongitude"]
              ) - sum(
                is.na(
                  df["decimalLongitude"]
                )
              )
            ) /
              nrow(
                df["decimalLongitude"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["coordinateUncertaintyInMeters"]
              ) - sum(
                is.na(
                  df["coordinateUncertaintyInMeters"]
                )
              )
            ) /
              nrow(
                df["coordinateUncertaintyInMeters"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["countryCode"]
              ) - sum(
                is.na(
                  df["countryCode"]
                )
              )
            ) /
              nrow(
                df["countryCode"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["locality"]
              ) - sum(
                is.na(
                  df["locality"]
                )
              )
            ) /
              nrow(
                df["locality"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["coordinatePrecision"]
              ) - sum(
                is.na(
                  df["coordinatePrecision"]
                )
              )
            ) /
              nrow(
                df["coordinatePrecision"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["elevation"]
              ) - sum(
                is.na(
                  df["elevation"]
                )
              )
            ) /
              nrow(
                df["elevation"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["elevationAccuracy"]
              ) - sum(
                is.na(
                  df["elevationAccuracy"]
                )
              )
            ) /
              nrow(
                df["elevationAccuracy"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["depth"]
              ) - sum(
                is.na(
                  df["depth"]
                )
              )
            ) /
              nrow(
                df["depth"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["depthAccuracy"]
              ) - sum(
                is.na(
                  df["depthAccuracy"]
                )
              )
            ) /
              nrow(
                df["depthAccuracy"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["establishmentMeans"]
              ) - sum(
                is.na(
                  df["establishmentMeans"]
                )
              )
            ) /
              nrow(
                df["establishmentMeans"]
              )
          ),
          2
        ) * 100
      )
      
    table <- data.frame(
      names,
      TotalRecords,
      MissingRecords,
      RecordsPercentage
    )
      
    customRed <- "#ff7f7f"
      
    unit.scale = function(x)
      (x - min(x)) / (max(x) - min(x))
      
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
        RecordsPercentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
  
  #Calculating missing data and create the table for Temporal Tab  
  output$temporal_table <- formattable::renderFormattable({
    df <- dataset_missing()
    names <-
      c(
        "event_date",
        "day",
        "month",
        "year",
        "date_identified",
        "last_interpreted"
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
        round(
          (
            (
              nrow(
                df["eventDate"]
              ) - sum(
                is.na(
                  df["eventDate"]
                )
              )
            ) /
              nrow(
                df["eventDate"]
               )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["day"]
              ) - sum(
                is.na(
                  df["day"]
                )
              )
            ) /
              nrow(
                df["day"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["month"]
              ) - sum(
                is.na(
                  df["month"]
                )
              )
            ) /
              nrow(
                df["month"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["year"]
              ) - sum(
                is.na(
                  df["year"]
                )
              )
            ) /
              nrow(
                df["year"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["dateIdentified"]
              ) - sum(
                is.na(
                  df["dateIdentified"]
                )
              )
            ) /
              nrow(
                df["dateIdentified"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["lastInterpreted"]
              ) - sum(
                is.na(
                  df["lastInterpreted"]
                )
              )
            ) /
              nrow(
                df["lastInterpreted"]
              )
          ),
          2
        ) * 100
      )
      
    table <- data.frame(
      names,
      TotalRecords,
      MissingRecords,
      RecordsPercentage
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
        RecordsPercentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
    
  #Calculating missing data and create the table for Taxonomic Tab
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
        "identified_by",
        "infraspecificEpithet",
        "taxon_rank",
        "scientific_name",
        "taxon_key",
        "species_key",
        "date_identified",
        "recorded_by",
        "record_number"
      )
      
    TotalRecords <-
      c(nrow(df["kingdom"]),
        nrow(df["phylum"]),
        nrow(df["order"]),
        nrow(df["family"]),
        nrow(df["genus"]),
        nrow(df["species"]),
        nrow(df["identifiedBy"]),
        nrow(df["infraspecificEpithet"]),
        nrow(df["taxonRank"]),
        nrow(df["scientificName"]),
        nrow(df["taxonKey"]),
        nrow(df["speciesKey"]),
        nrow(df["dateIdentified"]),
        nrow(df["recordedBy"]),
        nrow(df["recordNumber"])
      )
    
    MissingRecords <-
      c(sum(is.na(df["kingdom"])),
        sum(is.na(df["phylum"])),
        sum(is.na(df["order"])),
        sum(is.na(df["family"])),
        sum(is.na(df["genus"])),
        sum(is.na(df["species"])),
        sum(is.na(df["identifiedBy"])),
        sum(is.na(df["infraspecificEpithet"])),
        sum(is.na(df["taxonRank"])),
        sum(is.na(df["scientificName"])),
        sum(is.na(df["taxonKey"])),
        sum(is.na(df["speciesKey"])),
        sum(is.na(df["dateIdentified"])),
        sum(is.na(df["recordedBy"])),
        sum(is.na(df["recordNumber"]))
      )
    
    RecordsPercentage <-
      c(
        round(
          (
            (
            nrow(
              df["kingdom"]
              ) - sum(
                is.na(
                  df["kingdom"]
                )
              )
            ) /
              nrow(
                df["kingdom"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["phylum"]
              ) - sum(
                is.na(
                  df["phylum"]
                )
              )
            ) /
              nrow(
                df["phylum"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["order"]
              ) - sum(
                is.na(
                  df["order"]
                )
              )
            ) /
              nrow(
                df["order"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["family"]
              ) - sum(
                is.na(
                  df["family"]
                )
              )
            ) /
              nrow(
                df["family"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["genus"]
              ) - sum(
                is.na(
                  df["genus"]
                )
              )
            ) /
              nrow(
                df["genus"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["species"]
              ) - sum(
                is.na(
                  df["species"]
                )
              )
            ) /
              nrow(
                df["species"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["identifiedBy"]
              ) - sum(
                is.na(
                  df["identifiedBy"]
                )
              )
            ) /
              nrow(
                df["identifiedBy"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["infraspecificEpithet"]
              ) - sum(
                is.na(
                  df["infraspecificEpithet"]
                )
              )
            ) /
              nrow(
                df["infraspecificEpithet"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["taxonRank"]
              ) - sum(
                is.na(
                  df["taxonRank"]
                )
              )
            ) /
              nrow(
                df["taxonRank"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["scientificName"]
              ) - sum(
                is.na(
                  df["scientificName"]
                )
              )
            ) /
              nrow(
                df["scientificName"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["taxonKey"]
              ) - sum(
                is.na(
                  df["taxonKey"]
                )
              )
            ) /
              nrow(
                df["taxonKey"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["speciesKey"]
              ) - sum(
                is.na(
                  df["speciesKey"]
                )
              )
            ) /
              nrow(
                df["speciesKey"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["dateIdentified"]
              ) - sum(
                is.na(
                  df["dateIdentified"]
                )
              )
            ) /
              nrow(
                df["dateIdentified"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["recordedBy"]
              ) - sum(
                is.na(
                  df["recordedBy"]
                )
              )
            ) /
              nrow(
                df["recordedBy"]
              )
          ),
          2
        ) * 100,
        round(
          (
            (
              nrow(
                df["recordNumber"]
              ) - sum(
                is.na(
                  df["recordNumber"]
                )
              )
            ) /
              nrow(
                df["recordNumber"]
              )
          ),
          2
        ) * 100
      )
      
    table <- data.frame(
      names,
      TotalRecords,
      MissingRecords,
      RecordsPercentage
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
        RecordsPercentage = color_bar(
          customRed,
          fun = unit.scale
        )
      )
    )
  })
}