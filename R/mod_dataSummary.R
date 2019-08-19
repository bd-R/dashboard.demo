# Module UI

#' @title   module for data summary
#' @description  This shiny Module helps user to view the summary of data such as % of records missing.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_dataSummary
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_dataSummary_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      style = 'padding-bottom:0px;',
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("Gauge1")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("Gauge2")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("Gauge3")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("Gauge4")
        )
      )
    ),
    fluidRow(
      style = 'padding-top:-50px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_A"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_B"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_C"),
          width = "100%"
        )
      )
    ),
    fluidRow(
      style = 'padding-top:-50px;',
      column(
        12,
        style = 'padding:20px;',
        tabsetPanel(
          tabPanel(
            "spatial", br(),
            fluidRow(
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("geo_coordinates"),
                  width = "100%"
                )
              ),
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("country_code"),
                  width = "100%"
                )
              ),
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("locality"),
                  width = "100%"
                )
              )
            )
          ),
          tabPanel(
            "Temporal", br(),
            fluidRow(
              column(
                6,
                shinydashboard::infoBoxOutput(
                  ns("startyear"),
                  width = "100%"
                )
              ),
              column(
                6,
                shinydashboard::infoBoxOutput(
                  ns("endyear"),
                  width = "100%"
                )
              )
            )
          ),
          tabPanel(
            "Taxonomic",
            fluidRow(
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("kingdom"),
                  width = "100%"
                )
              ),
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("phylum"),
                  width = "100%"
                )
              ),
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("order"),
                  width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("family"),
                  width = "100%"
                )
              ),
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("genus"),
                  width = "100%"
                )
              ),
              column(
                4,
                shinydashboard::infoBoxOutput(
                  ns("species"),
                  width = "100%"
                )
              )
            )
          )
        )
      )
    )
  )#End of fluidPage
}

# Module Server

#' @rdname mod_dataSummary
#' @export
#' @keywords internal
mod_dataSummary_server <-
function(input, output, session, dataset) {
    ns <- session$ns
    output$Gauge1 <- flexdashboard::renderGauge({
      df <- dataset()
      latitude <-
        round(((nrow(df["decimalLatitude"]) - sum(is.na(
          df["decimalLatitude"]
        ))) / nrow(df["decimalLatitude"])), 2) * 100
      longitude <-
        round(((nrow(df["decimalLongitude"]) - sum(is.na(
          df["decimalLongitude"]
        ))) / nrow(df["decimalLongitude"])), 2) * 100
      if (latitude > longitude) {
        geo <- longitude
      } else {
        geo <- latitude
      }
      gauge(
        geo,
        min = 0,
        max = 100,
        symbol = "%",
        label = "% of Plotable\nGeo coordinates",
        gaugeSectors(
          success = c(80, 100),
          warning = c(40, 79),
          danger = c(0, 39)
        )
      )
    })
    
    output$Gauge2 <- flexdashboard::renderGauge({
      df <- dataset()
      countryRecord <-
        round(((nrow(df["countryCode"]) - sum(is.na(
          df["countryCode"]
        ))) / nrow(df["countryCode"])), 2) * 100
      gauge(
        countryRecord,
        min = 0,
        max = 100,
        symbol = "%",
        label = "% of rows\nwith dateIdentified records",
        gaugeSectors(
          success = c(80, 100),
          warning = c(40, 79),
          danger = c(0, 39)
        )
      )
    })
    
    output$Gauge3 <- flexdashboard::renderGauge({
      df <- dataset()
      institutionCode <-
        round(((nrow(df["institutionCode"]) - sum(is.na(
          df["institutionCode"]
        ))) / nrow(df["institutionCode"])), 2) * 100
      gauge(
        institutionCode,
        min = 0,
        max = 100,
        symbol = "%",
        label = "% of rows\nwith occurence remark",
        gaugeSectors(
          success = c(80, 100),
          warning = c(40, 79),
          danger = c(0, 39)
        )
      )
    })
    
    output$Gauge4 <- flexdashboard::renderGauge({
      df <- dataset()
      basisOfRecord <-
        round(((nrow(df["basisOfRecord"]) - sum(is.na(
          df["basisOfRecord"]
        ))) / nrow(df["basisOfRecord"])), 2) * 100
      gauge(
        basisOfRecord,
        min = 0,
        max = 100,
        symbol = "%",
        label = "% of rows\nwith eventTime records",
        gaugeSectors(
          success = c(80, 100),
          warning = c(40, 79),
          danger = c(0, 39)
        )
      )
    })
    
    

    output$box_A <-
      shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = (nrow(dataset()["decimalLatitude"])),
          subtitle = "# of Records",
          icon = icon("compass"),
          color = "aqua",
          width = 1
        )
      })
    
    output$box_B <-
      shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = nrow(unique(dataset()["scientificName"])),
          subtitle = "# of Taxa",
          icon = icon("file-signature"),
          color = "blue",
          width = 1
        )
      })
    
    output$box_C <-
      shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = length(dataset()),
          subtitle = "# of Attributes",
          icon = icon("area-chart"),
          color = "light-blue",
          width = 1
        )
      })
    
    
    
    #Spatial.......................................
    output$geo_coordinates <- shinydashboard::renderInfoBox({
      latitude <- nrow((na.omit(dataset()["decimalLatitude"])))
      longitude <- nrow((na.omit(dataset()["decimalLongitude"])))
      shinydashboard::infoBox(
        "# of Geo Coordinates",
        if(latitude>longitude){
          longitude
        }else{
          latitude
        },
        icon = icon("compass"),
        color = "red",
        width = 4
      )
    })
    
    output$country_code <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Countries",
        nrow(unique(na.omit(dataset(
          
        )["countryCode"]))),
        icon = icon("copyright"),
        color = "navy",
        width = 4
      )
    })
    
    output$locality <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Localities",
        nrow(unique(na.omit(dataset(
          
        )["locality"]))),
        icon = icon("street-view"),
        color = "yellow",
        width = 4
      )
    })
    
    output$coordinate_uncertainty <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of coordinateUncertaintyInMeters",
        nrow(unique(na.omit(dataset(
          
        )["coordinateUncertaintyInMeters"]))),
        icon = icon("compass"),
        color = "teal",
        width = 4
      )
    })
    
    #Temporal.......................................
    output$startyear <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Starting Year",
        min(na.omit(as.data.frame((dataset()["year"])))),
        icon = icon("stripe-s"),
        color = "teal",
        width = 6
      )
    })
    
    output$endyear <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "End Year",
        max(na.omit(as.data.frame((dataset()["year"])))),
        icon = icon("etsy"),
        color = "navy",
        width = 6
      )
    })
    
    
    #Taxonomic.......................................
    output$kingdom <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Kingdom",
        nrow(unique(na.omit(dataset()["kingdom"]))),
        icon = icon("korvue"),
        color = "red",
        width = 4
      )
    })
    
    output$phylum <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Phylum",
        nrow(unique(na.omit(dataset(
          
        )["phylum"]))),
        icon = icon("product-hunt"),
        color = "orange",
        width = 4
      )
    })
    
    output$order <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Order",
        nrow(unique(na.omit(dataset(
          
        )["order"]))),
        icon = icon("opera"),
        color = "green",
        width = 4
      )
    })
    
    output$family <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Family",
        nrow(unique(na.omit(dataset(
          
        )["family"]))),
        icon = icon("facebook-f"),
        color = "navy",
        width = 4
      )
    })
    
    output$genus <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Genus",
        nrow(unique(na.omit(dataset(
          
        )["genus"]))),
        icon = icon("gofore"),
        color = "yellow",
        width = 4
      )
    })
    
    output$species <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Species",
        nrow(unique(na.omit(dataset(
        )["species"]))),
        icon = icon("stripe-s"),
        color = "teal",
        width = 4)
    })
    

  }