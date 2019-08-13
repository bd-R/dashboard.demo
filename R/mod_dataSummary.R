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
            "spatial",
            DT::dataTableOutput(
              ns("spatialTable")
            )
          ),
          tabPanel(
            "Temporal",
            fluidRow(
              column(
                3,
                style = "padding:20px",
                fluidRow(
                  shinydashboard::valueBoxOutput(
                    ns("yearstart"),
                    width = "40%"
                  )
                ),
                fluidRow(
                  shinydashboard::valueBoxOutput(
                    ns("yearend"),
                    width = "40%"
                  )
                )
              ),
              column(
                9,
                DT::dataTableOutput(
                  ns("temporalTable")
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
    
    output$spatialTable <- DT::renderDataTable({
      df <- dataset()
      names <-
        c(
          "decimalLatitude",
          "decimalLongitude",
          "coordinateUncertaintyInMeters",
          "coordinatePrecision",
          "countryCode",
          "locality"
        )
      
      TotalRecords <-
        c(nrow(df["decimalLatitude"]),
          nrow(df["decimalLongitude"]),
          nrow(df["coordinateUncertaintyInMeters"]),
          nrow(df["coordinatePrecision"]),
          nrow(df["countryCode"]),
          nrow(df["locality"]))
      
      MissingRecords <-
        c(sum(is.na(df["decimalLatitude"])),
          sum(is.na(df["decimalLongitude"])),
          sum(is.na(df["coordinateUncertaintyInMeters"])),
          sum(is.na(df["coordinatePrecision"])),
          sum(is.na(df["countryCode"])),
          sum(is.na(df["locality"])))
      RecordsPercentage <-
        c(
          paste0(round((
            (nrow(df["decimalLatitude"]) - sum(is.na(df["decimalLatitude"]))) /
              nrow(df["decimalLatitude"])
          ), 2) * 100, "%"),
          
          paste0(round((
            (nrow(df["decimalLongitude"]) -
               sum(is.na(df["decimalLongitude"]))) /
              nrow(df["decimalLongitude"])
          ), 2) * 100,
          "%"),
          
          paste0(round((
            (nrow(df["coordinateUncertaintyInMeters"]) -
               sum(is.na(df["coordinateUncertaintyInMeters"]))) /
              nrow(df["coordinateUncertaintyInMeters"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["coordinatePrecision"]) -
               sum(is.na(df["coordinatePrecision"]))) /
              nrow(df["coordinatePrecision"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["countryCode"]) - sum(is.na(df["countryCode"]))) /
              nrow(df["countryCode"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["locality"]) - sum(is.na(df["locality"]))) /
              nrow(df["locality"])
          ), 2) * 100,
          "%")
        )
      
      table <-
        data.frame(names, TotalRecords, MissingRecords, RecordsPercentage)
      table
    })
    
    output$temporalTable <- DT::renderDataTable({
      df <- dataset()
      names <- c("eventDate",
                 "day",
                 "month",
                 "year",
                 "dateIdentified",
                 "lastInterpreted")
      
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
          paste0(round((
            (nrow(df["eventDate"]) - sum(is.na(df["eventDate"]))) /
              nrow(df["eventDate"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["day"]) - sum(is.na(df["day"]))) /
              nrow(df["day"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["month"]) - sum(is.na(df["month"]))) /
              nrow(df["month"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["year"]) - sum(is.na(df["year"]))) /
              nrow(df["year"])
          ), 2) * 100, "%"),
          paste0(round((
            (nrow(df["dateIdentified"]) - sum(is.na(df["dateIdentified"]))) /
              nrow(df["dateIdentified"])
          ), 2) * 100,
          "%"),
          paste0(round((
            (nrow(df["lastInterpreted"]) - sum(is.na(df["lastInterpreted"]))) /
              nrow(df["lastInterpreted"])
          ), 2) * 100, "%")
        )
      
      tableTemporal <-
        data.frame(names, TotalRecords, MissingRecords, RecordsPercentage)
      tableTemporal
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
          color = "aqua",
          width = 1
        )
      })
    
    output$box_C <-
      shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = length(dataset()),
          subtitle = "# of Attributes",
          icon = icon("area-chart"),
          color = "aqua",
          width = 1
        )
      })
    
    output$yearstart <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = min(na.omit(formattedData()["Year_"])),
        subtitle = "Starting Year",
        icon = icon("clock"),
        color = "aqua",
        width = 1
      )
    })
    
    output$yearend <- shinydashboard::renderValueBox({
      shinydashboard::valueBox(
        value = max(na.omit(formattedData()["Year_"])),
        subtitle = "ENd Year",
        icon = icon("clock"),
        color = "aqua",
        width = 1
      )
    })
    
    output$kingdom <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Kingdom",
        nrow(unique(na.omit(dataset(
          
        )["kingdom"]))),
        icon = icon("clock"),
        color = "aqua",
        width = 4
      )
    })
    
    output$phylum <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Kingdom",
        nrow(unique(na.omit(dataset(
          
        )["phylum"]))),
        icon = icon("clock"),
        color = "aqua",
        width = 4
      )
    })
    
    output$order <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Kingdom",
        nrow(unique(na.omit(dataset(
          
        )["order"]))),
        icon = icon("clock"),
        color = "aqua",
        width = 4
      )
    })
    
    output$family <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Kingdom",
        nrow(unique(na.omit(dataset(
          
        )["family"]))),
        icon = icon("clock"),
        color = "aqua",
        width = 4
      )
    })
    
    output$genus <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "# of Kingdom",
        nrow(unique(na.omit(dataset(
          
        )["genus"]))),
        icon = icon("clock"),
        color = "aqua",
        width = 4
      )
    })
    
    output$species <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox("# of Kingdom",
                              nrow(unique(na.omit(dataset(
                                
                              )["species"]))),
                              color = "aqua",
                              width = 4)
    })
    
    formattedData <- reactive({
      dataset <- dataset()
      dataForBar <- format_bdvis(dataset, source = 'rgbif')
      names(dataForBar) = gsub("\\.", "_", names(dataForBar))
      if ("Date_collected" %in% colnames(dataForBar)) {
        if (length(which(!is.na(dataForBar$Date_collected))) == 0) {
          stop("Date_collected has no data")
        }
        dayofYear <- as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%d"
          )
        )
        weekofYear <- as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%U"
          )
        )
        monthofYear <- as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%m"
          )
        )
        Year_ = as.numeric(
          strftime(
            as.Date(
              dataForBar$Date_collected,
              na.rm = T
            ),
            format = "%Y"
          )
        )
        dataForBar <-
          cbind(dataForBar[c("basisOfRecord",
                             "kingdom",
                             "phylum",
                             "order",
                             "family",
                             "genus",
                             "species")], dayofYear, weekofYear, monthofYear, Year_)
      } else {
        stop("Date_collected not found in data. Please use format_bdvis() to fix the problem")
      }
      return(dataForBar)
    })
    
    output$bar <- renderPlotly({
      dataForBar <-
        arrange(formattedData(), as.numeric(formattedData()$Year_))
      dataForBar <- dataForBar[c(input$barselect, "Year_")]
      dataForBar <-
        data.frame(table(dataForBar)) %>%
        dplyr::rename(
          group = input$barselect,
          variable = Year_,
          value = Freq
        )
      plot_ly(
        dataForBar,
        source = "barselected",
        x = ~ value,
        y = ~ variable,
        color = ~ group
      ) %>%  
        layout(showlegend = FALSE, height = 250) %>%
        add_bars()
    })
    output$totalCountry <-
      shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = nrow(unique(dataset()["countryCode"])),
          subtitle = "# of Countries",
          icon = icon("area-chart"),
          color = "aqua",
          width = 1
        )
      })
    
    output$naCountry <-
      shinydashboard::renderValueBox({
        shinydashboard::valueBox(
          value = rowSums(is.na(dataset()["countryCode"])),
          subtitle = "# Missing country",
          icon = icon("area-chart"),
          color = "aqua",
          width = 1
        )
      })
    
    output$countryBar <- renderPlotly({
      country <-
        data.frame(table(na.omit(dataset()["countryCode"]))) %>%
        dplyr::rename(CountryName = Var1,
                      NumberOfRecords = Freq)
      plot_ly(
        data = country,
        x = ~ CountryName,
        y = ~ NumberOfRecords,
        name = "Countries",
        type = "bar"
      ) %>%
        layout(showlegend = FALSE, height = 350)
    })
    
    output$sunbrust <- renderSunburst({
      data <- dataset()
      if (!nrow(data[-which(data[, "genus"] == ""), ]) == 0) {
        data <- data[-which(data[, "genus"] == ""), ]
      }
      if (!nrow(data[-which(data[, "family"] == ""), ]) == 0) {
        data <- data[-which(data[, "family"] == ""), ]
      }
      if (!nrow(data[-which(data[, "order"] == ""), ]) == 0) {
        data <- data[-which(data[, "order"] == ""), ]
      }
      if (!nrow(data[-which(data[, "phylum"] == ""), ]) == 0) {
        data <- data[-which(data[, "phylum"] == ""), ]
      }
      data <- arrange(data, family)
      temp <- as.data.frame(table(data["genus"]))
      data <- unique(data)
      temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
      temp <- temp[c("phylum", "order", "family", "genus", "Freq")]
      temp <- temp %>%
        mutate(path = paste(phylum, order, family, genus, sep = "-")) %>%
        dplyr::select(path, Freq)
      # Plot
      sunburst(temp, legend = FALSE)
    })
  }