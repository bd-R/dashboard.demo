# Module UI

#' @title   mod_spatial_ui and mod_spatial_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_spatial
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_spatial_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        class = "noPadding",
        4,
        plotlyOutput(
          ns("countryBar"),
          height = "360px"
        )
      ),
      column(
        class =  "noPadding",
        4,
        plotlyOutput(
          ns("pie"),
          height = "360px"
        ),
        absolutePanel(
          top = 10,
          left = 20,
          selectInput(
            ns("pieselect"),
            "Select Column to be displayed",
            c(
              "basisOfRecord",
              "kingdom",
              "phylum",
              "order",
              "family",
              "genus",
              "species"
            ),
            selected = "basisOfRecord"
          )
        )
      ),
      column(class = "noPadding",
             4,
             plotlyOutput(
               ns("records"),
               height = "360px")
      )
    ),
    fluidRow(
      column(class = "noPadding",
             12,
             leafletOutput(
               ns("mymap"),
               height = "240px"
             ),
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
      )
    )
  )
}

# Module Server

#' @rdname mod_spatial
#' @export
#' @keywords internal

mod_spatial_server <- function(input, output, session, data) {
  ns <- session$ns
  formattedData <- reactive({
    dataForBar <- format_bdvis(data(), source = 'rgbif')
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
  
  
  output$countryBar <- renderPlotly({
    country <-
      data.frame(table(na.omit(data()["countryCode"]))) %>%
      dplyr::rename(CountryName = Var1,
                    NumberOfRecords = Freq
      )
    plot_ly(
      data = country,
      source = "barCountrt",
      x = ~ CountryName,
      y = ~ NumberOfRecords,
      name = "Countries",
      type = "bar"
    ) %>%
      layout(
        showlegend = FALSE,
        height = 320,
        paper_bgcolor = '#000000',
        plot_bgcolor = '#000000',
        xaxis = list(color = '#ffffff'),
        yaxis = list(color = '#ffffff'),
        leagend = list(color = '#ffffff')
      )
  })
  
  observe({
    click <- event_data("plotly_click", source = "barCountrt")
    if (is.null(click)) {
      output$mymap <- renderLeaflet({
        leaflet(data = data()) %>%
          addProviderTiles(input$mapTexture) %>%
          addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
      })
      output$temp <- renderText("as")
    } else {
      new <- data() %>% 
        filter(countryCode %in% click$x)
      leafletProxy("mymap", data = new) %>% 
        clearShapes() %>%
        addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    }
  })
  
  observe({
    click <- event_data("plotly_selected", source = "barCountrt")
    if (is.null(click)) {
      output$mymap <- renderLeaflet({
        leaflet(data = data()) %>%
          addProviderTiles(input$mapTexture) %>%
          addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
      })
      output$temp <- renderText("as")
    } else {
      new <- data() %>% 
        filter(countryCode %in% click$x)
      leafletProxy("mymap", data = new) %>% 
        clearShapes() %>%
        addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
    }
  })
  
  observe({
    select <- event_data("plotly_click", source = "barCountrt")
    if (is.null(select)) {
      output$pie <- renderPlotly({
        if (input$pieselect == "kingdom") {
          label <- ~ kingdom
        } else if (input$pieselect == "phylum") {
          label <- ~ phylum
        } else if (input$pieselect == "family") {
          label <- ~ family
        } else if (input$pieselect == "genus") {
          label <- ~ genus
        } else if (input$pieselect == "species") {
          label <- ~ species
        } else if (input$pieselect == "order") {
          label <- ~ order
        } else {
          label <- ~ basisOfRecord
        }
        if (!nrow(data()[-which(data()[, input$pieselect] == ""),]) == 0) {
          dataa <- data()[-which(data()[, input$pieselect] == ""),]
        } else {
          dataa <- data()
        }
        
        plot_ly(
          data = na.omit(dataa[c("basisOfRecord",
                                 "kingdom",
                                 "phylum",
                                 "order",
                                 "family",
                                 "genus",
                                 "species")]),
          labels = label,
          type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text'
        ) %>% 
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
    } else {
      #create new dataset based on where user clicked on bar graph
      newData <- data() %>% 
        filter(countryCode %in% select$x)
      output$pie <- renderPlotly({
        if (input$pieselect == "kingdom") {
          label <- ~ kingdom
        } else if (input$pieselect == "phylum") {
          label <- ~ phylum
        } else if (input$pieselect == "family") {
          label <- ~ family
        } else if (input$pieselect == "genus") {
          label <- ~ genus
        } else if (input$pieselect == "species") {
          label <- ~ species
        } else if (input$pieselect == "order") {
          label <- ~ order
        } else {
          label <- ~ basisOfRecord
        }
        #Remove blank data from column(Blank! Not NA)
        if (!nrow(newData[-which(newData[, input$pieselect] == ""),]) == 0) {
          newData <- newData[-which(newData[, input$pieselect] == ""),]
        }
        
        plot_ly(
          data = na.omit(newData[c("basisOfRecord",
                                   "kingdom",
                                   "phylum",
                                   "order",
                                   "family",
                                   "genus",
                                   "species")]),
          labels = label,
          type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text'
        ) %>%
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
    }
  })
  
  observe({
    select <- event_data("plotly_selected", source = "barCountrt")
    if (is.null(select)) {
      output$pie <- renderPlotly({
        if (input$pieselect == "kingdom") {
          label <- ~ kingdom
        } else if (input$pieselect == "phylum") {
          label <- ~ phylum
        } else if (input$pieselect == "family") {
          label <- ~ family
        } else if (input$pieselect == "genus") {
          label <- ~ genus
        } else if (input$pieselect == "species") {
          label <- ~ species
        } else if (input$pieselect == "order") {
          label <- ~ order
        } else {
          label <- ~ basisOfRecord
        }
        if (!nrow(data()[-which(data()[, input$pieselect] == ""),]) == 0) {
          dataa <- data()[-which(data()[, input$pieselect] == ""),]
        } else {
          dataa <- data()
        }
        
        plot_ly(
          data = na.omit(dataa[c("basisOfRecord",
                                 "kingdom",
                                 "phylum",
                                 "order",
                                 "family",
                                 "genus",
                                 "species")]),
          labels = label,
          type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text'
        ) %>%
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
    } else {
      #create new dataset based on where user clicked on bar graph
      newData <- data() %>% 
        filter(countryCode %in% select$x)
      output$pie <- renderPlotly({
        if (input$pieselect == "kingdom") {
          label <- ~ kingdom
        } else if (input$pieselect == "phylum") {
          label <- ~ phylum
        } else if (input$pieselect == "family") {
          label <- ~ family
        } else if (input$pieselect == "genus") {
          label <- ~ genus
        } else if (input$pieselect == "species") {
          label <- ~ species
        } else if (input$pieselect == "order") {
          label <- ~ order
        } else {
          label <- ~ basisOfRecord
        }
        #Remove blank data from column(Blank! Not NA)
        if (!nrow(newData[-which(newData[, input$pieselect] == ""),]) == 0) {
          newData <- newData[-which(newData[, input$pieselect] == ""),]
        }
        
        plot_ly(
          data = na.omit(newData[c("basisOfRecord",
                                   "kingdom",
                                   "phylum",
                                   "order",
                                   "family",
                                   "genus",
                                   "species")]),
          labels = label,
          type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text'
        ) %>% 
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
    }
  })
  
  observe({
    select <- event_data("plotly_click", source = "barCountrt")
    if (is.null(select)) {
      output$records <- renderPlotly({
        dataload <- data()
        recordData1 <-
          (data.frame(
            names <-
              c(
                "kingdom",
                "phylum",
                "order",
                "family",
                "genus",
                "species"
              ),
            freq <-
              c(
                nrow(unique(na.omit(dataload["kingdom"]))),
                nrow(unique(na.omit(dataload["phylum"]))),
                nrow(unique(na.omit(dataload["order"]))),
                nrow(unique(na.omit(dataload["family"]))),
                nrow(unique(na.omit(dataload["genus"]))),
                nrow(unique(na.omit(dataload["species"])))
              )
          ))
        names(recordData1) <-
          c("NameOfField", "NumberOfUniqueNames")
        plot_ly(
          data = recordData1,
          x = ~ NameOfField,
          y = ~ NumberOfUniqueNames,
          name = "Frequency of records",
          type = "bar"
        ) %>%
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
    } else {
      output$records <- renderPlotly({
        newFilterData <- data() %>% 
          filter(countryCode %in% select$x)
        dataload <- newFilterData
        recordData1 <-
          (data.frame(
            names <-
              c(
                "kingdom",
                "phylum",
                "order",
                "family",
                "genus",
                "species"
              ),
            freq <-
              c(
                nrow(unique(na.omit(dataload["kingdom"]))),
                nrow(unique(na.omit(dataload["phylum"]))),
                nrow(unique(na.omit(dataload["order"]))),
                nrow(unique(na.omit(dataload["family"]))),
                nrow(unique(na.omit(dataload["genus"]))),
                nrow(unique(na.omit(dataload["species"])))
              )
          ))
        names(recordData1) <-
          c("NameOfField", "NumberOfUniqueNames")
        plot_ly(
          data = recordData1,
          x = ~ NameOfField,
          y = ~ NumberOfUniqueNames,
          name = "Frequency of records",
          type = "bar"
        ) %>% 
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
      
      
    }
  })
  
  observe({
    select <- event_data("plotly_selected", source = "barCountrt")
    if (is.null(select)) {
      output$records <- renderPlotly({
        dataload <- data()
        recordData1 <-
          (data.frame(
            names <-
              c(
                "kingdom",
                "phylum",
                "order",
                "family",
                "genus",
                "species"
              ),
            freq <-
              c(
                nrow(unique(na.omit(dataload["kingdom"]))),
                nrow(unique(na.omit(dataload["phylum"]))),
                nrow(unique(na.omit(dataload["order"]))),
                nrow(unique(na.omit(dataload["family"]))),
                nrow(unique(na.omit(dataload["genus"]))),
                nrow(unique(na.omit(dataload["species"])))
              )
          ))
        names(recordData1) <-
          c("NameOfField", "NumberOfUniqueNames")
        plot_ly(
          data = recordData1,
          x = ~ NameOfField,
          y = ~ NumberOfUniqueNames,
          name = "Frequency of records",
          type = "bar"
        ) %>%
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
    } else {
      output$records <- renderPlotly({
        newFilterData <- data() %>%
          filter(countryCode %in% select$x)
        dataload <- newFilterData
        recordData1 <-
          (data.frame(
            names <-
              c(
                "kingdom",
                "phylum",
                "order",
                "family",
                "genus",
                "species"
              ),
            freq <-
              c(
                nrow(unique(na.omit(dataload["kingdom"]))),
                nrow(unique(na.omit(dataload["phylum"]))),
                nrow(unique(na.omit(dataload["order"]))),
                nrow(unique(na.omit(dataload["family"]))),
                nrow(unique(na.omit(dataload["genus"]))),
                nrow(unique(na.omit(dataload["species"])))
              )
          ))
        names(recordData1) <-
          c("NameOfField", "NumberOfUniqueNames")
        plot_ly(
          data = recordData1,
          x = ~ NameOfField,
          y = ~ NumberOfUniqueNames,
          name = "Frequency of records",
          type = "bar"
        ) %>%
          layout(
            showlegend = FALSE,
            height = 320,
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      })
      
      
    }
  })
}


## To be copied in the UI
# mod_spatial_ui("spatial_ui_1")

## To be copied in the server
# callModule(mod_spatial_server, "spatial_ui_1")
