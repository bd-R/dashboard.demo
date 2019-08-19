# Module UI

#' @title   Module for visualizing spatial data
#' @description  This model is to visualize space related data such as latitude/longitude.
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
          ns("country_bar"),
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
             fluidRow(
               column(6,
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
                      )
                      ),
               column(6,
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
             leafletOutput(
               ns("mymap"),
               height = "240px"
             ),
             DT::dataTableOutput(ns("ta"))
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

  output$country_bar <- renderPlotly({
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

      output$mymap <- renderLeaflet({
        leaflet(data = na.omit(data()[c("decimalLatitude", "decimalLongitude")])) %>%
          addProviderTiles(input$mapTexture) %>%
          addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor) %>%
          leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                         circleOptions = FALSE,
                         markerOptions = FALSE,
                         rectangleOptions = FALSE,
                         circleMarkerOptions = FALSE,
                         editOptions = leaflet.extras::editToolbarOptions())%>%
          addLayersControl(overlayGroups = c('draw'), options =
                             layersControlOptions(collapsed=FALSE)) 
      })
      
      
      map_selected <- reactive({
        data <- na.omit(data()[c("decimalLatitude", "decimalLongitude")])
        cities_coordinates <- SpatialPointsDataFrame(data[,c("decimalLongitude","decimalLatitude")],data)
        
        #use the draw_stop event to detect when users finished drawing
        req(input$mymap_draw_stop)
        print(input$mymap_draw_new_feature)
        
        
        #get the coordinates of the polygon
        polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
        
        #transform them to an sp Polygon
        drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
        
        #use over from the sp package to identify selected cities
        selected_cities <- cities_coordinates %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
        
        #print the name of the cities
        geo <- as.data.frame(
          data[which(!is.na(selected_cities)), c("decimalLatitude", "decimalLongitude")])
        
        names(geo) <- c("decimalLatitude", "decimalLongitude")
        return(geo)
      })
      output$ta <- DT::renderDataTable({
        map_selected()
      })
    
  
  
  # observe({
  #   click <- event_data("plotly_click", source = "barCountrt")
  #   if (is.null(click)) {
  #     output$mymap <- renderLeaflet({
  #       leaflet(data = data()) %>%
  #         addProviderTiles(input$mapTexture) %>%
  #         addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  #     })
  #   } else {
  #     new <- data() %>% 
  #       filter(countryCode %in% click$x)
  #     leafletProxy("mymap", data = new) %>% 
  #       clearShapes() %>%
  #       addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  #   }
  # })
  
  # observe({
  #   click <- event_data("plotly_selected", source = "barCountrt")
  #   if (is.null(click)) {
  #     output$mymap <- renderLeaflet({
  #       leaflet(data = data()) %>%
  #         addProviderTiles(input$mapTexture) %>%
  #         addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  #     })
  #     output$temp <- renderText("as")
  #   } else {
  #     new <- data() %>% 
  #       filter(countryCode %in% click$x)
  #     leafletProxy("mymap", data = new) %>% 
  #       clearShapes() %>%
  #       addCircles( ~ decimalLongitude, ~ decimalLatitude, color = input$mapColor)
  #   }
  # })
  
  observe({
    select <- event_data("plotly_click", source = "barCountrt")
    if (is.null(select)) {
      output$pie <- renderPlotly({
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
          labels = label <- switch(input$pieselect,
                                   "basisOfRecord" = ~basisOfRecord,
                                   "kingdom" = ~kingdom,
                                   "phylum" =  ~phylum,
                                   "phylum"  = ~phylum,
                                   "family" = ~family,
                                   "genus" = ~genus,
                                   "species" = ~species
          ),
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
          labels = label <- switch(input$pieselect,
                                   "basisOfRecord" = ~basisOfRecord,
                                   "kingdom" = ~kingdom,
                                   "phylum" =  ~phylum,
                                   "phylum"  = ~phylum,
                                   "family" = ~family,
                                   "genus" = ~genus,
                                   "species" = ~species
          ),
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
          labels = label <- switch(input$pieselect,
                                   "basisOfRecord" = ~basisOfRecord,
                                   "kingdom" = ~kingdom,
                                   "phylum" =  ~phylum,
                                   "phylum"  = ~phylum,
                                   "family" = ~family,
                                   "genus" = ~genus,
                                   "species" = ~species
          ),
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
          labels = label <- switch(input$pieselect,
                                   "basisOfRecord" = ~basisOfRecord,
                                   "kingdom" = ~kingdom,
                                   "phylum" =  ~phylum,
                                   "phylum"  = ~phylum,
                                   "family" = ~family,
                                   "genus" = ~genus,
                                   "species" = ~species
          ),
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