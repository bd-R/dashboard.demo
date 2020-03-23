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
      leafletOutput(ns("mymap"), height = "450"),
      absolutePanel(
        top = 130,
        right = 20,
        selectInput(
          ns("mapTexture"),
          "Map Texture",
          choices = list(
            "OpenTopoMap" = "OpenTopoMap",
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
          selected = "OpenTopoMap"
        ),
        selectInput(
          ns("mapColor"),
          "Points Color",
          choices = list(
            "Red" = 'red',
            "Green" = "green",
            "Blue" = "blue",
            "Black" = "black"
          ),
          selected = "blue"
        )
      )
    ),
    fluidRow(
      br(),
      selectizeInput(
        ns("show_vars"),
        "Columns to show:",
        choices = NULL,
        multiple = TRUE
      )
    ),
    fluidRow(
      DT::dataTableOutput(
        ns("table")
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
  
  output$mymap <- renderLeaflet({
    dat <- data()
    validate(
      need(length(dat)>0, 'Please upload/download a dataset first')
    )
    latitudeName <- "verbatimLatitude"
    longitudeName <- "verbatimLongitude"
    
    if("decimalLatitude" %in% colnames(dat))
    {
      latitudeName <- "decimalLatitude"
    }
    
    if("decimalLongitude" %in% colnames(dat))
    {
      longitudeName <- "decimalLongitude"
    }
    
    validate(
      need(longitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    validate(
      need(latitudeName %in% colnames(dat), 'No location Data available in Databse to plot map')
    )
    
    switch (latitudeName,
      "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
      "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
    )
    switch (longitudeName,
      "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
      "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
    )
    
    leaflet(
      data = na.omit(
        dat[c(latitudeName, longitudeName)]
      )
    ) %>%
      addProviderTiles(
        input$mapTexture
      ) %>%
      addCircles(

        switch(
          longitudeName,
          "decimalLongitude" = ~decimalLongitude,
          "verbatimLongitude" = ~verbatimLongitude
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~decimalLatitude,
          "verbatimLatitude" = ~verbatimLatitude
        ),
        color = input$mapColor
      ) %>%
      fitBounds(

        switch(
          longitudeName,
          "decimalLongitude" = ~min(decimalLongitude),
          "verbatimLongitude" = ~min(verbatimLongitude)
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~min(decimalLatitude),
          "verbatimLatitude" = ~min(verbatimLatitude)
        ),
        switch(
          longitudeName,
          "decimalLatitude" = ~max(decimalLongitude),
          "verbatimLatitude" = ~max(verbatimLongitude)
        ),
        switch(
          latitudeName,
          "decimalLatitude" = ~max(decimalLatitude),
          "verbatimLatitude" = ~max(verbatimLatitude)
        )
        
      ) %>%
      leaflet.extras::addDrawToolbar(
        targetGroup='draw',
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        rectangleOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = leaflet.extras::editToolbarOptions()
      ) %>%
      addLayersControl(
        overlayGroups = c('draw'),
        options = layersControlOptions(
          collapsed=FALSE
        )
      ) 
  })
  
  observe({
    
    choices = c(
      "scientificName",
      "name",
      "countryCode",
      "generalComments",
      "state_province",
      "begin_date",
      "end_date",
      "locality",
      "decimalLatitude",
      "decimalLongitude",
      "verbatimLongitude",
      "verbatimLatitude",
      "coordinateUncertaintyInMeters",
      "coordinate_uncertainty_in_meters",
      "coordinatePrecision",
      "elevation",
      "elevationAccuracy",
      "depth",
      "depthAccuracy",
      "establishmentMeans",
      "basisOfRecord",
      "datasetName",
      "missing_name",
      "url",
      "observation_type",
      "date",
      "license",
      "datecollected",
      "kingdom",
      "phylum",
      "order",
      "family",
      "genus",
      "species",
      "species_guess"
    )
    column_names <- vector()
    for(i in choices){
      if(i %in% colnames(data())){
        column_names <- c(column_names, i)
      }
    }
    
    # Can also set the label and select items
    updateSelectInput(session, "show_vars",
                      "Select columns to show:",
                      choices = column_names,
                      selected = tail(column_names, 1)
    )
  })
  

  
  output$table <- DT::renderDataTable({
    validate(
      need(length(data())>0, 'Please upload/download a dataset first')
    )
    data()[input$show_vars]}, filter = list(position = 'top', clear = FALSE)
  )
  
  observeEvent(
    input$mymap_draw_new_feature,
    {
      dat <- data()
      
      latitudeName <- "verbatimLatitude"
      longitudeName <- "verbatimLongitude"
      
      if("decimalLatitude" %in% colnames(dat))
      {
        latitudeName <- "decimalLatitude"
      }
      
      if("decimalLongitude" %in% colnames(dat))
      {
        longitudeName <- "decimalLongitude"
      }
      
      switch (latitudeName,
              "verbatimLatitude" = dat$verbatimLatitude <- as.numeric(dat$verbatimLatitude),
              "decimalLatitude" = dat$decimalLatitude <- as.numeric(dat$decimalLatitude),
      )
      switch (longitudeName,
              "verbatimLongitude" = dat$verbatimLongitude <- as.numeric(dat$verbatimLongitude),
              "decimalLongitude" = dat$decimalLongitude <- as.numeric(dat$decimalLongitude),
      )
      
      output$table <- DT::renderDataTable({
        data <- na.omit(
          dat[c(
            latitudeName,
            longitudeName
          )]
        )
        cities_coordinates <- SpatialPointsDataFrame(
          data[,c(
            longitudeName,
            latitudeName
          )],
          data
        )
        
        #get the coordinates of the polygon
        polygon_coordinates <- 
          input$mymap_draw_new_feature$geometry$coordinates[[1]]
        
        #transform them to an sp Polygon
        drawn_polygon <- 
          Polygon(
            do.call(
              rbind,
              lapply(
                polygon_coordinates,
                function(x){c(x[[1]][1],x[[2]][1])}
              )
            )
          )
        
        #use over from the sp package to identify selected cities
        selected_cities <- 
          cities_coordinates %over% 
          SpatialPolygons(
            list(
              Polygons(
                list(
                  drawn_polygon
                ),
                "drawn_polygon"
              )
            )
          )
        
        #print the name of the cities
        geo <- as.data.frame(
          data()[which(
            !is.na(
              selected_cities
            )
          ),
          colnames(
            data()
          )]
        )
        
        geo[input$show_vars]
      })
    }
  )
  
  observeEvent(
    input$mymap_draw_deleted_features,
    {
      output$table <- DT::renderDT(
        data()[input$show_vars], filter = list(position = 'top', clear = FALSE)
      )
    }
  )
  
}
