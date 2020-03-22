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
          ns("gauge_one")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_two")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_three")
        )
      ),
      column(
        3,
        flexdashboard::gaugeOutput(
          ns("gauge_four")
        )
      )
    ),
    fluidRow(
      style = 'padding-top:-50px;',
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_a"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_b"),
          width = "100%"
        )
      ),
      column(
        4,
        shinydashboard::valueBoxOutput(
          ns("box_c"),
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
            "Spatial", br(),
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
                  ns("start_year"),
                  width = "100%"
                )
              ),
              column(
                6,
                shinydashboard::infoBoxOutput(
                  ns("end_year"),
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
  )
}

# Module Server

#' @rdname mod_dataSummary
#' @export
#' @keywords internal
mod_dataSummary_server <- function(input, output, session, dataset) {
  ns <- session$ns
  
  output$gauge_one <- flexdashboard::renderGauge({
    dat <- dataset()
    if("verbatimLatitude" %in% colnames(dat))
    {
      latitudeName <- "verbatimLatitude"
    }else {
      latitudeName <- "decimalLatitude"
    }
    
    if("verbatimLongitude" %in% colnames(dat))
    {
      longitudeName <- "verbatimLongitude"
    }else {
      longitudeName <- "decimalLatitude"
    }
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    df <- dataset()

    latitude <- round(
      (
        (
          nrow(
            df[latitudeName]) - sum(
              is.na(
                df[latitudeName]
              )
            )
        ) / nrow(
          df[latitudeName]
        )
      ),
      2
      ) * 100
      
    longitude <- round(
      (
        (
          nrow(
            df[latitudeName]) - sum(
              is.na(
                df[latitudeName]
              )
            )
        ) / nrow(
          df[latitudeName]
        )
      ),
      2
      ) * 100
      
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
      label = "% of georeferenced \nrecords",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
    
  output$gauge_two <- flexdashboard::renderGauge({
    df <- dataset()
    columnName <- 'year'
    if('dateModified' %in% colnames(df)){
      columnName <- 'dateModified'
    } else if('datecollected' %in% colnames(df)){
      columnName <- 'datecollected'
    } else if('begin_date' %in% colnames(df)){
      columnName <- 'begin_date'
    } else if('date' %in% colnames(df)){
      columnName <- 'date'
    } else if('observed_on' %in% colnames(df)){
      columnName <- 'observed_on'
    } 
    validate(
      need(length(df)>0, 'Please upload/download a dataset first')
    )
    validate(
      need(columnName %in% colnames(df), 'No appropriate Column with Date data present in Database!')
    )
    
    
    
    
    countryRecord <- round(
      (
        (
          nrow(
            df[columnName]) - sum(
              is.na(
                df[columnName]
              )
            )
        ) / nrow(
          df[columnName]
        )
      ),
      2
    ) * 100
      
    gauge(
      countryRecord,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of records\nwith date data",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
    
  output$gauge_three <- flexdashboard::renderGauge({
    df <- dataset()
    occurance_column_name <- 'occurrenceID'
    if('uri' %in% colnames(df)){
      occurance_column_name <- 'uri'
    } else if ('remote_resource' %in% colnames(df)){
    occurance_column_name <- 'remote_resource'
    }

    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need(occurance_column_name %in% colnames(df), 'No appropriate Column found with occurance remark data/link')
    )
    df <- dataset()
      
    institutionCode <- round(
      (
        (
          nrow(
            df[occurance_column_name]) - sum(
              is.na(
                df[occurance_column_name]
              )
            )
        ) / nrow(
          df[occurance_column_name]
        )
      ),
      2
    ) * 100
      
    gauge(
      institutionCode,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of records\nwith occurence remark/link",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
    
  output$gauge_four <- flexdashboard::renderGauge({
    
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('basisOfRecord' %in% colnames(dataset()), 'No appropriate Column found with basisOfRecord data')
    )
    df <- dataset()
    
    basisOfRecord <- round(
      (
        (
          nrow(
            df["basisOfRecord"]) - sum(
              is.na(
                df["basisOfRecord"]
              )
            )
        ) / nrow(
          df["basisOfRecord"]
        )
      ),
      2
    ) * 100
    
    gauge(
      basisOfRecord,
      min = 0,
      max = 100,
      symbol = "%",
      label = "% of records\nwith basisOfRecord data",
      gaugeSectors(
        success = c(80, 100),
        warning = c(40, 79),
        danger = c(0, 39)
      )
    )
  })
    
    

  output$box_a <- shinydashboard::renderValueBox({
    
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    shinydashboard::valueBox(
      value = (nrow(dataset())),
      subtitle = "# of Records",
      icon = icon("compass"),
      color = "aqua",
      width = 1
    )
  })
    
  output$box_b <-  shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('name' %in% colnames(dataset()), 'No appropriate Column found')
    )
    shinydashboard::valueBox(
      value = nrow(unique(dataset()["name"])),
      subtitle = "# of Taxa",
      icon = icon("file-signature"),
      color = "blue",
      width = 1
      )
  })
    
  output$box_c <-  shinydashboard::renderValueBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
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
    dat <- dataset()
    if("verbatimLatitude" %in% colnames(dat))
    {
      latitudeName <- "verbatimLatitude"
    }else {
      latitudeName <- "decimalLatitude"
    }
    
    if("verbatimLongitude" %in% colnames(dat))
    {
      longitudeName <- "verbatimLongitude"
    }else {
      longitudeName <- "decimalLatitude"
    }
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    latitude <- nrow(
      (
        na.omit(
          dataset()[latitudeName]
        )
      )
    )
    
    longitude <- nrow(
      (
        na.omit(
          dataset()[longitudeName]
        )
      )
    )
    
    shinydashboard::infoBox(
      "# of Geo Coordinates",
      if(latitude>longitude){
        longitude
      } else {
        latitude
      },
      icon = icon("compass"),
      color = "navy",
      width = 4
    )
  })
    
  output$country_code <- shinydashboard::renderInfoBox({
    df <- dataset()
    
    country_code_column_name <- 'countryCode'
    if('place_guess' %in% colnames(df)){
      country_code_column_name <- 'place_guess'
    } else if('calculatedCountry' %in% colnames(df)){
      country_code_column_name <- 'calculatedCountry'
    } else if('country' %in% colnames(df)){
      country_code_column_name <- 'country'
    } else if('country' %in% colnames(df)){
      country_code_column_name <- 'country'
    } 
    
    validate(
      need(length(df)>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need(country_code_column_name %in% colnames(df), 'No appropriate Column found with country names in it.')
    )
    
    shinydashboard::infoBox(
      "# of Countries",
      nrow(
        unique(
          na.omit(
            dataset()[country_code_column_name]
          )
        )
      ),
      icon = icon("copyright"),
      color = "navy",
      width = 4
    )
  })
    
  output$locality <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('locality' %in% colnames(dataset()), 'No appropriate Column found with locality data.')
    )
    
    shinydashboard::infoBox(
      "# of Localities",
      nrow(
        unique(
          na.omit(
            dataset()["locality"]
          )
        )
      ),
      icon = icon("street-view"),
      color = "navy",
      width = 4
    )
  })
    
  output$coordinate_uncertainty <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    shinydashboard::infoBox(
      "# of coordinateUncertaintyInMeters",
      nrow(
        unique(
          na.omit(
            dataset()["coordinateUncertaintyInMeters"]
          )
        )
      ),
      icon = icon("compass"),
      color = "navy",
      width = 4
    )
  })
    
  
  #Temporal.......................................
  output$start_year <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    df <- dataset()
    min_year <- ""
    
    if('datecollected' %in% colnames(df)){
      min_year <- (substr(na.omit(df$datecollected),1,4))
    }  else if('begin_date' %in% colnames(df)){
      min_year <- (substr(na.omit(df$begin_date),1,4))
    } else if('date' %in% colnames(df)){
      min_year <- (substr(na.omit(df$date),1,4))
    } else if('observed_on' %in% colnames(df)){
      min_year <- (substr(na.omit(df$observed_on),1,4))
    } else if('year' %in% colnames(df)){
      min_year <- substr(na.omit(df$year),1,4)
    }
    
    min <- min_year[1]
    for(val in min_year){
      if(val == ""){
        next()
      }
      if(val < min){
        min <- val
      }
    }

    
    
    
    validate(
      need(length(min_year)>0, 'No column found with year data')
    )
    shinydashboard::infoBox(
      "Starting Year",
      min,
      icon = icon("stripe-s"),
      color = "olive",
      width = 6
    )
  })
    
  output$end_year <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    df <- dataset()
    max_year <- ""
    if('datecollected' %in% colnames(df)){
      max_year <- (substr(na.omit(df$datecollected),1,4))
    }  else if('begin_date' %in% colnames(df)){
      max_year <- (substr(na.omit(df$begin_date),1,4))
    } else if('date' %in% colnames(df)){
      max_year <- (substr(na.omit(df$date),1,4))
    } else if('observed_on' %in% colnames(df)){
      max_year <- (substr(na.omit(df$observed_on),1,4))
    } else if('year' %in% colnames(df)){
      max_year <- substr(na.omit(df$year),1,4)
    }
    
    max <- max_year[1]
    for(val in max_year){
      if(val == ""){
        next()
      }
      if(val > max){
        max <- val
      }
    }
    

    
    validate(
      need(length(max_year)>0, 'No column found with year data')
    )
    shinydashboard::infoBox(
      "End Year",
      max,
      icon = icon("etsy"),
      color = "olive",
      width = 6
    )
  })
    
    
  #Taxonomic.......................................
  output$kingdom <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('kingdom' %in% colnames(dataset()), 'No appropriate Column found with kingdom Records.')
    )
    shinydashboard::infoBox(
      "# of Kingdom",
      nrow(
        unique(
          na.omit(
            dataset()["kingdom"]
          )
        )
      ),
      icon = icon("korvue"),
      color = "teal",
      width = 4
    )
  })
    
  output$phylum <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('phylum' %in% colnames(dataset()), 'No appropriate Column found with phylum Records.')
    )
    shinydashboard::infoBox(
      "# of Phylum",
      nrow(
        unique(
          na.omit(
            dataset()["phylum"]
          )
        )
      ),
      icon = icon("product-hunt"),
      color = "teal",
      width = 4
    )
  })
    
  output$order <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('order' %in% colnames(dataset()), 'No appropriate Column found with order Records.')
    )
    shinydashboard::infoBox(
      "# of Order",
      nrow(
        unique(
          na.omit(
            dataset()["order"]
          )
        )
      ),
      icon = icon("opera"),
      color = "teal",
      width = 4
    )
  })
    
  output$family <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('family' %in% colnames(dataset()), 'No appropriate Column found with family Records.')
    )
    shinydashboard::infoBox(
      "# of Family",
      nrow(
        unique(
          na.omit(
            dataset()["family"]
          )
        )
      ),
      icon = icon("facebook-f"),
      color = "teal",
      width = 4
    )
  })
    
  output$genus <- shinydashboard::renderInfoBox({
    
    
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('genus' %in% colnames(dataset()), 'No appropriate Column found with genus Records.')
    )
    shinydashboard::infoBox(
      "# of Genus",
      nrow(
        unique(
          na.omit(
            dataset()["genus"]
          )
        )
      ),
      icon = icon("gofore"),
      color = "teal",
      width = 4
    )
  })
    
  output$species <- shinydashboard::renderInfoBox({
    validate(
      need(length(dataset())>0, 'Please upload/download a dataset first')
    )
    
    validate(
      need('species' %in% colnames(dataset()), 'No appropriate Column found with species Records.')
    )
    shinydashboard::infoBox(
      "# of Species",
      nrow(
        unique(
          na.omit(
            dataset()["species"]
          )
        )
      ),
      icon = icon("stripe-s"),
      color = "teal",
      width = 4
    )
  })
  
}
