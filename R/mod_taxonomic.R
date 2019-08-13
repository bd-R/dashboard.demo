# Module UI

#' @title   mod_taxonomic_ui and mod_taxonomic_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_taxonomic
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_taxonomic_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        6,
        plotlyOutput(
          ns("taxonomicBar"),
          height = "250px"
        ),
        absolutePanel(
          top = 10,
          right = 10,
          selectizeInput(
            ns("taxoBarInput"),
            "Select Taxonomic Level",
            c("Kingdom", "Phylum", "Order", "Family", "Genus", "Species"),
            selected = "Order"
          )
        )
      ),
      column(
        6,
        circlepackeROutput(
          ns("circleplot"),
          height = "300px"
        )
      )
    ),
    fluidRow(
      column(
        6,
        sunburstOutput(
          ns("sunbrust"),
          height = "300px"
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_taxonomic
#' @export
#' @keywords internal

mod_taxonomic_server <- function(input, output, session, dataTaxo) {
  ns <- session$ns
  output$taxonomicBar <- renderPlotly({
    if (input$taxoBarInput == "Kingdom") {
      label <- ~ kingdom
    } else if (input$taxoBarInput == "Phylum") {
      label <- ~ phylum
    } else if (input$taxoBarInput == "Family") {
      label <- ~ family
    } else if (input$taxoBarInput == "Genus") {
      label <- ~ genus
    } else if (input$taxoBarInput == "Species") {
      label <- ~ species
    } else{
      label <- ~ order
    }
    plot_ly(data = dataTaxo(),
            y = label,
            source = "taxoBar")
  })
  
  observe({
    select <- event_data("plotly_click", source = "taxoBar")
    if (is.null(select)) {
      output$circleplot <- renderCirclepackeR({
        dataforCircle <- formatData(dataTaxo())
        dataforCircle$pathString <-
          paste(
            "Vis",
            dataforCircle$group,
            dataforCircle$subgroup,
            dataforCircle$subsubgroup,
            sep = "/"
          )
        population <- as.Node(dataforCircle)
        # Make the plot
        circlepackeR(population, size = "value")
      })
    } else {
      if (input$taxoBarInput == "Kingdom") {
        newData <- dataTaxo() %>%
          filter(kingdom %in% select$y)
      } else if (input$taxoBarInput == "Phylum") {
        newData <- dataTaxo() %>% 
          filter(phylum %in% select$y)
      } else if (input$taxoBarInput == "Family") {
        newData <- dataTaxo() %>% 
          filter(family %in% select$y)
      } else if (input$taxoBarInput == "Genus") {
        newData <- dataTaxo() %>%
          filter(genus %in% select$y)
      } else if (input$taxoBarInput == "Species") {
        newData <- dataTaxo() %>%
          filter(species %in% select$y)
      } else{
        newData <- dataTaxo() %>%
          filter(order %in% select$y)
      }
      if (nrow(newData) == 0) {
        output$circleplot <- renderCirclepackeR({
          dataforCircle <- formatData(dataTaxo())
          dataforCircle$pathString <-
            paste(
              "Vis",
              dataforCircle$group,
              dataforCircle$subgroup,
              dataforCircle$subsubgroup,
              sep = "/"
            )
          population <- as.Node(dataforCircle)
          # Make the plot
          circlepackeR(population, size = "value")
        })
      } else {
        output$circleplot <- renderCirclepackeR({
          dataforCircle <- formatData(newData)
          dataforCircle$pathString <-
            paste(
              "Vis",
              dataforCircle$group,
              dataforCircle$subgroup,
              dataforCircle$subsubgroup,
              sep = "/"
            )
          population <- as.Node(dataforCircle)
          # Make the plot
          circlepackeR(population, size = "value")
        })
      }
    }
  })
  
  output$sunbrust <- renderSunburst({
    data <- dataTaxo()
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
  
  #Function
  formatData <- function(data) {
    data <- na.omit(data[c("phylum", "order", "family", "genus")])
    if (!nrow(data[-which(data[, "phylum"] == ""),]) == 0) {
      data <- data[-which(data[, "phylum"] == ""),]
    }
    if (!nrow(data[-which(data[, "order"] == ""),]) == 0) {
      data <- data[-which(data[, "order"] == ""),]
    }
    if (!nrow(data[-which(data[, "family"] == ""),]) == 0) {
      data <- data[-which(data[, "family"] == ""),]
    }
    if (!nrow(data[-which(data[, "genus"] == ""),]) == 0) {
      data <- data[-which(data[, "genus"] == ""),]
    }
    data <- arrange(data, family)
    temp <- as.data.frame(table(data["genus"]))
    data <- unique(data)
    temp <- merge(data, temp , by.x = "genus", by.y = "Var1")
    temp <- temp[c("phylum", "order", "family", "genus", "Freq")]
    colnames(temp) <-
      c("root", "group", "subgroup", "subsubgroup", "value")
    
    return(temp)
  }
  
  
}

## To be copied in the UI
# mod_taxonomic_ui("taxonomic_ui_1")

## To be copied in the server
# callModule(mod_taxonomic_server, "taxonomic_ui_1")
