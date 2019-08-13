# Module UI

#' @title   Module to visualize taxonomic data
#' @description  Contain plots to visulaize taxonomic related data fields.
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
          ns("taxonomic_bar"),
          height = "250px"
        ),
        absolutePanel(
          top = 10,
          right = 10,
          selectizeInput(
            ns("taxo_bar_input"),
            "Select Taxonomic Level",
            c("Kingdom" = "1",
              "Phylum" = "2",
              "Order" = "3",
              "Family" = "4",
              "Genus" = "5",
              "Species" = "6"
            ),
            selected = "3"
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

mod_taxonomic_server <- function(input, output, session, data_taxo) {
  ns <- session$ns
  output$taxonomic_bar <- renderPlotly({
    label <- switch(as.integer(input$taxo_bar_input),
                    ~kingdom,
                    ~phylum,
                    ~order,
                    ~family,
                    ~genus,
                    ~species
                    )
    plot_ly(data = data_taxo(),
            y = label,
            source = "taxoBar")
  })
  
  observe({
    select <- event_data("plotly_click", source = "taxoBar")
    if (is.null(select)) {
      output$circleplot <- renderCirclepackeR({
        dataforCircle <- formatData(data_taxo())
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
      newData <- switch(as.integer(input$taxo_bar_input),
                        data_taxo() %>%
                          filter(kingdom %in% select$y),
                        data_taxo() %>%
                          filter(phylum %in% select$y),
                        data_taxo() %>%
                          filter(order %in% select$y),
                        data_taxo() %>%
                          filter(family %in% select$y),
                        data_taxo() %>%
                          filter(genus %in% select$y),
                        data_taxo() %>%
                          filter(species %in% select$y),
                        )
      if (nrow(newData) == 0) {
        output$circleplot <- renderCirclepackeR({
          dataforCircle <- formatData(data_taxo())
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
    data <- data_taxo()
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