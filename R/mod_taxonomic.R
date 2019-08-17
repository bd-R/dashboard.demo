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
        selectizeInput(
          ns("taxo_bar_input_1"),
          "Select Taxonomic Level",
          c("Kingdom" = "1",
            "Phylum" = "2",
            "Order" = "3",
            "Family" = "4",
            "Genus" = "5",
            "Species" = "6",
            "basisOfRecord" = "7"
          ),
          selected = "7"
        ),
        plotlyOutput(
          ns("bar_1"),
          height = "250px"
        )
      ),
      column(
        6,
        selectizeInput(
          ns("taxo_bar_input_2"),
          "Select Taxonomic Level",
          c("identifiedBy" = "1",
            "dateIdentified" = "2"
          ),
          selected = "1"
        ),
        plotlyOutput(
          ns("bar_2"),
          height = "250px"
        )
      )
    ),
    fluidRow(
      column(
        12,
        formattable::formattableOutput(ns("table"))
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
  output$bar_1 <- renderPlotly({
    label <- switch(as.integer(input$taxo_bar_input_1),
                    ~kingdom,
                    ~phylum,
                    ~order,
                    ~family,
                    ~genus,
                    ~species,
                    ~basisOfRecord
                    )
    plot_ly(data = data_taxo(),
            y = label,
            source = "taxobar_1")
  })
  
  output$bar_2 <- renderPlotly({
    label <- switch(as.integer(input$taxo_bar_input_2),
                    ~identifiedBy,
                    ~dateIdentified
    )
    plot_ly(data = data_taxo(),
            y = label,
            source = "taxobar_2")
  })
  
  output$a <- renderPrint({
    select1 <- event_data("plotly_click", source = "taxobar_1")
    select1
  })
  
  output$b <- renderPrint({
    select2 <- event_data("plotly_click", source = "taxobar_2")
    select2
  })

  
  output$table <- formattable::renderFormattable({
    select1 <- event_data("plotly_click", source = "taxobar_1")
    select2 <- event_data("plotly_click", source = "taxobar_2")
    if(is.null(select1)&&is.null(select2)){
      df <- data_taxo()[c("identifiedBy", "identifiedBy")]
      formattable::formattable(df)
    }else if(!is.null(select1)&&is.null(select2)){
      df <- data_taxo() %>%
        filter(switch(as.integer(input$taxo_bar_input_1),
                      kingdom,
                      phylum,
                      order,
                      family,
                      genus,
                      species,
                      basisOfRecord
        ) %in% select1$y)
      df <- df[c("scientificName", "identifiedBy")]
      formattable::formattable(df)
    }else if(is.null(select1)&&!is.null(select2)){
      df <- data_taxo() %>%
        filter(switch(as.integer(input$taxo_bar_input_2),
                      identifiedBy,
                      dateIdentified

        ) %in% select2$y)
      df <- df[c("scientificName", "identifiedBy")]
      formattable::formattable(df)
    }else if(!is.null(select1)&&!is.null(select2)){
      df <- data_taxo() %>%
        filter(switch(as.integer(input$taxo_bar_input_2),
                      identifiedBy,
                      dateIdentified

        ) %in% select2$y)
      df <- df %>%
        filter(switch(as.integer(input$taxo_bar_input_1),
                      kingdom,
                      phylum,
                      order,
                      family,
                      genus,
                      species,
                      basisOfRecord
        ) %in% select1$y)
      df <- df[c("scientificName", "identifiedBy")]
      formattable::formattable(df)
    }

  })

}