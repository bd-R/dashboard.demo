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
            "year" = "2",
            "countryCode" = "3"
            ),
          selected = "1"
        ),
        plotlyOutput(
          ns("bar_2"),
          height = "250px"
        )
      )
    ), 
    fluidRow(br(),
      column(
        12,
        selectizeInput(
          ns("show_vars"),
          "Columns to show:",
          choices = c(
            "scientificName",
            "kingdom",
            "phylum",
            "order",
            "family",
            "genus",
            "species",
            "identifiedBy",
            "dateIdentified",
            "year",
            "month",
            "day",
            "taxonRemarks",
            "taxonomicStatus"
          ),
          multiple = TRUE,
          selected = c("scientificName", "kingdom", "phylum")
        ),
        DT::dataTableOutput(ns("table"))
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
            color = label,
            source = "taxobar_1")%>%
      layout(paper_bgcolor='transparent',
             plot_bgcolor = "transparent",
             showlegend = FALSE,
             xaxis = list(
               color = '#ffffff',
               zeroline = TRUE,
               showline = TRUE,
               showticklabels = TRUE,
               showgrid = FALSE),
             yaxis = list(
               color = '#ffffff',
               showticklabels = TRUE,
               showgrid = FALSE))
  })
  
  output$bar_2 <- renderPlotly({
    label <- switch(as.integer(input$taxo_bar_input_2),
                    ~identifiedBy,
                    ~year,
                    ~countryCode
    )
    plot_ly(data = data_taxo(),
            y = label,
            color = label,
            source = "taxobar_2")%>%
      layout(paper_bgcolor='transparent',
             plot_bgcolor = "transparent",
             showlegend = FALSE,
             xaxis = list(
               color = '#ffffff',
               zeroline = TRUE,
               showline = TRUE,
               showticklabels = TRUE,
               showgrid = FALSE),
             yaxis = list(
               color = '#ffffff',
               showticklabels = TRUE,
               showgrid = FALSE))
  })
  
  output$a <- renderPrint({
    select1 <- event_data("plotly_click", source = "taxobar_1")
    select1
  })
  
  output$b <- renderPrint({
    select2 <- event_data("plotly_click", source = "taxobar_2")
    select2
  })

  
  output$table <- DT::renderDataTable({
    select1 <- event_data("plotly_click", source = "taxobar_1")
    select2 <- event_data("plotly_click", source = "taxobar_2")
    if(is.null(select1)&&is.null(select2)){
    as.datatable(formattable::formattable(data_taxo()[input$show_vars], align = c("l",rep("r", NCOL(df) - 1))))
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
      as.datatable(formattable::formattable(df[input$show_vars], align = c("l",rep("r", NCOL(table) - 1))))
    }else if(is.null(select1)&&!is.null(select2)){
      df <- data_taxo() %>%
        filter(switch(as.integer(input$taxo_bar_input_2),
                      identifiedBy,
                      year,
                      countryCode

        ) %in% select2$y)
      as.datatable(formattable::formattable(df[input$show_vars], align = c("l",rep("r", NCOL(table) - 1))))
    }else if(!is.null(select1)&&!is.null(select2)){
      df <- data_taxo() %>%
        filter(switch(as.integer(input$taxo_bar_input_2),
                      identifiedBy,
                      year,
                      countryCode

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
      as.datatable(formattable::formattable(df[input$show_vars], align = c("l",rep("r", NCOL(table) - 1))))
    }

  })

}