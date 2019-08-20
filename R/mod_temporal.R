# Module UI

#' @title   Module to visualize temporal data
#' @description  This module is for visualization of time related data.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_temporal
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
mod_temporal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    class = "darkbg",
    fluidRow(
      column(
        6,
        selectInput(
          ns("bar_select"),
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
        ),
        plotlyOutput(
          ns("bar")
        )
      ),
      column(
        6,
        class = "noPadding",
        selectInput(
          ns("violin_select"),
          "Select Column to be displayed",
          c(
            "year",
            "day",
            "month"
          ),
          selected = "year"
        ),
        plotlyOutput(
          ns("violin")
        ),
        verbatimTextOutput(ns("a"))
      )
    ),
    fluidRow(
      column(
        12,
        class = "noPadding",
        selectInput(
          ns("timeselect"),
          "Select Column to be displayed",
          c(
            "basisOfRecord" = "1",
            "kingdom" = "2",
            "phylum" = "3",
            "order" = "4",
            "family" = "5",
            "genus" = "6",
            "species" = "7"
          ),
          selected = "1"
        ),
        plotlyOutput(
          ns("time")
        ),
        uiOutput(ns("back"))
      )
    )
  )
}

# Module Server

#' @rdname mod_temporal
#' @export
#' @keywords internal
mod_temporal_server <-
  function(input, output, session, data_temporal) {
    ns <- session$ns

    #Plot bar graph
    output$bar <- renderPlotly({
      label <- switch(input$bar_select,
                      "basisOfRecord" = ~basisOfRecord,
                      "kingdom" = ~kingdom,
                      "phylum" =  ~phylum,
                      "order"  = ~order,
                      "family" = ~family,
                      "genus" = ~genus,
                      "species" = ~species
      )
        plot_ly(data = data_temporal(),
                x = label,
                source = "bar_selected") %>%
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

    #Violin Plot
    output$violin <- renderPlotly({
      df <- data_temporal()
      select <- event_data("plotly_click", source = "bar_selected")
      if(is.null(select)){
        df %>%
          plot_ly(
          y = switch(input$violin_select,
                     "year" = ~year,
                     "month" = ~month,
                     "day" =  ~day
          ),
          split = switch(input$bar_select,
                         "basisOfRecord" = ~basisOfRecord,
                         "kingdom" = ~kingdom,
                         "phylum" =  ~phylum,
                         "order"  = ~order,
                         "family" = ~family,
                         "genus" = ~genus,
                         "species" = ~species
          ),
          type = 'violin',
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          )
        ) %>% 
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
      }else {
          newData <- data_temporal() %>%
            filter(switch(input$bar_select,
                          "basisOfRecord" = basisOfRecord,
                          "kingdom" = kingdom,
                          "phylum" =  phylum,
                          "order"  = order,
                          "family" = family,
                          "genus" = genus,
                          "species" = species
            ) %in% select$x)
          if(nrow(newData) == 0){
            newData <- df
          }
    newData %>%
      plot_ly(
        y = switch(input$violin_select,
                   "year" = ~year,
                   "month" = ~month,
                   "day" =  ~day
        ),
        type = 'violin',
        box = list(
          visible = T
        ),
        meanline = list(
          visible = T
        ),
        x0 = input$violin_select
      ) %>% 
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
               showgrid = FALSE)
      )
    }
  })
    
    selections <- reactiveVal()
    
  output$time <- renderPlotly({
    
    nSelections <- length(selections())
    if (nSelections == 0) {
      count(data_temporal(), switch(as.integer(input$timeselect),
                                    basisOfRecord,
                                    kingdom,
                                    phylum,
                                    order,
                                    family,
                                    genus,
                                    species), year) %>%
        setNames(c("color", "year", "value")) %>%
        plot_ly()%>%
        add_lines(x = ~year, y = ~value, color = ~color)%>%
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
                 showgrid = FALSE)
        )
    } else if(length(selections())==1){
      data_temporal() %>%
        filter(year %in% selections()) %>%
        count(switch(as.integer(input$timeselect),
                     basisOfRecord,
                     kingdom,
                     phylum,
                     order,
                     family,
                     genus,
                     species), month) %>%
        setNames(c("color", "month", "value")) %>%
        plot_ly()%>%
        add_lines(x = ~month, y = ~value, color = ~color)%>%
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
                 showgrid = FALSE)
        )
    } else {
      data_temporal() %>%
        filter(month %in% selections()) %>%
        count(switch(as.integer(input$timeselect),
                     basisOfRecord,
                     kingdom,
                     phylum,
                     order,
                     family,
                     genus,
                     species), day) %>%
        setNames(c("color", "day", "value")) %>%
        plot_ly()%>%
        add_lines(x = ~day, y = ~value, color = ~color)%>%
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
                 showgrid = FALSE)
        )
    }
    
  })
  
  observeEvent(event_data("plotly_click"), {
    new <- event_data("plotly_click")$x
    old <- selections()
    selections(c(old, new))
  })
  
  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(selections())) 
      actionButton(ns("clear"), "Back", icon("chevron-left"))
  })
  observeEvent(input$clear, selections(NULL))


  }