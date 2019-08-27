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
        uiOutput(
          ns("back_bar")
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
    fluidRow(class = "top_padding",
      column(
        6,
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
          )
        ),
      column(
          3,
          "Year Selected: ",
          verbatimTextOutput(ns("year_clicked"))
      ),
      column(
        3,
        "Month Selected: ",
        verbatimTextOutput(ns("month_clicked"))
      )
    ),fluidRow(
      column(
        12,
        uiOutput(
          ns("back")
        ),
        plotlyOutput(
          ns("time")
        )
      )
    )
  )
}

# Module Server

#' @rdname mod_temporal
#' @export
#' @keywords internal
mod_temporal_server <- function(input, output, session, data_temporal){
  ns <- session$ns
  
  #Plot bar graph.
  selectionsbar <- reactiveVal()
    
  output$bar <- renderPlotly({
    if (length(selectionsbar()) == 0){
      plot_ly(
        data = data_temporal(),
        x = switch(
          input$bar_select,
          "basisOfRecord" = ~basisOfRecord,
          "kingdom" = ~kingdom,
          "phylum" =  ~phylum,
          "order"  = ~order,
          "family" = ~family,
          "genus" = ~genus,
          "species" = ~species
        ),
        source = "bar_selected"
      ) %>%
        layout(
          paper_bgcolor = 'transparent',
          plot_bgcolor = "transparent",
          showlegend = FALSE,
          xaxis = list(
            color = '#ffffff',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = FALSE
          ),
          yaxis = list(
            color = '#ffffff',
            showticklabels = TRUE,
            showgrid = FALSE
          )
        )
      } else {
        data_temporal() %>%
          filter(
            switch(
              input$bar_select,
              "basisOfRecord" = basisOfRecord,
              "kingdom" = kingdom,
              "phylum" =  phylum,
              "order"  = order,
              "family" = family,
              "genus" = genus,
              "species" = species
              ) %in% 
              selectionsbar()
          ) %>%
          plot_ly(
            x = switch(
              input$bar_select,
              "basisOfRecord" = ~basisOfRecord,
              "kingdom" = ~kingdom,
              "phylum" =  ~phylum,
              "order"  = ~order,
              "family" = ~family,
              "genus" = ~genus,
              "species" = ~species
            ),
            source = "bar_selected"
          ) %>%
          layout(
            paper_bgcolor='transparent',
            plot_bgcolor = "transparent",
            showlegend = FALSE,
            xaxis = list(
              color = '#ffffff',
              zeroline = TRUE,
              showline = TRUE,
              showticklabels = TRUE,
              showgrid = FALSE
            ),
            yaxis = list(
              color = '#ffffff',
              showticklabels = TRUE,
              showgrid = FALSE
            )
          )
      }
  })
    
  observeEvent(
    event_data(
      "plotly_click",
      source = "bar_selected"
    ),
    {
      new <- event_data(
        "plotly_click",
        source = "bar_selected")$x
      selectionsbar(new)
    }
  )
    
  # populate back button if category is chosen
  output$back_bar <- renderUI({
    if (length(selectionsbar())){
      actionButton(
        ns("clear_bar"),
        "Back/Reset",
        icon("chevron-left")
      )
    }
  })

  # clear the chosen category on back button press
  observeEvent(input$clear_bar, selectionsbar(NULL))

  #Violin Plot
  output$violin <- renderPlotly({
  df <- data_temporal()
  
  if(is.null(selectionsbar())){
    df %>%
      plot_ly(
        y = switch(
          input$violin_select,
          "year" = ~year,
          "month" = ~month,
          "day" =  ~day
        ),
        split = switch(
          input$bar_select,
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
      layout(
        paper_bgcolor = 'transparent',
        plot_bgcolor = "transparent",
        showlegend = FALSE,
        xaxis = list(
          color = '#ffffff',
          zeroline = TRUE,
          showline = TRUE,
          showticklabels = TRUE,
          showgrid = FALSE
        ),
        yaxis = list(
          color = '#ffffff',
          showticklabels = TRUE,
          showgrid = FALSE
        )
      )
  } else {
    newData <- data_temporal() %>%
      filter(
        switch(
          input$bar_select,
          "basisOfRecord" = basisOfRecord,  
          "kingdom" = kingdom,
          "phylum" =  phylum,
          "order"  = order,
          "family" = family,
          "genus" = genus,
          "species" = species
        ) %in% 
          selectionsbar()
      )
    if(nrow(newData) == 0){
      newData <- df
    }
    newData %>%
      plot_ly(
        y = switch(
          input$violin_select,
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
      layout(
        paper_bgcolor = 'transparent',
        plot_bgcolor = "transparent",
        showlegend = FALSE,
        xaxis = list(
          color = '#ffffff',
          zeroline = TRUE,
          showline = TRUE,
          showticklabels = TRUE,
          showgrid = FALSE
        ),
        yaxis = list(
          color = '#ffffff',
          showticklabels = TRUE,
          showgrid = FALSE
        )
      )
    }
  })
    
  selections <- reactiveVal()
    
  output$time <- renderPlotly({
    if (length(selections()) == 0) {
      count(
        data_temporal(),
        switch(
          as.integer(input$timeselect),
          basisOfRecord,
          kingdom,
          phylum,
          order,
          family,
          genus,
          species
        ),
        year
      ) %>%
        setNames(
          c(
            "color",
            "year",
            "value"
          )
        ) %>%
        plot_ly() %>%
        add_lines(
          x = ~year,
          y = ~value,
          color = ~color
        ) %>%
        layout(
          paper_bgcolor = 'transparent',
          plot_bgcolor = "transparent",
          showlegend = TRUE,
          legend = list(
                    x = 0.0, 
                    y = 1,
                    orientation = 'h',
                    font = list(
                      color = "#ffffff"
                    )
                    ),
          xaxis = list(
           title = "Years",
           color = '#ffffff',
           zeroline = TRUE,
           showline = TRUE,
           showticklabels = TRUE,
           showgrid = FALSE
          ),
          yaxis = list(
            title = "Number of Records",
            color = '#ffffff',
            showticklabels = TRUE,
            showgrid = FALSE
          )
        )
    } else if(length(selections())==1){
      data_temporal() %>%
        filter(
          year %in% 
            selections()
        ) %>%
        count(
          switch(
            as.integer(input$timeselect),
            basisOfRecord,
            kingdom,
            phylum,
            order,
            family,
            genus,
            species
          ),
          month
        ) %>%
        setNames(
          c(
            "color",
            "month",
            "value"
          )
        ) %>%
        plot_ly() %>%
        add_lines(
          x = ~month, 
          y = ~value, 
          color = ~color
        ) %>%
        layout(
          paper_bgcolor = 'transparent',
          plot_bgcolor = "transparent",
          showlegend = TRUE,
          legend = list(
            x = 0.0, 
            y = 1,
            orientation = 'h',
            font = list(
              color = "#ffffff"
            )
          ),
          xaxis = list(
            title = "Month",
            ticktext = list("Jan",
                            "Feb",
                            "March",
                            "Apr",
                            "May",
                            "June",
                            "July",
                            "Aug",
                            "Sep",
                            "Oct",
                            "Nov",
                            "Dec"), 
            tickvals = list(1,
                            2,
                            3,
                            4,
                            5,
                            6,
                            7,
                            8,
                            9,
                            10,
                            11,
                            12),
            color = '#ffffff',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = FALSE
          ),
          yaxis = list(
            title = "Number of Records",
            color = '#ffffff',
            showticklabels = TRUE,
            showgrid = FALSE
          )
        )
    } else {
      data_temporal() %>%
        filter(
          month %in% 
            selections()
        ) %>%
        count(
          switch(
            as.integer(input$timeselect),
            basisOfRecord,
            kingdom,
            phylum,
            order,
            family,
            genus,
            species
          ),
          day
        ) %>%
        setNames(
          c(
            "color",
            "day",
            "value"
          )
        ) %>%
        plot_ly() %>%
        add_lines(
          x = ~day,
          y = ~value,
          color = ~color
        ) %>%
        layout(
          paper_bgcolor = 'transparent',
          plot_bgcolor = "transparent",
          showlegend = TRUE,
          legend = list(
                    x = 0.0, 
                    y = 1,
                    orientation = 'h',
                    font = list(
                      color = "#ffffff"
                    )
                  ),
          xaxis = list(
            title = "Days",
            color = '#ffffff',
            zeroline = TRUE,
            showline = TRUE,
            showticklabels = TRUE,
            showgrid = FALSE
          ),
          yaxis = list(
            title = "Number of Records",
            color = '#ffffff',
            showticklabels = TRUE,
            showgrid = FALSE
          )
        )
    }
  })
  
  output$year_clicked <- renderText({
    "No year Selected"
  })
  
  output$month_clicked <- renderText({
    "No Month Selected"
  })
  
  observeEvent(
    event_data("plotly_click"),
    {
      new <- event_data("plotly_click")$x
      old <- selections()
      if(is.null(selections())){
        output$year_clicked <- renderText({
          new
        })
      }
      if(length(selections())==1){
        output$month_clicked <- renderText({
          switch (new,
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"
          )
        })
      }
      selections(
        c(
          old,
          new
        )
      )
    }
  )

  # populate back button if category is chosen
  output$back <- renderUI({
    if (length(selections())){
      actionButton(
        ns("clear"),
        "Back",
        icon("chevron-left")
      )
    }
  })
  
  observeEvent(input$clear, {
    selections(NULL)
    output$year_clicked <- renderText({
      "No Year Selected"
    })
    
    output$month_clicked <- renderText({
      "No Month Selected"
    })
  })

}
