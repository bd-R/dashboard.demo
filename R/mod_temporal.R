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
        6,
        class = "noPadding",
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
        ),
        plotlyOutput(
          ns("pie")
        )
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
    formattedData <- reactive({
      dataForBar <- format_bdvis(data_temporal(), source = 'rgbif')
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
          layout(
                showlegend = FALSE,
                paper_bgcolor = '#000000',
                plot_bgcolor = '#000000',
                xaxis = list(color = '#ffffff'),
                yaxis = list(color = '#ffffff'),
                leagend = list(color = '#ffffff')
        )
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
            yaxis = list(
              title = "",
              zeroline = F
            )
          )
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
      layout(
        yaxis = list(
          title = "",
          zeroline = F
        )
      )
    }
  })
    
    output$pie <- renderPlotly({
      select <- event_data("plotly_click", source = "bar_selected")
      if (!nrow(data_temporal()[-which(data_temporal()[, input$pieselect] == ""), ]) == 0) {
        dataa <-
          data_temporal()[-which(data_temporal()[, input$pieselect] == ""), ]
      } else {
        dataa <- data_temporal()
      }
      if(is.null(select)){
        plot_ly(
          data = na.omit(dataa[c("basisOfRecord",
                                 "kingdom",
                                 "phylum",
                                 "order",
                                 "family",
                                 "genus",
                                 "species")]),
          labels = switch(input$pieselect,
                          "basisOfRecord" = ~basisOfRecord,
                          "kingdom" = ~kingdom,
                          "phylum" =  ~phylum,
                          "order"  = ~order,
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
            paper_bgcolor = '#000000',
            plot_bgcolor = '#000000',
            xaxis = list(color = '#ffffff'),
            yaxis = list(color = '#ffffff'),
            leagend = list(color = '#ffffff')
          )
      } else {
        newData <-
          data_temporal() %>%
          filter(switch(input$bar_select,
                        "basisOfRecord" = basisOfRecord,
                        "kingdom" = kingdom,
                        "phylum" =  phylum,
                        "order"  = order,
                        "family" = family,
                        "genus" = genus,
                        "species" = species
          ) %in% select$x)
        if(nrow(newData)==0){
          newData = dataa
        }
          #Remove blank data from column(Blank! Not NA)
          if (!nrow(newData[-which(newData[, input$pieselect] == ""), ]) == 0) {
            newData <- newData[-which(newData[, input$pieselect] == ""), ]
          }
          plot_ly(
            data = na.omit(newData[c("basisOfRecord",
                                     "kingdom",
                                     "phylum",
                                     "order",
                                     "family",
                                     "genus",
                                     "species")]),
            labels = switch(input$pieselect,
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
              paper_bgcolor = '#000000',
              plot_bgcolor = '#000000',
              xaxis = list(color = '#ffffff'),
              yaxis = list(color = '#ffffff'),
              leagend = list(color = '#ffffff')
            )
      }
      
    })
  

    #redraw roseplot when any change made in barplot
    observe({
      select <- event_data("plotly_click", source = "bar_selected")
      if (is.null(select)) {
        output$roseplot <- renderPlot({
          dataForRose <-
            arrange(formattedData(),
                    as.numeric(formattedData()$monthofYear))
          dataForRose <-
            dataForRose[c("basisOfRecord", "monthofYear")]
          if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]) == 0) {
            dataForRose <-
              dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]
          }
          dataForRose <-
            data.frame(table(dataForRose)) %>%
            rename(group = basisOfRecord,
                   variable = monthofYear,
                   value = Freq
            )
          ggplot(data = dataForRose,
                 aes(
                   x = variable,
                   y = value,
                   fill = group
                 )
          ) +
            geom_bar(stat = "identity") +
            coord_polar() + xlab("") + ylab("") + theme(
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#000000", colour = "#000000"),
              plot.background = element_rect(fill = "#000000"),
              axis.text.x = element_text(color = "#ffffff", size = 17),
              axis.text.y = element_text(color = "#ffffff", size = 17),
              # Change legend
              legend.background = element_rect(fill = "black", color = NA),
              legend.key = element_rect(color = "gray", fill = "black"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white")
            )
        },  bg = "transparent")
      } else {
        output$roseplot <- renderPlot({
          dataForRose <-
            formattedData() %>%
            filter(Year_ %in% as.numeric(select))
          dataForRose <-
            arrange(dataForRose, as.numeric(dataForRose$monthofYear))
          dataForRose <-
            dataForRose[c("basisOfRecord", "monthofYear")]
          if (!nrow(dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]) == 0) {
            dataForRose <-
              dataForRose[-which(dataForRose[, "basisOfRecord"] == ""), ]
          }
          dataForRose <-
            data.frame(table(dataForRose)) %>%
            rename(group = basisOfRecord,
                   variable = monthofYear,
                   value = Freq
            )
          ggplot(
            data = dataForRose,
            aes(
              x = variable,
              y = value,
              fill = group
            )
          ) +
            geom_bar(stat = "identity") +
            coord_polar() + xlab("") + ylab("") + theme(
              panel.border = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "#000000", colour = "#000000"),
              plot.background = element_rect(fill = "#000000"),
              axis.text.x = element_text(color = "#ffffff", size = 17),
              axis.text.y = element_text(color = "#ffffff", size = 17),
              # Change legend
              legend.background = element_rect(fill = "black", color = NA),
              legend.key = element_rect(color = "gray", fill = "black"),
              legend.title = element_text(color = "white"),
              legend.text = element_text(color = "white")

            )
        })
      }
    })
  }