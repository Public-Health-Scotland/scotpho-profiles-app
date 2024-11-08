#' download_chart_mod UI function
#'
#' @description A button that when clicked, saves a highchart as a PNG using the 'exportChart' function from JS highcharts.
# see https://www.highcharts.com/docs/export-module/export-module-overview  for more details
#' 
#' This function creates the UI for the Shiny module. It creates a single 'save chart' button.
#'
#' @param id A unique identifier for the module.

download_chart_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(
      HTML(
        "Shiny.addCustomMessageHandler('triggerDownload', function(message) {
      var chartIndex = Highcharts.charts.findIndex(function(chart) {
        return chart && chart.renderTo.id === message.chartId;
      });
        Highcharts.charts[chartIndex].exportChart({
          sourceWidth: message.width,
          sourceHeight: message.height
        });
    });"
      )
    ),
    actionButton(
      ns("chart_download"), 
      label = "Save chart (PNG)", 
      class = "btn-sm me-2 btn-download", 
      icon = icon("chart-simple")
    )
  )
}


#' download_data_btns UI function
#'
#' This function creates the Server function for the module. It triggers the chart to be saves as a png when the button is clicked
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @param data The name of the reactive dataframe to be downloaded
#' @param chart_id The outputId of the highchart
#' @param width Width of the chart. Optional argument set to 600 by default
#' @param height Height of the chart. Optional argument set to 400 by default

download_chart_mod_server <- function(id, chart_id, width = 600, height = 400) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # when button clicked, trigger the JS code to run
    observeEvent(input$chart_download, {
      session$sendCustomMessage("triggerDownload",
                                list(chartId = chart_id,
                                     width = width,
                                     height = height)
      )
    })
  })
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(shiny)
# library(highcharter)
# 
# 
# ui <- fluidPage(
#    download_chart_mod_ui(id = "download_barchart"),
#    highchartOutput(outputId = "my_chart")
#    )
# 
# server <- function(input, output, session){
# 
# 
#   output$my_chart <- renderHighchart({
#     hchart(mtcars$hp) |>
#       hc_chart(backgroundColor = 'white')
# 
#   })
# 
#   download_chart_mod_server(id = "download_barchart", chart_id = "my_chart")
# 
# 
# 
# }
# 
# shinyApp(ui, server)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders - 
# add to main UI script:
#download_chart_mod_ui(id = "placeholder")

# add to main Server script:
# download_chart_mod_server(id = "placeholder", chart_id = "placeholder")

# If nesting inside another module, use the below and replace placeholders - 

# add to modules UI function:
#download_chart_mod_ui(id = ns("placeholder"))

# add to modules Server function:
# download_chart_mod_server(id = "placeholder", chart_id = ns("placeholder"))