#' card_footer_buttons module
#'
#' @description A bslib card footer with two download buttons that allow users to save a highchart or download a reactive dataset. 
#' Note that both buttons are individual shiny modules that are being nested in one module
#' 
#' # card_footer_buttons_UI
#' This function creates the UI for the Shiny module. It is a card footer with two download buttons.
#'
#' @param id A unique identifier for the module.

card_footer_buttons_UI <- function(id) {
  ns <- NS(id)
  # NOTE: the 'footer' argument for multi-tab cards (like navset_card_pill()) is currently not working
  # package maintainers are aware and working on a fix
  # using the card_footer argument for card() in the meantime and suppressing warnings until bug fixed
  suppressWarnings(
  card_footer(
    class = "d-flex justify-content-left",
    download_chart_mod_ui(id = ns("save_chart")),
    download_data_btns_ui(id = ns("save_data")))
  )
}


#' card_footer_buttons_Server
#' This function creates the Server for the module. It triggers the charts/data to download when the buttons are clicked
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @param chart_id The OutputId of the highchart to be saved
#' @param width The width of the chart. Optional argument that is set to 600 by default
#' @param height The height of the chart. Optional argument that is set to 400 by default
#' @param data The name of the reactive dataframe to be downloaded
#' @param file_name The filename of the data download
#' @param selected_columns a vector of column names to include in the data download. Optional argument set to NULL to include all by default.
#'
card_footer_buttons_Server <- function(id, chart_id, width = 600, height = 400, data, selected_columns = NULL, file_name) {
  moduleServer(
    id,
    function(input, output, session) {
      download_chart_mod_server(id = "save_chart", chart_id = chart_id, width = width, height = height)
      download_data_btns_server(id = "save_data", data = data, selected_columns = selected_columns, file_name = file_name)
    }
  )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(bslib)
# library(highcharter)
# library(shiny)
# library(dplyr)
# library(reactable)
# 
# source(paste0(getwd(),"/shiny_app/modules/buttons/chart_download_mod.R"))
# source(paste0(getwd(),"/shiny_app/modules/buttons/data_download_mod.R"))
# 
# ui <- fluidPage(
#   theme = bslib::bs_theme(version = 5),
#   bslib::navset_card_pill(
#     bslib::nav_panel("chart", highchartOutput(outputId = "example_chart")),
#     bslib::nav_panel("data", "placeholder"),
#     card_footer_buttons_UI(id = "example")
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   reactive_df <- reactive({mtcars})
#   
#  output$example_chart <- renderHighchart({
#    hchart(reactive_df()$hp) |>
#      hc_chart(backgroundColor = 'white')
#    })
#  
#  
#   
#   card_footer_buttons_Server(id = "example", chart_id = "example_chart", data = "example_data", file_name = "mtcars_data")
# }
# 
# shinyApp(ui, server)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders - 
# add to main UI script:
#card_footer_buttons_UI("placeholder")

# add to main Server script:
#card_footer_buttons_Server("placeholder", chart_id = "placeholder", data = "placeholder", file_name = "placeholder")

# If nesting inside another module, use the below and replace placeholders - 

# add to modules UI function:
#card_footer_buttons_UI(ns("placeholder"))

# add to modules Server function:
#card_footer_buttons_Server("placeholder", chart_id = ns("placeholder"), data = "placeholder", file_name = "placeholder")

