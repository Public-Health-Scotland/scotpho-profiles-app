# indicator filters module ------------------------------------------------------
# creates an indicator filter where the choices are updated dynamically
# this module can be nested inside other modules (i.e. rank, trends, inequalities)
# where the indicator choices will be dependent on geography and profile selections

# ui function
# id = unique id 
indicator_filter_mod_ui <- function(id) {
  ns <- NS(id) # namespace
  selectInput(ns("indicator_filter"), label = "Select indicator", choices = NULL)
}


# server function
# id = unique id
# filtered_data = reactive df which determines available choices for the filter 
indicator_filter_mod_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateSelectInput(session, "indicator_filter", choices = unique(filtered_data()$indicator))
    })
  })
}


##############################################################################
# example usage
##############################################################################
# library(shiny)
# library(dplyr)
# 
# data <- data.frame(indicator = c("a", "b", "c"),
#                    number = c(1, 2, 3))
# 
# 
# 
# 
# ui <- fluidPage(
#   radioButtons(inputId = "filter", label = "number:", choices = c(1, 2, 3)),
#   indicator_filter_mod_ui("dynamic_filter")
# )
# 
# 
# server <- function(input, output, session){
# 
#   selected_number <- reactive({
#       data |>
#       filter(number == input$filter)
#   })
# 
# indicator_filter_mod_server("dynamic_filter", filtered_data = selected_number)
# 
# 
# }
# 
# 
# shinyApp(ui, server)