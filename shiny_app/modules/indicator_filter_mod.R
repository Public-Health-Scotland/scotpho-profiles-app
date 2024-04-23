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
# 
# data_1 <- data.frame(indicator = c("a", "b", "c"),
#                      value = c(1, 2, 3))
# 
# data_2 <- data.frame(indicator = c("e", "f", "g"),
#                      value = c(4, 5, 6))
# 
# 
# ui <- fluidPage(
#   radioButtons(inputId = "dataset_filter", label = "dataset:", choices = c("dataset 1", "dataset 2")),
#   indicator_filter_mod_ui("dynamic_filter")
# )
# 
# 
# server <- function(input, output, session){
# 
#   selected_data <- reactive({
#       switch(input$dataset_filter,
#              "dataset 1" = data_1,
#              "dataset 2" = data_2)
#   })
# 
# indicator_filter_mod_server("dynamic_filter", filtered_data = selected_data)
# 
# 
# }
# 
# 
# shinyApp(ui, server)