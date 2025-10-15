###############################################################################.
# MODULE: clear_dropdown_button_mod ----
# creates a button that when clicked on removes all selected options in a vectorised list of dropdowns

clear_dropdown_ui <- function(button_id, button_label){
  ns <- shiny::NS(button_id)
  tagList(
    shiny::actionLink(inputId = ns("clear_dropdowns"),
                      label = button_label)
  ) #close tagList
} #close UI


clear_dropdown_server <- function(id, dropdown_id, parent_session){
  shiny::moduleServer(id, function(input, output, session) {
    
    observeEvent(input$clear_dropdowns, {
      for (i in seq_along(dropdown_id)) {
        
        # Get currently selected choices from input
        input_id <- dropdown_id[i]
        current_choices <- parent_session$input[[input_id]]
        
        # Refresh the dropdown with selected = NULL
        # Multiple steps required because just using updateSelectizeInput to update selected to NUll doesn't actually refresh
        updateSelectizeInput(
          session = parent_session,
          inputId = input_id,
          choices = current_choices,
          selected = NULL
        ) #Close update selectize input
      } #Close for loop
    }) #Close observe event
  }) #close moduleServer
} #close server function


###############################################################################.
# Example usage
###############################################################################.

# library(bslib)
# library(shiny)
# library(shinyjs)
# 
# ui <- page_navbar(id = "nav",
#                  useShinyjs(),
#                  title = "Clear Dropdown Button Mod Example",
#                  nav_panel(
#                    title = "Tab 1",
#                    value = "Tab1",
#                    selectizeInput("dropdown1", 
#                                    "Select letters:",
#                                    choices = c("A", "B", "C", "D"),
#                                    selected = c("B", "C"),
#                                    multiple = TRUE),
#                    selectizeInput("dropdown2",
#                                    "Select numbers:",
#                                    choices = c ("1", "2", "3", "4"),
#                                    selected = c("1", "4"),
#                                    multiple = TRUE),
#                    clear_dropdown_ui("clear_dropdowns_test", button_label = "Clear all dropdowns")
#                     )#close nav_panel
#                  )#close page_navbar
#            
#                
#                   
# server <- function(input, output, session) {
#   clear_dropdown_server(id = "clear_dropdowns_test", 
#                         dropdown_id = c("dropdown1", "dropdown2"), 
#                         parent_session = session)
# } #close server
# 
# shinyApp(ui, server)
# 
# 

