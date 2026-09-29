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

    ns <- session$ns

    observeEvent(input$clear_dropdowns, {
      for (input_id in dropdown_id) {

        # Refresh the dropdown with selected = NULL

        isolate({
        updateSelectizeInput(
          session = parent_session,
          inputId = input_id,
          selected = character(0)
        ) #Close update selectize input
        }) #close isolate()
      } #Close for loop
    }) #Close observe event
    
  }) #close moduleServer
} #close server function



###############################################################################.
# Example usage
###############################################################################.

library(bslib)
library(shiny)
library(shinyjs)

ui <- page_navbar(id = "nav",
                 useShinyjs(),
                 title = "Clear Dropdown Button Mod Example",
                 nav_panel(
                   title = "Tab 1",
                   value = "Tab1",
                   selectizeInput("dropdown1",
                                   "Select letters:",
                                   choices = c("A", "B", "C", "D"),
                                   selected = c("B", "C"),
                                   multiple = TRUE),
                   selectizeInput("dropdown2",
                                   "Select numbers:",
                                   choices = c ("1", "2", "3", "4"),
                                   selected = c("1", "4"),
                                   multiple = TRUE),
                   clear_dropdown_ui("clear_dropdowns_test", button_label = "Clear all dropdowns")
                    )#close nav_panel
                 )#close page_navbar



server <- function(input, output, session) {
  clear_dropdown_server(id = "clear_dropdowns_test",
                        dropdown_id = c("dropdown1", "dropdown2"),
                        parent_session = session)
} #close server

shinyApp(ui, server)

