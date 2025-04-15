# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metadata_scroll_button.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @description an action link that when clicked on, scrolls to a particular object on the page and 
#' returns the status of the button (i.e. when it has been clicked) 
#' object on page can then react based on button status


#' metadata_scroll_button_UI Function
#'
#' @param id unique id 
#' @param target_id the id of the object to scroll to 
#' 
#' 


# UI function 
metadata_scroll_button_UI <- function(id, target_id) {
  ns <- NS(id)
  tagList(
    actionLink(
      inputId = ns("go_to_metadata"),
      label = "Scroll to indicator metadata",
      icon = icon("arrow-down"),
      onclick = sprintf("document.getElementById('%s').scrollIntoView();", target_id)
    ) #sprintf pastes together scrolling functionality and page object id
  )
}

#' metadata_scroll_button_Server Function
#'
#' @param id unique id 
#' 
#' 

# Server function 
metadata_scroll_button_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # return results of input$go_to_metadata 
      # note: the value is 0/NULL by default and increments by 1 each time clicked
      # we set ignoreNULL = TRUE to only start tracking the button when it's actually been clicked
      return(
        eventReactive(input$go_to_metadata, {
          input$go_to_metadata
          }, ignoreNULL = TRUE)
      )
    }
  )
}


# To be copied in the server
# metadata_scroll_button_UI(id = "placeholder", target_id = "placeholder)
# btn_click <- metadata_scroll_button_Server(id = "placeholder")




# ~~~~~~~~~~~~~~~~~
# example ----
# ~~~~~~~~~~~~~~~~~

# library(shiny)
# 
# ui <- fluidPage(
# 
#   # modules UI function
#   metadata_scroll_button_UI(
#     id = "scroll", # the modules id
#     target_id = "my_div" # the target to scroll to
#     ),
# 
#   # to print the status of the button each time it's clicked
#   # to demonstrate how action buttons work and what's returned on each click
#   textOutput("button_status"),
# 
#   # a placeholder div
#   div(style = "height: 1000px"),
# 
#   # the area of the app to scroll to when button is clicked (i.e. the target)
#   div(id = "my_div", p("scroll to here!")),
# )
# 
# server <- function(input, output, session) {
# 
#   # the modules server function
#   btn_click <- metadata_scroll_button_Server(id = "scroll")
# 
#   # the information that the module returns
#   # notice how it doesn't show when the app is initially launched (due to use of ignoreNULL = TRUE)
#   # but changes each time the button is clicked
#   output$button_status <- renderText({paste0("button status:", btn_click())})
# }
# 
# shinyApp(ui, server)
# 
# 
# 


