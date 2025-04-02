# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# metadata_scroll_button.R
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @description an action link that when clicked on, scrolls to a particular object on the page and 
#' returns the status of the button (i.e. when it has been clicked) 


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
    )
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
      # note: value is initially 0/NULL by default, and increments by 1 each time link pressed
      return(reactive({input$go_to_metadata}))
    }
  )
}


# To be copied in the server
# metadata_scroll_button_UI(id = "placeholder", target_id = "placeholder)
# btn_click <- metadata_scroll_button_Server(id = "placeholder")




# # ~~~~~~~~~~~~~~~~~
# # example ----
# # ~~~~~~~~~~~~~~~~~
# 
# library(shiny)
# 
# ui <- fluidPage(
#   metadata_scroll_button_UI(id = "scroll", target_id = "my_div"),
#   textOutput("button_status"),
#   div(style = "height: 1000px"),
#   div(id = "my_div", p("scroll to here!")),
# )
# 
# server <- function(input, output, session) {
# 
#   btn_click <- metadata_scroll_button_Server(id = "scroll")
#   output$button_status <- renderText({paste0("button status:",btn_click())})
# }
# 
# shinyApp(ui, server)
# 
# 



