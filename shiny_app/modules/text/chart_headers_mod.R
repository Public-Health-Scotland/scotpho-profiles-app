#' chart_headers module
#'
#' @description 3 dynamic chart headers (a main header and 2 x sub-headers) to place above each chart 
#' 
#' # chart_headers_mod_UI
#' This function creates the UI for the Shiny module. It returns the dynamic text defined in the modules server function.
#'
#' @param id A unique identifier for the module.

chart_headers_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("headers"))
  )
}


#' chart_headers_mod_Server
#' This function creates the Server for the module. It dynamically creates the headers to describe the chart, depending on what 
#' indicator a user has selected, and what type of visualisation is being shown.
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @param data The name of the reactive dataset that is being used to create the chart that the headers are to describe
#' @param main_header_extra Optional argument to include in main header to describe what the data is split by (for instance the selected value from another filter)
#'

chart_headers_mod_Server <- function(id, data, main_header_extra = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$headers <- renderUI({
        
        # text to display to users if no indicators available
        shiny::validate(
          need(nrow(data()) > 0, "No indicators available")
        )
        
        # the main header - the name of the selected indicator 
        # plus some additional text if the main_header_extra argument is used
        main_header <- if(!is.null(main_header_extra)){
          if(is.reactive(main_header_extra)){
          paste0(data()$indicator[1], "; split by ", main_header_extra())
          } else {
            paste0(data()$indicator[1], "; split by ", main_header_extra)
          }
        } else {
          data()$indicator[1]
        }
        
        # the first sub-header describing the time period the chart covers
        # if there is more than 1 year worth of data in the dataframe passed to the module
        # then assume it's a trend and create header that describes the time period covered
        # otherwise display the name of the single year the chart covers
        sub_header1 <- if(length(unique(data()$trend_axis)) == 1){
          unique(data()$trend_axis[1])
        } else {
          paste0(min(data()$trend_axis), " to ", max(data()$trend_axis))
          
        }
        
        # the second sub-header - the type of measure the indicator is 
        # (i.e percentage, age-sex standardised rate, crude rate etc.)
        sub_header2 <- paste0(data()$type_definition[1])
        
        # return the 3 bits of information 
        return(
          tags$div(
            tags$h5(main_header, style = "font-weight: 700; font-size: 1.2rem;"), # make the main header bold
            tags$h6(sub_header1),
            tags$p(sub_header2)
          )
        )
      })
      
    }
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# library(shiny)
# library(bslib)
# 
# data <- data.frame(
#   indicator = c(rep("indicator 1", 4), rep("indicator 2", 4)),
#   type_definition = c(rep("age-sex standarised rate", 4), rep("crude rate", 4)),
#   trend_axis = c(rep("2021/22", 2), rep("2022/23", 2), rep("2023", 4)),
#   split = c(rep(c("age", "sex"), 2), rep("age", 2), rep("sex", 2))
#   )
# 
# ui <- fluidPage(
#   theme = bs_theme(version = 5),
#   selectizeInput("indicator_filter", label = "select indicator", choices = c("indicator 1", "indicator 2")),
#   selectizeInput("splits_filter", label = "select split", choices = c("age", "sex")),
#   bslib::card(
#   chart_headers_mod_UI(id = "chart_headers")
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   reactive_df <- reactive({
#     data |>
#       filter(indicator == input$indicator_filter & split == input$splits_filter) 
# 
#   })
# 
#   
#   chart_headers_mod_Server(id = "chart_headers", data = reactive_df, main_header_extra = reactive({input$splits_filter}))
# }
# 
# shinyApp(ui, server)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders - 
# add to main UI script:
#chart_headers_mod_UI("placeholder")

# add to main Server script:
#chart_headers_mod_Server("placeholder", data = placeholder, main_header_extra = NULL)

# If nesting inside another module, use the below and replace placeholders - 

# add to modules UI function:
#chart_headers_mod_UI(ns("placeholder"))

# add to modules Server function:
#chart_headers_mod_Server("placeholder", data = placeholder, main_header_extra = NULL)












