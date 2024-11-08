#' chart_controls module
#'
#' @description Creates additional chart controls placed inside a popover and captures the users selections. 
#' 
#' chart_controls_mod UI function
#' This function creates the UI for the Shiny module. It creates a popover with chart controls.
#'
#' @param id A unique identifier for the module.
#' @param ci_switch A checkbox to turn confidence intervals on/off. Set to TRUE by default. Change to FALSE to exclude option from popover
#' @param yaxis_switch A checkbox to start yaxis at 0. Set to FALSE by defaukt. Change to TRUE to include option in popover
#' @param measure_switch Radio buttons to toggle between Rate or Measure. Set to FALSE by default. Change to TRUE to include option in popover
#' @param year_filter Filter to choose which year to plot in the chart. Set to FALSE by default. Change to TRUE to include option in popover
#' @param average_switch A checkbox to include national average in the chart. Set to FALSE by default. Change to TRUE to include option in popover

chart_controls_mod_UI <- function(id, ci_switch = TRUE, yaxis_switch = FALSE, measure_switch = FALSE, average_switch = FALSE, year_filter = FALSE) {
  ns <- NS(id)
  
  # create a tagList of filters, depending on which arguments are set to TRUE
  popover_filters <- tagList(
    # create year filter choices are set to NULL as they are dynamically created in the modules server function
    # this is because choices will vary depending on which indicator a user selects
    if(year_filter){
      selectizeInput(ns("year_filter"), label = "Select time period", choices = NULL) 
    },
    # creates confidence interval checkbox
    if(ci_switch){
      checkboxInput(ns("ci_switch"), label = "Include 95% confidence intervals", FALSE)
    },
    # creates yaxis at 0 checkbox
    if(yaxis_switch){
      checkboxInput(ns("yaxis_switch"), label = "Start y-axis at zero", TRUE)
    },
    # creates radio buttons to choose which column to plot in chart 
    if(measure_switch){
      radioButtons(ns("measure_switch"), label = "Display as: ", choices = c("Rate", "Numerator"), selected = "Rate")
    },
    if(average_switch){
      checkboxInput(ns("average_switch"), label = "Include average", FALSE)
    },
    
  )
  
  # return a popover with the required filters
  return(
    bslib::popover(
      title = "Chart controls",
      trigger = bs_icon(
        name = "gear-fill",
        size = "2em",
        title = "Click here to view customise chart options",
        class = "chart-controls-icon"
      ),
      popover_filters)
  )
}


#' chart_controls_mod UI function
#' This function creates the Server function for the module. It returns what has been selected from each of the filters in the popover
#' # It also dynamically creates the choices for the year filter if required
#'
#' @param id A unique identifier for the module.
#' @param input, output, session Standard Shiny server arguments.
#' @param indicator_data A reactive dataset filtered by indicator. Optional argument only required if using year filter.

chart_controls_mod_Server <- function(id, indicator_data = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # update choices for year filter based on what years are available
      # in the dataset that has been filtered by selected indicator
      observe({
        req(!is.null(indicator_data))
        choices <- unique(indicator_data()$def_period)
        updateSelectizeInput(session = session, inputId = "year_filter", choices = choices, selected = choices[1])
      })
      
      
      return(
        list(
          ci_switch = reactive({input$ci_switch}),
          yaxis_switch = reactive({input$yaxis_switch}),
          measure_switch = reactive({input$measure_switch}),
          year_filter = reactive({input$year_filter}),
          average_switch = reactive({input$average_switch})
        )
      )
    }
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Example ----
# run the functions above and un-comment the code below for example
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(shiny)
# library(bslib)
# library(bsicons)
# 
# ui <- fluidPage(
#   theme = bs_theme(version = 5),
#   chart_controls_mod_UI(id = "example", ci_switch = TRUE, yaxis_switch = TRUE, measure_switch = TRUE),
#   uiOutput("selections")
#   )
# 
# 
# server <- function(input, output, session){
# 
# popover_selections <- chart_controls_mod_Server(id = "example")
# 
# output$selections <- renderUI({
#   div(
#     p("ci_switch selection is:", popover_selections$ci_switch()),
#     p("yaxis switch selection is:", popover_selections$yaxis_switch()),
#     p("measure switch selection is:", popover_selections$measure_switch())
#     )
# })
# 
# }
# 
# 
# shinyApp(ui, server)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# How do add to app ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# If adding to main UI and server scripts, use the below and replace placeholders/set options to TRUE or FALSE - 
# add to main UI script:
#chart_controls_mod_UI(id = "placeholder", ci_switch = TRUE, yaxis_switch = FALSE, measure_switch = FALSE, year_filter = FALSE)

# add to main Server script (and pass reactive df to indicator_data argument if year_filter is TRUE):
# popover_selections <- chart_controls_mod_Server(id = "placeholder", indicator_data = NULL)

# If nesting inside another module, use the below and replace placeholders - 

# add to modules UI function:
#chart_controls_mod_UI(id = ns("placeholder"))

# add to modules Server function (and pass reactive df to indicator_data argument if year_filter is TRUE):
# popover_selections <- chart_controls_mod_Server(id = "placeholder", indicator_data = NULL)
