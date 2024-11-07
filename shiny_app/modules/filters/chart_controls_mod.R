# library(bslib)
# library(shiny)
# library(bsicons)

chart_controls_UI <- function(id, ci_switch = TRUE, yaxis_switch = FALSE, measure_switch = FALSE, year_filter = FALSE) {
  ns <- NS(id)
  
  # create a tagList of filters, depending on which arguments are set to TRUE
  popover_filters <- tagList(
    if(year_filter){
    selectizeInput(ns("year_filter"), label = "Select time period", choices = NULL) 
    },
    if(ci_switch){
    checkboxInput(ns("ci_switch"), label = "Include 95% confidence intervals", TRUE)
      },
    if(yaxis_switch){
    checkboxInput(ns("yaxis_switch"), label = "Start y-axis at zero", TRUE)
    },
    if(measure_switch){
      radioButtons(ns("measure_switch"), label = "Display as: ", choices = c("Rate", "Numerator"), selected = "Rate")
    }
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

chart_controls_Server <- function(id, indicator_data = NULL) {
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
        reactive({
          list(
          ci_switch = input$ci_switch,
          yaxis_switch = input$yaxis_switch,
          measure_switch = input$measure_switch,
          year_filter = input$year_filter
          )
        })
      )
    }
  )
}





# ui <- page(
#   chart_controls_UI(id = "chart_controls"),
#   uiOutput("selections")
#   )
# 
# 
# server <- function(input, output, session){
#   
# chart_control_selections <- chart_controls_Server(id = "chart_controls")
# 
# output$selections <- renderUI({
#   div(
#     p("ci_switch is:", chart_control_selections()$ci_switch),
#     p("yaxis switch is:", chart_control_selections()$yaxis_switch),
#     p("measure switch is:", chart_control_selections()$measure_switch)
#     )
# })
# 
# }
# 
# 
# shinyApp(ui, server)
