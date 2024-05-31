# child geography filters module -----------------------------------------------
#creates an indicator filter where the choices are updated dynamically based on 
#the parent geography (HSCP) selected
#this module will be used in the trend tab to create filters for IMZ and locality
#based on HSCP selected

#ui function
#id = unique id
#label = text associated with dropdown 

child_geography_filters_mod_ui <- function(id, label) {
  ns <- NS(id) # namespace
  selectizeInput(ns("child_geog_filter"), label = label, choices = NULL, multiple = TRUE)
}

# server function
# id = unique id
# filtered_data = reactive df which determines available choices for the filter 
# HSCP_selection = reactive df containing list of available parent areas i.e. HSCPs
# child_areatype = selected child areatype - either "HSC locality" or "Intermediate zone"
# geo_selections = reactive df containing selected areatype, areaname and parent area from global filters


child_geography_filters_mod_server <- function(id, filtered_data, HSCP_selection, child_areatype, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    #create reactive object storing all available child geography areanames 
    #based on availability for indicator and HSCP parent area specified
    available_child_geography_names <- reactive({ 
      filtered_data() |>
        filter(areatype == child_areatype &
                 parent_area == HSCP_selection()$parent_area) |>
        pull(unique(areaname)) # extract the unique values from the areaname column
    })
    
    #create reactive object storing all available child geography area types
    #based on availability for indicator and HSCP selection
    available_areatype <- reactive({
      filtered_data() |> 
        filter(areatype == child_areatype &
                 parent_area == HSCP_selection()$parent_area) |> 
        pull(areatype)
    })
    
    # update child geography choices
    observe({
      updateSelectizeInput(session, "child_geog_filter", choices = available_child_geography_names())
    })
    
    #disable child filter if parent geography unavailable
    observe({
      if(child_areatype %in% available_areatype()) {
        shinyjs::enable("child_geog_filter")
      } else {
        shinyjs::disable("child_geog_filter")
      }
    })
    
    #remove areaname selected in global options to prevent duplication of lines
    observe({
      if(geo_selections()$areatype == child_areatype){
        updateSelectizeInput(session, "child_geog_filter", choices = available_child_geography_names()[available_child_geography_names()!= geo_selections()$areaname])
      } 
    })
    
    
    # store selected child geography as reactive value
    return(
      reactive({
        list(child_geography = input$child_geog_filter)
      })
    )
    
  })
}


##############################################################################
# example usage
##############################################################################
# library(shiny)
# library(dplyr)
# library(bslib)
# 
# # data
# dummy_data <- data.frame(
#                    tab = c("tab1", "tab1", "tab1", "tab1", "tab1", "tab1", "tab1", "tab1", "tab1", "tab1"),
#                    areaname = c("Culter", "Seaton", "Aberdeen Central", "Aberdeen South", "Westend", "Perth Road", "Dundee West", "Partick", "Firhill", "Glasgow North"),
#                    parent_area = c("Aberdeen City", "Aberdeen City", "Aberdeen City", "Aberdeen City", "Dundee City", "Dundee City", "Dundee City", "Glasgow City", "Glasgow City", "Glasgow City"),
#                    indicator = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"),
#                    areatype = c("Intermediate zone", "Intermediate zone", "HSC locality", "HSC locality", "Intermediate zone", "Intermediate zone", "HSC locality", "Intermediate zone", "Intermediate zone", "HSC locality"))
# 
# # ui
# ui <- page_navbar(
#   title = "child geography filters mod example",
#   id = "main_nav",
#   nav_panel(
#     title = "Tab 1", value = "tab1",
#   selectInput(inputId = "parent_geo", label = "Select a HSC Partnership:", choices = unique(dummy_data$parent_area), selected = "Aberdeen City"),
#   child_geography_filters_mod_ui(id = "child_geo_example", label = "Please select Intermediate Zone(s)"),
#   textOutput("child_geog_result")
#   )
# )
# 
# 
# # server
# server <- function(input, output, session){
# 
# # store selected geographies
#   HSCP_selection <- reactive({
#     list(
#       parent_area = input$parent_geo # correct input id
#     )
#   })
# 
# 
# # filter dummy data
# tab_data <- reactive({
#   dummy_data |>
#     filter(tab == input$main_nav)
# })
# 
# 
# 
# # call module which returns selected indicator
# available_child_geos <- child_geography_filters_mod_server("child_geo_example", filtered_data = tab_data, HSCP_selection = HSCP_selection, child_areatype = "HSC locality", geo_selections = geo_selections)
# 
# 
# 
# # print selected indicator
# output$child_geog_result <- renderText({
#   paste0("available child geographies: ", as.character(available_child_geos()))
# })
# 
# }
# 
# 
# shinyApp(ui, server)
