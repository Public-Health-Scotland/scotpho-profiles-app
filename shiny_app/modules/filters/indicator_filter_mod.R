# indicator filters module ------------------------------------------------------
# creates an indicator filter where the choices are updated dynamically
# this module can be nested inside other modules (i.e. rank, trends, inequalities)
# where the indicator choices will be dependent on geography and profile selections

# ui function
# id = unique id 
indicator_filter_mod_ui <- function(id, label = "Select indicator") {
  ns <- NS(id) # namespace
  selectizeInput(ns("indicator_filter"), label = label, choices = NULL, multiple = FALSE)
  
}

# server function
# id = unique id
# filtered_data = reactive df which determines available choices for the filter 
indicator_filter_mod_server <- function(id, filtered_data, geo_selections, selected_profile) {
  moduleServer(id, function(input, output, session) {
    
    
    # update indicator choices if user changes profile or geography
    observeEvent(c(selected_profile(), geo_selections()),{
      dt <- setDT(filtered_data())
      
      # filter data by selected geography to get available indicators for selected profile
      dt <- dt[areatype == geo_selections()$areatype & areaname == geo_selections()$areaname]
      
      if(selected_profile()$full_name == "All Indicators"){
        choices <- unique(dt$indicator)
      } else {
      
      # select columns and get unique rows
      dt <- unique(dt[, c("indicator", "ind_id", "domain")])
      
      # replace domain to be called 'Archive indicators' if indicator is in archived_indicators vector (in global script)
      dt <- dt[ind_id %in% archived_indicators, domain := "Archived indicators"]
      
      # arrange indicators alphabetically
      dt <- setorder(dt, indicator)
      
      # if the selected profile has a particular order the domains should appear in the table
      # (i.e. the domain_order is not NULL in the 'profiles_list' from the global script)
      # then covert the domain column to factor and set levels to ensure the data is ordered accordingly
      # in the indicator filter, whilst also ensuring that 'archived' indicators are always at the bottom
        if(!is.null(selected_profile()$domain_order)){
          dt$domain<- factor(dt$domain, levels = c(selected_profile()$domain_order, "Archived indicators"))
        } else {
          dt$domain<- factor(dt$domain, levels = c(setdiff(unique(dt$domain), "Archived indicators"), "Archived indicators"))
        }

      # Create a list of choices for the filter grouped by domain 
      choices <- split(dt$indicator, dt$domain) # create list that splits up indicators by domain
      choices <- lapply(choices, function(x) as.list(x)) # convert to list of lists
      
      }
      # populate the indicator filter with indicator choices grouped by domain
      updateSelectizeInput(session, "indicator_filter", 
                           choices = choices
      ) 
    })
    
    
    # store selected indicator as reactive value
    return(
      reactive({
        input$indicator_filter
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
#                    tab = c("tab1", "tab1", "tab1", "tab1"),
#                    areatype = c("Health board", "Health board", "Health board", "Health board"),
#                    areaname = c("NHS GGC", "NHS Grampian", "NH FV", "NHS Highland"),
#                    indicator = c("a", "b", "c", "d"))
# 
# 
# 
# # ui 
# ui <- page_navbar(
#   title = "indicator filter mod example",
#   id = "main_nav",
#   nav_panel(
#     title = "Tab 1", value = "tab1",
#   selectInput(inputId = "areatype", label = "areatype:", choices = c("Health board"), selected = "Health board"),
#   selectInput(inputId = "areaname", label = "areaname:", choices = unique(dummy_data$areaname)),
#   indicator_filter_mod_ui("dynamic_indicators"),
#   textOutput("indicator_result")
#   )
# )
# 
# 
# # server 
# server <- function(input, output, session){
# 
# # store selected geographies
# geo_selections <- reactive({
#   list(
#     areaname = input$areaname,
#     areatype = input$areatype
#   )
# })
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
# selected_indicator <- indicator_filter_mod_server("dynamic_indicators", filtered_data = tab_data, geo_selections = geo_selections)
# 
# 
# 
# # print selected indicator
# output$indicator_result <- renderText({
#   paste0("selected indicator: ", as.character(selected_indicator()))
# })
# 
# }
# 
# 
# shinyApp(ui, server)