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
indicator_filter_mod_server <- function(id, filtered_data, geo_selections, domain_order = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    # update indicator choices
    observe({
      
      all <- setDT(filtered_data())
      
      # filter data by selected geography to get available indicators for selected profile
      all <- all[areatype == geo_selections()$areatype & areaname == geo_selections()$areaname]
      
      # get lists of active indicators grouped by domain (and sorted: either alphabetically, or by specified domain_order)
      all_domains <- sort(unique(all$domain))
      
      if(!is.null(domain_order)) {
        all_domains <- all_domains[order(match(all_domains, domain_order))]
      }
      
      ind_list <- list()
      
      # Subset the indicators by domain, sort them, and add to the ind_list (which becomes a list of lists)
      for (i in all_domains) {
        # single item lists were being treated differently in the drop down, hence this logic
        if (length(unique(all[!(ind_id %in% archived_indicators) & (domain == i)]$indicator)) > 1) { 
          ind_list[[i]] <- sort(unique(all[!(ind_id %in% archived_indicators) & (domain == i)]$indicator))
        } else if (length(unique(all[!(ind_id %in% archived_indicators) & (domain == i)]$indicator)) == 1){
          ind_list[[i]] <- list(sort(unique(all[!(ind_id %in% archived_indicators) & (domain == i)]$indicator)))
        }
      }
      # Add the archived indicators
      # single item lists were being treated differently in the drop down, hence this logic
      if (length(unique(all[(ind_id %in% archived_indicators)]$indicator)) > 1) {
        ind_list[["Archived"]] <- sort(unique(all[(ind_id %in% archived_indicators)]$indicator))
      } else if (length(unique(all[(ind_id %in% archived_indicators)]$indicator)) == 1) {
        ind_list[["Archived"]] <- list(sort(unique(all[(ind_id %in% archived_indicators)]$indicator)))
      }
      
      
      # populate the indicator filter with indicator choices - grouping choices into domains, then archived at the end
      updateSelectizeInput(session, "indicator_filter", 
                           choices = ind_list
      ) 
      
      
    })
    
    
    # store selected indicator as reactive value
    return(
      reactive({
        list(indicator = input$indicator_filter)
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

