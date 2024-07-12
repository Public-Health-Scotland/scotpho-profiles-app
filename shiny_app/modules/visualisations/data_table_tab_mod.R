#####################################
# data_table_tab_mod.R
# This module creates the layout for the data tab
# It allows users to build a data table that can be downloaded in various formats
#####################################

# TO DO: expand to include other datasets (i.e. SIMD, pop groups splits etc.)


# UI function:
# id = unique id
data_tab_modUI <- function(id) {
  ns <- NS(id)# namespace
  tagList(
    # sidebar layout
    page_sidebar(
      fillable = FALSE,
      # sidebar with filters
      sidebar = sidebar(
        width = 300, 
        padding = 20, 
        h2("Filters"),
        # clear filters button
        actionButton(ns("clear_table_filters"),
                     label = "Clear all filters",
                     icon ("eraser"),
                     class = "btn-sm"),
        # mandatory geography filter
        jstreeOutput(ns("geography_selector")),
        # optional profile filter
        virtualSelectInput(inputId = ns("profile_selector"),
                           label = "Select profile(s)",
                           choices = unname(profiles_list),
                           disableSelectAll = FALSE,
                           multiple = TRUE,
                           search = TRUE,
                           searchByStartsWith = TRUE,
                           width = '100%',
                           zIndex = 100),
        # optional indicator filter
        virtualSelectInput(inputId = ns("indicator_selector"),
        label = "Select indicator(s)",
        noOptionsText = "Select atleast one geography to see what indicators are available",
        choices = NULL, # choices updated in server function depending on geography selected
        disableSelectAll = TRUE,
        multiple = TRUE,
        search = TRUE,
        searchByStartsWith = TRUE,
        dropboxWrapper = "body",
        dropboxWidth = '400px',
        width = '100%',
        zIndex = 100),
       # time period filter
        radioGroupButtons(
        inputId = ns("time_period_selector"),
        label = "Select time period:",
        choices = c("Latest available year", "All years"),
        selected = "Latest available year") # default to show max year for each filter
       ), # close sidebar
        
        # title         
        h1("Data table"),
        # instructions
        p("Use the filters to build a data table, which can then be downloaded in various
	         formats using the button below. Please note that the table below is a preview. 
	         The downloaded dataset will contain more columns containing metadata than are presented here."),
         # download data button
         download_data_btns_ui(id = ns("datatable_downloads")),
          # data table
         reactableOutput(ns("data_tab_table"))
      ) # close layout
  ) # close tagList
} # close module UI function 



# Server function
# id = unique id matching that assigned to the ui module
data_tab_mod_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    
      ###############################################
      # Dynamic filters
      ###############################################
      
      # Update indicator filter choices based on geography selected
      # (and further updating if profile also selected)
      observe({
        
        # return selected geographies
        paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
        
        # filter selected dataset by selected geographies
        data <- main_dataset |>
          subset(geo_path %in% paths)
        
        # create vector of available indicators
        available_indicators <- unique(data$indicator)
        
        # Store the current selection of indicators (if there were any)
        # i.e. if you've switched dataset but had previously selected some indicators
        current_selected_indicators <- input$indicator_selector
        
        
        # Further filter indicators if a profile is selected
        if (!is.null(input$profile_selector) && input$profile_selector != "") {
          
          profile_filtered_data <- data |>
            filter(if_any(contains("profile_domain"),
                          ~ substr(.x, 1, 3) %in% names(profiles_list)[match(input$profile_selector, profiles_list)]))
          
          available_indicators <- unique(profile_filtered_data$indicator)
          
        }
        
        # Update the filter choices
        updateVirtualSelect(session = session,
                            inputId = "indicator_selector",
                            choices = available_indicators)
        
        # Reapply the previous selection if they are still valid
        valid_selections <- intersect(current_selected_indicators, available_indicators)
        
        if (!is.null(valid_selections) && length(valid_selections) > 0) {
          updateVirtualSelect(session = session,
                              inputId = "indicator_selector",
                              selected = valid_selections)
          
        }
        
      })
      
      
      
      ## reset all filters when 'clear filters' button is clicked 
      observeEvent(input$clear_table_filters, {
        
        # reset the dataset selector to "Main Dataset"
        updateRadioGroupButtons(session, 
                                inputId = "dataset_selector", 
                                selected = "Main Dataset")
        
        # reset the geographies to those available for the main dataset
        jstreeUpdate(session, "geography_selector", main_data_geo_nodes)
        
        
        # reset the time period filter to max year per indicator
        updateRadioGroupButtons(session = session,
                                inputId = "time_period",
                                selected = "Latest available year")
        
        # reset the indicator list to those present in main dataset
        updateVirtualSelect(session = session,
                            inputId = "indicator",
                            selected = NULL,
                            choices = NULL)
        
        # reset the profile filter
        updateVirtualSelect(session = session,
                            inputId = "profile_selector",
                            selected = NULL,
                            choices = profiles_list)
        
      })
      
      
      
      
      
      # render the geography filter using the 'main_data_geo_nodes' file 
      # this file contains geography lists in the required format
      output$geography_selector <- renderJstree({
        jstree(
          main_data_geo_nodes,
          checkboxes = TRUE,
          selectLeavesOnly = TRUE,
          theme = "proton"
        )
      })
      
      
      
      #####################################
      # Reactive data
      ####################################
      
      # data to display/download ----
      tableData <- reactive({
        
        # selected dataset
        data <- main_dataset 
        
        # filter by selected geographies
        paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
        data <- data |> subset(geo_path %in% paths)
        
        
        # filter by time period 
        if(input$time_period_selector == "Latest available year") {
          setDT(data) # switch to data.table format here as quicker than grouping using dplyr
          data <- data[, .SD[year == max(year)], by = indicator]
        } else data
        
        
        # if profile selected (but indicators have not been)
        # then filter by selected profiles only 
        if(isTruthy(input$profile_selector) & !isTruthy(input$indicator_selector)) {
          data <- data |>
            filter(if_any(contains("profile_domain"),
                          ~ substr(.x, 1, 3) %in% names(profiles_list)[match(input$profile_selector, profiles_list)]))
          
          
          # if a profile has been selected (and some indicators too)
          # then filter by profile and indicator
        } else if(isTruthy(input$profile_selector) & isTruthy(input$indicator_selector)) {
          
          data <- data |>
            filter(if_any(contains("profile_domain"),
                          ~ substr(.x, 1, 3) %in% names(profiles_list)[match(input$profile_selector, profiles_list)])) |>
            filter(indicator %in% input$indicator_selector)
          
          
          
          # if no profile has been selected but some indicators have
          # then filter by indicators only 
        } else if(!isTruthy(input$profile_selector) & isTruthy(input$indicator_selector)) {
          
          data <- data |>
            filter(indicator %in% input$indicator_selector)
          
        } else {
          
          # if nothings been selected from profile or indicator filter then return all available indicators
          # for chosen dataset/geography/time period
          data <- data
        }
        
        # rename some columns 
        data <- data |>
          rename(area_code = code, 
                 area_type = areatype, 
                 area_name = areaname, 
                 period = def_period, 
                 upper_confidence_interval = upci, 
                 lower_confidence_interval = lowci)
        
        # columns to return
        data <- data |>
          select(area_code, area_type, area_name, year, period, type_definition,
                 indicator, numerator, measure, 
                 upper_confidence_interval, lower_confidence_interval) 
        
        
      })
      
      
      ####################################
      # Data table
      ###################################
      
      
      output$data_tab_table <- renderReactable({
        
        reactable(tableData() ,
                  compact = TRUE,
                  columns = list(
                    area_code = colDef(show = FALSE),
                    year = colDef(show = FALSE),
                    upper_confidence_interval = colDef(show = FALSE),
                    lower_confidence_interval = colDef(show = FALSE),
                    indicator = colDef(minWidth = 200),
                    area_type = colDef(name = "Area type"),
                    area_name = colDef(name = "Area name"),
                    type_definition = colDef(name = "Type definition")
                  )
        )
        
      })
      
      
      
      #################################
      # Downloads
      ##################################
      
      # data table bulk download (note this is a module )
      download_data_btns_server(id = "datatable_downloads", data = tableData)
      
      
      
    })
} # close module server function 



######################################
# EXAMPLE USAGE 
# uncomment the code below to see how this module works
# note this module requires the 'main_dataset' parquet file, the 'main_data_geo_nodes' file 
# and the 'profiles_list' object from the global script
# Therefore recommended that you first run the main app first so these 3 things become available in your global environment
######################################

# # packages
# library(shiny)
# library(bslib)
# library(jsTreeR)
# 
# # ui
# ui <- fluidPage(
#   data_tab_modUI(id = "dt") # assign the module a unique id 
#   
# )
# 
# # server
# server <- function(input, output, session) {
#   data_tab_mod_Server(id = "dt") # use the matching id assigned in the ui
# }
# 
# # run app 
# shinyApp(ui, server)