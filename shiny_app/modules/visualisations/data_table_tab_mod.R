#####################################.
# data_table_tab_mod.R
# This module creates the layout for the data tab
# It allows users to build a data table that can be downloaded in various formats
#####################################.

# TO DO: expand to include other datasets (i.e. pop groups splits etc.)

#######################################################.
## MODULE UI ----
#######################################################.

data_tab_modUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_sidebar(padding = 20, gap = 20,
                       sidebar = sidebar(width = 300, padding = 20,
                                         open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                                         
                                         # clear filters button
                                         actionButton(ns("clear_table_filters"),
                                                      label = "Clear all filters",
                                                      icon ("eraser"),
                                                      class = "down"),
                                         
                                         # dataset selection filter
                                         radioButtons(
                                           inputId = ns("dataset_selector"),
                                           label = bslib::tooltip(
                                             placement = "bottom",
                                             trigger = list("Select a dataset",icon("info-circle")),
                                                        "The main dataset contains data on all indicators at 
                                                        various geography levels. The inequalities dataset is a smaller subset of indicators 
                                             and geographies, which are split by SIMD quintiles and other measures of inequality 
                                             (this dataset underpins the visualisations in the inequalities sub-tabs).", 
                                           ),                                           choices = c("Main Dataset","Inequalities Dataset"),
                                           selected = "Main Dataset" # default on main opt dataset
                                         ),

                                         # quintile type filter (only if inequalities dataset is selected)
                                         conditionalPanel(
                                           id = ns(id),
                                           condition = "input['dataset_selector'] === 'Inequalities Dataset'",
                                           radioGroupButtons(
                                             inputId = ns("quint_type_selector"),
                                             label = "Select quintile type",
                                             choices = c("Local", "Scotland"),
                                             selected = "Scotland" # default on max year for each indicator
                                           )
                                         ),
                                         
                                         # Geography filters
                                         jstreeOutput(ns("geography_selector")),
                                         
                                         # profile filters
                                         virtualSelectInput(inputId = ns("profile_selector"),
                                                            label = "Select profile(s)",
                                                            choices = profiles_list,
                                                            disableSelectAll = FALSE,
                                                            multiple = TRUE,
                                                            search = TRUE,
                                                            searchByStartsWith = TRUE,
                                                            width = '100%',
                                                            zIndex = 100),
                                         
                                         # indicator filters
                                         virtualSelectInput(inputId = ns("indicator_selector"),
                                                            label = "Select indicator(s)",
                                                            noOptionsText = "Select atleast one geography to see what indicators are available",
                                                            choices = NULL,
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
                                           selected = "Latest available year"
                                         )
                                         
                       ), # close sidebar
                       
                       h1("Download data"),
                       p("Use the filters to build a data table, which can then be downloaded in various formats using the button below. 
                         Please note that the table below is a preview. The downloaded dataset will contain more columns containing metadata 
                         than are presented here."),
                 
                 
                       # download data button
                        download_data_btns_ui(id = ns("datatable_downloads")),
                       # data table
                       DTOutput(ns("data_tab_table"))
                       
        ) # close layout

  )
}

#######################################################.
## MODULE SERVER ----
#######################################################.

data_tab_mod_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      #############################.
      # REACTIVE DATASETS ----
      #############################.

      # selected dataset
      selectedData <- reactive({
        switch(input$dataset_selector,
               "Main Dataset" = main_dataset,
               "Inequalities Dataset" = simd_dataset)
        })
      
      
      
      # choices for geography filter, depending on what dataset is selected
      # this is required because the deprivation dataset only has Scotland, CA and HB level data
      GeographyNodes <- reactive({
        if(input$dataset_selector == "Main Dataset") {
          main_data_geo_nodes # full list of geographies
        } else {
          main_data_geo_nodes[c(1:3)] # scotland, hb and ca only 
        }
      })
      
      
      # data to display in table /download 
      tableData <- reactive({
        
        # selected dataset
        data <- selectedData() 
        
        # filter by selected geographies
        paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
        data <- data |> subset(geo_path %in% paths)
        
        
        # filter by time period 
        if(input$time_period_selector == "Latest available year") {
          setDT(data) # switch to data.table format here as quicker than grouping using dplyr
          data <- data[, .SD[year == max(year)], by = indicator]
        } else data
        
        
        # filter by quint type (if inequalities dataset selected)
        if(input$dataset_selector == "Inequalities Dataset") {
          if(input$quint_type_selector == "Scotland") {
            data <- data |> filter(quint_type == "sc_quin")
          } else {
            data <- data |> filter(quint_type != "sc_quin")
          }
        } else data
        
        
        # if profile selected (but indicators have not been)
        # then filter by selected profiles only 
        if(isTruthy(input$profile_selector) & !isTruthy(input$indicator_selector)) {
          if("All Indicators" %in% input$profile_selector) {
            data } else {
          profile_short_names <- paste(map_chr(input$profile_selector, ~ pluck(profiles_list, .x, "short_name")), collapse = "|")
          
          data <- data |>
            filter(grepl(profile_short_names, profile_domain))
            }
          
          
          # if a profile has been selected (and some indicators too)
          # then filter by profile and indicator
        } else if(isTruthy(input$profile_selector) & isTruthy(input$indicator_selector)) {
          profile_short_names <- paste(map_chr(input$profile_selector, ~ pluck(profiles_list, .x, "short_name")), collapse = "|")
          
          data <- data |>
            filter(grepl(profile_short_names, profile_domain)) |>
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
        
        # columns to return if main dataset was selected
        if(input$dataset_selector == "Main Dataset") {
          
          data <- data |>
            select(area_code, area_type, area_name, year, period, type_definition,
                   indicator, numerator, measure, 
                   upper_confidence_interval, lower_confidence_interval) } else {
                     
                     # columns to return if inequalities dataset was selected
                     # note this requires some reshaping due to the format of the inequalities dataset
                     
                     # all inequalities data
                     data <- data |>
                       rename(value = measure,
                              measure = type_definition)
                     
                     # sii data
                     sii <- data |>
                       filter(quintile == "Total") |>
                       mutate(value = sii,
                              upper_confidence_interval = upci_sii,
                              lower_confidence_interval = lowci_sii) |>
                       mutate(measure = "Slope index of inequality (SII)",
                              quintile = NULL
                       )
                     
                     # rii data
                     rii <- data |>
                       filter(quintile == "Total") |>
                       mutate(value = rii,
                              upper_confidence_interval = upci_rii,
                              lower_confidence_interval = lowci_rii) |>
                       mutate(measure = "Relative index of inequality (RII)",
                              quintile = NULL)
                     
                     # par data
                     par <- data |>
                       filter(quintile == "Total") |>
                       mutate(value = par,
                              upper_confidence_interval = upci_rii_int,
                              lower_confidence_interval = lowci_rii_int) |>
                       mutate(measure = "Population attributable risk (PAR)",
                              quintile = NULL)
                     
                     # different inequalities measures combined
                     data <- bind_rows(data, rii, sii, par) |>
                       select(area_code, area_type, area_name, year, period, indicator, 
                              quintile, measure, value, upper_confidence_interval, 
                              lower_confidence_interval, label_inequality) |>
                       arrange(indicator, area_name, year)
                   }
        
                  # add data source column
                  techdoc <- techdoc |>
                    select(indicator_name, data_source)
                  
                  data <- data |>
                    left_join(techdoc, by = c("indicator" = "indicator_name"))
        
      })
      
      
    #####################################.
    # DYNAMIC FILTERS ----
    #####################################.
      
      
      # create geography filter using GeographyNodes() reactive object
      # which stores available geographies, depending on what dataset was selected
      output$geography_selector <- renderJstree({
        jstree(
          GeographyNodes(),
          checkboxes = TRUE,
          selectLeavesOnly = TRUE,
          theme = "proton"
        )
      })
      
      
      # update geography choices when required
      observe({
        jstreeUpdate(session, ns("geography_selector"), GeographyNodes())
      })
      
      
      # Update indicator filter choices based on dataset and geography selected
      # (and further updating if profile also selected)
      observe({
        
        # return selected geographies
        paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
        
        # filter selected dataset by selected geographies
        data <- selectedData() |>
          subset(geo_path %in% paths)
        
        # create vector of available indicators
        available_indicators <- unique(data$indicator)
        
        # Store the current selection of indicators (if there were any)
        # i.e. if you've switched dataset but had previously selected some indicators
        current_selected_indicators <- input$indicator_selector
        
        
        # Further filter indicators if a profile is selected
        if (!is.null(input$profile_selector) && input$profile_selector != "") {
          
          profile_filtered_data <- data |>
            filter(grepl(paste(input$profile_selector, collapse = "|"), profile_domain))
          
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
      
      
      # # update profile choices based on chosen dataset -----
      observe({

        available_profile_choices <- switch(input$dataset_selector,
                                            "Main Dataset" = names(profiles_list),
                                            "Inequalities Dataset" = names(Filter(function(x)  "simd_tab" %in% x$subtabs & x$active == TRUE, profiles_list)))

        updateVirtualSelect(session = session,
                            inputId = "profile_selector",
                            choices = available_profile_choices)
      })


      
      
      
      ## reset all filters when 'clear filters' button is clicked 
      observeEvent(input$clear_table_filters, {
        
        # reset the dataset selector to "Main Dataset"
        updateRadioButtons(session, 
                                inputId = "dataset_selector", 
                                selected = "Main Dataset")
        
        # reset the geographies to those available for the main dataset
        jstreeUpdate(session, "geography_selector", main_data_geo_nodes)
        
        
        # reset the time period filter to max year per indicator
        updateRadioButtons(session = session,
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
      
      
      
    ##############################.
    # DATA TABLE ----
    ##############################.
      
      output$data_tab_table <- renderDT({
        
        # columns to hide in table
        if(input$dataset_selector == "Main Dataset") {
          cols_to_display = list(list(visible=FALSE, targets=c(0,3, 9,10, 11)))
          
        } else {
          cols_to_display = list(list(visible=FALSE, targets=c(0,3,9,10,11, 12)))
        }
        
        
        
        # column names for table
        if(input$dataset_selector == "Main Dataset") {
          col_names = c("hidden", "Type", "Area", "hidden", "Period", "Measure Type", 
                        "Indicator", "Numerator", "Measure", "hidden", "hidden", "hidden")
          
        } else {
          col_names = c("hidden", "Type", "Area", "hidden", "Period", "Indicator", 
                        "Quintile", "Measure", "Value", "hidden", "hidden", "hidden", "hidden")
        }
        
        
        datatable(tableData(),
                  style = 'bootstrap', 
                  caption = sprintf('Total rows: %s', nrow(tableData())),
                  rownames = FALSE,
                  colnames = col_names,
                  options = list(scrollX = TRUE,
                                 scrollY = "600px", 
                                 pageLength = 20,
                                 searching = FALSE,
                                 language = list(
                                   zeroRecords = "Select atleast one geography to display results."),
                                 columnDefs = cols_to_display
                  ))
        
        
        
      })
      
      
            ##################################.
            # Downloads ----
            ##################################.

            # data table bulk download (note this is a module )
            # note: use filename argument once data downloads PR merged
            download_data_btns_server(id = "datatable_downloads", data = tableData, file_name="ScotPHO_datatab_extract")
      
      
      
    }
  )
}



######################################.
# EXAMPLE USAGE 
# uncomment the code below to see how this module works
# note this module requires the 'main_dataset' parquet file, the 'main_data_geo_nodes' file 
# and the 'profiles_list' object from the global script
# Therefore recommended that you first run the main app first so these 3 things become available in your global environment
######################################.

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