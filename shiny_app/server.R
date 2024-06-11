###############################################
#
# App main server script
#
##############################################



function(input, output, session) {
  
  # Keeps the shiny app from timing out quickly 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  
  # logic controlling which tabs are opened when buttons on homepage are selected -------------------
  # each line calls a module that will create a button with a unique ID and nav_id corresponding to a single profile
  # to add a new button to the app front page requires adding a new line below then also editing main ui script to specify
  # where the new buttons should appear relative to other buttons and what content to include on that tab 
  profile_homepage_btn_modSERVER("hwb_nav", nav_id = "HWB", parent_session = session)
  profile_homepage_btn_modSERVER("cyp_nav", nav_id = "CYP", parent_session = session)
  profile_homepage_btn_modSERVER("cwb_nav", nav_id = "CWB", parent_session = session)
  profile_homepage_btn_modSERVER("alc_nav", nav_id = "ALC", parent_session = session)
  profile_homepage_btn_modSERVER("men_nav", nav_id = "MEN", parent_session = session)
  profile_homepage_btn_modSERVER("pop_nav", nav_id = "POP", parent_session = session)
  profile_homepage_btn_modSERVER("tob_nav", nav_id = "TOB", parent_session = session)
  profile_homepage_btn_modSERVER("drg_nav", nav_id = "DRG", parent_session = session)
  profile_homepage_btn_modSERVER("all_nav", nav_id = "ALL", parent_session = session)
  
  
  
  # logic controlling opening of About ScotPHO, About ScotPHO and Explore Indicators pages from landing page
  navigation_button_modSERVER("about_us", nav_id="About ScotPHO", parent_session = session)
  navigation_button_modSERVER("about_profiles", nav_id="About Profiles", parent_session = session)
  navigation_button_modSERVER("explore_indicators", nav_id="Indicator Definitions", parent_session = session)
  navigation_button_modSERVER("indicator_schedule", nav_id="Indicator Definitions", parent_session = session)
  
  
  # logic controlling buttons within the "About Profiles" tab linking back to profile pages
  navigation_button_modSERVER("view_profile_HWB", nav_id="HWB", parent_session = session)
  navigation_button_modSERVER("view_profile_CYP", nav_id="CYP", parent_session = session)
  navigation_button_modSERVER("view_profile_CWB", nav_id="CWB", parent_session = session)
  navigation_button_modSERVER("view_profile_ALC", nav_id="ALC", parent_session = session)
  navigation_button_modSERVER("view_profile_MEN", nav_id="MEN", parent_session = session)
  navigation_button_modSERVER("view_profile_POP", nav_id="POP", parent_session = session)
  navigation_button_modSERVER("view_profile_DRG", nav_id="DRG", parent_session = session)
  navigation_button_modSERVER("view_profile_TOB", nav_id="TOB", parent_session = session)
  
  
  # logic controlling buttons on profile tabs linking to About Profiles tab
  navigation_button_modSERVER("about_hwb", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_cyp", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_cwb", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_alc", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_men", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_pop", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_tob", nav_id="about_accordion", parent_session = session)
  navigation_button_modSERVER("about_drg", nav_id="about_accordion", parent_session = session)
  
  
  # stores selected 'areaname' and selected 'areatype' which can be used throughout other modules
  # to filter data by geography, like this: geo_selections()$areaname 
  geo_selections <- global_geography_filters_server("geo_filters", geo_lookup)
  
  
  # reactive values to store info on selected profile 
  profile_name <- reactiveVal() # to store full name (i.e. Health and Wellbeing)
  profile <- reactiveVal() # to store abbreviated name (i.e. HWB)
  
  
  # when a user switches tab, update the 2 x reactive values created above 
  observeEvent(input$nav, {
    profile(input$nav) # update the object called 'profile' with the nav id (i.e. HWB)
    profile_name(profiles_list[[input$nav]]) # update the object called 'profile_name' with the long version of the name (i.e. Health and wellbeing)
  })
  
  
  # dynamic header showing selected profile 
  output$profile_header <- renderUI({
    tags$h1("Profile:", profile_name(), class = "profile-header")
  })
  
  
  # dynamic header showing selected areatype
  output$areatype_header <- renderUI({
    
    # don't show this section in the header if Scotland is selected
    # to avoid having both areatype: scotland and areaname: scotland in the header
    shiny::validate(
      need(geo_selections()$areatype != "Scotland", "")
    )
  
    # if scotland is not selected then show the areatype in the header
    tags$h2("Area type:", geo_selections()$areatype, class = "geography-header")
  })
  
  
  # dynamic header showing selected areaname
  output$areaname_header <- renderUI({
    tags$h2("Area name:", geo_selections()$areaname, class = "geography-header")
  })
  
  
  
  
  
  # data filtered by profile (input$nav stores info on the tab the user is on)
  # i.e. the alcohol tab has been assigned an id/value called 'ALC' (see ui script) so when a user selects the alcohol tab, the results of input$nav is 'ALC'
  # note: since not yet filtered by geography here, this can be used to pass to other modules to create
  profile_data <- reactive({
    # set to data.table
    dt <- setDT(main_dataset)
    
    # filter rows where profile abbreviation exists in one of the 3 profile_domain columns in the technical document 
    dt <- dt[substr(profile_domain1, 1, 3) == profile() |
               substr(profile_domain2, 1, 3) == profile() |
               substr(profile_domain3, 1, 3) == profile()]
    
    # create a domain column - this ensures we return the correct domain for the chosen profile in cases where an indicator
    # is assigned to more than one profile (and therefore more than one domain)
    dt <- dt[, domain := fifelse(substr(profile_domain1, 1, 3) == profile(), 
                                 substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
                                 fifelse(substr(profile_domain2, 1, 3) == profile(), 
                                         substr(profile_domain2, 5, nchar(as.vector(profile_domain2))),
                                         substr(profile_domain3, 5, nchar(as.vector(profile_domain3))))
    )]
    
    # Convert 'domain' to factor
    dt <- dt[, domain := as.factor(domain)]
    
    # Arrange by 'domain'
    dt <- setorder(dt, domain)
  })
  
  # create reactive dataset for all indicators to pass to module
  # note: this will need modified when we move to individual profile datasets
  # will need to combine the profile datasets and then remove any duplicates
  all_indicators_data <- reactive({
    main_dataset
  })

  
  # temporarily making simd data reactive 
  simd_data2 <- reactive({
    simd_dataset
  })
  
  # logic controlling summary tables
  # takes profile data and further filters by selected geography
  # prepares summary data and displays in a table with spinecharts
  summary_table_server("hwb_summary", geo_selections, profile_name, profile_data)
  summary_table_server("cyp_summary", geo_selections, profile_name, profile_data)
  summary_table_server("cwb_summary", geo_selections, profile_name, profile_data)
  summary_table_server("alc_summary", geo_selections, profile_name, profile_data)
  summary_table_server("drg_summary", geo_selections, profile_name, profile_data)
  summary_table_server("men_summary", geo_selections, profile_name, profile_data)
  summary_table_server("pop_summary", geo_selections, profile_name, profile_data)
  summary_table_server("tob_summary", geo_selections, profile_name, profile_data)
  
  
  # logic controlling trends tab
  # takes profile data and further filters by selected geography
  # displays trend chart based on selected indicator from drop down list
  trend_mod_server("hwb_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("cyp_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("cwb_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("alc_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("drg_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("men_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("pop_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("tob_trends", filtered_data = profile_data, geo_selections = geo_selections)
  trend_mod_server("all_trends", filtered_data = all_indicators_data, geo_selections = geo_selections)
  
  # logic controlling rank visualisations
  # # takes profile data and further filters by selected areatype
  rank_mod_server("hwb_rank", profile_data, geo_selections)
  rank_mod_server("cyp_rank", profile_data, geo_selections)
  rank_mod_server("cwb_rank", profile_data, geo_selections)
  rank_mod_server("alc_rank", profile_data, geo_selections)
  rank_mod_server("drg_rank", profile_data, geo_selections)
  rank_mod_server("men_rank", profile_data, geo_selections)
  rank_mod_server("pop_rank", profile_data, geo_selections)
  rank_mod_server("tob_rank", profile_data, geo_selections)
  rank_mod_server("all_rank", all_indicators_data, geo_selections)
  
  
  # logic controlling simd visualisations
  simd_navpanel_server("cwb_simd", simd_data2, geo_selections)
  simd_navpanel_server("hwb_simd", simd_data2, geo_selections)
  simd_navpanel_server("cyp_simd", simd_data2, geo_selections)
  simd_navpanel_server("pop_simd", simd_data2, geo_selections)
  
  # logic controlling population group dataset
  # makes dataset reactive so can be used by module - potentially remove when data prep is complete - could be renamed as it supplies data for 
  # population groups layout
  ineq_splits_temporary <- reactive({
    ineq_splits_data })  
  
  # logic controlling population groups visualisation
  # takes profile data and further filters by selected areatype
  pop_groups_server("cwb_pop_groups", dataset = ineq_splits_temporary, geo_selections = geo_selections)
  
  ###############################################
  #
  # Server logic for data tab
  #
  ###############################################

  # Update indicator filter choices ----
  # based on geography selected
  # (and further updating if profile also selected)
  observe({
    
    # return selected geographies
    paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
    
    # filter selected dataset by selected geographies
    data <- main_dataset |>
      subset(path %in% paths)
    
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
  
  
  
  ## reset all filters -----
  # when 'clear filters' button is clicked 
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
  
  

  
  
  # render the geography filter ----
  #using the dynamic choices from step above
  output$geography_selector <- renderJstree({
    jstree(
      main_data_geo_nodes,
      checkboxes = TRUE,
      selectLeavesOnly = TRUE,
      theme = "proton"
    )
  })
  

  
  
  # data to display/download ----
  tableData <- reactive({
    
    # selected dataset
    data <- main_dataset 
    
    # filter by selected geographies
    paths <- sapply(input$geography_selector_checked_paths, `[[`, "path")
    data <- data |> subset(path %in% paths)
    
    
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
  
  
  # table of results ---------
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
  
  
  
  
  # data table bulk download  -------
  download_data_btns_server(id = "datatable_downloads", data = tableData)
  
  


  
} # close main server function

##END
