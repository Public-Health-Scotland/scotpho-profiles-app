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
  


  
  
} # close main server function

##END