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
  
  
  # hide header with profile name/geographies on non-profile tabs
  observe({
    if(input$nav %in% c("Home", "dt", "ind_def", "about_scotpho", "about_profiles", "definitions")){
      shinyjs::hide("header")
    } else {
      shinyjs::show("header")
    }
  })
  
  
  ##############################################################################
  # NAVIGATION TO DIFFERENT TABS
  #############################################################################
  
  
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
  navigation_button_modSERVER("about_us", nav_id="about_scotpho", parent_session = session)
  navigation_button_modSERVER("about_profiles", nav_id="about_profiles", parent_session = session)
  navigation_button_modSERVER("explore_indicators", nav_id="definitions", parent_session = session)

  # logic controlling about profiles button in profile header
  navigation_button_modSERVER("about_profiles_header", nav_id = "About Profiles", parent_session = session)
  
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
  
  
  
  ##########################################
  # REACTIVE VALUES
  ##########################################
  
  # stores selected 'areaname', selected 'areatype' and 'parent area' (if applicable)
  # this object is passed to other modules to display indicators specific to the selected geography
  geo_selections <- global_geography_filters_server("geo_filters", geo_lookup)
  

  # stores selected profile name
  # this object is passed to the summary module
  # to create the header for the PDF download
  profile_name <- eventReactive(input$nav, {
    profiles_list[[input$nav]]
  })
  
  
  
  #########################################
  # DYNAMIC TEXT 
  #########################################
  
  # header showing selected profile 
  output$profile_header <- renderUI({
    tags$h1("Profile:", profiles_list[[input$nav]], class = "profile-header")
  })
  
  
  # header showing selected areatype
  output$areatype_header <- renderUI({
    
    # don't show this section in the header if Scotland is selected
    # to avoid having both areatype: scotland and areaname: scotland in the header
    shiny::validate(
      need(geo_selections()$areatype != "Scotland", "")
    )
  
    # if scotland is not selected then show the areatype in the header
    tags$h2("Area type:", geo_selections()$areatype, class = "geography-header")
  })
  
  
  # header showing selected areaname
  output$areaname_header <- renderUI({
    tags$h2("Area name:", geo_selections()$areaname, class = "geography-header")
  })
  
  
  
  
  ###################################################
  # REACTIVE DATA 
  ###################################################
  
  
  # data filtered by profile (input$nav stores info on the tab the user is on)
  # i.e. the alcohol tab has been assigned an id/value called 'ALC' (see ui script) so when a user selects the alcohol tab, the results of input$nav is 'ALC'
  # this dataframe is passed to modules that create the visuals for 'Summary', 'Rank' and 'Trend' subtabs
  profile_data <- reactive({

    req(input$nav %in% c("HWB", "CWB", "POP", "MEN", "ALC", "DRG", "CYP", "TOB", "ALL")) # only run when a profile has been selected
    
    if(input$nav == "ALL") {
      main_dataset
      
    } else {
      
      dt <- setDT(main_dataset)
      

    # filter rows where profile abbreviation exists in one of the 3 profile_domain columns in the technical document
    dt <- dt[substr(profile_domain1, 1, 3) == input$nav |
               substr(profile_domain2, 1, 3) == input$nav |
               substr(profile_domain3, 1, 3) == input$nav]

    # create a domain column - this ensures we return the correct domain for the chosen profile in cases where an indicator
    # is assigned to more than one profile (and therefore more than one domain)
    dt <- dt[, domain := fifelse(substr(profile_domain1, 1, 3) == input$nav,
                                 substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
                                 fifelse(substr(profile_domain2, 1, 3) == input$nav,
                                         substr(profile_domain2, 5, nchar(as.vector(profile_domain2))),
                                         substr(profile_domain3, 5, nchar(as.vector(profile_domain3))))
    )]
    
    }

  })
  

  
 
  
  
  # logic controlling population group dataset
  # makes dataset reactive so can be used by module - potentially remove when data prep is complete - could be renamed as it supplies data for
  # population groups layout
  ineq_splits_temporary <- reactive({
    ineq_splits_data })





########################################################
# SERVER LOGIC FOR DIFFERENT TABS
# i.e Summary, rank, trends, deprivation, population groups, indicator definitions, data table
########################################################
  

  # run modules for sub-tabs on each profile tab
  # ensuring each profiles sub-tab modules are only run when the user is on that profiles tab
  observe({
    
    req(profile_data()) # don't run until profile data is ready 

    # Health and wellbeing 
    if(input$nav == "HWB") {
    summary_table_server("hwb_summary", geo_selections, profile_name, profile_data) # summary sub-tab
    rank_mod_server("hwb_rank", profile_data, geo_selections) # rank sub-tab
    trend_mod_server("hwb_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends sub-tab
    simd_navpanel_server("hwb_simd", simd_data, geo_selections) # deprivation sub-tab

    # Care and wellbeing 
    } else if(input$nav == "CWB") {
      summary_table_server("cwb_summary", geo_selections, profile_name, profile_data, domain_order = c("Over-arching indicators", "Early years", "Healthy places", "Impact of ill health prevention")) # summary sub-tab
      rank_mod_server("cwb_rank", profile_data, geo_selections) # rank sub-tab
      trend_mod_server("cwb_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends sub-tab
      simd_navpanel_server("cwb_simd", simd_data, geo_selections) # deprivation sub-tab
      pop_groups_server("cwb_pop_groups", dataset = ineq_splits_temporary, geo_selections = geo_selections) # population groups sub-tab
      
      
    # Children and young people
    } else if(input$nav == "CYP"){
      summary_table_server("cyp_summary", geo_selections, profile_name, profile_data) # summary sub-tab
      rank_mod_server("cyp_rank", profile_data, geo_selections) # rank sub-tab
      trend_mod_server("cyp_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends sub-tab
      
    
    # Tobacco
    } else if(input$nav == "TOB"){
      summary_table_server("tob_summary", geo_selections, profile_name, profile_data) # summary sub-tab
      rank_mod_server("tob_rank", profile_data, geo_selections) # rank sub-tab
      trend_mod_server("tob_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends sub-tab
    
    
    # Alcohol
    } else if(input$nav == "ALC") {
      summary_table_server("alc_summary", geo_selections, profile_name, profile_data) # summary sub-tab
      rank_mod_server("alc_rank", profile_data, geo_selections) # rank sub tab 
      trend_mod_server("alc_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends sub tab 
    
    # Drugs 
    } else if(input$nav == "DRG"){
      summary_table_server("drg_summary", geo_selections, profile_name, profile_data) # summary subtab
      rank_mod_server("drg_rank", profile_data, geo_selections) # rank subtab
      trend_mod_server("drg_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends sub-tab
     
    # Mental Health 
    } else if(input$nav == "MEN"){
      summary_table_server("men_summary", geo_selections, profile_name, profile_data) # summary subtab
      rank_mod_server("men_rank", profile_data, geo_selections) # rank subtab
      trend_mod_server("men_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends subtab
      
    # Population
    } else if(input$nav == "POP"){
      summary_table_server("pop_summary", geo_selections, profile_name, profile_data) # summary subtab
      rank_mod_server("pop_rank", profile_data, geo_selections) # rank subtab
      trend_mod_server("pop_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends subtab
      simd_navpanel_server("pop_simd", simd_data, geo_selections) # deprivation subtab
      
    # All indicators
    } else if(input$nav == "ALL"){
      rank_mod_server("all_rank", profile_data, geo_selections) # rank subtab
      trend_mod_server("all_trends", filtered_data = profile_data, geo_selections = geo_selections, techdoc) # trends subtab
    }

  })
  


  # server logic for the 'more information' tabs
  definitions_tab_Server("metadata") # indicator definition tab
  data_tab_mod_Server("data_tab") # data tab

  
} # close main server function

##END
