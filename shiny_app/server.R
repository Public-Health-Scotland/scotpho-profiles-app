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

  
  # logic controlling opening of About ScotPHO and Explore Indicators pages
  # from landing page. The logic for this is effectively the same as the above module.
  navigation_button_modSERVER("about_us", nav_id="about", parent_session = session)
  navigation_button_modSERVER("about_profiles", nav_id="profiles", parent_session = session)
  navigation_button_modSERVER("explore_indicators", nav_id="definitions", parent_session = session)
  
  
  
  # data filtered by profile (input$nav stores info on the tab the user is on)
  # i.e. the alcohol tab has been assigned an id/value called 'ALC' (see ui script) so when a user selects the alcohol tab, the results of input$nav is 'ALC'
  # note: since not yet filtered by geography here, this can be used to pass to other modules to create
  # summary views, rank views, trend views where other geography comparators may be required
  profile_data <- reactive({
    main_dataset |>
      # filter rows where profile abbreviation exists in one of the 3 profile_domain columns in the technical document 
      filter(substr(profile_domain1, 1, 3) == input$nav |
               substr(profile_domain2, 1, 3) == input$nav |
               substr(profile_domain3, 1, 3) == input$nav) |>
      # create a domain column - this ensures we return the correct domain for the chosen profile in cases where an indicator
      # is assigned to more than one profile (and therefore more than one domain)
      mutate(domain = as.factor(case_when(
        substr(profile_domain1,1,3)== input$nav ~
          substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
        substr(profile_domain2,1,3)== input$nav ~
          substr(profile_domain2, 5, nchar(as.vector(profile_domain2))),
        TRUE ~ substr(profile_domain3, 5, nchar(as.vector(profile_domain3)))))) |>
      arrange(domain)
  })
  


  
  # get the full profile name of the profile selected
  # (see profile_list in global script)
  # this is passed to tab_header_mod_server to display the selected profile in the tab header 
  profile_name_full <- reactive({
    profiles_list[[input$nav]]
  })
  
  
  # logic controlling the header at the top of each tab which contains details on the profile selected
  # and dynamic text displaying the areatype, areaname and profile selected
  # as well as an action link which when clicked on displays the areatype and areaname filters
  # also then returns the selected areaname and areatype which can be used within other modules
  # for example, you could pass the 'profile_data' dataframe to a module, alongside 'geo_selections' 
  # to further filter the profile data. To filter using these values within a module , you would do i.e. 
  # data <- reactive({
  # profile_data() |>
  # filter(areaname == geo_selections()$areaname & areatype == geo_selections()$areatype)
  # })
  
  geo_selections <- tab_header_mod_server("main_header", profile_name_full)
  
  
  # logic controlling summary tabs for each profile
  # creates tables with spine charts embedded on each row
  # geo_selections and profile_data are passed here to further filter profile data by selected geography
  # profile_name_full is used within the text that appears in the pop-up when a user clicks the help button
  summary_table_server("hwb_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("cyp_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("cwb_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("alc_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("men_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("pop_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("tob_summary", geo_selections, profile_name_full, profile_data)
  summary_table_server("drg_summary", geo_selections, profile_name_full, profile_data)
  
  
  
  
# rank data based on global rank filters (indicator, comparators)
rank_mod_server("hwb_rank", profile_data, geo_selections)
rank_mod_server("cyp_rank", profile_data, geo_selections)
rank_mod_server("cwb_rank", profile_data, geo_selections)
rank_mod_server("alc_rank", profile_data, geo_selections)
rank_mod_server("men_rank", profile_data, geo_selections)
rank_mod_server("pop_rank", profile_data, geo_selections)
rank_mod_server("tob_rank", profile_data, geo_selections)
rank_mod_server("drg_rank", profile_data, geo_selections)

  
} # close main server function

##END