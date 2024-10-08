###############################################.
#
# App main server script
#
##############################################.



function(input, output, session) {
  
  # Keeps the shiny app from timing out quickly on Posit 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  

  
  
  ###################################################.
  # NAVIGATION TO DIFFERENT TABS ----
  ###################################################.
  
  # throughout the app, there are various buttons included to help users navigate their way through the dashboard to various different tabs
  # all these buttons are created using modules. You'll find many of these modules included in the main UI script or nested within larger modules
  # below, there are various modules being called which control the server logic that determins what happens when a button is clicked:
  
  
  # these modules correspond to the profile buttons on the homepage
  # each line calls a module that will navigate to the profiles tab and update the profile filter to match the button clicked
  profile_homepage_btn_modSERVER("hwb_nav", profile_name = "Health and Wellbeing", parent_session = session)
  profile_homepage_btn_modSERVER("cyp_nav", profile_name = "Children and Young People",  parent_session = session)
  profile_homepage_btn_modSERVER("cwb_nav", profile_name = "Care and Wellbeing",  parent_session = session)
  profile_homepage_btn_modSERVER("alc_nav", profile_name = "Alcohol", parent_session = session)
  profile_homepage_btn_modSERVER("men_nav", profile_name = "Mental Health", parent_session = session)
  profile_homepage_btn_modSERVER("pop_nav", profile_name = "Population", parent_session = session)
  profile_homepage_btn_modSERVER("tob_nav", profile_name = "Tobacco",  parent_session = session)
  profile_homepage_btn_modSERVER("drg_nav", profile_name = "Drugs",  parent_session = session)
  profile_homepage_btn_modSERVER("all_nav", profile_name = "All Indicators",  parent_session = session)
  
  
  # these modules corresponding to the 3 buttons in the banner at the top of the landing page
  # they controll the opening of About ScotPHO, About ScotPHO and Explore Indicators pages
  navigation_button_modSERVER("about_us", nav_id="about_scotpho", parent_session = session)
  navigation_button_modSERVER("about_profiles", nav_id="about_profiles", parent_session = session)
  navigation_button_modSERVER("explore_indicators", nav_id="definitions", parent_session = session)

  
  # ensure that if a user directly clicks the 'Profiles'tab in the navigation bar, 'All indicators' is selected from the profile filter
  # This avoids a user being faced with a blank screen with no charts when they haven't used the profile buttons on the homepage to navigate
  observeEvent(input$nav, {
    req(input$profile_choices == "")
    req(input$nav == "Profiles")
    updateSelectizeInput(inputId = "profile_choices", selected = "All Indicators", session = session)
    
  })

  
  #####################################################.
  # REACTIVE VALUES ----
  #####################################################.
  
  # create an object to store selected 'areaname', 'areatype' and 'parent_area' from the geography filters
  # note: when the app is initially launched, these values are set to "Scotland" because Scotland
  # until a user chooses to change this and explore a local area
  geo_selections <- reactiveVal(
    list(
      areatype = "Scotland",
      areaname = "Scotland",
      parent_area = NA
    )
  )
  
  
  # When a user clicks the 'Apply geography filters' button, the object called geo_selections created above is updated to store what the user has selected
  # this reactive object called geo_selections() is used throughout the rest of the app to determine what indicators are available for a user to explore
  # depending on what profile and geography the user has selected. This is because not all indicators are available at every single geography level
  # For instance, there are far more indicators available at Health board level than there are Intermediate zone level, so we need to know what
  # geography users have selected to control what they can/can't see 
  observeEvent(input$apply_geo_filters, {
    geo_selections(
      list(
        areatype = input$areatype, # selected areatype 
        areaname = input$areaname, # selected areaname 
        parent_area = input$parent_area # selected parent area (only applicable if HSC locality/Intermediate zone is selected)
      )
    )
  })
  
  # store name of selected profile - this is passed to each module to check if the selected profile
  # needs it's domains to be ordered in a particular way 
  # (i.e. in the summary table, or within the indicator filter itself across the other sub-tabs)
  selected_profile <- reactive({input$profile_choices})

  
  ###########################################################################################.
  # CONDITIONAL UI FOR THE HEADER OF THE PROFILES TAB ----
  ###########################################################################################.
  
  
  # these 2 bits of dynamic text form the part of the two headers at the top
  # of the profiles tab that state the selected profile and the selected geography
  
  # 1. header showing selected profile
  output$profile_header <- renderUI({
    tags$h1("Profile:", input$profile_choices, class = "profile-header")
  })
  
  
  # 2. header showing selected areatype and areaname
  output$geography_header <- renderUI({
    
    if(geo_selections()$areatype == "Scotland") {
      tags$h2("Scotland", class = "geography-header") 
    } else {
      tags$h2(geo_selections()$areatype, ": ", geo_selections()$areaname, class = "geography-header")
    }
  })
  

  # update choices for areaname filter based on areatype selection
  observeEvent(c(input$areatype, input$parent_area), {
    if (input$areatype %in% c("HSC locality", "Intermediate zone") && !is.null(input$parent_area)) {
      
      area_choices <- geo_lookup[areatype == input$areatype & parent_area == input$parent_area]$areaname
    } else {
      area_choices <- geo_lookup[areatype == input$areatype]$areaname
    }
    updateSelectizeInput(session, "areaname", choices = sort(area_choices))
  }, ignoreInit = TRUE)
  
  
  
  # when a user clicks the 'change profile' filter, toggle the hidden profile filter to make it visible
  observeEvent(input$show_profile_filter, {
    toggle("prof_filter_hidden")
  })
  
  # once a user has updated their choice in the profile filter, hide the filter again 
  observeEvent(input$profile_choices, {
    hide("prof_filter_hidden")
  })
  
  
  # when a user clicks the 'change area' filter, toggle the hidden geography filters to make them visible
  observeEvent(input$show_geo_filters, {
    toggle("geo_filters_hidden")
  })
  
  
  # once a user has clicked the button to updated their geography choices, hide the filter again 
  observeEvent(input$apply_geo_filters, {
    hide("geo_filters_hidden")
  })
  
  
  
  
  
  
  ###############################################################.
  # DETERMINING WHICH SUB-TABS TO SHOW/HIDE ON THE PROFILES TAB ----
  ###############################################################.
  
  # run the module containing server logic for the trends tab - this is visible for every single profile
  trend_mod_server("trends", profile_data, geo_selections, selected_profile)


  # run the module containing server logic for the  rank tab - this is visible for every single profile
  rank_mod_server("rank", areatype_data, geo_selections, selected_profile)


  #run the module containing the server logic for the  deprivation tab ONLY when specific profiles are selected, otherwise hide the tab
  observe({
    req(input$profile_choices != "")
    if (profiles_list[[input$profile_choices]] %in% c("CWB", "HWB", "CYP", "MEN", "ALC", "DRG", "TOB") & !is.null(profiles_list[[input$profile_choices]])) {
      nav_show("sub_tabs", target = "simd_tab")
      simd_navpanel_server("simd", simd_data, geo_selections, selected_profile)

    } else {
      nav_hide("sub_tabs", target = "simd_tab")
    }
  })


  # run the module that creates the server logic for the population groups tab ONLY when specific profiles are selected, otherwise hide the tab
  observe({
    req(input$profile_choices != "")
    if (profiles_list[[input$profile_choices]] %in% c("CWB", "MEN") & !is.null(profiles_list[[input$profile_choices]])) {
      nav_show("sub_tabs", target = "pop_groups_tab")
      pop_groups_server("pop_groups",popgroup_data, geo_selections, selected_profile)
    } else {
      nav_hide("sub_tabs", target = "pop_groups_tab")
    }
  })


  
  # always default to the summary tab when user switches profile (unless all indicators has been selected as there is no summary for this - default to trends tab instead)
  # this ensures if a user is on a profile that contains a custom tab that is not available for other profiles
  # and then switches profile, they won't be faced with a blank screen
  observe({
    req(input$profile_choices != "") # ensure a profile has been selected
    if(input$profile_choices != "All Indicators"){
    nav_select(id = "sub_tabs",selected = "summary_tab", session = session)
    } else {
      nav_select(id = "sub_tabs",selected = "trends_tab", session = session)
    }
  })



  # run the module that creates the server logic for the summary tab, unless 'all indicators' is selected as the profile, in which case hide the tab
  observe({
    req(input$profile_choices != "")

    if (input$profile_choices == "All Indicators"){ # if all indicators selected then hide the summary view
      nav_hide("sub_tabs", target = "summary_tab")

    } else {
      nav_show("sub_tabs", target = "summary_tab")
      summary_table_server("summary", geo_selections, selected_profile, areatype_data)
    }

  })


  # only show the 'About profile' sub-tab when 1 of 5 particular profiles has been selected
  # otherwise hide it (as we don't yet have the text created for some profiles)
  observeEvent(input$profile_choices, {
    req(input$profile_choices != "")
    if(profiles_list[[input$profile_choices]] %in% c("CWB", "MEN", "CYP", "ALC", "HWB")){
      nav_show("sub_tabs", target = "about_profile_tab")
    } else {
      nav_hide("sub_tabs", target = "about_profile_tab")
    }
  })


  # # ############################################.
  # # # MODULES FOR THE ADDITIONAL INFO TABS ----
  # # ############################################.

  # indicator definitions tab server logic
  definitions_tab_Server("metadata")

  # data tab server logic
  observeEvent(input$nav, {
    req(input$nav == "dt")
  data_tab_mod_Server("data_tab")
  })


  
  ###################################################.
  # REACTIVE DATASETS ----
  ###################################################.

  # 1. MAIN DATASET FILTERED BY PROFILE
  # searches for the abbreviated name of the selected profile across the 3 profile columns in the main dataset
  # note: there are 3 profile columns because some indicators belong to more than 1 profile 
  # if 'all indicators' is selected then entire dataset is used instead
  # this dataset is passed to the trends tab module
  # it's also used to create a smaller dataset which further filters by selected areatype
  profile_data <- reactive({

    if(input$profile_choices == "All Indicators") {
    main_dataset
    } else {
      prepare_profile_data( #function from global script
        dataset = main_dataset,
        selected_profile = input$profile_choices
      )
    }
  })
  
  
  
  # 2. MAIN DATASET FILTERED BY BOTH PROFILE AND AREATYPE
  # used for rank/deprivation/summary tabs 
  areatype_data <- reactive({
    req(profile_data())
    profile_data() |>
      filter(areatype == geo_selections()$areatype | areatype == "Scotland")
  })
  
  


  # 3. DEPRIVATION DATASET
  # filters the deprivation dataset by selected profile, filtered data then passed to the depriavtion visualisation module 
  simd_data <- reactive({
      prepare_profile_data(
      dataset = simd_dataset,
      selected_profile = input$profile_choices,
      selected_areatype = geo_selections()$areatype,
      selected_areaname = geo_selections()$areaname
    )
  })



  # 4. POPULATION GROUPS DATASET
  # filters the population groups dataset by selected profile, filtered data then passed to the pop group visualisation module 
  popgroup_data <- reactive({

     prepare_profile_data(
      dataset = popgroup_dataset,
      selected_profile = input$profile_choices,
      selected_areatype = geo_selections()$areatype,
      selected_areaname = geo_selections()$areaname
    )
  })
  
  
  


} # close main server function

##END
