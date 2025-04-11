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
  # below, there are various modules being called which control the server logic that determines what happens when a button is clicked:
  
  # this code runs the server function that matches the UI function that creates the profile buttons on the landing page
  # for any profiles that are 'active' (i.e. active = TRUE in the profiles_list in the global script)
  # it ensures when a user clicks a button, a user is taken to the profiles tab and the profiles filter
  # is updated to reflect the profile selected.
  lapply(active_profiles, function(profile) {
    profile_homepage_btn_modSERVER(id = profile, profile_name = profile, parent_session = session)
  })
  
  
  # these modules corresponding to the 2 buttons in the banner at the top of the landing page
  # they controll the opening of About ScotPHO, About ScotPHO and Explore Indicators pages
  navigation_button_modSERVER("about_us", nav_id="about_scotpho", parent_session = session)
  navigation_button_modSERVER("explore_indicators", nav_id="definitions", parent_session = session)
  
  
  # ensure that if a user directly clicks the 'Profiles'tab in the navigation bar, 'All indicators' is selected from the profile filter
  # This avoids a user being faced with a blank screen with no charts when they haven't used the profile buttons on the homepage to navigate
  observeEvent(input$nav, {
    req(input$profiles_choices == "")
    req(input$nav == "Profiles")
    updateSelectizeInput(inputId = "profile_choices", selected = "All Indicators", session = session)
  })
  
  
  # reactive object to store selected sub-tab
  selected_subtab <- reactiveVal(NULL)
  
  # update reactive object whenever a user changes sub-tab
  observeEvent(input$sub_tabs, {
    selected_subtab(input$sub_tabs)
  })
  
  # if a user switches profile and the tab they were on doesn't exist for the newly selected profile, then default to the
  # first available tab for that profile
  observeEvent(input$profile_choices, {
    if (selected_subtab() %in% pluck(profiles_list, input$profile_choices, "subtabs")) {
      nav_select(id = "sub_tabs", selected = selected_subtab())
    } else {
      nav_select(id = "sub_tabs", selected = pluck(profiles_list, input$profile_choices, "subtabs", 1))
    }
  }, ignoreInit = TRUE)
  
  
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
  

  # this object stores info about the selected profile by taking relevant bits of info
  # from the profiles_list in the global script. The object is used for various different things
  # 1. storing subtabs to be shown for the selected profile to determine which tabs should be hidden/shown
  # 2. storing a domain order for the selected profile. This is passed to each of the large visualisation modules
  # so that the indicator filter modules which are nested inside these larger modules can access it. This ensures
  # the indicators are ordered correctly by domain in the indicator filters.
  # 3. storing full and short name of the selected profile ( Currently, this particular info is only being used in the 
  # trend module to ensure that police divisions only appear as a filter if mental health is selected)
  selected_profile <- reactive({
    list(
      full_name = input$profile_choices, 
      short_name = pluck(profiles_list, input$profile_choices, "short_name"),
      domain_order = pluck(profiles_list, input$profile_choices, "domain_order"),
      subtabs = pluck(profiles_list, input$profile_choices, "subtabs")
    )
  })

  
  
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
  
  # conditionally show/hide subtabs depending on what profile was selected
  # whenever a user selects a profile, this code goes through each of the possible subtabs (stored in a vector called 'all_subtabs')
  # and checks it against the list of subtabs assigned to each profile in the profiles list
  # for example, if "All indicators" has been selected, th
  observeEvent(input$profile_choices, {
    walk(all_subtabs, function(subtab) {
      if (subtab %in% pluck(profiles_list, input$profile_choices, "subtabs")) {
        nav_show(id = "sub_tabs", target = subtab)
      } else {
        nav_hide(id = "sub_tabs", target = subtab)
      }
    })
  }, ignoreInit = TRUE)
  
  
  # running modules for each sub-tab
   trend_mod_server("trends", profile_data, geo_selections, selected_profile, session)
   rank_mod_server("rank", areatype_data, geo_selections, selected_profile, session)
   summary_table_server("summary", geo_selections, selected_profile, areatype_data)
   simd_navpanel_server("simd", simd_data, geo_selections, selected_profile, session)
   pop_groups_server("pop_groups",popgroup_data, geo_selections, selected_profile, session)
  
  
  
  # # ############################################.
  # # # MODULES FOR THE ADDITIONAL INFO TABS ----
  # # ############################################.
  
  # indicator definitions tab server logic
  definitions_tab_Server("metadata")
  
  # data tab server logic
  # note: temporarily wrapped in observe event so it doesn't run when app launches
  # as it is slow - code to be revisited and amended
  observeEvent(input$nav, {
    req(input$nav == "dt")
    data_tab_mod_Server("data_tab")
  })
  
  
  
  ###################################################.
  # REACTIVE DATASETS ----
  ###################################################.
  
  # 1. MAIN DATASET FILTERED BY PROFILE
  # searches for the abbreviated name of the selected profile across the profile_domain column
  # if 'all indicators' is selected then entire dataset is used instead
  # this dataset is passed to the trends tab module
  # it's also used to create a smaller dataset which further filters by selected areatype
  profile_data <- reactive({
    req(input$profile_choices != "")
    if(input$profile_choices == "All Indicators") {
      main_dataset
    } else {
      prepare_profile_data( #function from global script
        dataset = main_dataset,
        selected_profile = input$profile_choices
      )
    }
  }) |>
    # cache the filtered dataset
    # this results in improved performance if there's more than 1 user looking at the same profile 
    bindCache(input$profile_choices)  
  
  
  
  # 2. MAIN DATASET FILTERED BY BOTH PROFILE AND AREATYPE
  # used for rank/summary tabs 
  areatype_data <- reactive({
    req(profile_data())
    profile_data() |>
      filter(areatype == geo_selections()$areatype | areatype == "Scotland")
  })
  
  
  
  
  # 3. DEPRIVATION DATASET
  # filters the deprivation dataset by selected profile, filtered data then passed to the depriavtion visualisation module 
  simd_data <- reactive({
    req(input$profile_choices != "")
    if(input$profile_choices == "All Indicators"){
      simd_dataset |>
        filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname)
    } else {
    prepare_profile_data(
      dataset = simd_dataset,
      selected_profile = input$profile_choices,
      selected_areatype = geo_selections()$areatype,
      selected_areaname = geo_selections()$areaname
    )
    }
  })
  
  
  
  # 4. POPULATION GROUPS DATASET
  # filters the population groups dataset by selected profile, filtered data then passed to the pop group visualisation module 
  popgroup_data <- reactive({
    req(input$profile_choices != "")
    prepare_profile_data(
      dataset = popgroup_dataset,
      selected_profile = input$profile_choices,
      selected_areatype = geo_selections()$areatype,
      selected_areaname = geo_selections()$areaname
    )
  })


  
} # close main server function

##END
