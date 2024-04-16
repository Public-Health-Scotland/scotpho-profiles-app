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
  
  
  # reactive values to store geography selections
  selected_areatype <- reactiveVal()
  selected_areaname <- reactiveVal()
  
  
  # reactive value to store profile selected
  selected_profile <- reactiveVal()
  
  
  # update reactive value above when user switches tabs
  observeEvent(input$nav, {
    selected_profile(input$nav)

  })
  
  
  
  # data filtered by profile ONLY using reactive value above
  # note: since not yet filtered by geography here, this can be used to pass to other modules to create
  # summary views, rank views, trend views where other geography comparators may be required
  # i.e.  use this data (filtered by profile only) alongside the reactive values that store selected areaname and selected areatype
  profile_data <- reactive({
    main_dataset |>
        filter(substr(profile_domain1, 1, 3) == selected_profile() |
                 substr(profile_domain2, 1, 3) == selected_profile() |
                 substr(profile_domain3, 1, 3) == selected_profile()) |>
        mutate(domain = as.factor(case_when(
          substr(profile_domain1,1,3)== selected_profile() ~
            substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
          substr(profile_domain2,1,3)== selected_profile() ~
            substr(profile_domain2, 5, nchar(as.vector(profile_domain2))),
          TRUE ~ substr(profile_domain3, 5, nchar(as.vector(profile_domain3)))))) %>%
        arrange(domain)

  })

 
 

  
  # logic controlling the header at the top of each tab which contains details on the profile selected
  # and as dynamic text displaying the areatype and areaname selected
  # as well as an action link which when clicked on displays the areatype and areaname filters
  tab_header_mod_server("hwb_header", selected_areatype, selected_areaname)
  tab_header_mod_server("cyp_header", selected_areatype, selected_areaname)
  tab_header_mod_server("cwb_header", selected_areatype, selected_areaname)
  tab_header_mod_server("alc_header", selected_areatype, selected_areaname)
  tab_header_mod_server("men_header", selected_areatype, selected_areaname)
  tab_header_mod_server("pop_header", selected_areatype, selected_areaname)
  tab_header_mod_server("tob_header", selected_areatype, selected_areaname)
  tab_header_mod_server("drg_header", selected_areatype, selected_areaname)
  
  
  
  # logic controlling summary tabs for each profile
  # creates tables with spine charts embedded on each row
  summary_table_server("hwb_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("cyp_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("cwb_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("alc_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("men_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("pop_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("tob_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  summary_table_server("drg_summary", selected_areaname, selected_areatype, selected_profile, profile_data)
  
} # close main server function

##END