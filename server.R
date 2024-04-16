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
  # from landing page 
  button_modSERVER("about_us", nav_id="About ScotPHO", parent_session = session)
  button_modSERVER("about_profiles", nav_id="About Profiles", parent_session = session)
  button_modSERVER("explore_indicators", nav_id="Indicator Definitions", parent_session = session)
  
  
  # reactive values to store geography selections
  selected_areatype <- reactiveVal()
  selected_areaname <- reactiveVal()
  
  
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
  
  
  
  
} # close main server function

##END