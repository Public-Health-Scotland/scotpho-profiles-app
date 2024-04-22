###############################################
#
# App main ui script
#
##############################################


page_navbar(
  fillable = FALSE, # controlling how items grow/shrink when browser different sizes
  window_title = "ScotPHO profiles",
  id = "nav", # id required for profile buttons - works with profile_homepage_btn_mod to control navigation
  collapsible = TRUE, # collapse tabs on smaller screens
  lang = "en",
  useShinyjs(), # need to declare this to enable geography filter to call on functions within shinyjs package 
  # display a header with info on selected profile/geography + geography filters (only when not on homepage)
  header = conditionalPanel(condition = "input.nav !== 'Home' && input.nav !== 'profiles' && input.nav !== 'definitions' && input.nav !== 'about'", tab_header_mod_ui(id = "main_header")), 
  bg = phs_colours(colourname = "phs-purple"), # background navbar colour
  theme = phs_theme, # dashboard theme - defined in global script
  tags$head(
    # required for spinecharts
    tags$script(src = "https://code.highcharts.com/highcharts.js"),
    # required for homepage styling
    includeCSS("www/styles.css")), # required to specify formatting (particularly of landing page)
  
  # homepage ---------------------------------------------------------------------
  nav_panel(value = "Home",
            title = "Home",
            htmlTemplate("landing-page.html", # sits in separate file in app folder
                         
                         # buttons to navigate to about scotpho, about profiles and indicator definitions tabs
                         additional_info_buttons = layout_columns(navigation_button_modUI(button_id="about_us", button_name = "About us", button_icon = icon("circle-info")),
                                                                  navigation_button_modUI(button_id = "about_profiles",button_name = "About the profiles", button_icon = icon("circle-info")),
                                                                  navigation_button_modUI(button_id="explore_indicators", button_name = "Explore indicators", button_icon = icon("magnifying-glass"))
                         ),
                         # buttons to navigate to profile tabs
                         profile_buttons = tagList(
                           layout_column_wrap(
                             profile_homepage_btn_modUI(id = "hwb_nav", profile_name = "Health and Wellbeing", profile_icon = "line-chart"),   
                             profile_homepage_btn_modUI(id = "cyp_nav", profile_name = "Children and Young People", profile_icon = "line-chart"),
                             profile_homepage_btn_modUI(id = "cwb_nav", profile_name = "Care and Wellbeing", profile_icon = "line-chart"),
                             profile_homepage_btn_modUI(id = "alc_nav", profile_name = "Alcohol", profile_icon = "line-chart")
                           ),
                           layout_column_wrap(
                             profile_homepage_btn_modUI(id = "drg_nav", profile_name = "Drugs", profile_icon = "line-chart"),
                             profile_homepage_btn_modUI(id = "men_nav", profile_name = "Mental Health", profile_icon = "line-chart"),
                             profile_homepage_btn_modUI(id = "pop_nav", profile_name = "Population", profile_icon = "line-chart"),
                             profile_homepage_btn_modUI(id = "tob_nav", profile_name = "Tobacco", profile_icon = "line-chart")
                           )
                         )
            )
  ),
  # drop-down menu containing profile tabs
  navbarMenu(
    title = "Change profile",
    
    # Health and wellbeing
    nav_panel(value = "HWB", 
              title = "Health & Wellbeing", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("hwb_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # Children and young people 
    nav_panel(value = "CYP", 
              title = "Children & Young people", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("cyp_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # care and wellbeing 
    nav_panel(value = "CWB", 
              title = "Care & Wellbeing", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("cwb_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # alcohol
    nav_panel(value = "ALC", 
              title = "Alcohol", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("alc_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # drugs
    nav_panel(value = "DRG", 
              title = "Drugs", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("drg_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # mental health
    nav_panel(value = "MEN", 
              title= "Mental Health", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("men_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # population
    # mental health
    nav_panel(value = "POP", 
              "Population", 
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("pop_summary")),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities")))
  ), #close nav menu
  
  nav_spacer(),
  
  # data tab -------------------------------------------------------------------
  nav_panel("Download data"),
  
  # source code link -------------------------------------------------------------------
  nav_item(tags$a(icon("github"), "SourceCode", href = "https://github.com/Public-Health-Scotland/scotpho-profiles-tool/tree/master/shiny_app", target = "_blank")),
  
  # other tabs -----------------------------------------------------------------
  nav_menu(
    title = "More information",
    nav_panel(title = "About ScotPHO", value = "about"),
    nav_panel(title = "About Profiles", value = "profiles"),
    nav_panel(title = "Indicator definitions", value = "definitions")
    )
  
) #close main ui

### END


