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
  bg = phs_colours(colourname = "phs-purple"), # background navbar colour
  #theme = phs_theme, # dashboard theme - defined in global script
  tags$head(
    # required for spinecharts
    tags$script(src = "https://code.highcharts.com/highcharts.js"),
    # required for homepage styling
    includeCSS("www/styles.css")), # required to specify formatting (particularly of landing page)

  useShinyjs(), # need to declare this to enable geography filter to call on functions within shinyjs package  
  
  # homepage ---------------------------------------------------------------------
  nav_panel(value = "Home",
            title = "Home",
            htmlTemplate("landing-page.html", # sits in separate file in app folder
                         # buttons to navigate to about scotpho, about profiles and indicator definitions tabs
                         
                         additional_info_buttons = layout_columns(button_modUI(button_id="about_us", button_name = "About us", button_icon = icon("circle-info")),
                                                                      button_modUI(button_id = "about_profiles",button_name = "About the profiles", button_icon = icon("circle-info")),
                                                                      button_modUI(button_id="explore_indicators", button_name = "Explore indicators", button_icon = icon("magnifying-glass"))
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
              tab_header_mod_ui("hwb_header", profile_name = "Health and Wellbeing"),
              navset_tab(
                nav_panel(title = "Summary"),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # Children and young people 
    nav_panel(value = "CYP", 
              title = "Children & Young people", 
              tab_header_mod_ui("cyp_header", profile_name = "Children and Young People"),
              navset_tab(
                nav_panel(title = "Summary"),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # care and wellbeing 
    nav_panel(value = "CWB", 
              title = "Care & Wellbeing", 
              tab_header_mod_ui("cwb_header", profile_name = "Care and Wellbeing"),
              navset_tab(
                nav_panel(title = "Summary"),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # alcohol
    nav_panel(value = "ALC", 
              title = "Alcohol", 
              tab_header_mod_ui("alc_header", profile_name = "Alcohol"),
              navset_tab(
                nav_panel(title = "Summary"),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # drugs
    nav_panel(value = "DRG", 
              title = "Drugs", 
              tab_header_mod_ui("drg_header", profile_name = "Drugs"),
              navset_tab(
                nav_panel(title = "Summary"),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
    
    # mental health
    nav_panel(value = "MEN", 
              title= "Mental Health", 
              tab_header_mod_ui("men_header", profile_name = "Mental Health"),
              navset_tab(
                nav_panel(title = "Summary"),
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank"),
                nav_panel(title = "Inequalities"))),
              
    # population
    # mental health
    nav_panel(value = "POP", 
              "Population", 
              tab_header_mod_ui("pop_header", profile_name = "Population"),
              navset_tab(
                nav_panel(title = "Summary"),
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
    nav_panel(title = "About ScotPHO"),
    nav_panel(title = "About Profiles"),
    nav_panel(title = "Indicator Definitions"))

) #close main server function

### END
