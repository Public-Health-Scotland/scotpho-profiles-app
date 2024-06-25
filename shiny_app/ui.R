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
  theme = phs_theme, # dashboard theme - defined in global script
  tags$head(
    # required for spinecharts
    tags$script(src = "https://code.highcharts.com/highcharts.js"),
    #required for saving leaflet map as png (see this for more info: https://stackoverflow.com/questions/47343316/shiny-leaflet-easyprint-plugin)
    tags$script(src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"),
    # required for homepage styling
    includeCSS("www/styles.css")), # required to specify formatting (particularly of landing page)

  useShinyjs(), # need to declare this to enable geography filter to call on functions within shinyjs package
  
  # custom js function to close the nav menu in the nav bar 1000 millisecs after any navigation buttons are clicked
  shinyjs::extendShinyjs(text = " shinyjs.closeNavMenu = function() {
        setTimeout(function() {$('.dropdown-menu').removeClass('show');}, 1000);}", functions = c("closeNavMenu")),

  # header that appears across the top of each profile tab
  # including geography filters and info detailing selected geography and profile
  header = conditionalPanel(condition = "input.nav !== 'Home' && input.nav !== 'About Profiles' && input.nav !== 'Indicator Definitions' && input.nav !== 'About ScotPHO' && input.nav !== 'dt'",
                            tagList(
                              uiOutput("profile_header"),
                              uiOutput("areatype_header"),
                              layout_columns(
                                col_widths = c(8,-1,2,-1),
                                uiOutput("areaname_header"),
                                navigation_button_modUI(button_id="about_profiles_header", button_name = "About this profile", button_icon = icon("circle-info"))), 
                              global_geography_filters_ui(id = "geo_filters", areatype_choices = areatype_list, parent_area_choices = hscp_list)
                            )),
  

  # homepage ---------------------------------------------------------------------
  nav_panel(value = "Home", style = "margin: 0;", # remove margin so no white space at top of landing page
            title = "Home",
            htmlTemplate("landing-page.html", # sits in separate file in app folder
                         # buttons to navigate to about scotpho, about profiles and indicator definitions tabs
                         additional_info_buttons = layout_columns(navigation_button_modUI(button_id="about_us", button_name = "About us", button_icon = icon("circle-info"), class = "btn-hero"),
                                                                      navigation_button_modUI(button_id = "about_profiles",button_name = "About the profiles", button_icon = icon("circle-info"), class = "btn-hero"),
                                                                      navigation_button_modUI(button_id="explore_indicators", button_name = "Explore indicators", button_icon = icon("magnifying-glass"), class = "btn-hero")
                         ),

                         # buttons to navigate to profile tabs
                         profile_buttons = tagList(
                           layout_columns(
                             profile_homepage_btn_modUI(id = "hwb_nav", profile_name = "Health and Wellbeing", description = markdown("View indicators relating to **Behaviours**, **Crime**, **Economy**, **Life expectancy** and **Mortality, ill health and injury**.")),
                             profile_homepage_btn_modUI(id = "cwb_nav", profile_name = "Care and Wellbeing", description = markdown("View indicators relating to **Population health and wider determinants** (part of the Scottish Government's Care and Wellbeing Portfolio).")),
                             profile_homepage_btn_modUI(id = "tob_nav", profile_name = "Tobacco control", description = markdown("View indicators relating to **Adult prevalence**, **Smoking during and post pregnancy**, **Smoking attributable deaths and diseases** and **Smoking cessation and services.**")),
                             
                           ),
                           layout_columns(
                             profile_homepage_btn_modUI(id = "alc_nav", profile_name = "Alcohol", description = markdown("View indicators relating to **Community safety**, **Environment**, **Health**, **Prevalence** and **Services**.")),
                             profile_homepage_btn_modUI(id = "drg_nav", profile_name = "Drugs", description = markdown("View indicators relating to **Community safety**, **Environment**, **Health**, **Prevalence** and **Services**.")),
                             profile_homepage_btn_modUI(id = "men_nav", profile_name = "Mental Health", description = markdown("View indicators relating to **adult mental health**, for both males and females."))
                           ),
                           
                           layout_columns(
                             profile_homepage_btn_modUI(id = "cyp_nav", profile_name = "Children and Young People", description = markdown("View indicators relating to **Active**, **Healthy**, **Achieving**, **Safe** and **Nurtured**.")),
                             profile_homepage_btn_modUI(id = "pop_nav", profile_name = "Population", description = markdown("View **population estimates** for different age groups.")),
                             profile_homepage_btn_modUI(id = "all_nav", profile_name = "All Indicators", description = markdown("View **all indicators** in this tool from across every profile."))
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
                nav_panel(title = "Trends", trend_mod_ui("hwb_trends")),
                nav_panel(title = "Rank", rank_mod_ui("hwb_rank")),  
                nav_panel(title = "Deprivation", simd_navpanel_ui("hwb_simd"))
              )),
    
    
    # Children and young people
    nav_panel(value = "CYP",
              title = "Children & Young people",
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("cyp_summary")),
                nav_panel(title = "Trends", trend_mod_ui("cyp_trends")),
                nav_panel(title = "Rank", rank_mod_ui("cyp_rank")),
                nav_panel(title = "Deprivation", simd_navpanel_ui("cyp_simd"))
              )),
    
    # care and wellbeing
    nav_panel(value = "CWB",
              title = "Care & Wellbeing",
              
              card(max_height = 150,
                   card_header(bs_icon("info-circle-fill", size = "1.2em"),
                               "Indicator set in development",
                               class = "info-box-header"),
                   p("The Care and Wellbeing indicator set will be further developed following user feedback. 
                     If you have any feedback please contact us at", 
                     tags$a("phs.scotpho@phs.scot.", href = "mailto:phs.scotpho@phs.scot?subject=Care%20and%20Wellbeing%20Indicator%20Feedback"))),
              
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("cwb_summary")),
                nav_panel(title = "Trends", trend_mod_ui("cwb_trends")),
                nav_panel(title = "Rank", rank_mod_ui("cwb_rank")),
                nav_panel(title = "Deprivation", simd_navpanel_ui("cwb_simd")),
                nav_panel(title = "Population Groups", pop_groups_ui("cwb_pop_groups"))
              )),
    
    # alcohol
    nav_panel(value = "ALC",
              title = "Alcohol",
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("alc_summary")),
                nav_panel(title = "Trends", trend_mod_ui("alc_trends")),
                nav_panel(title = "Rank", rank_mod_ui("alc_rank"))
              )),
    
    # drugs
    nav_panel(value = "DRG",
              title = "Drugs",
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("drg_summary")),
                nav_panel(title = "Trends", trend_mod_ui("drg_trends")),
                nav_panel(title = "Rank", rank_mod_ui("drg_rank"))
              )),
    
    # mental health
    nav_panel(value = "MEN",
              title= "Mental Health",
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("men_summary")),
                nav_panel(title = "Trends", trend_mod_ui("men_trends")),
                nav_panel(title = "Rank", rank_mod_ui("men_rank"))
              )),
    
    # population
    nav_panel(value = "POP",
              "Population",
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("pop_summary")),
                nav_panel(title = "Trends", trend_mod_ui("pop_trends")),
                nav_panel(title = "Rank", rank_mod_ui("pop_rank")),
                nav_panel(title = "Deprivation", simd_navpanel_ui("pop_simd"))
              )),
    
    
    # tobacco
    nav_panel(value = "TOB",
              "Tobacco",
              navset_tab(
                nav_panel(title = "Summary", summary_table_ui("tob_summary")),
                nav_panel(title = "Trends", trend_mod_ui("tob_trends")),
                nav_panel(title = "Rank", rank_mod_ui("tob_rank"))
              )),
    
    nav_panel(value = "ALL",
              "All Indicators",
              navset_tab(
                nav_panel(title = "Trends", trend_mod_ui("all_trends")),
                nav_panel(title = "Rank", rank_mod_ui("all_rank"))
              ))
  ), #close nav menu

  nav_spacer(),

  # data tab -------------------------------------------------------------------
            nav_panel("Download data",
                      value = "dt",
                      data_tab_modUI("data_tab")

            ), # close data table nav

  # source code link -------------------------------------------------------------------
  nav_item(tags$a(icon("github"), "SourceCode", href = "https://github.com/Public-Health-Scotland/scotpho-profiles-tool/tree/master/shiny_app", target = "_blank")),

  # other tabs -----------------------------------------------------------------
  nav_menu(
    title = "More information",

    # about scotpho tab
    nav_panel(title = "About ScotPHO",about_scotpho_text),

    # about profiles tab (to do: replace placeholder text)
    nav_panel(title = "About Profiles",
              accordion(
                open= FALSE, #no accordion panels open on loading
                multiple = TRUE, #allows multiple profile accordion panels to be open at once
                h1("About the ScotPHO Profiles"),
                p("Here is some information about each of the ScotPHO profiles."),
                accordion_panel("Health and Wellbeing",
                      about_hwb_text,
                      navigation_button_modUI(button_id="view_profile_HWB", button_name="View Profile")
                      ),
                accordion_panel("Children and Young People",
                      about_cyp_text,
                      navigation_button_modUI(button_id="view_profile_CYP", button_name="View Profile")
                      ),
                accordion_panel("Care and Wellbeing",
                      about_cwb_text,
                      navigation_button_modUI(button_id="view_profile_CWB", button_name="View Profile")
                      ),
                accordion_panel("Alcohol",
                      about_alc_text,
                      navigation_button_modUI(button_id="view_profile_ALC", button_name="View Profile")
                      ),
                accordion_panel("Drugs",
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_DRG", button_name="View Profile")
                      ),
                accordion_panel("Mental Health",
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_MEN", button_name="View Profile")
                      ),
                accordion_panel("Population",
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_POP", button_name="View Profile")
                      ),
                accordion_panel("Tobacco",
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_TOB", button_name="View Profile")
                      )
    )),

    # indicator definitions tab
    nav_panel(title = "Indicator Definitions",
              definitions_tab_UI("metadata")
    )
    
    ) # close nav menu

) #close main server function

### END






