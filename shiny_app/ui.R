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

  # header that appears across the top of each profile tab
  # including geography filters and info detailing selected geography and profile
  header = conditionalPanel(condition = "input.nav !== 'Home' && input.nav !== 'About Profiles' && input.nav !== 'Indicator Definitions' && input.nav !== 'About ScotPHO' && input.nav !== 'dt'",
                            tagList(
                              uiOutput("profile_header"),
                              uiOutput("areatype_header"),
                              uiOutput("areaname_header"),
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
              navigation_button_modUI(button_id="about_hwb", button_name = "About this profile", button_icon = icon("circle-info")),
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
                nav_panel(title = "Trends"),
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
                nav_panel(title = "Trends"),
                nav_panel(title = "Rank", rank_mod_ui("all_rank"))
              ))
  ), #close nav menu

  nav_spacer(),

  # data tab -------------------------------------------------------------------
            nav_panel("Download data",
                      value = "dt",
                      page_sidebar(fillable = FALSE,
                                   sidebar = sidebar(width = 300, padding = 20,
                                                     
                                                     h2("Filters"),
                                                     
                                                     # clear filters button
                                                     actionButton("clear_table_filters",
                                                                  label = "Clear all filters",
                                                                  icon ("eraser"),
                                                                  class = "down"),

                                                     
                                                     # Geography filters
                                                     jstreeOutput("geography_selector"),
                                                     
                                                     # profile filters
                                                     virtualSelectInput(inputId = "profile_selector",
                                                                        label = "Select profile(s)",
                                                                        choices = unname(profiles_list),
                                                                        disableSelectAll = FALSE,
                                                                        multiple = TRUE,
                                                                        search = TRUE,
                                                                        searchByStartsWith = TRUE,
                                                                        width = '100%',
                                                                        zIndex = 100),
                                                     
                                                     # indicator filters
                                                     virtualSelectInput(inputId = "indicator_selector",
                                                                        label = "Select indicator(s)",
                                                                        noOptionsText = "Select atleast one geography to see what indicators are available",
                                                                        choices = NULL,
                                                                        disableSelectAll = TRUE,
                                                                        multiple = TRUE,
                                                                        search = TRUE,
                                                                        searchByStartsWith = TRUE,
                                                                        dropboxWrapper = "body",
                                                                        dropboxWidth = '400px',
                                                                        width = '100%',
                                                                        zIndex = 100),
                                                     
                                                     
                                                     # time period filter
                                                     radioGroupButtons(
                                                       inputId = "time_period_selector",
                                                       label = "Select time period:",
                                                       choices = c("Latest available year", "All years"),
                                                       selected = "Latest available year"
                                                     )
                                                     
                                   ), # close sidebar
                                   
                                   h1("Data table"),
                                   p("Use the filters to build a data table, which can then be downloaded in various
	                                 formats using the button below. Please note that the table below is a preview. 
	                                 The downloaded dataset will contain more columns containing metadata than are presented here."),
                                   
                                   # download data button
                                   download_data_btns_ui(id = "datatable_downloads"),
                                   
                                   # data table
                                   reactableOutput("data_tab_table")
                                   
                      ) # close layout
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
                accordion_panel("Health and Wellbeing", icon=icon("line-chart"),
                      about_hwb_text,
                      navigation_button_modUI(button_id="view_profile_HWB", button_name="View Profile")
                      ),
                accordion_panel("Children and Young People", icon=icon("line-chart"),
                      about_cyp_text,
                      navigation_button_modUI(button_id="view_profile_CYP", button_name="View Profile")
                      ),
                accordion_panel("Care and Wellbeing", icon=icon("line-chart"),
                      about_cwb_text,
                      navigation_button_modUI(button_id="view_profile_CWB", button_name="View Profile")
                      ),
                accordion_panel("Alcohol", icon=icon("line-chart"),
                      about_alc_text,
                      navigation_button_modUI(button_id="view_profile_ALC", button_name="View Profile")
                      ),
                accordion_panel("Drugs", icon=icon("line-chart"),
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_DRG", button_name="View Profile")
                      ),
                accordion_panel("Mental Health", icon=icon("line-chart"),
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_MEN", button_name="View Profile")
                      ),
                accordion_panel("Population", icon=icon("line-chart"),
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_POP", button_name="View Profile")
                      ),
                accordion_panel("Tobacco", icon=icon("line-chart"),
                      p(" "),
                      navigation_button_modUI(button_id="view_profile_TOB", button_name="View Profile")
                      )
    )),

    # indicator definitions tab
    nav_panel(title = "Indicator Definitions")
    ) # close nav menu

) #close main server function

### END






# ###################################################################################################
# 
# # ALTERNATIVE UI
# 
# ##################################################################################################
# 
# 
# page_navbar(
#   tags$head(
#     tags$style(HTML("
# #nav {
#   position: -webkit-sticky;
#   position: sticky;
#   top: 0;
#   z-index: 100;
#   height: calc(100vh - 40px);
#   overflow-y: auto;
# }")),
#     tags$script(src = "https://code.highcharts.com/highcharts.js"),
#     includeCSS("www/styles.css")
#   ),
#   # header = tagList(h4("This is a new service - your feedback will help us to improve it.", hr())),
#   fillable = FALSE,
#   window_title = "ScotPHO Profiles",
#   id = "main_nav",
#   bg = phs_colors("phs-purple"),
#   collapsible = TRUE,
#   lang = "en",
#   theme = phs_theme,
#   useShinyjs(),
# 
# 
#   nav_panel(value = "Home",
#             title = "Home",
#             htmlTemplate("landing-page.html", # sits in separate file in app folder
# 
#                          # buttons to navigate to about scotpho, about profiles and indicator definitions tabs
#                          additional_info_buttons = layout_columns(navigation_button_modUI(button_id="about_us", button_name = "About us", button_icon = icon("circle-info")),
#                                                                   navigation_button_modUI(button_id = "about_profiles",button_name = "About the profiles", button_icon = icon("circle-info")),
#                                                                   navigation_button_modUI(button_id="explore_indicators", button_name = "Explore indicators", button_icon = icon("magnifying-glass"))
#                          ),
# 
#                          # buttons to navigate to profile tabs
#                          profile_buttons = tagList(
#                            layout_column_wrap(
#                              profile_homepage_btn_modUI(id = "hwb_nav", profile_name = "Health and Wellbeing", profile_icon = "line-chart"),
#                              profile_homepage_btn_modUI(id = "cyp_nav", profile_name = "Children and Young People", profile_icon = "line-chart"),
#                              profile_homepage_btn_modUI(id = "cwb_nav", profile_name = "Care and Wellbeing", profile_icon = "line-chart"),
#                              profile_homepage_btn_modUI(id = "alc_nav", profile_name = "Alcohol", profile_icon = "line-chart")
#                            ),
#                            layout_column_wrap(
#                              profile_homepage_btn_modUI(id = "drg_nav", profile_name = "Drugs", profile_icon = "line-chart"),
#                              profile_homepage_btn_modUI(id = "men_nav", profile_name = "Mental Health", profile_icon = "line-chart"),
#                              profile_homepage_btn_modUI(id = "pop_nav", profile_name = "Population", profile_icon = "line-chart"),
#                              profile_homepage_btn_modUI(id = "tob_nav", profile_name = "Tobacco", profile_icon = "line-chart")
#                            )
#                          ),
#                          indicator_schedule_button = navigation_button_modUI(button_id="indicator_schedule", button_name = "View updates schedule"),
#                           recent_updates_button = navigation_button_modUI(button_id="recent_updates", button_name="View recent updates")
#             )
#   ),
# 
#   nav_panel(title = "Profiles",
#             navset_pill_list(
#               header = tagList(
#                 uiOutput("profile_header"),
#                 uiOutput("areatype_header"),
#                 uiOutput("areaname_header"),
#                 global_geography_filters_ui("geo_filters", areatype_choices = areatype_list, parent_area_choices = hscp_list)
#               ),
# 
#               id = "nav",
#               well = FALSE,
#               fluid = FALSE,
#               widths = c(2, 10),
#               nav_panel(title = "Health & Wellbeing", value = "HWB",
#                         br(),
#                         h3("Contents: "),
#                         layout_columns(a("Summary", href = "#summary_header")),
#                         layout_columns(a("Rank", href = "#rank_header")),
#                         layout_columns(a("Trends", href = "#trends_header")),
#                         h2("Summary", id = "summary_header", class = "sub-section-header"),
#                         summary_table_ui("hwb_summary"),
#                         hr(),
#                         h2("Rank", id = "rank_header", class = "sub-section-header")
#               ),
#               nav_panel(title = "Care and Wellbeing", value = "CWB",
#                         br(),
#                         navset_tab(
#                           nav_panel(title = "Summary", summary_table_ui("cwb_summary")),
#                           nav_panel(title = "Trends"),
#                           nav_panel(title = "Rank"),
#                           nav_panel(title = "Inequalities"))
#               ),
#               nav_panel(title = "Children and young people", value = "CYP"),
#               nav_panel(title = "Alcohol", value = "ALC"),
#               nav_panel(title = "Tobacco", value = "TOB"),
#               nav_panel(title = "Population", value = "POP"),
#               nav_panel(title = "Mental Health", value = "MEN"),
#               nav_panel(title = "Drugs", value = "DRG")
# 
# 
# 
#             )
#   )
# 
# )
# 
