###############################################.
#
# App main ui script
#
##############################################.

#######################################.
# Initial structure  ------
#######################################.

# this first part of the UI creates a purple navigation bar to place individual tabs in
# it's also where some external script are sourced that are required for different part of the app to work
page_navbar(
  fillable = FALSE, # controlling how items grow/shrink when browser different sizes
  window_title = "ScotPHO profiles",
  id = "nav", # id required for profile buttons - works with profile_homepage_btn_mod to control navigation
  lang = "en",
  theme = phs_theme, # dashboard theme - defined in global script
  navbar_options = navbar_options(
    bg = phs_colours(colourname = "phs-purple"), # background navbar colour
    collapsible = TRUE # collapse tabs on smaller screens
  ),
  # place external scripts in footer argument to avoid warnings as recommended by package developers
  header = tags$head(
    useShinyjs(), # need to declare this to use functions from the shinyjs package, e.g. to show/hide parts of the UI
    use_cicerone(), # required for guided tours
    tags$script(src = "https://code.highcharts.com/highcharts.js"), # required for spinecharts
    tags$script(src = "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"),# required for saving leaflet map as png (see this for more info: https://stackoverflow.com/questions/47343316/shiny-leaflet-easyprint-plugin)
    includeCSS("www/styles.css") # required to specify formatting (particularly of landing page)
    ),

  #######################################.
  # Homepage ----
  ######################################.
  
  # this tab is the homepage of the app. Note that some of the code to create this landing page sits in a seperate HTML file
  # the profile_homepage_btn_mod_UI is a module that creates a button to navigate to the profiles tab of the app.
  # It also updates the profile filter depending on what profile you select
  
  nav_panel(value = "Home", style = "margin: 0; padding:0;", # remove margin so no white space at top of landing page
            title = "Home",
            style = "background-color:#F7F7F7;",
            htmlTemplate("landing-page.html", # sits in separate file in app folder
                         # buttons to navigate to about scotpho, about profiles and indicator definitions tabs
                         additional_info_buttons = layout_columns(
                           navigation_button_modUI(button_id="about_us", button_name = "About us", button_icon = icon("circle-info"), class = "btn-hero"),
                           navigation_button_modUI(button_id="explore_indicators", button_name = "About indicators/updates", button_icon = icon("circle-info"), class = "btn-hero")
                         ),
                         
                         # buttons to navigate to profile tabs
                         # this piece of code goes through the named profiles list from the global script, and for each profile,
                         # generates a profile button using the name of the profile and the homepage description 
                         # note: setting width = 1/3 inside layout_column_wrap lays out 3 profile buttons per row
                         profile_buttons = layout_column_wrap(
                           style = "padding: 15px;", width = 1/3,
                           !!!lapply(names(profiles_list), function(profile) {
                             profile_homepage_btn_modUI(id = profile, 
                                                        profile_name = profile, 
                                                        description = profiles_list[[profile]]$homepage_description,
                                                        class = ifelse(profiles_list[[profile]]$active == TRUE, "profile-btn", "profile-btn-disabled"))
                           })
                         ),

            whats_new = layout_columns(
              width = 1,
              card(
                card_header(bs_icon("info-circle-fill", size = "1.2em"), "What's new",class = "info-box-header"),
                card_body(gap = 0,
                          # CYP Mental Health profile info
                          div(
                            h4("April 2025 : New Mental Health Profile for Children & Young People", class = "profile-header"),
                            p("The Mental Health Profile for Children & Young People is the result of Public Health Scotland's", 
                              tags$a("Mental Health Indicators project.", href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/", 
                              target = "_blank")," The mental health indicators include measures of mental health outcomes, as well as of a wide range 
                              of interconnected determinants (risk factors and protective factors) of these outcomes. This profile follows ScotPHO's publication of the adult mental health profile in late 2024.")
                          ),
                          hr(),
                          # Correction of Emergency admissions, aged 65+ years indicator
                          div(
                            h4("23/01/2025 : Correction of emergency admissions, aged 65+ years indicator", class = "profile-header"),
                            p("We have recently identified an issue with code calculating emergency admissions, aged 65+ years figures. A correction has been made and revised figures are now available in this tool.
                            Previously published indicator data underestimated both indicator numerators and rates across all geographies and over the full time series.
                            If you have any queries regarding this correction please contact ",tags$a("phs.scotpho@phs.scot", href = "mailto:phs.scotpho@phs.scot", target = "_blank"))
                          ),
                          hr(),
                # Profiles tool re-design info
                div(
                  h4("Profiles Tool Redesign", class = "profile-header"),
                  p("Following user feedback ScotPHO have redesigned this online profiles tool. The new design simplifies navigation between 
                  different visualisations and over time will allow us to introduce additional visualisations such as indicator splits by 
                  variables such as age, sex and ethnicity. The changes will also enable ScotPHO to respond to requests for additional profiles.", 
                tags$a("Further details", href = "https://www.scotpho.org.uk/comparative-health/profiles/online-profiles-tool", target = "_blank"),
                     " of the new features and additional user guidance are available.")
                ),
              hr(),
              # Adult Mental Health profile info
              div(
                h4("October 2024 : New Mental Health Profile for Adults", class = "profile-header"),
                p("The Mental Health Profile for Adults is the result of Public Health Scotland's", tags$a("Mental Health Indicators project.", 
                  href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/", 
                  target = "_blank")," The mental health indicators include measures of mental health outcomes, as well as of a wide range 
                  of interconnected determinants (risk factors and protective factors) of these outcomes. ScotPHO have 
                 recently published this profile for indicators in adults and are currently working on the profile for children and young people.")
                ),
              hr(),
              # Feedback info
              div(
                h4("Feedback", class = "profile-header"),
                p("If you have feedback please contact us at ", tags$a("phs.scotpho@phs.scot", href = "mailto:phs.scotpho@phs.scot", target = "_blank"))
              )
              )) # close card
              ) # close what's new section 
  ) # close html template
  ), # close homepage
  
  
  
  ############################################.
  # PROFILES TAB ----
  ############################################.
  nav_panel(title = "Profiles",
            
  # From here down to around line 115 is all the elements that make up the top section of the profiles tab
  # this includes the 2 x headers with buttons next to them, and the filters that appear when the buttons are clicked
  # to allow users to update the profile and geography of interest
  
            
            # profile header with button
              div(class = "header-elements",
                uiOutput("profile_header"),
                actionButton(inputId = "show_profile_filter", label = "Change profile", icon = icon("filter"), class = "btn-sm ms-3 btn-global")
                ),
            # hidden profile filter to display when button clicked 
                hidden(div(id = "prof_filter_hidden",
                      selectizeInput(inputId = "profile_choices", 
                                     label = "", 
                                     choices =  active_profiles, #declared in global script
                                     options = list(onInitialize = I('function() { this.setValue(""); }')))
                    )),
              # geography header with button
                div(class = "header-elements",
                  uiOutput("geography_header"),
                  actionButton("show_geo_filters", label = "Change area", icon = icon("filter"), class = "btn-sm ms-3 btn-global")
                ),
              # hidden geography filterers to display when button clicked
                hidden(div(id = "geo_filters_hidden",
                
                           # notice to display to users if they select police divisions as they are only 
                           # available for some indicators within the mental health profile.
                           conditionalPanel(condition = "input.areatype == 'Police division'",
                                            br(),
                                            layout_columns(
                                              col_widths = c(8, -4),
                                              card(
                                                card_header(bs_icon("info-circle-fill", size = "1.2em"),"Police divisions",class = "info-box-header"),
                                                card_body("Please note that data split by police division is currently only available for a small subset of indicators within the mental health profile.")
                                              )
                                            )
                           ),         
                           
                           
                  layout_columns(widths = c(4, 4, 2),fillable = FALSE,
                    # area type filter 
                    selectizeInput("areatype", "Area type", choices = areatype_list, selected = "Scotland"),
                    layout_column_wrap(width = 1,
                      # areaname filter 
                      selectizeInput("areaname", "Area name", choices = c("Scotland"), selected = "Scotland"),
                      # parent area filter (only appear when HSC locality or Intermediate zone selected)
                      conditionalPanel(condition = "input.areatype == 'HSC locality' || input.areatype == 'Intermediate zone'",
                                       selectizeInput("parent_area", label = "First select a region for localities or intermediate zones", choices = hscp_list, width = "100%"))
                      ),
                    # button to apply geography filters
                    actionButton("apply_geo_filters", label = "Apply geography filters", class = "btn-apply-geo-filter")
                    ) # close layout columns
                  )), # close hidden div
             br(), # add space between header and sub-tabs

  
  ########################################.
  # Profiles sub-tabs ----
  ########################################.
  
  # the navset_tab() function creates sub-tabs within the profiles tab to allow the users to explore the data in various ways
  # Note that although all the possible sub-tabs that can exist in the app appear here in this UI script, they are not all shown on the dashboard at once
  # the sub-tabs that appear will depend on what profile a user has selected (see the server script for code that conditionally shows/hides some of these tabs)
  # This is achieved by assigning the navset_tab an id (i.e. sub_tabs) and assigning each individual sub-tab a value (i.e. the deprivation tab has a value called 'simd_tab')
  # we can therefore say i.e. when the user selects "Drugs" from the profile filter, hide the sub-tab that's called 'simd_tab' (since drugs data isn't available at SIMD level)
  # Each sub-tab includes a module. A module consists of a UI function and corresponding server function (refer to the visualisations sub- folder within the modules folder)
  
            navset_tab(id ="sub_tabs", 
                       
                       # summary sub-tab
                       nav_panel(title = "Summary", value = "summary_tab",
                                 
                                 # Display message at top of summary tab for specific profiles only
                                 # commenting out Jan 2025 but leaving script as placeholder as new text may come along 
                                 conditionalPanel(condition = "input.profile_choices == 'Population Health'",
                                                  br(),
                                                  card(max_height = 150,
                                                       card_header(bs_icon("info-circle-fill", size = "1.2em"),"Indicator set in development",class = "info-box-header"),
                                                       p("The Population Health dashboard has been developed to support the ambitions of Scotland’s Population Health Framework, and the Collaboration 
                                                         for Health Equity in Scotland by providing access to the latest data on population health outcomes and inequalities. The indicators are structured 
                                                         around the “Marmot Eight” principles as laid out in the Institute of Health Equity’s work on ",
                                                         tags$a("Marmot Places", href = "https://www.instituteofhealthequity.org/taking-action/marmot-places", target = "_blank"),". ")
                                                       )
                                                  ),
                                 # only display this card when Mental Health profile selected
                                 conditionalPanel(condition = "input.profile_choices == 'Children & Young People Mental Health'",
                                                  br(),
                                                  card(max_height = 150,
                                                       card_header(bs_icon("info-circle-fill", size = "1.2em"),"Profile in development",class = "info-box-header"),
                                                       p("March 2025: Additional ",
                                                       tags$a("mental health indicators for children and young people", 
                                                              href = "https://www.publichealthscotland.scot/publications/children-and-young-people-mental-health-indicator-resources/",
                                                              target = "_blank"),
                                                       " will be added as they become available.")
                                                  )
                                 ),
                                 summary_table_ui("summary")
                                 ),
                       
                       # trends sub-tab
                       nav_panel(title = "Trends", value = "trends_tab", trend_mod_ui("trends")),
                       
                       # rank sub-tab 
                       nav_panel(title = "Rank", value = "rank_tab", rank_mod_ui("rank")),
                       
                       # deprivation sub-tab 
                       nav_panel(title = "Deprivation", value = "simd_tab", simd_navpanel_ui("simd")),
                       
                       # population groups sub-tab
                       nav_panel(title = "Population groups", value = "pop_groups_tab", pop_groups_ui("pop_groups")),
                       
                       # About this profile sub tab (text that appears is conditional depending on selected profile)
                       # the text for each profile is defined in the 'narrative' subfolder in the 'about_profiles_narrative.R' script
                       # note there is only text for the 5 profiles listed below at the moment
                       nav_panel(title = "About this profile", value = "about_profile_tab",
                                 br(),
                                 conditionalPanel("input.profile_choices == 'Health & Wellbeing'", about_hwb_text),
                                 conditionalPanel("input.profile_choices == 'Population Health'", about_cwb_text),
                                 conditionalPanel("input.profile_choices == 'Adult Mental Health'", about_men_text),
                                 conditionalPanel("input.profile_choices == 'Children & Young People Mental Health'", about_cmh_text),
                                 conditionalPanel("input.profile_choices == 'Alcohol'", about_alc_text),
                                 conditionalPanel("input.profile_choices == 'Children & Young People'", about_cyp_text)
                       )

                       
            ) # close subtabs
            ), # close entire profiles tab
  
  nav_spacer(), # add space to navbar 
  
  ########################################.
  # Data table tab -------
  ########################################.
  nav_panel("Download data", value = "dt", data_tab_modUI("data_tab")), 
  
  
  ########################################.
  # Link to github repo  -------
  ########################################.
  nav_item(tags$a(icon("github"), "SourceCode", href = "https://github.com/Public-Health-Scotland/scotpho-profiles-app", target = "_blank")),
  
  ########################################.
  # Menu with additional tabs  -------
  ########################################.
  
  # this section creates a drop-down menu containing 3 tabs which provide further information on ScotPHO
  # i.e. an about scotpho tab, an indicator definitions tab and an about profiles tab 
  
  nav_menu(
    title = "More information",
    align = "right", # ensures tab names inside the menu are not cut-off
    
    # about scotpho tab
    nav_panel(title = "About ScotPHO", value = "about_scotpho", about_scotpho_text),

    
    # indicator definitions tab
    nav_panel(title = "Indicator Definitions",
              value = "definitions",
              definitions_tab_UI("metadata")
    )
  ) # close nav menu
  
) #close main ui function

### END





