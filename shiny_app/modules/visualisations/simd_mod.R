###########################################################################.
# MODULE: simd_mod ---- 
# prepares the nav_panel layout displaying SIMD based deprivation data
###########################################################################.

#######################################################.
## MODULE UI ----
#######################################################.

# id = unique id 
simd_navpanel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      
      # sidebar for filters -----
      sidebar = sidebar(width = 300,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        
                        # indicator filter (note this is a module)
                        div(id = ns("deprivation_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("simd_indicator_filter"))),
                        
                        # button to scroll to metadata
                        metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section")),

                        # sex filter (for the mental health profile only as some SIMD indicators have sex splits)
                        # it will be hidden for all other profiles
                        shinyjs::hidden(
                          selectizeInput(inputId = ns("sex_filter"), 
                                       label = "Select sex:", 
                                       choices = c("Total", "Male", "Female"), 
                                       selected = "Total")
                          ),
                        
                        # measure filter
                        radioButtons(
                          inputId = ns("depr_measures"),
                          label = "Explore data by:",
                          choices = c("Patterns of inequality", "Inequality gap", "Potential for improvement"),
                          selected = c("Patterns of inequality")
                        ),
                        
                        # quint type filter 
                        div(id = ns("deprivation_quintile_type_wrapper"), 
                            radioButtons(inputId = ns("quint_type"), 
                                         label = "Quintile Type",
                                         choices = c("Scotland", "Local"), 
                                         selected = "Scotland")),
                        
                        
                        
                        #guided tour button
                        actionLink(inputId = ns("deprivation_tour_button"),
                                   label = "Take a guided tour of this page"),
                        
                        # About SIMD button
                        actionLink(inputId = ns("simd_help"), 
                                   label = "What is SIMD?", icon = icon("info-circle"))
                        
                        
      ), # close sidebar
      
      
      # Left hand side card 
      layout_column_wrap(
        width = 1/2,
          navset_card_pill(
            id = ns("left_card"),
            height = 650,
            full_screen = TRUE,
            
            # chart tab 
            nav_panel(
              title = "Chart",
              div(
                uiOutput(ns("left_chart_header")) # chart header 
              ),
              highchartOutput(ns("left_chart")) |> # chart
                withSpinner() |> 
                bslib::as_fill_carrier() 
            ),
            
            # Data tab 
            nav_panel(
              title = "Data",
              reactableOutput(ns("left_table"))
            ),
            
            # Interpretation tab 
            nav_panel(
              title = "Help",
              uiOutput(ns("left_chart_narrative"))
            ),
            
            nav_spacer(),

            # Popover with filters
            nav_item(
              popover(
                title = "Filters",
                trigger = chart_controls_icon(),
                checkboxInput(ns("left_ci_switch"), label = " include confidence intervals", FALSE),
                checkboxInput(ns("left_zero_axis_switch"), label = "Start y-axis at 0", TRUE),
                checkboxInput(ns("left_average_switch"), label = "Include averages", FALSE)
                
              )
            ),
            
            # card footer with download buttons
            footer = card_footer(
              class = "d-flex justify-content-left",
              download_chart_mod_ui(ns("save_left_chart")),
              download_data_btns_ui(ns("save_left_data")))
          ),
        
        # right hand side card 
          navset_card_pill(
            id = ns("right_card"),
            height = 650,
            full_screen = TRUE,
            
            # chart tab 
            nav_panel(
              title = "Chart",
              div(
                uiOutput(ns("right_chart_header"))
              ),
              highchartOutput(ns("right_chart")) |>
                withSpinner() |> 
                bslib::as_fill_carrier() 
            ),
            
            # data tab 
            nav_panel(
              title = "Data",
              reactableOutput(ns("right_table"))
            ),
            
            # interpretation tab 
            nav_panel(
              title = "Help",
              uiOutput(ns("right_chart_narrative"))
            ),
            nav_spacer(),
            
            # popover with filters
            nav_item(
              popover(
                title = "Filters",
                trigger = chart_controls_icon(),
                checkboxInput(ns("right_ci_switch"), label = " include confidence intervals", FALSE),
                checkboxInput(ns("right_zero_axis_switch"), label = "Start y-axis at 0", TRUE),
                checkboxInput(ns("right_average_switch"), label = "Include averages", FALSE)
              )
            ),
            
            # card footer with download buttons
            footer = card_footer(
              class = "d-flex justify-content-left",
              download_chart_mod_ui(ns("save_right_chart")),
              download_data_btns_ui(ns("save_right_data")))
          )
      ), # close layout column wrap
      
      # accordion panel with metadata table 
      div(id = ns("metadata_section"), metadata_panel_UI(ns("metadata_table")))
          
        ) # close layout sidebar
      ) # close taglist

  
}


#######################################################.
## MODULE SERVER----
#######################################################.

# id = unique id 
# simd_data = reactive dataframe created in main server script contains simd data already filtered by profile
# geo_selections = reactive values in main server stores global geography selections
# selected_profile = name of reactive value stores selected profile from main server script



  simd_navpanel_server <- function(id, simd_data, geo_selections, selected_profile, root_session){
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    #######################################################.
    ## Dynamic filters -----
    #######################################################.
    
    # hide the confidence intervals switch from the popovers when potential for improvement
    # has been selected as there are no CIs for these 2 charts
    observeEvent(input$depr_measures, {
      if(input$depr_measures == "Potential for improvement"){
        updateCheckboxInput(session, "left_ci_switch", value = FALSE)
        updateCheckboxInput(session, "right_ci_switch", value = FALSE)
        shinyjs::hide("left_ci_switch")
        shinyjs::hide("right_ci_switch")
      } else{
        shinyjs::show("left_ci_switch")
        shinyjs::show("right_ci_switch")
      }
    })
    
    
    # show the option to start y-axis at zero when trend charts are being presented
    # and have it pre-set to be switched on, otherwise turn the switch off and hide it
    observeEvent(input$depr_measures, {
      if(input$depr_measures == "Inequality gap"){
        shinyjs::show("left_zero_axis_switch")
        updateCheckboxInput(session, "left_zero_axis_switch", value = TRUE)
      } else{
        updateCheckboxInput(session, "left_zero_axis_switch", value = FALSE)
        shinyjs::hide("left_zero_axis_switch")
      }
    })
    
    
    # show the option to include averages only when patterns of inequality has been selected
    # and have it pre-set to be switched off, otherwise turn the switch off and hide it
    observeEvent(input$depr_measures, {
      if(input$depr_measures == "Patterns of inequality"){
        shinyjs::show("left_average_switch")
        shinyjs::show("right_average_switch")
      } else{
        updateCheckboxInput(session, "left_average_switch", value = FALSE)
        updateCheckboxInput(session, "right_average_switch", value = FALSE)
        shinyjs::hide("left_average_switch")
        shinyjs::hide("right_average_switch")
      }
    })
    
    
        # determining which quint types are available
        # and enabling/disabling quint type filter accordingly
        observeEvent(indicator_data(), {

          # check what quint types are available for selected indicator
          available_quints <- unique(indicator_data()$quint_type)

          # If there's only 1 quint type available for the selected geography and area (i.e. only scotland OR local quintiles)
          # then disable filter and default to the 1 that is available
          if (length(available_quints)==1){
            if(available_quints == "sc_quin"){
              updateRadioButtons(session, "quint_type", selected = "Scotland")
            } else {
              updateRadioButtons(session, "quint_type", selected = "Local")
            }
            shinyjs::disable("quint_type")
          } else {

            # otherwise if both local and scottish quintiles available then enable filter so
            # users can toggle between the two options (default to Scotland)
            shinyjs::enable("quint_type")
            updateRadioButtons(session, "quint_type", selected = "Scotland")
          }
        })
    
    
        # update sex filter choices depending on what splits are available for the selected indicator
        # if only totals available (i.e. no male/female splits) then hide filter, otherwise show it
        observeEvent(indicator_data(), {

          # update filter choices
          choices <- unique(indicator_data()$sex) # get choices
          selection <- if (input$sex_filter %in% choices) input$sex_filter else "Total" # reapply previous selection if still valid
          updateSelectizeInput(session, "sex_filter", choices = choices, selected = selection) # update filter with choices

          # show/hide filter
          if(length(choices) == 1){
            shinyjs::hide("sex_filter")
          } else {
            shinyjs::show("sex_filter")
          }
        }, ignoreNULL = TRUE)
    
    
    
    ####################################################.
    # TAB NAVIGATION ----
    ####################################################.
    
    # go to the help tab when 'learn more' is clicked (for the left card)
    observeEvent(input$left_chart_info_link, {
      nav_select(id = "left_card", selected = "Help", session = session)
      
    })
    
    # go to the help tab when 'learn more' is clicked (for the right card)
    observeEvent(input$right_chart_info_link, {
      nav_select(id = "right_card", selected = "Help", session = session)
      
    })
    
    
    
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # generate list of indicators (from the simd indicators dataset) available
    # and return the selected indicator
    selected_indicator <- indicator_filter_mod_server(id="simd_indicator_filter", simd_data, geo_selections, selected_profile)
    
    
    # filter data passed to the module by the selected indicator and selected area
    # and further filter by quint type
    indicator_data<- reactive({
      req(simd_data())
      
      dt <- simd_data() |>
        filter(indicator == selected_indicator()) |>
        mutate(across(.cols=sii:abs_range,.fns=abs)) # convert any negative numbers to positive so that indicators where higher number is good plot properly

    })
    
    
    # create a reactive object which stores a list of 2 filtered datasets; 1 for the left chart/table and 1 for the right chart/table
    # it also stores a list of the columns to show/hide in the data tables on the data tab within the card (as we want some to be hidden in the table,
    # but to appear in the data download itself)
    # the 2 datasets that are created are dependent on which measure a user selects (this is what the switch() function does)
    # (i.e. patterns of inequality, inequality gap or potential for improvement)
    # this is because each selection results in 2 different charts being displayed
    # It will therefore create a 'left_data' (i.e. data for the left-hand chart) and a 'right_data' (data for the right hand chart)
    simd_measures_data <- reactive({
      req(indicator_data())
      
      
      # filter by quint type 
      if(input$quint_type == "Scotland"){
        dt <- indicator_data() |>
          filter(quint_type == "sc_quin")
      } else {
        dt <- indicator_data() |>
          filter(quint_type != "sc_quin")
      }

      
      data <- switch(input$depr_measures,
                     
                     
                     # the 2 x datasets and 2 x list of columns to prepare if patterns of inequality is selected
                     "Patterns of inequality" = list(
                       
                       # SIMD dataset - snapshot 
                       left_data = dt |>
                         filter(sex == input$sex_filter) |>
                         filter(year == max(year)) |>
                         mutate(total = measure[quintile == "Total"]) |>
                         filter(quintile != "Total") |>
                         select(indicator, type_definition, areaname, areatype, trend_axis, quintile, measure, total, upci, lowci),
                       
                       # SIMD dataset - trend 
                       right_data = dt |>
                         filter(sex == input$sex_filter) |>
                         group_by(year) |>
                         mutate(total = measure[quintile == "Total"]) |>
                         ungroup() |>
                         filter(quintile != "Total") |>
                         arrange(year) |>
                         select(indicator, type_definition, areaname, areatype, trend_axis, quintile, measure, upci, lowci, total),
                       
                       # columns to show/hide in the SIMD data table
                       left_table_cols = list(indicator = colDef(show = FALSE), 
                                              type_definition = colDef(show = FALSE), 
                                              areaname = colDef(show = FALSE), 
                                              areatype = colDef(show = FALSE), 
                                              trend_axis = colDef(show = FALSE),
                                              upci = colDef(show = FALSE),
                                              lowci = colDef(show = FALSE),
                                              total = colDef(show = FALSE),
                                              trend_axis = colDef(show = FALSE),
                                              quintile = colDef(name = "Quintile"),
                                              measure = colDef(name = "Measure")
                       ),
                       
                       # columns to show/hide in the SIMD trend data table
                       right_table_cols = list(indicator = colDef(show = FALSE), 
                                               type_definition = colDef(show = FALSE), 
                                               areaname = colDef(show = FALSE), 
                                               areatype = colDef(show = FALSE), 
                                               upci = colDef(show = FALSE),
                                               lowci = colDef(show = FALSE),
                                               total = colDef(show = FALSE),
                                               trend_axis = colDef(name = "Time period"),
                                               quintile = colDef(name = "Quintile"),
                                               measure = colDef(name = "Measure")
                       )
                     ),
                     
                     
                     
                     # the 2 x datasets and 2 x list of columns to to prepare if inequality gap is selected
                     "Inequality gap" = list(
                       
                       # SII trend dataset
                       left_data = dt |>
                         filter(sex == input$sex_filter) |>
                         filter(quintile == "Total") |>
                         select(indicator, type_definition, areaname, areatype, trend_axis, sii, upci_sii, lowci_sii),
                       
                       # RII trend dataset
                       right_data = dt |>
                         filter(sex == input$sex_filter) |>
                         filter(quintile == "Total") |>
                         select(indicator, type_definition, areaname, areatype, trend_axis, rii_int, upci_rii_int, lowci_rii_int),
                       
                       # columns to show/hide in the SII data table
                       left_table_cols = list(indicator = colDef(show = FALSE), 
                                              type_definition = colDef(show = FALSE), 
                                              areaname = colDef(show = FALSE), 
                                              areatype = colDef(show = FALSE), 
                                              upci_sii = colDef(show = FALSE),
                                              lowci_sii = colDef(show = FALSE),
                                              trend_axis = colDef(name = "Time period"),
                                              sii = colDef(name = "Slope index of inequality (SII)")
                       ),
                       
                       
                       # columns to show/hide in the RII data table
                       right_table_cols = list(indicator = colDef(show = FALSE), 
                                               type_definition = colDef(show = FALSE), 
                                               areaname = colDef(show = FALSE), 
                                               areatype = colDef(show = FALSE), 
                                               upci_rii_int = colDef(show = FALSE),
                                               lowci_rii_int = colDef(show = FALSE),
                                               trend_axis = colDef(name = "Time period"),
                                               rii_int = colDef(name = "Relative index of inequality (RII)")
                       )
                     ),
                     
                     # the 2 x datasets and 2 x list of columns to to prepare if potential for improvement is selected
                     "Potential for improvement" = list(
                       
                       # attributable to inequality -barchart dataset
                       left_data = dt |>
                         filter(sex == input$sex_filter) |>
                         filter(year == max(year) & quintile != "Total") |>
                         #add columns which allow columns behind PAF bar to be created
                         #since some indicators are considered better with higher values and some better when lower values
                         mutate(highest_measure= case_when(quintile==qmax ~ measure, TRUE ~ 0),
                                lowest_measure= case_when(quintile==qmin ~ measure, TRUE ~0),
                                highest=max(highest_measure),
                                lowest=max(lowest_measure),
                                baseline=case_when(interpret=="L" ~ lowest,
                                                   interpret=="H" ~ measure),
                                `attributable to inequality`=case_when(interpret=="L" ~ abs(measure-baseline),
                                                                       (interpret=="H" ~ abs(measure-highest)))) |>
                         pivot_longer(cols = c("attributable to inequality", "baseline"), names_to = "measure_name") |>
                         select(indicator, type_definition, areaname, areatype, trend_axis, quintile, measure_name, value),
                       
                       # PAR - trend dataset
                       right_data = dt |>
                         filter(sex == input$sex_filter) |>
                         filter(quintile == "Total") |>
                         select(indicator, type_definition, trend_axis, areaname, areatype, par),
                       
                       
                       # columns to show/hide in the attributable to inequality table
                       left_table_cols = list(indicator = colDef(show = FALSE), 
                                              type_definition = colDef(show = FALSE), 
                                              areaname = colDef(show = FALSE), 
                                              areatype = colDef(show = FALSE), 
                                              trend_axis = colDef(show = FALSE)
                       ),
                       
                       # columns to show/hide in the PAR trend table
                       right_table_cols = list(indicator = colDef(show = FALSE), 
                                               type_definition = colDef(show = FALSE), 
                                               areaname = colDef(show = FALSE), 
                                               areatype = colDef(show = FALSE), 
                                               trend_axis = colDef(name = "Time period"),
                                               par = colDef(name = "Population attributable risk (PAR)")
                       )
                     )
                     
      ) # close switch function 
      data
    })
    
    
    # create a reactive object which stores a list of 10 pieces of information at one time (5 for each chart):
    # a. the narrative to display in the interpretation tab (i.e. stored under 'left_chart_narrative' for the left chart)
    # b. the chart title to display (i.e. stored under 'left_chart_title')
    # c . the 2 x charts subtitle to display (i.e. stored under 'left_chart_subtitle_1 and left_chart_subtitle_2)
    # d. the filename of the chart if saved as a png (i.e. stored under 'left_chart_filename')
    chart_text <- reactive({
      req(simd_measures_data())
      
      switch(input$depr_measures,
             
             
             # Prepare the 10 bits of text for if patterns of inequality are selected
             "Patterns of inequality" = list(
               
               # SIMD bar chart titles/subtitles/interpretation/filename
               left_chart_narrative = about_simd, # narrative text is stored in separate R script called 'about_simd_charts.R'
               left_chart_title = paste0(selected_indicator(), " by SIMD quintile - snapshot"),
               left_chart_subtitle_1 = paste0(simd_measures_data()$left_data$trend_axis[1]),
               left_chart_subtitle_2 = paste0(simd_measures_data()$left_data$type_definition[1]),
               left_chart_filename = paste0("ScotPHO SIMD barchart- ", selected_indicator()),
               
               # SIMD trend chart titles/subtitles/interpretation/filename
               right_chart_narrative = about_simd_trend,
               right_chart_title = paste0(selected_indicator(), " by SIMD quintile - trend"),
               right_chart_subtitle_1 = paste0(first(simd_measures_data()$right_data$trend_axis)," to ",last(simd_measures_data()$right_data$trend_axis)),
               right_chart_subtitle_2 = paste0(simd_measures_data()$right_data$type_definition[1]),
               right_chart_filename = paste0("ScotPHO SIMD trendchart- ", selected_indicator())
             ),
             
             
             # Prepare the 8 bits of text for if Inequality gap is selected
             "Inequality gap" = list(
               
               # SII trend chart titles/subtitles/interpretation/filename
               left_chart_narrative = about_sii,
               left_chart_title = paste0("Inequalities over time: absolute differences"),
               left_chart_subtitle_1 = paste0("The chart shows the difference between most and least deprived areas (expressed as ", simd_measures_data()$left_data$type_definition[1], ")."),
               left_chart_subtitle_2 = "An increasing trend suggests the gap between the most and least deprived areas is growing.",
               left_chart_filename = paste0("ScotPHO SII chart- ", selected_indicator()),
               
               
               
               # RII trend chart titles/subtitles/interpretation/filename
               right_chart_narrative = about_rii,
               right_chart_title = paste0("Inequalities over time: relative differences"),
               right_chart_subtitle_1 = "The chart shows the differences between the most disadvantaged area and the overall average for Scotland (expressed as a percentage).",
               right_chart_subtitle_2 = "An increasing trend suggests that the gap between the most disadvantaged area and the average is growing",
               right_chart_filename = paste0("ScotPHO RII chart - ", selected_indicator())
             ),
             
             
             # Prepare the 9 bits of text for if potential for improvement is selected
             "Potential for improvement" = list(
               
               # potential for improvement bar chart titles/subtitles/interpretation/filename
               left_chart_narrative = about_par,
               left_chart_title = paste0("Attributable to inequality; ", simd_measures_data()$left_data$trend_axis[1]),
               left_chart_subtitle_1 = paste0("The portion of ", selected_indicator(), " which could be attributed to socioeconomic inequalities."),
               left_chart_subtitle_2 = paste0(simd_measures_data()$left_data$type_definition[1]),
               left_chart_filename = paste0("ScotPHO Attributable to inequality chart - ", selected_indicator()),
               
               right_chart_narrative = about_par_trend,
               right_chart_title = paste0("Potential for improvement"),
               right_chart_subtitle_1 = paste0("How much (%) ", selected_indicator(), " could be improved if the levels of the least deprived area were experienced across the whole population."),
               right_chart_filename = paste0("ScotPHO PAR barchart- ", selected_indicator())
             )
      )
    })
    
    
    #####################################.
    # DYNAMIC TEXT ----
    ####################################.
    
    
    # render the title and subtitle for the left hand side card
    output$left_chart_header <- renderUI({
      
      if(nrow(indicator_data()) > 0) {
        div(
          h5(chart_text()$left_chart_title, class = "chart-header"),
          h6(chart_text()$left_chart_subtitle_1),
          p(chart_text()$left_chart_subtitle_2),
          actionLink(ns("left_chart_info_link"), label = "Learn more") # link to interpretation tab 
        )
      }
    })
    
    # render the title and subtitle for the right hand side card
    output$right_chart_header <- renderUI({
      
      if(nrow(indicator_data()) > 0) {
        div(
          h5(chart_text()$right_chart_title, class = "chart-header"),
          h6(chart_text()$right_chart_subtitle_1),
          p(chart_text()$right_chart_subtitle_2),
          actionLink(ns("right_chart_info_link"), label = "Learn more") # link to interpretation tab 
        )
      }
    })
    
    
    # render the narrative for left cards interpretation tab
    output$left_chart_narrative <- renderUI({
      chart_text()$left_chart_narrative
    })
    
    # render the narrative for the right cards interpretation tab
    output$right_chart_narrative <- renderUI({
      chart_text()$right_chart_narrative
    })
    
    
    #######################################.
    # CHARTS/TABLES ----
    #######################################.
    
    # render the chart to display on left-hand side (conditional depending on which measure was selected)
    # using the data stored within simd_measures_data()$left_data (which is also conditional depending on what measure was selected)
    output$left_chart <- renderHighchart({
      
      # only create charts if there is data available to plot
      # validation message suggests the other two area types the user might try (although dep data aren't necessarily available at the lower geog: should the message only suggest available geogs?)
      shiny::validate(
        need(nrow(indicator_data()) > 0,
             paste0("Deprivation data are not available at ", geo_selections()$areatype, " level in this profile. Please try a different geography, of either ", 
                    paste(setdiff(c("Scotland", "Health board", "Council area"), geo_selections()$areatype), collapse = " or "), 
                    "."))
      )
      
      
      hc <- switch(input$depr_measures,
                   
                   # SIMD bar chart
                   "Patterns of inequality" = create_bar_chart(data = simd_measures_data()$left_data,
                                                               xaxis_col = "quintile",
                                                               yaxis_col = "measure",
                                                               include_average = input$left_average_switch,
                                                               include_confidence_intervals = input$left_ci_switch,
                                                               colour_palette = "simd"),
                   
                   # SII trend chart
                   "Inequality gap" = create_single_line_trend_chart(
                     data = simd_measures_data()$left_data, 
                     yaxis_col = "sii", 
                     upci_col = "upci_sii",
                     lowci_col = "lowci_sii",
                     reduce_xaxis_labels = TRUE,
                     zero_yaxis = input$left_zero_axis_switch, # filter returns TRUE/FALSE
                     include_confidence_intervals = input$left_ci_switch), # filter returns TRUE/FALSE
                   
                   
                   # attributable to inequality bar chart
                   # note there is no custom function yet for stacked bar charts
                   # as this is the only one used currently in the dashboard
                   "Potential for improvement" = hchart(
                     simd_measures_data()$left_data, 
                     type = 'column', 
                     hcaes(y = value, group = measure_name, x = quintile)) |>
                     hc_plotOptions(series = list(stacking = "normal")) |>
                     hc_colors(c(phs_colors("phs-blue"), phs_colors("phs-purple"))) |>
                     hc_add_theme(theme) |>
                     hc_xAxis(title = list(text = "")) |>
                     hc_yAxis(title = list(text = ""))
      )
      
      # add options for downloaded version only
      hc <- hc |>
        hc_exporting(
          filename = chart_text()$left_chart_filename,
          chartOptions = list(
            title = list(text = chart_text()$left_chart_title, align = "left"),
            subtitle = list(text = chart_text()$left_chart_subtitle_1, align = "left"),
            caption = list(text = paste0("<b>Source: ScotPHO Profiles tool</b><br><em>Area: ", geo_selections()$areaname, "</em>"))
          )
        )
      
      hc
    })
    
    
    
    # render the chart to display on right-hand side (conditional depending on which measure was selected)
    # using the data stored within simd_measures_data()$right_data (which is also conditional depending on what measure was selected)
    output$right_chart <- renderHighchart({
      
      # only create charts if there is data available to plot
      shiny::validate(
        need(nrow(indicator_data()) > 0,
             paste0("Deprivation data are not available at ", geo_selections()$areatype, " level in this profile. Please try a different geography, of either ", 
                    paste(setdiff(c("Scotland", "Health board", "Council area"), geo_selections()$areatype), collapse = " or "), 
                    "."))
      )
      
      
      hc <- switch(input$depr_measures,
                   
                   # SIMD trend chart                        
                   "Patterns of inequality" = create_multi_line_trend_chart(
                     data = simd_measures_data()$right_data, 
                     grouping_col = "quintile", 
                     legend_position = "bottom",
                     reduce_xaxis_labels = TRUE,
                     colour_palette = "simd",
                     zero_yaxis = input$right_zero_axis_switch, # filter returns TRUE/FALSE
                     include_confidence_intervals = input$right_ci_switch, # filter returns TRUE/FALSE
                     include_average = input$right_average_switch # filter returns TRUE/FALSE
                   ),
                   
                   # RII trend chart
                   "Inequality gap" = create_single_line_trend_chart(
                     data = simd_measures_data()$right_data, 
                     yaxis_col = "rii_int", 
                     upci_col = "upci_rii_int",
                     lowci_col = "lowci_rii_int",
                     reduce_xaxis_labels = TRUE,
                     zero_yaxis = input$right_zero_axis_switch, # filter returns TRUE/FALSE
                     include_confidence_intervals = input$right_ci_switch), # filter returns TRUE/FALSE
                   
                   
                   # PAR trend chart
                   "Potential for improvement" =  create_single_line_trend_chart(
                     data = simd_measures_data()$right_data, 
                     yaxis_col = "par", 
                     reduce_xaxis_labels = TRUE,
                     include_confidence_intervals = FALSE,
                     zero_yaxis = input$right_zero_axis_switch) # filter returns TRUE/FALSE
      )
      
      # add options for downloaded version only
      hc <- hc |>
        hc_exporting(
          filename =chart_text()$right_chart_filename,
          chartOptions = list(
            title = list(text = chart_text()$right_chart_title, align = "left"),
            subtitle = list(text = chart_text()$right_chart_subtitle_1, align = "left"),
            caption = list(text = paste0("<b>Source: ScotPHO Profiles tool</b><br><em>Area: ", geo_selections()$areaname, "</em>"))
          )
        )
      
      hc
    })
    
    
    
    
    # render the data to be displayed on the left
    output$left_table <- renderReactable({
      
      reactable(simd_measures_data()$left_data,
                defaultExpanded = T,
                defaultPageSize = nrow(simd_measures_data()$left_data),
                columns = simd_measures_data()$left_table_cols
      )
      
    })
    
    
    # render the data to be displayed on the left
    output$right_table <- renderReactable({
      reactable(simd_measures_data()$right_data,
                defaultExpanded = T,
                defaultPageSize = nrow(simd_measures_data()$right_data),
                columns = simd_measures_data()$right_table_cols
      )
    })
    
    
    # about SIMD text when help button is clicked in sidebar
    observeEvent(input$simd_help, {
      showModal(modalDialog(
        title = "About SIMD",
        about_simd_quintiles
      ))
    })
    
    
    #########################.
    # Metadata ----
    #########################.
    indicator_metadata <- filter_metadata_Server("metadata", r_indicator = selected_indicator) # techdoc filtered by selected indicator 
    btn_click <- metadata_scroll_button_Server("scroll_btn") # tracking when metadata scroll button clicked 
    metadata_panel_Server("metadata_table", r_event = btn_click, r_metadata = indicator_metadata, parent_session = root_session) # panel with metadata table
    
    
    
    ###############################.
    # DOWNLOADS ----
    ###############################.
    
    # chart downloads (note these are modules)
    download_chart_mod_server(id = "save_left_chart", chart_id = ns("left_chart"))
    download_chart_mod_server(id = "save_right_chart", chart_id = ns("right_chart"))
    
    # data downloads (note these are modules)
    download_data_btns_server(id = "save_left_data", data = simd_measures_data()$left_data, file_name = paste0("ScotPHO data - ", input$depr_measures))
    download_data_btns_server(id = "save_right_data", data = simd_measures_data()$right_data, file_name = paste0("ScotPHO data - ", input$depr_measures))
    
    
  }) #close moduleServer
} # close server function