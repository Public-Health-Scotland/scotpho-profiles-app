### to do
# add content to help button
# write small example app at end of script
# add description of module at top of script


trend_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = 600,
      
      # sidebar for filters ------------------
      sidebar = sidebar(width = 415,
                        accordion(
                          open = c("ind_filter_panel"), # have the panel with indicator filter open by default
                          multiple = TRUE, # allow multiple panels to be open at once
                          
                          # accordion panel with 2 x help buttons
                          accordion_panel(
                            value = "help_panel",
                            "Help", icon = bsicons::bs_icon("question-circle"),
                            layout_columns(
                              actionButton(ns("help"), label = "Help", icon = icon("question"), class = "btn-sm"),
                              indicator_definition_btn_ui(ns("inequalities_ind_def")),
                            )
                          ),
                          
                          # accordion panel with indicator filter 
                          accordion_panel(
                            value = "ind_filter_panel",
                            "Indicator filter", icon = bsicons::bs_icon("filter"),
                            indicator_filter_mod_ui(ns("trend_indicator_filter"))
                          ),
                          
                          # accordion panel with geography filters
                          accordion_panel(
                            value = "geo_filter_panel",
                            "Geography comparator filters (optional)", icon = bsicons::bs_icon("sliders"),
                            textOutput(ns("geo_instructions")),  # explanation of how to use geography filters
                            br(),
                            checkboxInput(ns("scot_switch_trends"), label = "Scotland", FALSE), # scotland checkbox filter
                            
                            # all other geography filters
                            # note these filters are enabled/disabled in the server function based on selected indicator
                            layout_columns(
                              selectInput(inputId = ns("hb_filter"), label = "Health Boards:", choices = hb_list, multiple = TRUE),
                              selectInput(inputId = ns("ca_filter"), label = "Council areas:", choices = ca_list, multiple = TRUE)
                            ),
                            
                            layout_columns(
                              selectInput(inputId = ns("hscp_filter"), label = "Health and Social Care Partnerships:", choices = hscp_list, multiple = TRUE),
                              selectInput(inputId = ns("adp_filter"), label = "Alcohol and Drugs Partnerships:", choices = adp_list, multiple = TRUE)
                            ),
                            
                            selectInput(inputId = ns("hscp_filter_2"), label = "To select a locality or intermediate zone, first select a HSC partnership:", choices = hscp_list),
                            
                            layout_columns(
                              child_geography_filters_mod_ui(id = ns("child_imz"), label = "Intermediate Zones"),
                              child_geography_filters_mod_ui(id = ns("child_locality"), label = "HSC Localities")
                            )
                          )
                        ) # close accordion
      ), # close sidebar
      
      # create a multi-tab card 
      navset_card_pill(
        full_screen = TRUE,
        
        # charts tab -----------------------
        nav_panel("charts",
                  uiOutput(ns("trend_title")), # title 
                  highchartOutput(outputId = ns("trend_chart")) # chart
        ), 
        
        # data tab ------------------
        nav_panel("Data",
                  reactableOutput(ns("trend_table")) # table
        ), 
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          bslib::popover(
            title = "Decide how to present data in the chart",
            bsicons::bs_icon("gear", size = "1.7em"),
            # rate/numerator toggle
            radioButtons(inputId = ns("numerator_button_trends"), label = NULL, 
                         choices = c("Rate", "Numerator"),
                         selected = "Rate"),
            # ci switch
            checkboxInput(ns("ci_switch_trends"), label = "95% confidence intervals", FALSE),
          )
        ),
        
        # footer with download buttons
        card_footer(class = "d-flex justify-content-between",
                    download_chart_mod_ui(ns("download_trends_chart")),
                    download_data_btns_ui(ns("download_trends_data")))
      ) # close navset card pill
    ) # close layout sidebar
  ) # close taglist
} # close ui function 



trend_mod_server <- function(id, filtered_data, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    
    
    #######################################################
    # Dynamic filters
    #######################################################
    
    # enable/ disable geography filters and update thee filters labels, 
    # depending on what indicator was selected 
    observe({
      
      # stores available areatypes, depending on what indicator was selected
      available_areatypes <- indicator_filtered_data() |>
        pull(unique(areatype))
      
      
      # If 'Health board' is available, enable hb_filter, otherwise disable it
      if("Health board" %in% available_areatypes) {
        shinyjs::enable("hb_filter")
        updateSelectInput(session, "hb_filter", label = "Health Boards:")
      } else {
        shinyjs::disable("hb_filter")
        updateSelectInput(session, "hb_filter", label = "Health Boards (not available)")
      }
      
      # If 'Council area' is available, enable ca_filter, otherwise disable it
      if("Council area" %in% available_areatypes) {
        shinyjs::enable("ca_filter")
        updateSelectInput(session, "ca_filter", label = "Council Areas:")        
      } else {
        shinyjs::disable("ca_filter")
        updateSelectInput(session, "ca_filter", label = "Council Areas: (not available)")
      }
      
      # If 'HSC partnership' is available, enable hscp_filter, otherwise disable it
      if("HSC partnership" %in% available_areatypes) {
        shinyjs::enable("hscp_filter")
        updateSelectInput(session, "hscp_filter", label = "Health and Social Care Partnerships:")
      } else {
        updateSelectInput(session, "hscp_filter", label = "Health and Social Care Partnerships: (not available)")
        shinyjs::disable("hscp_filter")
      }
      
      # If 'Alcohol & drug partnership' is available, enable adp_filter, otherwise disable it
      if("Alcohol & drug partnership" %in% available_areatypes) {
        shinyjs::enable("adp_filter")
        updateSelectInput(session, "adp_filter", label = "Alcohol and Drug Partnerships:")
      } else {
        shinyjs::disable("adp_filter")
        updateSelectInput(session, "adp_filter", label = "Alcohol and Drug Partnerships: (not available)")
      }
      
      # If 'HSC Locality' or 'Intermediate zone' is available, enable parent area filter (hscp_filter_2), otherwise disable it
      if("HSC locality" %in% available_areatypes | "Intermediate zone" %in% available_areatypes) {
        shinyjs::enable("hscp_filter_2")
        updateSelectInput(session, "hscp_filter_2", label = "To select a locality or intermediate zone, first select an HSC partnership:")
        
      } else{
        shinyjs::disable("hscp_filter_2")
        updateSelectInput(session, "hscp_filter_2", label = "To select a locality or intermediate zone, first select an HSC partnership: (not available)")
        
      }
      
    })
    
    
    
    # remove globally selected areaname from available areas in dropdowns
    observe({
      if(geo_selections()$areatype == "Health board"){
        updateSelectInput(session, "hb_filter", choices = hb_list[hb_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "Council area"){
        updateSelectInput(session, "ca_filter", choices = ca_list[ca_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "HSC partnership"){
        updateSelectInput(session, "hscp_filter", choices = hscp_list[hscp_list_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "Alcohol & drug partnership"){
        updateSelectInput(session, "adp_filter", choices = adp_list[adp_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "Intermediate zone"){
        updateSelectInput(session, "hscp_filter_2", selected = geo_selections()$parent_area)
      }
      else if(geo_selections()$areatype == "HSC locality"){
        updateSelectInput(session, "hscp_filter_2", selected = geo_selections()$parent_area)
      }
      
    })
    
    
    # Clear what was prevously selected from the filters if a user changes
    # selection from global geography filter (otherwise they remain selected)
    observeEvent(geo_selections()$areaname, {
      updateSelectInput(session, "hb_filter", selected = character(0))
      updateSelectInput(session, "ca_filter", selected = character(0))
      updateSelectInput(session, "hscp_filter", selected = character(0))
      updateSelectInput(session, "hscp_filter_2", selected = character(0))
      updateSelectInput(session, "adp_filter", selected = character(0))
      updateSelectInput(session, "child_imz", selected = character(0))
      updateSelectInput(session, "child_locality", selected = character(0))
    })
    
    
    
    # disable CI checkbox when numerator is selected
    observeEvent(input$numerator_button_trends, {
      if(input$numerator_button_trends == "Numerator") {
        disable("ci_switch_trends")
        updateCheckboxInput(session, "ci_switch_trends", value = FALSE)
      } else if (input$numerator_button_trends == "Rate") {
        enable("ci_switch_trends")
      }
    })
    
    
    
    # disable Scotland checkbox when Scotland already selected in global options
    observe({
      if(geo_selections()$areaname == "Scotland"){
        shinyjs::hide("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = FALSE)
      } else if(geo_selections()$areaname != "Scotland"){
        shinyjs::show("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = TRUE)
      }
    })
    
    
    
    #######################################################
    ## Reactive data / values ----
    #######################################################
    
    # store selected indicator (note this is a module)
    selected_indicator <- indicator_filter_mod_server("trend_indicator_filter",
                                                      filtered_data = filtered_data,
                                                      geo_selections = geo_selections)
    
    # create reactive data - filtering by selected indicator
    indicator_filtered_data <- reactive({
      filtered_data() |>
        filter(indicator == selected_indicator())
    })
    
    
    # create reactive object that stores selected HSCP for child geography filters
    # note: this 'parent area' value is passed to the module that creates the choices for the IZ/HSCL filter
    HSCP_selection <- reactive({
      list(
        parent_area = input$hscp_filter_2 
      )
    })
    
    
    # server logic for dynamic filters for child geographies based on HSCP selected
    IMZ_selection <- child_geography_filters_mod_server(id = "child_imz", filtered_data = indicator_filtered_data, HSCP_selection = HSCP_selection, child_areatype = "Intermediate zone", geo_selections = geo_selections)
    Locality_selection <- child_geography_filters_mod_server(id = "child_locality", filtered_data = indicator_filtered_data, HSCP_selection = HSCP_selection, child_areatype = "HSC locality", geo_selections = geo_selections)
    
    
    
    # create reactive dataset filtered by selected indicator and geography area
    # change y variable depending on whether rate/numerator is selected
    trend_data <- reactive({
      
      df <- indicator_filtered_data() |> # take reactive df already filtered by selected indicator
        filter(
          (areaname == geo_selections()$areaname & areatype == geo_selections()$areatype) | # filter by global geography selection
            areaname %in% input$hb_filter |  # filter by selected health boards
            areaname %in% input$ca_filter | # filter by selected council areas
            areaname %in% input$adp_filter |# filter by selected adps
            areaname %in% input$hscp_filter | #filter by selected hscps
            areaname %in% IMZ_selection() | # filter by selected IZs
            areaname %in% Locality_selection() # filter by selected HSC localities
        )
      
      # if scotland is selected from the global geography filter OR the scotland checkbox has been ticked
      # also filter by scotland
      if(geo_selections()$areatype == "Scotland" | input$scot_switch_trends == TRUE){
        scotland <- indicator_filtered_data() |>
          filter(areaname == "Scotland")
        
        # add scotland rows onto already filtered df
        df <- df |>
          rbind(scotland)
      }
      
      # create a y-axis column depending on whether user selects numerator or rate
      df <- df |>
        mutate(y = case_when(input$numerator_button_trends == "Numerator" ~ numerator,
                             input$numerator_button_trends == "Rate" ~ measure)) |>
        
        # arrange data by year
        arrange(year)
    })
    
    
    
    #######################################################
    ## dynamic text  ----
    #######################################################
    
    output$trend_title <- renderUI({
      
      # display 3 x titles
      tagList(
        tags$h5(selected_indicator()), # selected indicator
        tags$h6(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis)), # time range
        tags$p(trend_data()$type_definition[1]) # type definiton
      )
      
    })
    
    #server logic for indicator definition button - shows indidcator definition when definition button clicked 
    # (note this is a module)
    indicator_definition_btn_server("inequalities_ind_def", selected_indicator = selected_indicator) 
    
    
    # info to display when user clicks help button
    observeEvent(input$help, {
      showModal(modalDialog(
        title = "How to interpret these trends",
        tagList(
          paste0("Text helping user interpret line chart"),
          br(),)
      ))
    })
    
    
    output$geo_instructions <- renderText({
      paste0("Select areas to plot and compare with ", geo_selections()$areaname,". You can select multiple areas of any available geography type.")
    })
    
    
    ############################################
    # Charts/tables 
    #############################################
    
    # trend chart
    output$trend_chart <- renderHighchart({
      
      
      type_definition <- case_when(
        input$numerator_button_trends == "Numerator" ~ "Number",
        input$numerator_button_trends == "Rate" ~ paste0(unique(trend_data()$type_definition)))
      
      
      # create highchart object
      chart <- hchart(trend_data(), 
                      "line", 
                      hcaes(x = trend_axis, y = measure, group = areaname)) |>
        hc_plotOptions(series=list(animation=FALSE)) |>
        hc_xAxis(title = "") |>
        hc_yAxis(gridLineWidth = 0) |> # remove gridlines 
        hc_yAxis(title = list(text = type_definition)) |>
        hc_legend(align = "left", verticalAlign = "top") |>
        hc_chart(backgroundColor = 'white') |>
        
        # format tooltip
        hc_tooltip(headerFormat = "",
                   crosshairs = TRUE,
                   shared = TRUE) |>
        
        
        # title for downloaded version
        hc_exporting(
          chartOptions = list(
            title = list(text = selected_indicator())
          )
        )
      
      
      # add confidence intervals if box is checked
      if(input$ci_switch_trends == TRUE) {
        
        chart <- chart |>
          hc_add_series(
            trend_data(),
            type = "arearange",
            name = "95% confidence interval",
            linked_to = ":previous",
            hcaes(x = trend_axis, low = lowci, high = upci, group = areaname),
            fillOpacity = 0.2,
            enableMouseTracking = FALSE, # removes from tooltip
            showInLegend = FALSE, # don't need CI labels in legend
            zIndex = -1, # plots the CI series behind the line series
            marker = list(enabled = FALSE, # removes the markers for the CI series
                          states = list(
                            hover = list(
                              enabled = FALSE)))) 
        
      }
      
      chart
    })
    
    
    # data table
    output$trend_table <- renderReactable({
      
      data <- trend_data() |>
        select(areatype, areaname, trend_axis, y)
      
      reactable(data,
                columns = list(
                  areatype = colDef(name = "Area type"),
                  areaname = colDef(name = "Area name"),
                  trend_axis = colDef(name = "Period"),
                  y = colDef(name = input$numerator_button_trends)
                ))
      
      
    })
    
    
    
    ###################################
    # Downloads
    ###################################
    
    # server for chart and data downloads
    download_chart_mod_server(id = "download_trends_chart", chart_id = session$ns("trend_chart"))
    download_data_btns_server(id = "download_trends_data", data = trend_chart_data_global)
    
    
  }
  )
}