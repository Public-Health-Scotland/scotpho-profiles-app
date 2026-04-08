### to do
# write small example app at end of script

###########################################################################.
# MODULE: trend_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.


trend_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      full_screen = FALSE,
      sidebar = sidebar(
        width = 500,
        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
        accordion(
          open = c("indicator_filter_panel", "geo_filter_panel"), # guided tour panel closed by default
          multiple = TRUE, # allow multiple panels to be open at once
          
          # accordion panel with indicator filter and definitions button
          accordion_panel(
            value = "indicator_filter_panel",
            "Select an indicator", 
            # indicator filter and metadata scroll button (note these are modules)
            div(id = ns("trend_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("trend_indicator_filter"), label = NULL)),
            div(id = ns("trend_scroll_button"), metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section")))
          ), # close accordion panel
          
          # accordion panel with geography filters
          accordion_panel(
            value = "geo_filter_panel",
            "Add area(s) to chart", 
            div(
              id = ns("trend_geography_wrapper"), # wrapping in div for tour guide
              
              # scotland checkbox filter
              checkboxInput(ns("scot_switch_trends"), label = "Scotland", FALSE), 
              
              # health board/council area filters 
              layout_columns(
                selectizeInput(inputId = ns("hb_filter"), label = "Health Boards:", choices = hb_list, multiple = TRUE),
                selectizeInput(inputId = ns("ca_filter"), label = "Council areas:", choices = ca_list, multiple = TRUE)
              ),
              
              # police division filter (hidden unless mental health profile is selected)
              hidden(
                selectizeInput(inputId = ns("pd_filter"), label = "Police divisions:", choices = pd_list, multiple = TRUE)
              ),
              
              # HSCP/ADP filters
              layout_columns(
                selectizeInput(inputId = ns("hscp_filter"), label = "Health and Social Care Partnerships:", choices = hscp_list, multiple = TRUE),
                selectizeInput(inputId = ns("adp_filter"), label = "Alcohol and Drugs Partnerships:", choices = adp_list, multiple = TRUE)
              ),
              
              # IZ/locality filters with corresponding parent filter 
              # Note unlike the rest of area filters, the choices for these are set dynamically in the modules
              # server function, depending on parent area selected from 'parent_filter'
              selectizeInput(inputId = ns("parent_filter"), label = "To select a locality or intermediate zone, first select a HSC partnership:", choices = hscp_list),
              layout_columns(
                selectizeInput(inputId = ns("locality_filter"), label = "HSC localities:", choices = NULL, multiple = TRUE),
                selectizeInput(inputId = ns("iz_filter"), label = "Intermediate zones:", choices = NULL, multiple = TRUE)
              )
            ) # close div
          ), # close accordion panel
          
          # accordion panel with guided tour button
          accordion_panel(
            value = "help_panel",
            title = "Get help", 
            icon = icon("info-circle"),
            actionLink(inputId = ns("trend_tour_button"), label = "Take a guided tour of this page")
          ) # close accordion panel
        ) # close all accordion
      ), # close sidebar
      
      
      # multi-tab card
      # note: wrapping in div for guided tour in order to highlight whole card as the 'id' argument
      # of navset_card_pill relates to the navset rather than the whole card itself
      div(
        id = ns("trend_card_wrapper"),
        navset_card_pill(
          id = ns("trend_navset_card_pill"),
          full_screen = TRUE,
          
          # chart tab 
          nav_panel(
            title = "Charts",
            value = ns("trend_chart_tab"), # id for guided tour
            # titles and subtitles
            div(
              h3(textOutput(ns("title")), class = "chart-header"),
              textOutput(ns("subtitle_1")),
              textOutput(ns("subtitle_2"))
            ),
            
            # radio button to toggle between numerator/rate
            radioButtons(
              inputId = ns("numerator_button_trends"), 
              label = NULL,
              choices = c("Rate", "Numerator"),
              inline = TRUE,
              selected = "Rate"
            ),
            
            # chart (note this is a module )
            multi_trend_chart_mod_UI(ns("chart"))
            
          ), # close chart tab of card
          
          # data tab 
          nav_panel(
            title = "Data", 
            value = ns("trend_data_tab"), #id for guided tour
            data_table_mod_UI(ns("tbl")) # note this is module 
          ), 
          
          # add space
          bslib::nav_spacer(),
          
          # popover with extra controls for trend chart (note this is module)
          chart_controls_mod_UI(id = ns("controls"), controls = c(ci_switch = FALSE, zero_yaxis_switch = TRUE)),
          
          # footer with download buttons (note these are modules)
          footer = card_footer(
            class = "d-flex justify-content-left",
            div(id = ns("trend_download_chart"), download_chart_mod_ui(ns("download_trends_chart"))),
            div(id = ns("trend_download_data"), download_data_btns_ui(ns("download_trends_data"))))
        )# close trend tab of card
      ), # close navset card pill
      
      # accordion panel with metadata table 
      div(id = ns("metadata_section"), metadata_panel_UI(ns("metadata_table")))
    ) # close layout sidebar
  ) # close taglist
} # close ui function 



#######################################################.
## MODULE SERVER ----
#######################################################.

# id = unique id
# filtered_data =  reactive dataframe where data has already been filtered by profile 
# geo_selections = reactive values that come main server script contain the geonames & geotypes to display
# selected_profile = name of reactive value storing selected profile from main server script


trend_mod_server <- function(id, filtered_data, geo_selections, selected_profile, root_session) {
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    #######################################################.
    # Reactive values 
    #######################################################.
    
    # For tracking which areatypes are available depending on selected indicator 
    valid_areatypes <- reactiveValues(
      hb = TRUE,
      ca = TRUE,
      pd = TRUE,
      hscp = TRUE,
      adp = TRUE,
      parent = TRUE,
      iz = TRUE,
      locality = TRUE
    )
    
    # For saving a users selections in the event that they had selected area(s) 
    # from the trend filters, that became unavailable when they switched to a different indicator
    # saving their selection before disabling the filter ensures their selections can automatically 
    # be re-appliedwhen that areatype becomes available again 
    saved_selections <- reactiveValues(
      hb = NULL,
      ca = NULL,
      pd = NULL,
      hscp = NULL,
      adp = NULL,
      parent = NULL,
      iz = NULL,
      locality = NULL
    )
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Setting up the first filter selection on the trends tab ------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    # Ensure that the globally selected area is pre-selected from the area filters on the trend sub-tab, 
    # so that there's always atleast 1 line drawn on the trends tab
    # (and also clear any previous selections in the event that user switches globally selected area)
    observeEvent(geo_selections(), {
      
      # clear any previously selected areas from the trend tab filters (if applicable)
      updateSelectizeInput(session, "hb_filter", selected = character(0))
      updateSelectizeInput(session, "ca_filter", selected = character(0))
      updateSelectizeInput(session, "hscp_filter", selected = character(0))
      updateSelectizeInput(session, "adp_filter", selected = character(0))
      updateSelectizeInput(session, "iz_filter", selected = character(0))
      updateSelectizeInput(session, "locality_filter", selected = character(0))
      updateSelectizeInput(session, "pd_filter", selected = character(0))
      updateSelectizeInput(session, "parent_filter", selected = character(0))
      updateSelectizeInput(session, "iz_filter", selected = character(0))
      updateSelectizeInput(session, "locality_filter", selected = character(0))
      
      # clear the reactive vals (if applicable)
      saved_selections$hb <- NULL
      saved_selections$ca <- NULL
      saved_selections$pd <- NULL
      saved_selections$hscp <- NULL
      saved_selections$adp <- NULL
      saved_selections$parent <- NULL
      saved_selections$iz <- NULL
      saved_selections$locality <- NULL
      
      
      # If scotland is selected, tick the scotland checkbox, otherwise untick it
      if(geo_selections()$areatype == "Scotland"){
        updateCheckboxInput(session, "scot_switch_trends", value = TRUE)
      } else {
        updateCheckboxInput(session, "scot_switch_trends", value = FALSE)
      }
      
      # If a health board is selected, select that board from the hb_filter
      if(geo_selections()$areatype == "Health board"){
        updateSelectizeInput(inputId = "hb_filter", selected = geo_selections()$areaname)
      }
      
      # If a Council area is selected, select that council from the ca_filter
      if(geo_selections()$areatype == "Council area"){
        updateSelectizeInput(inputId = "ca_filter", selected = geo_selections()$areaname)
      }
      
      # If a Polive divisions is selected, select that council from the pd_filter
      if(geo_selections()$areatype == "Polive division"){
        updateSelectizeInput(inputId = "pd_filter", selected = geo_selections()$areaname)
      }
      
      # If a HSCP is selected, select that hscp from the hscp_filter
      if(geo_selections()$areatype == "HSC partnership"){
        updateSelectizeInput(inputId = "hscp_filter", selected = geo_selections()$areaname)
      }
      
      # If an ADP area is selected, select that adp from the adp_filter
      if(geo_selections()$areatype == "Alcohol & drug partnership"){
        updateSelectizeInput(inputId = "adp_filter", selected = geo_selections()$areaname)
      }
      
      # If an IZ or HSC locality is selected, a. select the globally selected parent area from the parent_filter
      if(geo_selections()$areatype %in% c("HSC locality", "Intermediate zone")){
        updateSelectizeInput(inputId = "parent_filter", selected = geo_selections()$parent_area)
        
        # then b. dynamically set to the locality/iz filter choices to those that fall within selected parent area
        # and select the globally selected iz/locality
        if(geo_selections()$areatype == "HSC locality"){
          locality_choices <- ind_data()$areaname[ind_data()$parent_area == geo_selections()$parent_area & ind_data()$areatype == "HSC locality"]
          updateSelectizeInput(inputId = "locality_filter", choices = locality_choices, selected = geo_selections()$areaname)
        } else {
          iz_choices <- ind_data()$areaname[ind_data()$parent_area == geo_selections()$parent_area & ind_data()$areatype == "Intermediate zone"]
          updateSelectizeInput(inputId = "iz_filter", choices = iz_choices, selected = geo_selections()$areaname)
        }
      }
    })
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Indicator data -----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # dynamically updates choices for indicator filter depending on globally
    # selected profile/geography and returns name of the selected indicator
    # each time user makes new selection (note this is module)
    selected_indicator <- indicator_filter_mod_server("trend_indicator_filter",
                                                      filtered_data,
                                                      geo_selections,
                                                      selected_profile)
    
    
    # take profile-filtered dataset passed to this module (filtered in apps main server script)
    # and further filter by selected indicator using value returned from module above
    ind_data <- reactive({
      req(selected_indicator()) # dont run until selected indicator is ready
      
      filtered_data() |>
        filter(indicator == selected_indicator())
    })
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dynamically enabling and disabling area filters ------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    # each time the indicator filtered dataset above is updated (ind_data()), the code below is triggered
    # It checks which areatypes are available for the selected indicator and updates the 'valid_areatypes' rv object
    # note the globally selected areatype will always be valid, because the indicator choices are
    # based on what's available for the globally selected area/profile combination
    
    # These values are then used to enable/disable the area filters accordingly
    observeEvent(ind_data(), {
      
      # check available areatypes from indicator filtered data
      areatypes <- unique(ind_data()$areatype)
      
      # update reactive values with TRUE or FALSE
      valid_areatypes$hb <- ifelse("Health board" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$ca <- ifelse("Council area" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$pd <- ifelse("Police division" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$hscp <- ifelse("HSC partnership" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$adp <- ifelse("Alcohol & drug partnership" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$iz <- ifelse("Intermediate zone" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$locality <- ifelse("HSC locality" %in% areatypes, TRUE, FALSE) 
      valid_areatypes$parent <- ifelse(any(c("Intermediate zone", "HSC locality") %in% areatypes), TRUE, FALSE) 
      
    })
    
    
    # track changes to the reactive values objects updated in step above. Any time 
    # a change is observed, enable/disable the filter accordingly.
    # In the event that a filter is being disabled, if anything had been selected from that filter,
    # it is saved, so it can be automatically re-applied when the filter becomes enabled again
    
    #handling health board filter
    observeEvent(valid_areatypes$hb, {
      if(isTRUE(valid_areatypes$hb)){
        enable("hb_filter")
        updateSelectizeInput(inputId = "hb_filter", options = list(placeholder = NULL), selected = saved_selections$hb)
      } else {
        saved_selections$hb <- isolate(input$hb_filter)
        disable("hb_filter")
        updateSelectizeInput(inputId = "hb_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    # handling council area filter 
    observeEvent(valid_areatypes$ca, {
      if(isTRUE(valid_areatypes$ca)){
        enable("ca_filter")
        updateSelectizeInput(inputId = "ca_filter", options = list(placeholder = NULL), selected = saved_selections$ca)
      } else {
        disable("ca_filter")
        saved_selections$ca <- isolate(input$ca_filter)
        updateSelectizeInput(inputId = "ca_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    
    # handling police divison filter 
    observeEvent(valid_areatypes$pd, {
      if(isTRUE(valid_areatypes$pd)){
        enable("pd_filter")
        updateSelectizeInput(inputId = "pd_filter", options = list(placeholder = NULL), selected = saved_selections$pd)
      } else {
        disable("pd_filter")
        saved_selections$pd <- isolate(input$pd_filter)
        updateSelectizeInput(inputId = "pd_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    
    # handling ADP filter 
    observeEvent(valid_areatypes$adp, {
      if(isTRUE(valid_areatypes$adp)){
        enable("adp_filter")
        updateSelectizeInput(inputId = "adp_filter", options = list(placeholder = NULL), selected = saved_selections$adp)
      } else {
        disable("adp_filter")
        saved_selections$adp <- isolate(input$adp_filter)
        updateSelectizeInput(inputId = "adp_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    # handling HSCP filter 
    observeEvent(valid_areatypes$hscp, {
      if(isTRUE(valid_areatypes$hscp)){
        enable("hscp_filter")
        updateSelectizeInput(inputId = "hscp_filter", options = list(placeholder = NULL), selected = saved_selections$hscp)
      } else {
        disable("hscp_filter")
        saved_selections$hscp <- isolate(input$hscp_filter)
        updateSelectizeInput(inputId = "hscp_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    # handling IZ filter  
    observeEvent(valid_areatypes$iz, {
      if(isTRUE(valid_areatypes$iz)){
        enable("iz_filter")
        updateSelectizeInput(inputId = "iz_filter", options = list(placeholder = NULL), selected = saved_selections$iz)
      } else {
        disable("iz_filter")
        saved_selections$iz <- isolate(input$iz_filter)
        updateSelectizeInput(inputId = "iz_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    
    # handling locality filter 
    observeEvent(valid_areatypes$locality, {
      if(isTRUE(valid_areatypes$locality)){
        enable("locality_filter")
        updateSelectizeInput(inputId = "locality_filter", options = list(placeholder = NULL), selected = saved_selections$locality)
      } else {
        disable("locality_filter")
        saved_selections$locality <- isolate(input$locality_filter)
        updateSelectizeInput(inputId = "locality_filter", options = list(placeholder = "Unavailable"))
      }
    })
    
    
    #handling IZ/locality choices in response to input$parent being updated
    observeEvent(input$parent_filter, {
      req(isTRUE(valid_areatypes$parent))
      
      iz_choices <- ind_data()$areaname[ind_data()$parent_area == input$parent_filter & ind_data()$areatype == "Intermediate zone"]
      locality_choices <- ind_data()$areaname[ind_data()$parent_area == input$parent_filter & ind_data()$areatype == "HSC locality"]
      
      updateSelectizeInput(inputId = "iz_filter", choices = iz_choices)
      updateSelectizeInput(inputId = "locality_filter", choices = locality_choices)
      
      
    })
    
    
    
    
    
    
    # # disable CI checkbox when numerator is selected
    # observeEvent(input$numerator_button_trends, {
    #   if(input$numerator_button_trends == "Numerator") {
    #     disable("ci_switch_trends")
    #     updateCheckboxInput(session, "ci_switch_trends", value = FALSE)
    #   } else if (input$numerator_button_trends == "Rate") {
    #     enable("ci_switch_trends")
    #   }
    # })
    
    
    
    
    
    
    
    
    # # dynamically enabling/disabling the  numerator/rate radio buttons depending on selected indicator
    # # this is required because for some indicators, we only publish the rate so numerator not always available
    # observeEvent(selected_indicator(), {
    #   req(indicator_filtered_data())
    #   # check the first row of the data filtered by selected indicator
    #   # if the numerator column is empty ensure the selected option to plot in the trend chart is 'rate'
    #   # and disable the filter
    #   if(is.na(indicator_filtered_data()$numerator[1])){
    #     updateRadioButtons(session, "numerator_button_trends", selected = "Rate")
    #     shinyjs::disable("numerator_button_trends")
    #     # otherwise enable the filter to allow users to toggle between numerator/rate
    #   } else{
    #     shinyjs::enable("numerator_button_trends")
    #   }
    #   
    # })
    
    
    
    
    # create reactive dataset filtered by selected indicator and geography area
    # change y variable depending on whether rate/numerator is selected
    area_data <- reactive({
      
      ind_data() |> # take reactive df already filtered by selected indicator
        filter(
          (input$scot_switch_trends == TRUE & areatype == "Scotland") |
            (areaname %in% input$hb_filter & areatype == "Health board")|  # filter by selected health boards
            (areaname %in% input$ca_filter & areatype == "Council area")| # filter by selected council areas
            (areaname %in% input$adp_filter & areatype == "Alcohol & drug partnership") |# filter by selected adps
            (areaname %in% input$hscp_filter & areatype == "HSC partnership")| #filter by selected hscps
            (areaname %in% input$iz_filter & areatype == "Intermediate zone" & parent_area == input$parent_filter)| # filter by selected IZs
            (areaname %in% input$locality_filter & areatype == "HSC locality" & parent_area == input$parent_filter)| # filter by selected HSC localities
            (areaname %in% input$pd_filter & areatype == "Police division") # filter by selected police divisions
        ) |>
        
        # arrange data by year
        arrange(year)
      
    })
    
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    
    output$title <- renderText({
      selected_indicator()
    })
    
    output$subtitle_1 <- renderText({
      # create dynamic text if no indicators available for selected profile and geography
      shiny::validate(
        need(selected_indicator(), "No indicators available for this profile and area type. Please select another.")
      )
      
      min_year <- first(area_data()$trend_axis)
      max_year <- last(area_data()$trend_axis)
      
      paste(min_year, "to", max_year)
      
    })
    
    output$subtitle_2 <- renderText({
      area_data()$type_definition[1]
    })
    
    
    #############################################.
    # Charts/tables ----
    #############################################.
    
    
    settings <- chart_controls_mod_server("controls")
    
    multi_trend_chart_mod_Server(id = "chart", r_data = area_data, group_col = "areaname", r_chart_controls = settings)
    
    # # trend chart
    # output$trend_chart <- renderHighchart({
    #   req(trend_data())
    #   
    #   type_definition <- case_when(
    #     input$numerator_button_trends == "Numerator" ~ "Number",
    #     input$numerator_button_trends == "Rate" ~ paste0(unique(trend_data()$type_definition)))
    # 
    #   create_multi_line_trend_chart(
    #     data = trend_data(),
    #     xaxis_col = "trend_axis", 
    #     yaxis_col = "y", 
    #     upci_col = "upci",
    #     lowci_col = "lowci",
    #     grouping_col = "areaname",
    #     legend_position = "top",
    #     zero_yaxis = input$zero_trend,
    #     include_confidence_intervals = input$ci_switch_trends,
    #     colour_palette = "multi"
    #   ) |>
    #     hc_exporting(
    #       filename = paste0("ScotPHO trend - ", selected_indicator()),
    #       chartOptions = list(
    #         title = list(text = paste0(selected_indicator())),
    #         subtitle = list(text = paste0(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis)))
    #       )
    #     )
    # })
    # 
    
    
    
    # data table
    output$trend_table <- renderReactable({
      req(trend_data())
      
      data <- trend_data() |>
        select(areatype, areaname, trend_axis, y, upci, lowci)
      
      reactable(data,
                columns = list(
                  areatype = colDef(name = "Area type"),
                  areaname = colDef(name = "Area name"),
                  trend_axis = colDef(name = "Period"),
                  y = colDef(name = input$numerator_button_trends),
                  upci = colDef(name = "Upper CI"),
                  lowci = colDef(name = "Lower CI")
                )
      )
      
    })
    
    #########################.
    # Metadata ----
    #########################.
    indicator_metadata <- filter_metadata_Server("metadata", r_indicator = selected_indicator) # techdoc filtered by selected indicator 
    btn_click <- metadata_scroll_button_Server("scroll_btn") # tracking when metadata scroll button clicked 
    metadata_panel_Server("metadata_table", r_event = btn_click, r_metadata = indicator_metadata, parent_session = root_session) # panel with metadata table
    
    
    
    
    
    ###################################.
    # Downloads ----
    ###################################.
    
    # server for chart and data downloads
    download_chart_mod_server(id = "download_trends_chart", chart_id = ns("trend_chart"))
    
    download_data_btns_server(id = "download_trends_data", 
                              data = trend_data, 
                              file_name = "Trends_ScotPHO_data_extract", 
                              selected_columns = c("code", 
                                                   "areatype", 
                                                   "areaname", 
                                                   "indicator", 
                                                   "type_definition", 
                                                   "definition_period" = "def_period",
                                                   "trend_axis",
                                                   "numerator", 
                                                   "measure", 
                                                   "upper_confidence_interval" = "upci", # rename column 
                                                   "lower_confidence_interval" = "lowci")) # rename column 
    
    
    ###########################################.
    # Guided tour ----
    ###########################################.
    
    #Set up trend steps
    guide_trend <- Cicerone$
      new(
        padding = 8
      )$
      step(
        ns("trend_card_wrapper"), # trend chart
        title = "Chart Tab",
        description = "The trend chart is designed to explore how a single indicator has changed over time for one or more geographical areas. <br>
        Use the mouse to hover over a data point and see detailed information on its value, time period and area."
      )$
      step(
        ns("trend_popover"), # popover icon
        "Adjust Chart Settings",
        "Click here to see chart settings. Confidence intervals (95%) can be added to the chart. They are shown as shaded areas and give an indication of the precision of a rate or percentage. The width of a confidence interval is related to sample size.
        The chart can also be switched from a measure (e.g. rate or percentage) to actual numbers (e.g. the number of births with a healthy birthweight)."
      )$
      step(
        ns("trend_navset_card_pill"), # tabs within the multi-tab card
        title = "Other tabs",
        description = "You can switch between viewing the chart or data for your selected indicator using the buttons highlighted."
      )$
      step(
        ns("trend_indicator_filter_wrapper"), #indicator filter
        "Indicator Filter",
        "First select an indicator.<br>
        The indicator list has been filtered based on profile and area type selected at the top of the page.<br>
        The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
        position = "bottom"
      )$
      step(
        ns("trend_scroll_button"), #Scroll to metadata button
        "Scroll to Metadata Button",
        "Click here to scroll to the metadata panel below.<br>
        It contains information about the selected indicator, including indicator definition, 
        data source, notes and caveats and links to relevant publications and pages on the ScotPHO website. "
      )$
      step(
        ns("trend_geography_wrapper"), # all geography filters
        "Geography Filters",
        "Add one or more geographical areas of any type to the chart to compare with your selected geography.<br>
        There may be some indicators for which data is not available for the full time series or at a particular geography level.<br>
        If an area type other than Scotland is selected in the global options, the Scotland checkbox can be clicked to add or remove the trend line.",
        position = "right"
      )$
      step(
        ns("trend_download_chart"), #downlaod chart button
        "Download Chart Button",
        "Click here to download the chart with all selected geographies as a PNG.",
        position = "bottom"
      )$
      step(
        ns("trend_download_data"), #download data button
        "Download Data Button",
        "Click here to download the selected data as a CSV, RDS or JSON file.",
        position = "left"
      )
    
    #initiate the guide
    guide_trend$init()
    
    #when guided tour button is clicked, start the guide
    observeEvent(input$trend_tour_button, {
      guide_trend$start()
    })
    
    
  }) # close moduleServer
} # close server function