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
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        accordion(
                          open = c("indicator_filter_panel", "geo_filter_panel"), # guided tour panel closed by default
                          multiple = TRUE, # allow multiple panels to be open at once
                          
                          # accordion panel with indicator filter and definitions button
                          accordion_panel(
                            value = "indicator_filter_panel",
                            "Select an indicator", 
                                            #indicator filter (note this is a module)
                            div(id = ns("trend_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("trend_indicator_filter"), label = NULL)),
                            
                            metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section"))

                          ),
                          
                          # accordion panel with geography filters
                          accordion_panel(
                            value = "geo_filter_panel",
                            "Add area(s) to chart", 
                            
                            div(id = ns("trend_geography_wrapper"), #wrapping for tour guide

                            checkboxInput(ns("scot_switch_trends"), label = "Scotland", FALSE), # scotland checkbox filter
                            
                            # all other geography filters
                            # note these filters are enabled/disabled in the server function based on selected indicator
                            layout_columns(
                              selectizeInput(inputId = ns("hb_filter"), label = "Health Boards:", choices = hb_list, multiple = TRUE),
                              selectizeInput(inputId = ns("ca_filter"), label = "Council areas:", choices = ca_list, multiple = TRUE)
                            ),
                            
                            # police division filter (hidden unless mental health profile is selected)
                            div(id = ns("pd_panel"),
                            selectizeInput(inputId = ns("pd_filter"), label = "Police divisions:", choices = pd_list, multiple = TRUE)
                            ),
                            

                            
                            layout_columns(
                              selectizeInput(inputId = ns("hscp_filter"), label = "Health and Social Care Partnerships:", choices = hscp_list, multiple = TRUE),
                              selectizeInput(inputId = ns("adp_filter"), label = "Alcohol and Drugs Partnerships:", choices = adp_list, multiple = TRUE)
                            ),
                            
                            selectizeInput(inputId = ns("hscp_filter_2"), label = "To select a locality or intermediate zone, first select a HSC partnership:", choices = hscp_list),
                            
                            layout_columns(
                            selectizeInput(inputId = ns("locality_filter"), label = "HSC localities:", choices = character(0), multiple = TRUE),
                            selectizeInput(inputId = ns("iz_filter"), label = "Intermediate zones:", choices = character(0), multiple = TRUE)
                            )
                            
                            
                            )),
                          accordion_panel(
                            value = "help_panel",
                            title = "Get help", icon = icon("info-circle"),
                            actionLink(inputId = ns("trend_tour_button"), label = "Take a guided tour of this page")
                          )
                                                  ) # close all accordion
      ), # close sidebar

      
      # create a multi-tab card 
      div(id = ns("trend_card_wrapper"),
            navset_card_pill(
              id = ns("trend_navset_card_pill"),
              full_screen = TRUE,
        
        # charts tab -----------------------
        nav_panel("Charts",
                  value = ns("trend_chart_tab"), #id for guided tour
                  uiOutput(ns("trend_title")), # title 
                  uiOutput(ns("trend_caveats")), # caveats
                  highchartOutput(outputId = ns("trend_chart")) |> # chart
                    withSpinner() |> 
                    bslib::as_fill_carrier() 
        ), 
        
        # data tab ------------------
        nav_panel("Data", 
                  value = ns("trend_data_tab"), #id for guided tour
                  reactableOutput(ns("trend_table")) # table
        ), 
        
        # add space
        bslib::nav_spacer(),
        
        # popover with extra controls for trend chart
        bslib::nav_item(
          div(id = "trend_popover", bslib::popover(
            title = "Decide how to present data in the chart",
            chart_controls_icon(), 
            # rate/numerator toggle
            radioButtons(inputId = ns("numerator_button_trends"), label = NULL, 
                         choices = c("Rate", "Numerator"),
                         selected = "Rate"),
            # constrain y-axis to start at zero
            checkboxInput(ns("zero_trend"), label = "y-axis should include zero", value = TRUE),
            # ci switch
            checkboxInput(ns("ci_switch_trends"), label = "95% confidence intervals", FALSE),
          ))
        ),
        
        # footer with download buttons
        footer = card_footer(class = "d-flex justify-content-left",
                    div(id = ns("trend_download_chart"), download_chart_mod_ui(ns("download_trends_chart"))),
                    div(id = ns("trend_download_data"), download_data_btns_ui(ns("download_trends_data"))))
      )
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
    # Dynamic filters
    #######################################################.
    

    
    # enable/ disable geography filters depending on the selected indicator
    observe({
      req(indicator_filtered_data())
      
      # stores available areatypes, depending on what indicator was selected
      available_areatypes <- indicator_filtered_data() |>
        pull(unique(areatype))
      
      # stores available HSC localities depending on what parent area was selected
      available_localities <- indicator_filtered_data() |>
        filter(areatype == "HSC locality" & parent_area == input$hscp_filter_2) |>
        pull(unique(areaname))
      
      # stores available Intermediate zones depending on what parent area was selected
      available_izs <- indicator_filtered_data() |>
        filter(areatype == "Intermediate zone" & parent_area == input$hscp_filter_2) |>
        pull(unique(areaname))
      
      
      # If 'Health board' is available, enable hb_filter, otherwise disable it
      if("Health board" %in% available_areatypes) {
        shinyjs::enable("hb_filter")
        updateSelectizeInput(session, "hb_filter", options = list(placeholder = NULL), selected = hb_selections())
      } else {
        shinyjs::disable("hb_filter")
        updateSelectizeInput(session, "hb_filter", options = list(placeholder = "Unavailable"))
      }
      
      # If 'Council area' is available, enable ca_filter, otherwise disable it
      if("Council area" %in% available_areatypes) {
        shinyjs::enable("ca_filter")
        updateSelectizeInput(session, "ca_filter", options = list(placeholder = NULL), selected = ca_selections())        
      } else {
        shinyjs::disable("ca_filter")
        updateSelectizeInput(session, "ca_filter", options = list(placeholder = "Unavailable"))
      }
      
      # If 'HSC partnership' is available, enable hscp_filter, otherwise disable it
      if("HSC partnership" %in% available_areatypes) {
        shinyjs::enable("hscp_filter")
        updateSelectizeInput(session, "hscp_filter", options = list(placeholder = NULL), selected = hscp_selections())
      } else {
        updateSelectizeInput(session, "hscp_filter", options = list(placeholder = "Unavailable"))
        shinyjs::disable("hscp_filter")
      }
      
      # If 'Alcohol & drug partnership' is available, enable adp_filter, otherwise disable it
      if("Alcohol & drug partnership" %in% available_areatypes) {
        shinyjs::enable("adp_filter")
        updateSelectizeInput(session, "adp_filter", options = list(placeholder = NULL), selected = adp_selections())
      } else {
        shinyjs::disable("adp_filter")
        updateSelectizeInput(session, "adp_filter", options = list(placeholder = "Unavailable"))
      }
      
      # If 'HSC Locality' or 'Intermediate zone' is available, enable parent area filter (hscp_filter_2), otherwise disable it
      if("HSC locality" %in% available_areatypes | "Intermediate zone" %in% available_areatypes) {
        shinyjs::enable("hscp_filter_2")
        updateSelectizeInput(session, "hscp_filter_2", label = "To select a locality or intermediate zone, first select an HSC partnership:")
        
      } else{
        shinyjs::disable("hscp_filter_2")
      }
      
      # If 'HSC Locality' is available, enable the filter otherwise disable it
      if("HSC locality" %in% available_areatypes){
        shinyjs::enable("locality_filter")
        updateSelectizeInput(session, "locality_filter", choices = available_localities, options = list(placeholder = NULL), selected = locality_selections())
      } else {
        updateSelectizeInput(session, "locality_filter", options = list(placeholder = "Unavailable"), selected = character(0))
        shinyjs::disable("locality_filter")
      }
      
      # If 'Intermediate zone' is available, enable the filter otherwise disable it
      if("Intermediate zone" %in% available_areatypes){
        shinyjs::enable("iz_filter")
        updateSelectizeInput(session, "iz_filter", choices = available_izs, options = list(placeholder = NULL), selected = iz_selections())
      } else {
        updateSelectizeInput(session, "iz_filter", options = list(placeholder = "Unavailable"), selected = character(0))
        shinyjs::disable("iz_filter")
      }
      
      # hide the pd filter if any profile other than 'mental health' has been selected
      # as police divisions are only available for a small subset of MH indicators
      # If MH profile has been selected and 'Police division' is available for selected indicator, enable pd_filter, otherwise disable it
      if(selected_profile()$full_name != "Mental Health"){
        hide("pd_panel")
      } else {
        show("pd_panel")
        if("Police division" %in% available_areatypes) {
          shinyjs::enable("pd_filter")
          updateSelectizeInput(session, "pd_filter", options = list(placeholder = NULL), selected = pd_selections())
        } else {
          shinyjs::disable("pd_filter")
          updateSelectizeInput(session, "pd_filter", options = list(placeholder = "Unavailable"))
        }
      }
      
    })
    
    
    
    # remove globally selected areaname from available areas in dropdowns
    observe({
      if(geo_selections()$areatype == "Health board"){
        updateSelectizeInput(session, "hb_filter", choices = hb_list[hb_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "Council area"){
        updateSelectizeInput(session, "ca_filter", choices = ca_list[ca_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "HSC partnership"){
        updateSelectizeInput(session, "hscp_filter", choices = hscp_list[hscp_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype == "Alcohol & drug partnership"){
        updateSelectizeInput(session, "adp_filter", choices = adp_list[adp_list!=geo_selections()$areaname])
      }
      else if(geo_selections()$areatype %in% c("Intermediate zone", "HSC locality")){
        updateSelectizeInput(session, "hscp_filter_2", selected = geo_selections()$parent_area)
      }
      else if(geo_selections()$areatype == "Police division"){
        updateSelectizeInput(session, "pd_filter", choices = pd_list[pd_list!=geo_selections()$areaname])
      }
      
    })
    
    
    # Clear what was previously selected from the filters if a user changes selection from geography filter (otherwise they remain selected)
    observeEvent(geo_selections(), {
      
      # clear the filters
      updateSelectizeInput(session, "hb_filter", selected = character(0))
      updateSelectizeInput(session, "ca_filter", selected = character(0))
      updateSelectizeInput(session, "hscp_filter", selected = character(0))
      updateSelectizeInput(session, "adp_filter", selected = character(0))
      updateSelectizeInput(session, "iz_filter", selected = character(0))
      updateSelectizeInput(session, "locality_filter", selected = character(0))
      updateSelectizeInput(session, "pd_filter", selected = character(0))
      
      
      # clear the reactive vals
      hb_selections(NULL)
      ca_selections(NULL)
      hscp_selections(NULL)
      adp_selections(NULL)
      locality_selections(NULL)
      iz_selections(NULL)
      pd_selections(NULL)
      
      
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
    #or when a geography with no available indicators is selected
    observe({
      req(filtered_data())
      if(geo_selections()$areaname == "Scotland" | !(geo_selections()$areatype %in% unique(filtered_data()$areatype))  ){
        shinyjs::hide("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = FALSE)
      } else if(geo_selections()$areaname != "Scotland" | geo_selections()$areatype %in% unique(filtered_data()$areatype)){
        shinyjs::show("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = TRUE)
      }
    })
    
    
    # dynamically enabling/disabling the  numerator/rate radio buttons depending on selected indicator
    # this is required because for some indicators, we only publish the rate so numerator not always available
    observeEvent(selected_indicator(), {
      req(indicator_filtered_data())
      # check the first row of the data filtered by selected indicator
      # if the numerator column is empty ensure the selected option to plot in the trend chart is 'rate'
      # and disable the filter
      if(is.na(indicator_filtered_data()$numerator[1])){
        updateRadioButtons(session, "numerator_button_trends", selected = "Rate")
        shinyjs::disable("numerator_button_trends")
        # otherwise enable the filter to allow users to toggle between numerator/rate
      } else{
        shinyjs::enable("numerator_button_trends")
      }
      
    })
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # create reactive objects to store selected geographies
    hb_selections <- reactiveVal()
    ca_selections <- reactiveVal()
    hscp_selections <- reactiveVal()
    adp_selections <- reactiveVal()
    locality_selections <- reactiveVal()
    iz_selections <- reactiveVal()
    pd_selections <- reactiveVal()
    
    
    # update the reactive objects whenever selections are made
    # from each of the geography filters
    observeEvent(input$hb_filter, {
      hb_selections(input$hb_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$ca_filter, {
      ca_selections(input$ca_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$hscp_filter, {
      hscp_selections(input$hscp_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$adp_filter, {
      adp_selections(input$adp_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$locality_filter, {
      locality_selections(input$locality_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$iz_filter, {
      iz_selections(input$iz_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$pd_filter, {
      pd_selections(input$pd_filter)
    }, ignoreNULL = FALSE)
    
    
    selected_indicator <- indicator_filter_mod_server("trend_indicator_filter",
                                                      filtered_data,
                                                      geo_selections,
                                                      selected_profile)
    
    # create reactive data - filtering by selected indicator
    indicator_filtered_data <- reactive({
      req(filtered_data())
      filtered_data() |>
        filter(indicator == selected_indicator())
    })
    
    
    
    
    # create reactive dataset filtered by selected indicator and geography area
    # change y variable depending on whether rate/numerator is selected
    trend_data <- reactive({
      req(indicator_filtered_data())
      
      df <- indicator_filtered_data() |> # take reactive df already filtered by selected indicator
        filter(
          (areaname == geo_selections()$areaname & areatype == geo_selections()$areatype) | # filter by global geography selection
            (areaname %in% input$hb_filter & areatype == "Health board")|  # filter by selected health boards
            (areaname %in% input$ca_filter & areatype == "Council area")| # filter by selected council areas
            (areaname %in% input$adp_filter & areatype == "Alcohol & drug partnership") |# filter by selected adps
            (areaname %in% input$hscp_filter & areatype == "HSC partnership")| #filter by selected hscps
            (areaname %in% input$iz_filter & areatype == "Intermediate zone" & parent_area == input$hscp_filter_2)| # filter by selected IZs
            (areaname %in% input$locality_filter & areatype == "HSC locality" & parent_area == input$hscp_filter_2)| # filter by selected HSC localities
            (areaname %in% input$pd_filter & areatype == "Police division") # filter by selected police divisions
        )
      
      # if scotland is selected from the global geography filter OR the scotland checkbox has been ticked
      # also filter by scotland
      if(input$scot_switch_trends == TRUE){
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
      
      
      df
      
    })
    
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    output$trend_title <- renderUI({
      req(trend_data())
      
      # create dynamic text if no indicators available for selected profile
      # and geography
      shiny::validate(
        need( nrow(trend_data()) > 0, "No indicators available for this profile and area type. Please select another.")
      )
      
      # display 3 x titles
      div(
        tags$h5(selected_indicator(), class = "chart-header"), # selected indicator
        tags$h6(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis)), # time range
        tags$p(trend_data()$type_definition[1]) # type definiton
      )
      
    })
    

    #############################################.
    # Charts/tables ----
    #############################################.
    
    # trend chart
    output$trend_chart <- renderHighchart({
      req(trend_data())
      
      type_definition <- case_when(
        input$numerator_button_trends == "Numerator" ~ "Number",
        input$numerator_button_trends == "Rate" ~ paste0(unique(trend_data()$type_definition)))

      create_multi_line_trend_chart(
        data = trend_data(),
        xaxis_col = "trend_axis", 
        yaxis_col = "y", 
        upci_col = "upci",
        lowci_col = "lowci",
        grouping_col = "areaname",
        legend_position = "top",
        zero_yaxis = input$zero_trend,
        include_confidence_intervals = input$ci_switch_trends,
        colour_palette = "multi"
      ) |>
        hc_exporting(
          filename = paste0("ScotPHO trend - ", selected_indicator()),
          chartOptions = list(
            title = list(text = paste0(selected_indicator())),
            subtitle = list(text = paste0(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis)))
          )
        )
    })
    
    
    # data table
    output$trend_table <- renderReactable({
      req(trend_data())
      
      data <- trend_data() |>
        select(areatype, areaname, trend_axis, y)

      reactable(data,
                columns = list(
                  areatype = colDef(name = "Area type"),
                  areaname = colDef(name = "Area name"),
                  trend_axis = colDef(name = "Period"),
                  y = colDef(name = input$numerator_button_trends)
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
        ns("trend_chart"), # trend chart 
        title = "Chart Tab",
        description = "The trend chart is designed to explore how a single indicator has changed over time for one or more geographical areas. <br>
        Use the mouse to hover over a data point and see detailed information on its value, time period and area.",
        tab_id = ns("trend_navset_card_pill"), 
        tab = ns("trend_chart_tab")
      )$
      step(
        "trend_popover", # popover icon
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
