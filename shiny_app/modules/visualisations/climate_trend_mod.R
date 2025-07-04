
###########################################################################.
# MODULE: climate trend_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.


climate_trend_mod_ui <- function(id) {
  ns <- NS(id)
    page_sidebar(
      
      # sidebar with filters
      sidebar = sidebar(
        # sidebar settings 
        width = 500,
        open = list(mobile = "always-above"),
        
        # accordion settings
        accordion(
          open = TRUE,
          multiple = TRUE,
          
          # panel with indicator filter 
          accordion_panel(
            "Select an indicator", 
            indicator_filter_mod_ui(ns("trend_indicator_filter")),
            metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section"))
            ),
          
          # panel with geography filters 
          accordion_panel(
            "Add area(s) to chart", 
            
            # Scotland checkbox 
            checkboxInput(
              inputId = ns("scot_switch_trends"), 
              label = "Scotland", 
              FALSE # unticked by default
              ),
            
            layout_columns(
              # health board filter
              selectizeInput(
                inputId = ns("hb_filter"), 
                label = "Health Boards:", 
                choices = hb_list, 
                multiple = TRUE
                ),
              # council area filter 
              selectizeInput(
                inputId = ns("ca_filter"), 
                label = "Council areas:", 
                choices = ca_list, 
                multiple = TRUE
                )
            )
          )
        ) # close accordion
      ),
      
      # multi-tab card
      navset_card_pill(
        full_screen = TRUE,
        
        # chart tab
        nav_panel(
          title = "Charts",
          uiOutput(ns("trend_title")), # title 
          highchartOutput(ns("trend_chart")) |>
            withSpinner() |> 
            as_fill_carrier() 
            ), 
        
        # data tab
        nav_panel(
          title = "Data", 
          reactableOutput(ns("trend_table"))
          ), 
        
        # add space 
        nav_spacer(),
        
        # chart controls 
        nav_item(
          popover(
            title = "Chart controls",
            chart_controls_icon(),
            # numerator/rate switch
            radioButtons(
              inputId = ns("numerator_button_trends"), 
              label = NULL, 
              choices = c("Rate", "Numerator"),
              selected = "Rate"
              ),
            # start y-axis at 0 switch
            checkboxInput(
              inputId = ns("zero_trend"), 
              label = "Start y-axis at 0", 
              value = TRUE # swtiched on by default
              ),
            # add confidence intervals switch
            checkboxInput(
              inputId = ns("ci_switch_trends"), 
              label = "95% confidence intervals", 
              FALSE # switched off by default 
              )
            )
          ),
        footer = card_footer(
          class = "d-flex justify-content-left",
          download_chart_mod_ui(ns("download_trends_chart")), # chart download 
          download_data_btns_ui(ns("download_trends_data")) # data download
          )
      ), # close multi-tab card
      
      # metadata panel
      div(
        id = ns("metadata_section"), 
        metadata_panel_UI(ns("metadata_table"))
        ) 
    ) # close layout sidebar

} # close ui function 



#######################################################.
## MODULE SERVER ----
#######################################################.

# id = unique id
# filtered_data =  reactive dataframe where data has already been filtered by profile 
# geo_selections = reactive values that come main server script contain the geonames & geotypes to display
# selected_profile = name of reactive value storing selected profile from main server script


climate_trend_mod_server <- function(id, filtered_data, geo_selections, selected_profile, root_session) {
  moduleServer(id, function(input, output, session) {
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # update indicator filter ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # dynamically updated indicator choices depending on selected profile and geography 
    # and return name of selected indicator 
    selected_indicator <- indicator_filter_mod_server(
      id = "trend_indicator_filter",
      filtered_data,
      geo_selections,
      selected_profile
      )
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # indicator filtered data ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # further filter profile filtered dataset by selected indicator 
    indicator_filtered_data <- reactive({
      req(filtered_data())
      filtered_data() |>
        filter(indicator == selected_indicator())
    })
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # update geography filters ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # dynamically enable/disable geography filters depending
    # on what geographies are available for the selected indicator 

    observe({

      
      # stores available areatypes, depending on what indicator was selected
      available_areatypes <- unique(indicator_filtered_data()$areatype)

      
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
     
    })
    
    
    
    # Clear what was previously selected from the filters if a user changes selection 
    # from global geography filter (otherwise they remain selected)
    observeEvent(geo_selections(), {
      
      # clear the filters
      updateSelectizeInput(session, "hb_filter", selected = character(0))
      updateSelectizeInput(session, "ca_filter", selected = character(0))
      
      
      # clear the reactive vals
      hb_selections(NULL)
      ca_selections(NULL)
 
    })
    
    
    # disable Scotland checkbox when Scotland already selected in global options
    #or when a geography with no available indicators is selected
    observeEvent(geo_selections(), {
      if(geo_selections()$areaname == "Scotland" | !(geo_selections()$areatype %in% unique(filtered_data()$areatype))  ){
        shinyjs::hide("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = FALSE)
      } else if(geo_selections()$areaname != "Scotland" | geo_selections()$areatype %in% unique(filtered_data()$areatype)){
        shinyjs::show("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = TRUE)
      }
    })
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # update chart controls ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # disable CI checkbox when numerator is selected
    observeEvent(input$numerator_button_trends, {
      if(input$numerator_button_trends == "Numerator") {
        disable("ci_switch_trends")
        updateCheckboxInput(session, "ci_switch_trends", value = FALSE)
      } else if (input$numerator_button_trends == "Rate") {
        enable("ci_switch_trends")
      }
    })
    

    
    
    # dynamically enabling/disabling thenumerator/rate radio buttons depending on selected indicator
    # this is required because for some indicators, we only publish the rate so numerator not always available
    observeEvent(selected_indicator(), {
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

    
    # update the reactive objects whenever selections are made
    # from each of the geography filters
    observeEvent(input$hb_filter, {
      hb_selections(input$hb_filter)
    }, ignoreNULL = FALSE)
    
    observeEvent(input$ca_filter, {
      ca_selections(input$ca_filter)
    }, ignoreNULL = FALSE)


    

    
    
    
    
    # create reactive dataset filtered by selected indicator and geography area
    # change y variable depending on whether rate/numerator is selected
    trend_data <- reactive({
      req(indicator_filtered_data())
      
      df <- indicator_filtered_data() |> # take reactive df already filtered by selected indicator
        filter(
          (areaname == geo_selections()$areaname & areatype == geo_selections()$areatype) | # filter by global geography selection
            (areaname %in% input$hb_filter & areatype == "Health board")|  # filter by selected health boards
            (areaname %in% input$ca_filter & areatype == "Council area") # filter by selected council areas
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
    indicator_metadata <- filter_metadata_Server("metadata", metadata = climate_techdoc, r_indicator = selected_indicator) # techdoc filtered by selected indicator 
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

    
    
  }) # close moduleServer
} # close server function
