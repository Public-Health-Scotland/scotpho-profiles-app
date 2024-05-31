### to do
# add content to help button
# write small example app at end of script
# add description of module at top of script
# change x-axis from year to trend_axis
# format tooltip



trend_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      height = 600,
      width = 350,
      padding = 20,
      sidebar = sidebar(width = 400,
                        
                        layout_column_wrap(
                          1/2,
                          actionButton(ns("help"), label = "Help", icon = icon("question")),
                          indicator_definition_btn_ui(ns("inequalities_ind_def")),
                        ),
                        
                        indicator_filter_mod_ui(ns("trend_indicator_filter")),
                        HTML("<b>Your selected area is:</b>"),
                        textOutput(ns("selected_indicator_name")),
                        HTML("<b>Select areas to plot and compare with your selected area:</b>",
                             "You can select multiple areas of any geography type."),
                        checkboxInput(ns("scot_switch_trends"), label = "Scotland", FALSE),
                        selectInput(inputId = ns("hb_filter"), label = "Select Health Board(s):", choices = hb_list, multiple = TRUE),
                        selectInput(inputId = ns("ca_filter"), label = "Select Council Area(s):", choices = ca_list, multiple = TRUE),
                        selectInput(inputId = ns("hscp_filter"), label = "Select Health and Social Care Partnership(s):", choices = hscp_list, multiple = TRUE),
                        selectInput(inputId = ns("adp_filter"), label = "Select Alcohol and Drugs Partnership(s):", choices = adp_list, multiple = TRUE),
                        selectInput(inputId = ns("hscp_filter_2"), label = "To select a locality or intermediate zone, first select an HSC partnership:", choices = hscp_list),
                        child_geography_filters_mod_ui(id = ns("child_imz"), label = "Intermediate Zone"),
                        child_geography_filters_mod_ui(id = ns("child_locality"), label = "HSC Locality"),
                        
                        HTML("<b>Decide how to present data in the chart</b>"),
                        radioButtons(inputId = ns("numerator_button_trends"), label = NULL, 
                                     choices = c("Rate", "Numerator"),
                                     selected = "Rate"),
                        checkboxInput(ns("ci_switch_trends"), label = "95% confidence intervals", FALSE),
                        
                        
      ),
      
      
      card(
        height = 600,
        full_screen = TRUE,
        card_body(withSpinner(highchartOutput(outputId = ns("trend_chart")))),
        card_footer(class = "d-flex justify-content-between",
                    download_chart_mod_ui(ns("download_trends_chart")),
                    download_data_btns_ui(ns("download_trends_data")))
      )
      
    ) # close layout sidebar
  ) # close taglist
}


trend_mod_server <- function(id, filtered_data, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    # return selected indicator
    selected_indicator <- indicator_filter_mod_server("trend_indicator_filter",
                                                      filtered_data = filtered_data,
                                                      geo_selections = geo_selections)
    
    # create reactive data - filtering by selected indicator
    filtered_df <- reactive({
      filtered_data() |>
        filter(indicator == selected_indicator())
    })
    
    
    # create reactive object that stores available areatypes, depending on what indicator was selected
    available_areatypes <- reactive({
      filtered_df() |>
        pull(unique(areatype))
    })
    
    # create reactive object that stores selected HSCP for child geography filters
    
    HSCP_selection <- reactive({
      list(
        parent_area = input$hscp_filter_2 # correct input id
      )
    })
    
    # update filters if areatype is in list above 
    observe({
      
      # If 'Health board' is available, enable hb_filter, otherwise disable it
      if("Health board" %in% available_areatypes()) {
        shinyjs::enable("hb_filter")
        updateSelectInput(session, "hb_filter", label = "Select Health Board(s):")
      } else {
        shinyjs::disable("hb_filter")
        updateSelectInput(session, "hb_filter", label = "Select Health Board(s): (not available)")
      }
      
      # If 'Council area' is available, enable ca_filter, otherwise disable it
      if("Council area" %in% available_areatypes()) {
        shinyjs::enable("ca_filter")
        updateSelectInput(session, "ca_filter", label = "Select Council Area(s):")        
      } else {
        shinyjs::disable("ca_filter")
        updateSelectInput(session, "ca_filter", label = "Select Council Area(s): (not available)")
      }
      
      if("HSC partnership" %in% available_areatypes()) {
        shinyjs::enable("hscp_filter")
        updateSelectInput(session, "hscp_filter", label = "Select Health and Social Care Partnership(s):")
      } else {
        updateSelectInput(session, "hscp_filter", label = "Select Health and Social Care Partnership(s): (not available)")
        shinyjs::disable("hscp_filter")
      }
      
      if("Alcohol & drug partnership" %in% available_areatypes()) {
        shinyjs::enable("adp_filter")
        updateSelectInput(session, "adp_filter", label = "Select Alcohol and Drug Partnership(s):")
      } else {
        shinyjs::disable("adp_filter")
        updateSelectInput(session, "adp_filter", label = "Select Alcohol and Drug Partnership(s): (not available)")
      }
      
      if("HSC locality" %in% available_areatypes() | "Intermediate zone" %in% available_areatypes()) {
        shinyjs:: enable("hscp_filter_2")
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
    
    #server logic for dynamic text displaying selected geog from global
    output$selected_indicator_name = renderText({
      geo_selections()$areaname
    })
    
    #server logic for indicator definition button
    # required to make selected indicator name available to definitions module
    indicator_definition_btn_server("inequalities_ind_def", selected_indicator = selected_indicator) 
    
    
    # server logic for dynamic filters for child geographies based on HSCP selected
    IMZ_selection <- child_geography_filters_mod_server(id = "child_imz", filtered_data = filtered_df, HSCP_selection = HSCP_selection, child_areatype = "Intermediate zone", geo_selections = geo_selections)
    Locality_selection <- child_geography_filters_mod_server(id = "child_locality", filtered_data = filtered_df, HSCP_selection = HSCP_selection, child_areatype = "HSC locality", geo_selections = geo_selections)
    
    
    # info to display when user clicks help button
    observeEvent(input$help, {
      showModal(modalDialog(
        title = "How to interpret these trends",
        tagList(
          paste0("Text helping user interpret line chart"),
          br(),)
      ))
    })
    
    # server logic for about indicators button
    navigation_button_modSERVER("about_indicators_trend", nav_id = "Indicator Definitions", parent_session = session)
    
    # server for chart and data downloads
    download_chart_mod_server(id = "download_trends_chart", chart_id = session$ns("trend_chart"))
    download_data_btns_server(id = "download_trends_data", data = trend_chart_data())
    
    
    # disable CI checkbox when numerator is selected
    observeEvent(input$numerator_button_trends, {
      if(input$numerator_button_trends == "Numerator") {
        disable("ci_switch_trends")
        updateCheckboxInput(session, "ci_switch_trends", value = FALSE)
      } else if (input$numerator_button_trends == "Rate") {
        enable("ci_switch_trends")
      }
    })
    
    # disable Scotland checkbox when Scotland selected in global options
    observe({
      if(geo_selections()$areaname == "Scotland"){
        disable("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = FALSE)
      } else if(geo_selections()$areaname != "Scotland"){
        enable("scot_switch_trends")
        updateCheckboxInput(session, "scot_switch_trends", value = TRUE)
      }
    })
    
    # create reactive dataset filtered by selected indicator and geography area
    # change y variable depending on whether rate/numerator is selected
    trend_chart_data_global <- reactive({
      filtered_df() |>
        filter(areaname == geo_selections()$areaname) |> 
        mutate(y = case_when(input$numerator_button_trends == "Numerator" ~ numerator,
                             input$numerator_button_trends == "Rate" ~ measure)) |> 
        arrange(year)
    })
    
    # create reactive dataset for scotland level data
    scot_trend_chart_data <- reactive({
      filtered_df() |>
        filter(areaname == "Scotland") |> 
        mutate(y = case_when(input$numerator_button_trends == "Numerator" ~ numerator,
                             input$numerator_button_trends == "Rate" ~ measure)) |>  
        arrange(year)
    })
    
    # reactive dataset for comparator filter selections
    trend_chart_data_comparators <- reactive({
      filtered_df() |> 
        filter(areaname %in% input$hb_filter | areaname %in% input$ca_filter | areaname %in% input$adp_filter | areaname %in% input$hscp_filter |areaname %in% IMZ_selection() | areaname %in% Locality_selection()) |> 
        mutate(y = case_when(input$numerator_button_trends == "Numerator" ~ numerator,
                             input$numerator_button_trends == "Rate" ~ measure)) |> 
        arrange(year)
      
    })
    
    # trend chart
    output$trend_chart <- renderHighchart({
      
      
      # define objects for chart titles and labels
      chart_title <- paste0(selected_indicator())
      selected_area <- paste0(geo_selections()$areaname)
      #selected_comparator <- paste0(trend_chart_data_comparators()$areaname)
      type_definition <- case_when(
        input$numerator_button_trends == "Numerator" ~ "Number",
        input$numerator_button_trends == "Rate" ~ paste0(unique(filtered_df()$type_definition)))
      
      # create highchart object
      chart <- highchart() %>%
        hc_add_series(trend_chart_data_global(),
                      type = "line",
                      hcaes(x = year,
                            y = y,
                            group = areaname)) %>%
        hc_add_series(trend_chart_data_comparators(),
                      type = "line",
                      hcaes(x = year,
                            y = y,
                            group = areaname)) |> 
        
        
        # rename titles and format legend
        hc_title(text = chart_title) %>%
        hc_subtitle(text = type_definition) %>%
        hc_xAxis(title = "") %>%
        hc_yAxis(title = list(text = type_definition)) %>%
        hc_legend(align = "left", verticalAlign = "top") %>%
        
        # format tooltip
        hc_tooltip(headerFormat = "",
                   shared = TRUE) %>%
        
        # set theme (defined in global script)
        hc_add_theme(chart_theme)
      
      
      # add confidence intervals if box is checked
      if(input$ci_switch_trends == TRUE) {
        
        chart <- chart %>% 
          hc_add_series(
            trend_chart_data(),
            type = "arearange",
            name = "95% confidence interval",
            linked_to = ":previous",
            hcaes(x = year, low = lowci, high = upci),
            color = hex_to_rgba("grey", 0.2),
            zIndex = -1, # plots the CI series behind the line series
            marker = list(enabled = FALSE, # removes the markers for the CI series
                          states = list(
                            hover = list(
                              enabled = FALSE))))
        
      } else {
        chart
      }
      
      # add Scotland trend if box is checked and isn't already the selected area
      if(input$scot_switch_trends == TRUE 
         # && isTRUE(input$areaname != "Scotland) # can't get this to work
      ) {
        
        chart <- chart %>% 
          hc_add_series(
            scot_trend_chart_data(),
            type = "line",
            hcaes(x = year,
                  y = y),
            name = "Scotland")
        
        
        # also add Scotland CIs if box is checked
        if(input$ci_switch_trends) {
          
          chart <- chart %>%
            hc_add_series(
              scot_trend_chart_data(),
              type = "arearange",
              name = "95% confidence interval",
              linked_to = "Scotland",
              hcaes(x = year, low = lowci, high = upci),
              fillOpacity = 0.2,
              zIndex = -1, # plots the CI series behind the line series
              marker = list(enabled = FALSE, # removes the markers for the CI series
                            states = list(
                              hover = list(
                                enabled = FALSE))))
          
        } else {
          chart
        }
        
      } else {
        chart
      }
      
      chart
    })
    
  }
  )
}