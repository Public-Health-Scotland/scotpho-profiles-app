##########################################################################.
# MODULE: Population module ---- 
# prepares the nav_panel layout displaying the population group splits
##########################################################################.


#######################################################.
## MODULE UI ----
################################################Fse#######.

# id = unique id 
pop_groups_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      # sidebar for filters ----
      sidebar = sidebar(width = 300,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        
                        
                        # indicator filter (note this is a module)
                        indicator_filter_mod_ui(ns("indicator_filter")),
                        
                        # button to scroll to metadata
                        metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section")),
                        
                        # filter to select pop split (set choices to NULL 
                        # as they are updated dynamically on the server side, depending on selected indicator)
                        selectInput(
                          inputId = ns("split_filter"),
                          label = "Select equality split:",
                          selectize = TRUE,
                          choices = NULL
                        )
      ), # close sidebar
      
      
      layout_column_wrap(
        width = 1/2,
        
        # Bar chart card ----
          bslib::navset_card_pill(
            height = 600,
            full_screen = TRUE,
            
            # tab 1: bar chart 
            bslib::nav_panel("Chart",
                             uiOutput(ns("pop_rank_title")), # title 
                             highchartOutput(ns("pop_rank_chart")) |> # chart 
                               withSpinner() |> 
                               bslib::as_fill_carrier() 
            ),
            
            # tab 2: data table
            bslib::nav_panel("Table",
                             reactableOutput(ns("pop_rank_table")) # table
            ),

            bslib::nav_spacer(),
            
            # extra controls for bar chart 
            bslib::nav_item(
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("rank_avg_switch"), label = "Include average", FALSE),
                checkboxInput(ns("ci_switch"), label = " include confidence intervals", FALSE),
                selectInput(ns("pop_years_filter"), label = "select year", choices = NULL)
              )
            ),
            
            # card footer - download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                        download_chart_mod_ui(ns("save_pop_rankchart")),
                        download_data_btns_ui(ns("pop_rank_download")))

          ), # close bar chart card
        
        
        ######  based on deprivation trend card addeded "pop_" to distinguish the two

          bslib::navset_card_pill(
            height = 600,
            full_screen = TRUE,
            
            # tab 1: trend chart 
            bslib::nav_panel("Chart",
                             uiOutput(ns("pop_trend_title")), # title
                             highchartOutput(ns("pop_trend_chart")) |> # chart
                               withSpinner() |> 
                               bslib::as_fill_carrier() 

            ),
            # tab 2: data table 
            bslib::nav_panel("Table",
                             reactableOutput(ns("pop_trend_table"))
            ),
            
            bslib::nav_spacer(),
            
            # extra controls for filters
            bslib::nav_item(
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                # add average line
                checkboxInput(ns("trend_avg_switch"), label = "Include average", FALSE),
                # constrain y-axis to start at zero
                checkboxInput(ns("zero_popgp"), label = "y-axis should include zero", value = TRUE),
                # too many CI for age split, removed at this stage
                checkboxInput(ns("trend_ci_switch"), label = " include confidence intervals", FALSE) 
              )
            ),
            # card footer - download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                        download_chart_mod_ui(ns("save_pop_trendchart")),
                        download_data_btns_ui(ns("pop_trend_download")))
          )# close trend card
      ), # close layout column wrap
      
      # accordion panel with metadata table 
      div(id = ns("metadata_section"), metadata_panel_UI(ns("metadata_table")))
      
    ) # close sidebar layout
  ) # close taglist
} # close ui function 



#######################################################.
## MODULE SERVER----
#######################################################.


pop_groups_server <- function(id, dataset, geo_selections, selected_profile, root_session) {
  moduleServer(id, function(input, output, session) {
    
    # required for chart downloads
    ns <- session$ns
    
    
    ##########################.
    ## Dynamic filters -----
    #########################.
    
    ## update choices for population split filter, depending on what indicator was selected, include sort on split name
    observe({

      available_splits <- dataset() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype) |>
        arrange(split_name) |>
        pull(unique(split_name))
      
      updateSelectInput(session, inputId = "split_filter", choices = available_splits)
      
      
    })
    
    # update years choices for bar chart filter, depending on indicator selected
    observe({
      
      available_years <- dataset() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
        arrange(desc(year)) |>
        pull(unique(def_period))
      
      updateSelectInput(session, inputId = "pop_years_filter",
                        choices = available_years, selected = available_years[1])
    })
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # generate list of indicators (from the simd indicators dataset) available 
    selected_indicator <- indicator_filter_mod_server(id = "indicator_filter", dataset, geo_selections, selected_profile)

    
    # creates trend data
    pop_trend_data <- reactive({
      dataset() |>
        filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>  # filter by selected geography
        filter(indicator == selected_indicator() & split_name == input$split_filter) |> # filter by selected indicator and selected split
        # create total column
        group_by(year) |>
        mutate(total = ifelse(any(split_value == "Total"), measure[split_value == "Total"], NA)) |>
        #mutate(total = measure[split_value == "Total"])|>
        ungroup() |>
        filter(split_value != "Total") |>
        arrange(year)
    })
    
    # create single year data for the bar chart 
    pop_rank_data <- reactive({
      pop_trend_data() |>
        filter(def_period == input$pop_years_filter)
    })
    
    #######################################################.
    ## dynamic text  ----
    #######################################################.
    
    output$pop_rank_title <- renderUI({
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(pop_trend_data()) > 0, "No indicators available")
      )
      
      # if data is available display chart title
      div(
        tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
        tags$h6(pop_rank_data()$trend_axis[1]), # time period 
        tags$p(pop_rank_data()$type_definition[1]) # measure type
      )
    })
    
    # need to add pop-trend title stuff
    
    output$pop_trend_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(pop_trend_data()) > 3, "There are insufficent data points for this indicator to create a trend chart")
      )
      
      # if data is available display chart title
      div(
        tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
        tags$h6(first(pop_trend_data()$trend_axis)," to ",last(pop_trend_data()$trend_axis)), # time period 
        tags$p(pop_trend_data()$type_definition[1]) # measure type
      )
    })
    
    ############################################.
    # charts -----
    ############################################.
    
    # pop rank bar chart  ---------------
    
    output$pop_rank_chart <- renderHighchart({
      
      shiny::validate(
        need( nrow(pop_rank_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      
      create_bar_chart(
        data = pop_rank_data(),
        xaxis_col = "split_value",
        yaxis_col = "measure",
        include_confidence_intervals = input$ci_switch,
        upci_col = "upci",
        lowci_col = "lowci",
        horizontal = TRUE,
        colour_palette = "single",
        include_average = input$rank_avg_switch
      ) |>

        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO ", selected_indicator(), " split by ", input$split_filter),
          chartOptions = list(
            title = list(text = paste0(selected_indicator(), " split by ", input$split_filter)),
            subtitle = list(text = paste0(pop_rank_data()$trend_axis[1])),
            yAxis = list(title = list(text = paste0(pop_rank_data()$type_definition[1])))
          )
        )
      
      
    }) # end pop_rank_chart
    
    # pop trend bar chart  ---------------
    
    output$pop_trend_chart <- renderHighchart({
      
      
      create_multi_line_trend_chart(
        data = pop_trend_data(),
        xaxis_col = "trend_axis", 
        yaxis_col = "measure", 
        grouping_col = "split_value",
        legend_position = "bottom",
        reduce_xaxis_labels = TRUE,
        zero_yaxis = input$zero_popgp,
        include_confidence_intervals = input$trend_ci_switch,
        chart_theme = theme,
        colour_palette = "multi",
        include_average = input$trend_avg_switch
      ) |>
     
        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO trend - ", selected_indicator(), " split by ", input$split_filter),
          chartOptions = list(
            title = list(text = paste0(selected_indicator(), " split by ", input$split_filter)),
            subtitle = list(text = paste0(first(pop_trend_data()$trend_axis)," to ",last(pop_trend_data()$trend_axis))),
            yAxis = list(title = list(text = paste0(pop_trend_data()$type_definition[1])))
          )
        )
      

    }) #end  pop trend chart
    
    
    
    ##########################################.
    # Tables ----
    ##########################################.
    
    # rank data table -------
    output$pop_rank_table <- renderReactable({
      
      data <- pop_rank_data() |>
        select(def_period, split_value, measure)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data),
                # rename some columns 
                columns = list(
                  def_period = colDef(name = "Time Period"),
                  split_value = colDef(name = "Population Group"),
                  measure = colDef(name = "Measure")
                )
      )
    })
    
    # trend data table -------
    output$pop_trend_table <- renderReactable({
      
      data <- pop_trend_data() |>
        select(def_period, split_value, measure)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data),
                # rename some columns 
                columns = list(
                  def_period = colDef(name = "Time Period"),
                  split_value = colDef(name = "Population Group"),
                  measure = colDef(name = "Measure")
                )
      )
      
      
    })
    
    
    #########################.
    # Metadata ----
    #########################.
    indicator_metadata <- filter_metadata_Server("metadata", r_indicator = selected_indicator) # techdoc filtered by selected indicator 
    btn_click <- metadata_scroll_button_Server("scroll_btn") # tracking when metadata scroll button clicked 
    metadata_panel_Server("metadata_table", r_event = btn_click, r_metadata = indicator_metadata, parent_session = root_session) # panel with metadata table
    
    
    
    ### need to create pop trend table
    
    ############################################.
    # Downloads  ----
    ############################################.
    download_chart_mod_server(id = "save_pop_rankchart", chart_id = ns("pop_rank_chart"))
    download_data_btns_server(id = "pop_rank_download", data = pop_trend_data, file_name = "Popgroup_ScotPHO_data_extract")
    
    download_chart_mod_server(id = "save_pop_trendchart", chart_id = ns("pop_trend_chart"))
    download_data_btns_server(id = "pop_trend_download", data = pop_trend_data, file_name = "Popgroup_ScotPHO_data_extract")
  } # module server
  )# module server
} # pop groups server
