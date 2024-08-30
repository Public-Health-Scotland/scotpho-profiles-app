################################
# MODULE: Population module ---- 
# prepares the layout displaying the population group splits

################################

#######################################################
## MODULE UI
#######################################################

## ui function -----------------------------------------------------------------------
# id = unique id 
pop_groups_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      # sidebar for filters -----------------------------
      sidebar = sidebar(width = 300,
                        
                        # indicator filter (note this is a module)
                        indicator_filter_mod_ui(ns("indicator_filter")),
                        
                        # filter to select pop split (set choices to NULL 
                        # as they are updated dynamically on the server side, depending on selected indicator)
                        selectInput(
                          inputId = ns("split_filter"),
                          label = "Select population split:",
                          selectize = TRUE,
                          choices = NULL
                        ),
                        selectizeInput(inputId = ns("split2_filter"), 
                                       label = "Select 2nd population split:", 
                                       choices = NULL
                        )
      ), # close sidebar
      
      
      layout_column_wrap(
        1/2,
        
        # Bar chart card ------------------------------------------

        # footer with download buttons
        # NOTE: the 'footer' argument for navset_card_pill() is currently not working
        # package maintainers are aware and working on a fix
        # using the card_footer argument for card() in the meantime and suppressing warnings until bug fixed
        suppressWarnings(
          bslib::navset_card_pill(
            height = 550,
            full_screen = TRUE,
            
            # tab 1: bar chart 
            bslib::nav_panel("Chart",
                             uiOutput(ns("pop_rank_title")), # title 
                             highchartOutput(ns("pop_rank_chart"))|> # chart 
                               withSpinner() |> (\(x) {
                                 x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
                                 x})()
            ),
            
            # tab 2: data table
            bslib::nav_panel("Table",
                             reactableOutput(ns("pop_rank_table")) # table
            ),
            
            # tab 3: indicator metadata
            bslib::nav_panel("Metadata",
                             metadata_table_mod_UI(ns("indicator_metadata"))
            ),
            
            
            bslib::nav_spacer(),
            
            # extra controls for bar chart 
            bslib::nav_item(
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("ci_switch"), label = " include confidence intervals", FALSE),
                selectInput(ns("pop_years_filter"), label = "select year", choices = NULL)
              )
            ),
            
            # card footer - download buttons
            card_footer(class = "d-flex justify-content-between",
                        download_chart_mod_ui(ns("save_pop_rankchart")),
                        download_data_btns_ui(ns("pop_rank_download")))
          )), # close bar chart card
        
        
        ######  based on deprivation trend card addeded "pop_" to distinguish the two

        suppressWarnings(
          bslib::navset_card_pill(
            height = 550,
            full_screen = TRUE,
            
            # tab 1: trend chart 
            bslib::nav_panel("Chart",
                             uiOutput(ns("pop_trend_title")), # title
                             highchartOutput(ns("pop_trend_chart")) |> # chart
                               # issue described here: https://github.com/daattali/shinycssloaders/issues/76 
                               # solution posted here: https://stackoverflow.com/questions/77184183/how-to-use-shinycssloaders-withspinner-with-a-plot-output-in-a-bslib-card 
                               withSpinner() |> (\(x) {
                                 x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
                                 x})()
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
                # too many CI for age split, removed at this stage
                checkboxInput(ns("trend_ci_switch"), label = " include confidence intervals", FALSE) 
              )
            ),
            # card footer - download buttons
            card_footer(class = "d-flex justify-content-between",
                        download_chart_mod_ui(ns("save_pop_trendchart")),
                        download_data_btns_ui(ns("pop_trend_download")))
          )
        ) # close trend card
      ) # close layout column wrap
      
    ) # close sidebar layout
  ) # close taglist
} # close ui function 



#######################################################
## MODULE SERVER
#######################################################


pop_groups_server <- function(id, dataset, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    #######################################################
    ## Dynamic filters -----
    ######################################################
    
    ## update choices for population split filter, depending on what indicator was selected
    observe({
      req(popgroup_filtered_data())
      available_splits <- popgroup_filtered_data() |>
        pull(unique(split_name))
      
      updateSelectInput(session, inputId = "split_filter", choices = available_splits)
      
      
    })
    
    # update years choices for bar chart filter, depending on indicator selected
    observe({
      req(popgroup_filtered_data())
      available_years <- popgroup_filtered_data() |>
        arrange(desc(year)) |>
        pull(unique(def_period))
      
      updateSelectInput(session, inputId = "pop_years_filter",
                        choices = available_years, selected = available_years[1])
    })
    
    
    observe( {
      req(popgroup_filtered_data()) 

      # stores available 2nd splits (is empty if no 2nd splits available)
      available_2nd_splits <- popgroup_filtered_data() %>% # clunky: needs work
        filter(split_name == input$split_filter & !is.na(split_value2)) %>%
        select(split_value2) %>%
        group_by(split_value2) %>%
        summarise() %>%
        arrange(desc(split_value2)) %>%  # to give Total first, and then Male and Female if available (could factorise also when more options)
        pull(split_value2)
      
      # If 2nd splits are available, enable split2_filter, otherwise disable it
      if(length(available_2nd_splits)>0) {
        shinyjs::enable("split2_filter")
        updateSelectizeInput(session, 
                             inputId = "split2_filter",
                             options = list(placeholder = NULL), 
                             label = "Select 2nd population split:", 
                             choices = available_2nd_splits
        )
      } else {
        updateSelectizeInput(session, 
                             inputId = "split2_filter", 
                             options = list(placeholder = "Unavailable"),
                             choices = character(0),
                             selected = character(0)
                             )
        shinyjs::disable("split2_filter")
      }
    })
      

    
    #######################################################
    ## Reactive data / values ----
    #######################################################
    
    # generate list of indicators (from the popgroup dataset) available 
    selected_indicator <- indicator_filter_mod_server(id = "indicator_filter", dataset, geo_selections)

    
    # create reactive data - filtering by selected indicator
    popgroup_filtered_data <- reactive({
      
      req(dataset())
      
      dataset() |>
        filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>  # filter by selected geography
        filter(indicator == selected_indicator()) # filter by selected indicator
    })

    # creates trend data
    pop_trend_data <- reactive({

      req(popgroup_filtered_data())
      
      #should be an easier logic, but "if (exists("input$split2_filter"))" wasn't working...
      available_2nd_splits <- popgroup_filtered_data() %>% # clunky: needs work
        filter(split_name == input$split_filter & !is.na(split_value2)) %>%
        select(split_value2) %>%
        group_by(split_value2) %>%
        summarise() %>%
        arrange(desc(split_value2)) %>%  # to give Total first, and then Male and Female if available (could factorise also when more options)
        pull(split_value2)
      
      # If 2nd splits are available, enable split2_filter, otherwise disable it
      if(length(available_2nd_splits)>0) {

      df <- popgroup_filtered_data() %>%
        filter(split_name == input$split_filter) %>% # filter by selected split
        filter(split_value2 == input$split2_filter) %>% # filter by the 2nd split value
        filter(!split_value == "Total") %>% # when SIMD data has Total row (not sure whether this will be used yet...)
        arrange(year, split_value2) 
      
      } else {
        
      df <- popgroup_filtered_data() %>%
          filter(split_name == input$split_filter) %>% # filter by selected split
          arrange(year)
      
      } 
       
      df
      
    })
    
    # create single year data for the bar chart 
    pop_rank_data <- reactive({
      req(pop_trend_data())
      pop_trend_data() |>
        filter(def_period == input$pop_years_filter) |>
        mutate(colour_pal = case_when(grepl("All", split_value) ~ phs_colors("phs-blue"), TRUE ~ phs_colors("phs-blue-50")))
    })
    
    #######################################################
    ## dynamic text  ----
    #######################################################
    
    output$pop_rank_title <- renderUI({
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(pop_trend_data()) > 0, "No indicators available")
      )
      
      # if data is available display chart title
      div(
        tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
        tags$h6(pop_rank_data()$trend_axis[1]), # time period 
        tags$p(pop_rank_data()$rate_type[1]) # measure type
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
        tags$p(pop_trend_data()$rate_type[1]) # measure type
      )
    })
    
    ############################################
    # charts -----
    #############################################
    
    # pop rank bar chart  ---------------
    
    output$pop_rank_chart <- renderHighchart({
      
      shiny::validate(
        need( nrow(pop_rank_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )

      
      x <- hchart(pop_rank_data(), 
                  type = "column", hcaes(x = split_value, y = measure, color = colour_pal)) %>%
        hc_yAxis(gridLineWidth = 0) %>%
        hc_chart(backgroundColor = 'white') %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_plotOptions(series = list(animation = FALSE),
                       column = list(groupPadding = 0))|>
        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO ", selected_indicator(), " split by ", input$split_filter),
          chartOptions = list(
            title = list(text = paste0(selected_indicator(), " split by ", input$split_filter)),
            subtitle = list(text = paste0(pop_rank_data()$trend_axis[1])),
            yAxis = list(title = list(text = paste0(pop_rank_data()$rate_type[1])))
          )
        )
      
      
      
      if(input$ci_switch) {
        x <- x |>
          hc_add_series(pop_rank_data(), "errorbar", hcaes(x = split_value, low = lowci, high = upci), zIndex = 10)
      }
      
      x

      
    }) # end pop_rank_chart
    
    # pop trend bar chart  ---------------
    
    output$pop_trend_chart <- renderHighchart({
      
      
      # create vector of colours - needs to be the same length as the 
      # number of lines that need to be plotted otherwise CI colours (the lighter colour plotted behind the main line)
      # wont match up properly
      purple_and_blues <- unname(phs_colours()[grepl("blue|purple", names(phs_colours()))])
      colours <- head(purple_and_blues, length(unique(pop_trend_data()$split_value)))
      
      
      x <- hchart(pop_trend_data(), 
                  "line",
                  hcaes(x = trend_axis, y = measure, group = split_value)) |>
        hc_yAxis(gridLineWidth = 0) |> # remove gridlines 
        hc_xAxis(title = list(text = "")) |>
        hc_yAxis(title = list(text = "")) |>
        hc_xAxis(categories = unique(pop_trend_data()$trend_axis)) |>
        # style xaxis labels - keeping only first and last label
        hc_xAxis(labels = list(
          rotation = 0,
          style = list(
            whiteSpace = 'nowrap',
            textOverflow = 'none'
          ),
          formatter = JS("function() {
               if (this.isFirst || this.isLast) {
                 return this.value;
               } else {
                 return '';
               }
             }"))) |>
        hc_chart(backgroundColor = 'white') |>
        hc_plotOptions(series = list(animation = FALSE)) |>
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 1,
          table = TRUE
        ) |>
        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO trend - ", selected_indicator(), " split by ", input$split_filter),
          chartOptions = list(
            title = list(text = paste0(selected_indicator(), " split by ", input$split_filter)),
            subtitle = list(text = paste0(first(pop_trend_data()$trend_axis)," to ",last(pop_trend_data()$trend_axis))),
            yAxis = list(title = list(text = paste0(pop_trend_data()$rate_type[1])))
          )
        )
      

      
      
      # if the confidence interval switch turned on, plot cis
      if(input$trend_ci_switch == TRUE){
        
        x <- x |>
          hc_add_series(pop_trend_data(), 
                        type = "arearange", 
                        hcaes(x = trend_axis, low = lowci, high = upci, group = split_value),  
                        color = hex_to_rgba("grey", 0.2), 
                        linkedTo = ":previous",
                        showInLegend = FALSE,
                        enableMouseTracking = FALSE,
                        zIndex = -1, # plots the CI series behind the line series
                        marker = list(enabled = FALSE, # removes the markers for the CI series
                                      states = list(
                                        hover = list(
                                          enabled = FALSE))))
      }
      
      x <- x |>
        # add phs colours
        hc_colors(colours)
      
      x
      
      
    }) #end  pop trend chart
    
    
    
    ##########################################
    # Tables ---------
    ###########################################
    
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
    
    
    
    # metadata table
    metadata_table_mod_Server("indicator_metadata", selected_indicator)
    
    ### need to create pop trend table
    
    ############################################
    # Downloads  ----
    #############################################
    download_chart_mod_server(id = "save_pop_rankchart", chart_id = ns("pop_rank_chart"))
    download_data_btns_server(id = "pop_rank_download", data = pop_trend_data, file_name = "Popgroup_ScotPHO_data_extract")
    
    download_chart_mod_server(id = "save_pop_trendchart", chart_id = ns("pop_trend_chart"))
    download_data_btns_server(id = "pop_trend_download", data = pop_trend_data, file_name = "Popgroup_ScotPHO_data_extract")
  } # module server
  )# module server
} # pop groups server