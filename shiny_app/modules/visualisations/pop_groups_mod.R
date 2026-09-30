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
                        div(id = ns("pop_groups_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("indicator"))),
                        
                        # button to scroll to metadata
                        div(id = ns("pop_groups_scroll_button"), metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section"))),
                        
                        # filter to select pop split (set choices to NULL 
                        # as they are updated dynamically on the server side, depending on selected indicator)
                        div(id = ns("pop_groups_split_filter"), selectizeInput(
                          inputId = ns("split_filter"),
                          label = "Select equality split:",
                          choices = NULL
                        )
                        ),
                        
                        #guided tour button
                        actionLink(inputId = ns("pop_groups_tour_button"),
                                   label = "Take a guided tour of this page")
                        
      ), # close sidebar
      
      
      layout_column_wrap(
        width = 1/2,
        
        # Bar chart card ----
          div(id = ns("pop_bar_chart_wrapper"), 
            bslib::navset_card_pill(
              id = ns("pop_groups_bar_card"),
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
              div(id = ns("pop_groups_popover_left"),
              chart_controls_mod_UI(id = ns("bar_controls"), controls = c(ci_switch = FALSE, avg_switch = FALSE))
              )
            ),
            
            # card footer - download buttons
            footer = card_footer(
              toolbar(
                align = "left",
                share_button_mod_UI(ns("bar_share")),
                toolbar_divider(),
                div(id = ns("pop_groups_save_chart"), download_chart_mod_ui(ns("save_pop_rankchart"))),
                toolbar_divider(),
                div(id = ns("pop_groups_save_data"), download_data_btns_ui(ns("pop_rank_download"))))
            )

          ) # close bar chart card
       ), #close bar chart card wrapper
        
        
        ######  based on deprivation trend card addeded "pop_" to distinguish the two
          
          div(id = ns("pop_line_chart_wrapper"),
          bslib::navset_card_pill(
            id = ns("pop_groups_trend_card"),
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
              chart_controls_mod_UI(id = ns("trend_controls"))
            ),
            # card footer - download buttons
            footer = card_footer(
              toolbar(
                align = "left",
                share_button_mod_UI(ns("trend_share")),
                toolbar_divider(),
                download_chart_mod_ui(ns("save_pop_trendchart")),
                toolbar_divider(),
                download_data_btns_ui(ns("pop_trend_download"))
                )
            )
          )# close trend card
          )# close wrapper div for tour
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
    observeEvent(ind_data(), {
      
      # temporarily freeze input whilst choices are being updated
      freezeReactiveValue(input, "split_filter")

      # get available splits depending on selected indicator/area
      available_splits <- ind_data() |>
        arrange(split_name) |>
        distinct(split_name) |>
        pull()

      # determine what the default selection should be - either bookmarked value or first option in list or choices
      if(is.null(bookmarked_split())){
        selection <- available_splits[1]
      } else {
        selection <- isolate(bookmarked_split())
        bookmarked_split(NULL) # re-set rv back to NULL
      }
      
     # update the filter
     updateSelectizeInput(inputId = "split_filter", choices = available_splits, selected = selection)
      
    })
    

    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # generate list of indicators (from the simd indicators dataset) available 
    selected_indicator <- indicator_filter_mod_server(id = "indicator", dataset, geo_selections, selected_profile)
    
    
    # filter profile dataset by selected indicator
    ind_data <- reactive({
      req(selected_indicator())
      
      dataset() |>
        filter(
          areatype == geo_selections()$areatype & areaname == geo_selections()$areaname,  # filter by selected geography
          indicator == selected_indicator() # filter by selected indicator
          )
    })

    
    # creates trend data
    pop_trend_data <- reactive({
      req(input$split_filter)
      
      ind_data() |>
        filter(split_name == input$split_filter) |> # filter by selected indicator and selected split
        # create total column
        group_by(year) |>
        mutate(total = ifelse(any(split_value %in% c("Total", "All")), measure[split_value %in% c("Total", "All")], NA)) |>
        ungroup() |>
        filter(!split_value %in% c("Total", "All")) |>
        arrange(year)
    })
    
    # create single year data for the bar chart 
    pop_rank_data <- reactive({
      pop_trend_data() |>
        filter(year == max(year))
    })
    
    #######################################################.
    ## dynamic text  ----
    #######################################################.
    
    output$pop_rank_title <- renderUI({
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need(selected_indicator(), "No indicators available")
      )
      
      # if data is available display chart title
      div(
        tags$h5(selected_indicator(), ";split by ", input$split_filter, class = "chart-header"),
        tags$h6(pop_rank_data()$trend_axis[1]), # time period 
        tags$p(pop_rank_data()$type_definition[1]) # measure type
      )
    })
    
    # need to add pop-trend title stuff
    
    output$pop_trend_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need(selected_indicator(), "There are insufficent data points for this indicator to create a trend chart")
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
    
    
    # returns TRUE/FALSE for each of the switch inputs in the chart controls popover
    # which can then be used to update the chart accordingly
    bar_controls <- chart_controls_mod_server("bar_controls")
    trend_controls <- chart_controls_mod_server("trend_controls")
    
    # pop rank bar chart  ---------------
    
    output$pop_rank_chart <- renderHighchart({
      
      shiny::validate(
        need(selected_indicator(), paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      
      create_bar_chart(
        data = pop_rank_data(),
        xaxis_col = "split_value",
        yaxis_col = "measure",
        include_confidence_intervals = bar_controls$ci_switch,
        upci_col = "upci",
        lowci_col = "lowci",
        horizontal = TRUE,
        colour_palette = "single",
        include_average = bar_controls$avg_switch
      ) |>

        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO ", selected_indicator(), " split by ", input$split_filter),
          chartOptions = list(
            title = list(
              text = sprintf("%s, split by %s; %s", selected_indicator(), input$split_filter, geo_selections()$areaname),
              style = list(fontWeight = "bold"), align = "left"
            ),
            subtitle = list(
              text = sprintf("%s <br> %s",pop_trend_data()$trend_axis[1], pop_trend_data()$type_definition[1]), 
              useHTML = TRUE, align = "left"
              ),
            caption = list(text = "Source: ScotPHO Profiles Tool")
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
        zero_yaxis = trend_controls$zero_yaxis_switch,
        include_confidence_intervals = trend_controls$ci_switch,
        chart_theme = theme,
        colour_palette = "multi",
        include_average = trend_controls$avg_switch
      ) |>
     
        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO trend - ", selected_indicator(), " split by ", input$split_filter),
          chartOptions = list(
            title = list(
              text = sprintf("%s, split by %s; %s", selected_indicator(), input$split_filter, geo_selections()$areaname),
              style = list(fontWeight = "bold"), align = "left"
            ),
            subtitle = list(
              text = sprintf("%s to %s<br>%s",first(pop_trend_data()$trend_axis),last(pop_trend_data()$trend_axis),first(pop_trend_data()$type_definition)), 
              useHTML = TRUE, align = "left"
              ),
            caption = list(text = "Source: ScotPHO Profiles Tool")
          )
        )
      

    }) #end  pop trend chart
    
    
    
    ##########################################.
    # Tables ----
    ##########################################.
    
    # rank data table -------
    output$pop_rank_table <- renderReactable({
      
      data <- pop_rank_data() |>
        select(def_period, split_value, measure, upci, lowci)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data),
                # rename some columns 
                columns = list(
                  def_period = colDef(name = "Time Period"),
                  split_value = colDef(name = "Population Group"),
                  measure = colDef(name = "Measure"),
                  upci = colDef(name = "Upper CI"),
                  lowci = colDef(name = "Lower CI")
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
    
    
    
    ############################################.
    # Share buttons -----
    ############################################.
    share_button_mod_Server(id = "bar_share", card_id = ns("pop_groups_bar_card"))
    share_button_mod_Server(id = "trend_share", card_id = ns("pop_groups_trend_card"))
    
    ############################################.
    # Guided tour ----
    ############################################.
    
    guide_pop_groups <- Cicerone$
      new()$
      step(
        ns("pop_bar_chart_wrapper"), #left-hand bar chart tab
        "Bar Chart",
        "This chart shows how different population groups, split by age or sex for example, measure up for the selected indicator in the most recent year.",
        position = "right",
      )$
      step(
        ns("pop_line_chart_wrapper"), #right-hand line chart tab
        "Trend Chart",
        "This chart shows how the measure of the selected indicator has changed over time for each of the different population groups.",
        position = "left",
      )$
      step(
        ns("pop_groups_popover_left"), #popover icon
        "Adjust Chart Settings",
        "Click here to add confidence intervals to the chart and select year to plot."
      )$
      step(
        ns("pop_groups_bar_card"), #Navigation for left hand card
        "Other views",
        "You can switch between showing the chart and data for your selected indicator.",
        position = "right"
      )$
      step(
        ns("pop_groups_indicator_filter_wrapper"), #Indicator filter
        "Indicator Filter",
        "First select an indicator.<br>
     The list has been filtered based on profile and area type selected at the top of the page.<br>
     The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
        position = "bottom"
      )$
      step(
        ns("pop_groups_scroll_button"), #Scroll to metadata button
        "Scroll to Metadata Button",
        "Click here to scroll to the metadata panel below.<br>
        It contains information about the selected indicator, including indicator definition, 
        data source, notes and caveats and links to relevant publications and pages on the ScotPHO website. "
      )$
      step(
        ns("pop_groups_split_filter"), #Split filter
        "Select the population groups by which the data should be split.",
        "You can split the data for the selected indicator into population groups such as age and sex."
      )$
      step(
        ns("pop_groups_save_chart"), #Download chart button
        "Download Chart Button",
        "Click here to download this chart as a PNG.",
        position = "bottom"
      )$
      step(
        ns("pop_groups_save_data"), #Download data button
        "Download Data Button",
        "Click here to download the selected data as a CSV, RDS or JSON file.",
        position = "left"
      )
    
    #initiate the guide
    guide_pop_groups$init()
    
    #when guided tour button is clicked, start the guide
    observeEvent(input$pop_groups_tour_button, {
      guide_pop_groups$start()
    })
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Bookmarking  -----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # exclude some inputs from appearing in bookmarked URL (i.e. buttons, card ids)
    setBookmarkExclude(c("pop_groups_bar_card", # bar chart card id
                         "pop_groups_trend_card", # trend chart card id
                         "pop_groups_tour_button")) # tour button id
    
    
    # reactive value for storing bookmarked split 
    bookmarked_split <- reactiveVal(NULL)
    
    
    # when bookmarked URL is opened, and BEFORE any other server code is run, update the
    # reactive val with the bookmarkd split - this will be used in the observer that dynamically
    # updates filter choices and sets default selection for the split filter.
    onRestore(function(state){
      bookmarked_split(state$input$split_filter)
    })

    
    
    
  } # module server
  )# module server
} # pop groups server
