################################
# MODULE: inequality_mod ---- 
# prepares the layout displaying the inequalities splitss
################################

#######################################################
## MODULE UI
#######################################################

## ui function -----------------------------------------------------------------------
# id = unique id 
inequality_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      # sidebar for filters -----------------------------
      sidebar = sidebar(width = 300,
                        # 2 x help buttons
                        layout_column_wrap(
                          1/2,
                          actionButton(ns("help"), label = "Help", class = "btn-sm"), # help button
                          indicator_definition_btn_ui(ns("ind_def")) # indicator definitions button
                        ),
                        
                        # indicator filter (note this is a module)
                        indicator_filter_mod_ui(ns("indicator_filter")),
                        
                        # filter to select inequality split (set choices to NULL 
                        # as they are updated dynamically on the server side, depending on selected indicator)
                        selectInput(
                          inputId = ns("split_filter"),
                          label = "Select equality split:",
                          choices = NULL
                        )
      ), # close sidebar
      
      
      layout_column_wrap(
        1/2,
        
        # Bar chart card ------------------------------------------
        bslib::navset_card_pill(
          height = 550,
          full_screen = TRUE,
          
          # tab 1: bar chart 
          bslib::nav_panel("Chart",
                           uiOutput(ns("pop_rank_title")), # title 
                           highchartOutput(ns("pop_rank_chart")) # chart 
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
              bsicons::bs_icon("gear", size = "1.7em"),
              checkboxInput(ns("ci_switch"), label = " include confidence intervals", FALSE)
            )
          ),
          
          # card footer - download buttons
          card_footer(class = "d-flex justify-content-between",
                      download_chart_mod_ui(ns("save_pop_rankchart")),
                      download_data_btns_ui(ns("pop_rank_download")))
        ), # close bar chart card
        
        "add trend chart here"
      ) # close layout column wrap
      
    ) # close sidebar layout
  ) # close taglist
} # close ui function 






#######################################################
## MODULE SERVER
#######################################################


inequality_server <- function(id, dataset, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    
    #######################################################
    ## Dynamic filters -----
    ######################################################
    
    ## update choices for inequalities split filter, depending on what indicator was selected
    observe({
      
      available_splits <- dataset() |>
        filter(indicator == selected_indicator()) |>
        pull(unique(split_name))
      
      
      updateSelectInput(session, inputId = "split_filter", choices = available_splits)
      
      
    })
    
    
    #######################################################
    ## Reactive data / values ----
    #######################################################
    
    # generate list of indicators (from the simd indicators dataset) available 
    selected_indicator <- indicator_filter_mod_server(id = "indicator_filter", dataset, geo_selections)
    
    
    # calls definition button module server script and passes the actual indicator selected)
    indicator_definition_btn_server("ind_def", selected_indicator = selected_indicator)  
    
    
    # creates trend data
    pop_trend_data <- reactive({
      dataset() |>
        filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>  # filter by selected geography
        filter(indicator == selected_indicator() & split_name == input$split_filter) # filter by selected indicator and selected split
    })
    
    
    # take trend data and createsingle year data (for rank bar chart)
    pop_rank_data <- reactive({
      pop_trend_data() |>
        filter(year == max(year))
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
      tagList(
        tags$h5(selected_indicator(), " split by ", input$split_filter, class = "chart-header"),
        tags$h6(pop_trend_data()$year[1]), # time period
        tags$p("Percentage meeting the MVPA") # measure type
      )
    })
    
    
    
    
    ############################################
    # charts -----
    #############################################
    
    # rank bar chart  chart ---------------
    
    output$pop_rank_chart <- renderHighchart({
      
      shiny::validate(
        need( nrow(pop_rank_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      x <- hchart(pop_rank_data(), 
                  type = "column", hcaes(x = sub_code, y = rate, color = phs_colors("phs-blue"))) %>%
        hc_yAxis(gridLineWidth = 0) %>%
        hc_chart(backgroundColor = 'white') %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_plotOptions(series = list(animation = FALSE),
                       column = list(groupPadding = 0))
      
      
      
      if(input$ci_switch) {
        x <- x |>
          hc_add_series(pop_rank_data(), "errorbar", hcaes(x = sub_code, low = lowci, high = upci), zIndex = 10)
      }
      
      x
    }) # end pop_rank_chart
    
    
    
    ##########################################
    # Tables ---------
    ###########################################
    
    # trend data table -------
    output$pop_rank_table <- renderReactable({
      
      data <- pop_trend_data() |>
        select(year, sub_code, rate)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data)
      )
    })
    
    
    
    ############################################
    # Downloads  ----
    #############################################
    
    download_chart_mod_server(id = "save_simd_barchart", chart_id = session$ns("pop_trend_chart"))
    download_data_btns_server(id = "simd_trendchart_download", data = pop_trend_data())
    
    
  }
  )
}