

rank_mod_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      height = 500, width = 300, padding = 20,
      sidebar = sidebar(width = 300,
        indicator_filter_mod_ui(ns("indicator_filter")),
        bslib::input_switch(id = ns("comparator_switch"), 
                            value = FALSE,
                                     label = bslib::tooltip(
                                       trigger = list(
                                         "Include comparator",
                                          icon("info-circle")
                                       ),
                                       "Including a comparator will allow you to see whether each area 
                                       within your chosen geography level (e.g. health boards) is statistically significantly 
                                       better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago)." 
                                       
                                     )),
        # hidden filters to display when comparator toggled on 
        shinyjs::hidden(
          tags$div(
            id = ns("comparator_filters"),
           "hidden filters here"

          ) # close div
        )# close hidden function
      ), # close sidebar
      layout_column_wrap(
        1/2,
        # bar chart
        card(
          height = 250,
          full_screen = TRUE,
          card_header(
            "Bar chart",
            checkboxInput(ns("ci_switch"), label = " include confidence intervals", TRUE)
          ),
          card_body(highchartOutput(ns("rank_barchart"))),
          card_footer(download_chart_mod_ui(ns("save_rank_barchart")))
        ),
        
        # map
        card(
          height = 250,
          full_screen = TRUE,
          card_header("map plot"),
          card_body(highchartOutput(ns("rank_map"))),
          card_footer(download_chart_mod_ui(ns("save_rank_map")))
        )
        
      ) # close column wrap
    ) # close layout sidebar
  ) # close taglist
  
}







rank_mod_server <- function(id, profile_data, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    
    # # # show/hide comparator filters
    # observe({
    #   if (input$comparator_switch) {
    #     shinyjs::show(id = "comparator_filters")
    #   } else {
    #     shinyjs::hide(id = "comparator_filters")
    #   }
    # })
    
    # return selected indicator
    selected_indicator <- indicator_filter_mod_server("indicator_filter", profile_data, geo_selections)
    
    # filter profile data by selected indicator and selected areatype
    rank_data <- reactive({
      x <- profile_data() |>
      filter(indicator == selected_indicator() & areatype == geo_selections()$areatype) |>
        filter(year == max(year)) |>
        select(areaname, measure, upci, lowci) |>
        arrange(measure)
      
      x$colour <- ifelse(x$areaname == geo_selections()$areaname, phs_colors(colourname = "phs-blue"), phs_colors(colourname = "phs-blue-50"))
      
      x
    })
    
    
    download_chart_mod_server(id = "save_rank_barchart", chart_id = sessions$ns("rank_barchart"))
    
    
    # output$table <- renderReactable({
    #   reactable(rank_data(),
    #             defaultPageSize = nrow(rank_data())
    #   )
      
    #})

    
    output$rank_barchart <- renderHighchart({
      
            x <-hchart(object = rank_data(),
                  type = "bar",
                  hcaes(x = areaname, y = measure, color = colour)) |>
             hc_yAxis(gridLineWidth = 0) |>
        hc_xAxis(title = list(text = "")) |>
        hc_yAxis(title = list(text = "")) |>
        hc_chart(margin = c(0, 0, 0, 150)) |>
        hc_plotOptions(series = list(animation = FALSE))
      
      # include confidence intervals when switch is on
      if(input$ci_switch) {
        x <- x |>
          hc_add_series(rank_data(), "errorbar",
                        hcaes(x = areaname,
                              low = lowci,
                              high = upci),
                        zIndex = 10)
      } else {
        
        x 
        
      }
      
    })
    
 
    
  })
}





  
  

