pop_groups_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_pill(
      full_screen = FALSE,
      height = 650,
      
      # sidebar for filters ------------------
    
    sidebar = sidebar(width = 300,
                      indicator_filter_mod_ui(ns("indicator_filter")),
                      paste0("some text"),
                      indicator_definition_btn_ui(ns("inequalities_ind_def")                                                    ),
                      #actionButton(ns("indicator_definition"), label = "Definition", icon= icon('info'))
                      #indicator_definition_btn_ui(ns("hwb_inequality"))
                      selectInput(
                        inputId = "split_filter",
                        label = "Select demographic split:",
                        choices = {unique(test_data$split_name)},
                        selected="age") # split input
    )# close sidebar
      ,
  # charts tab -----------------------
  nav_panel("charts",
            layout_column_wrap(
              1/2,
             #  pop bar chart card
              card(
                height = 500,
                full_screen = TRUE,
                card_header(
                  checkboxInput(ns("ci_switch"), label = " include confidence intervals", TRUE)
                ),
                card_body(highchartOutput(ns("pop_rank_chart"))),

                 card_footer()
                #class = "d-flex justify-content-between",
                #             download_chart_mod_ui(ns("save_rank_chart")),
                #             download_data_btns_ui(ns("rank_download")))
              ), #pop rank card
             # pop trend chart card
              card(
                height = 500,
                full_screen = TRUE,
                card_header(),
                card_body(class = "p-0", highchartOutput(ns("pop_trend_chart"))),
                card_footer()
              )# pop trend card
            )
  )#, # close charts tab  
  )# close navset card pill 
  )#  close taglist
  
}

pop_groups_server <- function(id, profile_data, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    
  #  return selected indicator
    selected_indicator <- indicator_filter_mod_server("indicator_filter", profile_data, geo_selections)


    indicator_definition_btn_server("inequalities_ind_def", selected_indicator = selected_indicator)
    
    #ci  reactive switch
    observe({
      {  enable("ci_switch")}
      })

    # set data to be used in rank and trend charts
    filtered_data<- reactive({
      subset(test_data, split_name== input$split_filter)
    })


    
  ############################################
  # Visualisations / data table  ----
  #############################################
  # 
  output$pop_rank_chart <- renderHighchart({

    # if there' no comparator selected, or the selected comparator is "area" then create a bar chart
    # if(input$ci_switch == FALSE ) {

      pop_rank_data = filtered_data%>%
        filter(year==max(year))

      x <-  hchart(object = pop_rank_data(),
                   type = "bar", hcaes(x = split_value, y = rate)) |>
        hc_yAxis(gridLineWidth = 0) 
      #}

  #     # include confidence intervals when ci switch is turned on on
  # else if(input$ci_switch == TRUE) {
  #       x <- x |>
  #         hc_add_series(pop_rank_data(), "errorbar", hcaes(x = split_value, y = rate, low = lowci, high = upci), zIndex = 10)
  #     }
  # 
  #   #x

  })
    
    # pop trend chart
    output$pop_trend_chart <- renderHighchart({
      
      # create reactive dataset filtered by selected indicator and geography area
      pop_trend_chart <- reactive({
        filtered_data()  %>%
          filter(split_name== input$split_filter)
        })
      
      # define objects for chart titles and labels
      selected_area <- unique(filtered_data()$location_name)
      definition <- unique(filtered_data()$def_period)
      split_name <- input$split_filter
      chart_title <- paste(definition, "in", selected_area, "by", split_name)
      
      
      # generate name value for line chart
      selected_split <- unique(filtered_data()$split_value)
      
        # create highchart object
      chart <- highchart() %>%
        hc_add_series(pop_trend_chart(),
                      type = "line",
                      hcaes(x = year, y = rate, group=split_value),
                      name = selected_split) %>%
        
        
        # rename titles and format legend
        hc_title(text = chart_title) %>%
        # hc_subtitle(text = type_definition) %>%
        hc_xAxis(title = "") %>%
        #  hc_yAxis(title = list(text = type_definition)) %>%
        hc_legend(align = "left", verticalAlign = "top") %>%
        #format tooltip
        hc_tooltip(headerFormat = "", shared = TRUE) %>%
        # set theme (defined in global script) currently copied from cd_trend and run in console
        hc_add_theme(chart_theme)

    
    })
    }
    
 )    
}# close server function
  
  