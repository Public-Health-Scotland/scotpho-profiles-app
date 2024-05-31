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
      test_data() %>% 
        filter(split_name== input$split_filter)
    })


    pop_rank_data <- reactive({
      filtered_data() %>%
        filter(year == max(year))
    })
    
  ############################################
  # Visualisations / data table  ----
  #############################################
  # 
    output$pop_rank_chart <- renderHighchart({
      shiny::validate(
        need(nrow(pop_rank_data()) > 0, "Data data are not available at the selected level. Please select either Scotland, Health board or Council area.")
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
    
    }# closemodule server
 )    # module server
}# close server function
  
  