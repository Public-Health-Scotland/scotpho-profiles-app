rank_mod_ui <- function(id) {

  ns <- NS(id)

  tagList(
      layout_sidebar(
        height = 600, 
        width = 350, 
        padding = 20,
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
                          conditionalPanel(
                            ns=NS(id),
                            condition = "input['comparator_switch'] === true",
                            radioButtons(inputId = ns("comparator_type"),
                                         label = "Compare by: ",
                                         choices = c("Area", "Time"),
                                         selected = "Area"
                            ),

                            conditionalPanel(
                              ns = NS(id),
                              condition = "input['comparator_type'] === 'Area'",
                              selectInput(inputId = ns("area_comparator"),
                                          label = "Select comparator area",
                                          choices = rank_area_comparators_list,
                                          selected = "Scotland")
                            ),


                            conditionalPanel(
                              ns = NS(id),
                              condition = "input['comparator_type'] === 'Time'",
                              selectInput(inputId = ns("year_comparator"),
                                          label = "Select comparator year",
                                          choices = NULL)
                            )


                          )
        ), # close sidebar



        layout_column_wrap(
          1/2,
          # bar chart
          card(
            height = 600,
            full_screen = TRUE,
            card_header(class = "d-flex justify-content-between",
                        "Bar chart",
                        checkboxInput(ns("ci_switch"), label = " include confidence intervals", TRUE)
            ),
            card_body(withSpinner(highchartOutput(ns("rank_barchart")))),
            card_footer(class = "d-flex justify-content-between",
                        download_chart_mod_ui(ns("save_rank_barchart")),
                        download_data_btns_ui(ns("rank_download")))
          ),

          # map
          card(
            height = 600,
            full_screen = TRUE,
            card_header("map plot"),
            card_body(withSpinner(leafletOutput(ns("rank_map")))),
            card_footer(download_chart_mod_ui(ns("save_rank_map")))
          )

        ) # close column wrap



      )

  ) # close taglist

}







rank_mod_server <- function(id, profile_data, geo_selections) {
  moduleServer(id, function(input, output, session) {





    # return selected indicator
    selected_indicator <- indicator_filter_mod_server("indicator_filter", profile_data, geo_selections)


    # create basic rank data - filtering by selected indicator, selected areatype and the max year
    rank_data <- reactive({
      profile_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype) |>
        filter(year == max(year)) |>
        arrange(measure)
    })
    
    
    # update years to use as baseline comparator for dumbell chart 
    observe({
      
      x <- profile_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype)
      
      updateSelectInput(session, inputId = "year_comparator", 
                           choices = unique(x$def_period))
    })
    
    
    





    output$ind_title <- renderText({as.character(selected_indicator())})



    download_chart_mod_server(id = "save_rank_barchart", chart_id = session$ns("rank_barchart"))
    download_data_btns_server(id = "rank_download", data = rank_data())



    output$rank_barchart <- renderHighchart({

      # # get comparator value
      comp_value <- if(input$comparator_switch) {
        if(input$comparator_type == "Area"){
          area_comp <- profile_data() |>
            filter(indicator == selected_indicator() & areaname == input$area_comparator) |>
            filter(year == max(year)) |>
            pull(measure)


        }
      }


      # create colour palette for bars
      if(input$comparator_switch == TRUE){
        if(input$comparator_type == "Area"){
          df <- rank_data() |>
            mutate(colour_pal = case_when(interpret == "O" ~ '#999966',
                                          is.na(lowci) | is.na(upci) | is.na(comp_value) | is.na(measure) | measure == 0 ~ '#999966',
                                          lowci <= comp_value & upci >= comp_value ~'#cccccc',
                                          (lowci > comp_value & interpret == "H") |  (upci < comp_value & interpret == "L") ~ '#4da6ff',
                                          (lowci > comp_value & interpret == "L") | (upci < comp_value & interpret == "H") ~ '#ffa64d',
                                          TRUE ~ '#ccccff'))
        }
      } else {

        df <- rank_data() |>
          mutate(colour_pal = phs_colors(colourname = "phs-blue"))
      }



      # build basic highchart
      x <- hchart(object = df, type = "bar",
                  hcaes(x = areaname, y = measure, color = colour_pal)) |>
        hc_yAxis(gridLineWidth = 0) |>
        hc_xAxis(title = list(text = "")) |>
        hc_yAxis(title = list(text = "")) |>
        hc_chart(margin = c(0, 0, 0, 150),
                 backgroundColor = 'white') |>
        hc_plotOptions(series = list(animation = FALSE))


      # add red comparator line if area comparator selected
      if(input$comparator_switch == TRUE){
        if(input$comparator_type == "Area"){
          x <- x |>
            hc_yAxis(plotLines = list(list(color = "red", width = 5, value = comp_value, zIndex = 10)))
        }
      } else {

        x
      }
      #
      #
      # include confidence intervals when switch is on
      if(input$ci_switch == TRUE) {
        x <- x |>
          hc_add_series(df, "errorbar",
                        hcaes(x = areaname, low = lowci, high = upci),
                        zIndex = 10)
      } else {
        x
      }

      x

    })





    shapefile <- reactive({
      switch(geo_selections()$areatype,
             "Health board" = hb_bound,
             "Council area" = ca_bound,
             "HSC partnership" = hscp_bound,
             "HSC locality" = hscloc_bound,
             "Intermediate zone" = iz_bound
      )
    })



    map_data <- reactive({

      comp_value <- NA
      
      if(input$comparator_switch && input$comparator_type == "Area") {
        area_comp <- profile_data() |>
          filter(indicator == selected_indicator() & areaname == input$area_comparator) |>
          filter(year == max(year)) |>
          pull(measure)
        
        if(!is.na(area_comp))
          comp_value <- area_comp
      }
      
      x <- shapefile() |>
        left_join(rank_data(), by = join_by(code)) |>
        mutate(colour_pal = case_when(
          interpret == "O" ~ '#999966',
          is.na(lowci) | is.na(upci) | is.na(comp_value) | is.na(measure) | measure == 0 ~ '#999966',
          lowci <= comp_value & upci >= comp_value ~'#cccccc',
          (lowci > comp_value & interpret == "H") |  (upci < comp_value & interpret == "L") ~ '#4da6ff',
          (lowci > comp_value & interpret == "L") | (upci < comp_value & interpret == "H") ~ '#ffa64d',
          TRUE ~ '#ccccff'
        ))
      
      x
    })
    
    
    
    
    output$rank_map <- renderLeaflet({


      leaflet() |>
        addPolygons(data = map_data(),
                    weight = 1,
                    color = "black",
                    fillColor = ~ colour_pal,
                    smoothFactor = 0.5,
                    opacity = 1,
                    fillOpacity = 0.5,
                    highlightOptions = highlightOptions(
                      color = "white",
                      weight = 2,
                      bringToFront = TRUE
                    )) |>
        addProviderTiles(provider = providers[["OpenStreetMap"]])
    })


  })
}

