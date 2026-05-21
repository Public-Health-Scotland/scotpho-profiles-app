###########################################################################.
# MODULE: pop_pyramid_mod ---- 
# prepares the nav_panel layout displaying population pyramid
###########################################################################.


# id = unique id 
demographics_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      #height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content)

                        # year filter
                        shinyWidgets::sliderTextInput(
                          inputId = ns("period_filter"),
                          label = "Select year:",
                          grid = TRUE,
                          animate = TRUE,
                          choices = c("2024")
                        ),
    
                        # scotland comparator filter
                        checkboxInput(
                          inputId = ns("comp_filter"),
                          label = "Compare against Scotland",
                          value = FALSE
                        ),
                        
                        # hover information 
                        helpText("Hover over (or tap on mobile devices) an age group in the pyramid chart to update the figures below."),
                        value_box(
                          title = textOutput(ns("age_group")),
                          value = textOutput(ns("total_pop")),
                          textOutput(ns("perc_pop")),
                          showcase = highchartOutput(ns("bars"), height = "70px"),
                          showcase_layout = "bottom"
                        )
                        ), # close sidebar

      
      # create a multi-tab card 
      div(id = ns("demog_card_wrapper"),
          navset_card_pill(
            id = ns("demog_navset_card_pill"),
            full_screen = TRUE,
            
            
            
            # charts tab -----------------------
            nav_panel("Chart",
                      value = ns("demog_chart_tab"), #id for guided tour
                      # title and subtitle 
                      div(
                        h4(textOutput(ns("areaname"), inline = TRUE), " population, split by age and sex", class = "text-header"),
                        textOutput(ns("year"))
                      ),
                      
                      # population pyramid
                      highchartOutput(outputId = ns("pop_pyramid_chart")) |> # chart
                        withSpinner() |> 
                        bslib::as_fill_carrier(), #required to ensure chart fills panel
                      p("Source: NRS population estimates")
            ),
            
            # data tab
            nav_panel("Data",
                      value = ns("demog_data_tab"), #id for guided tour
                      reactableOutput(ns("pyramid_table")) # table
            ),
 
          # footer with download buttons
          footer = card_footer(class = "d-flex justify-content-left",
                               div(id = ns("pyramid_download_chart"), download_chart_mod_ui(ns("save_pyramid_chart"))),
                               div(id = ns("pyramid_download_data"), download_data_btns_ui(ns("download_pyramid_data")))
          )
      )

    ) # close
  ))  # close layout sidebar)                 

} #close ui function

############




demographics_mod_server <- function(id, dataset, geo_selections, selected_profile, root_session){
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # create population pyramid dataframe
    # further filtering dataset (already filtered in server to only include globally selected area and scotland)
    # filtering by globally selected area and selected year from slider
    pyramid_data <- reactive({
      dataset() |>
        filter(year== input$period_filter & areaname == geo_selections()$areaname & areatype == geo_selections()$areatype) 
    })
    
    
    # comparator data (for lines on pop pyramid)
    # filering on scotland for selected year from slider 
    comparator_data <- reactive({
      req(isTRUE(input$comp_filter))
      dataset() |>
        filter(year == input$period_filter & areatype == "Scotland")
    })
    
    
    
    # further filter pyramid data to get totals info (no age splits)
    # for selected area/year - these are displayed in the value box 
    totals_data <- reactive({
      pyramid_data() |>
        group_by(code) |>
        summarise(
          age = "All ages",
          population_All = (sum(population_Male) + sum(population_Female)),
          population_Male = sum(population_Male),
          population_Female = sum(population_Female),
          percentage_Male = abs(sum(percentage_Male)),
          percentage_Female = sum(percentage_Female),
          .groups = "drop"
        )
    })
    
    
    # if user has hovered over an age category in the pyramid
    # chart (causing 'input$pyramid_hover' to update with the name of age age group)
    # then filter on that age group and summarise info to update the value box
    # with age-specific info
    hover_data <- eventReactive(input$pyramid_hover, {
      pyramid_data() |>
        filter(age == input$pyramid_hover) |>
        group_by(code) |>
        summarise(
          age = paste0("Age ", age),
          population_All = (sum(population_Male) + sum(population_Female)),
          population_Male = population_Male,
          population_Female = population_Female,
          percentage_Male = (population_Male / (population_Male + population_Female) * 100),
          percentage_Female = (population_Female / (population_Male + population_Female) * 100),
          .groups = "drop"
        )
    }, ignoreNULL = TRUE)
    
    

    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Dynamic filters ------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    ## year filter ----
    observe({
      
      # get available def periods for selected indicator AND selected area
      # (important as e.g. Scotland may have more up to date data
      # or have different level of aggregation than selected area)
      choices <- sort(unique(dataset()$year[dataset()$areaname == geo_selections()$areaname]),decreasing = FALSE)

      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = "period_filter",
        choices = choices,
        selected = max(choices)
      )
      
    })
    
    
    # only show scotland checkbox when scotland is not selected
    observeEvent(geo_selections()$areatype, {
      if(geo_selections()$areatype == "Scotland"){
        hide("comp_filter")
      } else {
        show("comp_filter")
      }
    })
    
    
    #####################################.
    # DYNAMIC TEXT ----
    ####################################.


    # Dynamic elements of population pyramid title/subtitles
    output$areaname <- renderText({geo_selections()$areaname})
    output$year <- renderText({input$period_filter})
    
    
    # age category (for value box)
    output$age_group <- renderText({
      if(is.null(input$pyramid_hover)){
        "All ages"
      } else {
        paste("Age", input$pyramid_hover)
      }
    })
    
    
    # population size (for value box)
    output$total_pop <- renderText({
      # if user isn't hovered over an age group, show total pop
      if(is.null(input$pyramid_hover)){
        format(totals_data()$population_All, big.mark = ",")
      } else {
        # otherwise show age-specific pop
        format(hover_data()$population_All, big.mark = ",")
      }
    })
    
    
    # percentage of pop (for value box)
    output$perc_pop <- renderText({
      
      # only run if users hovered over an age group in pyramid chart 
      req(!is.null(input$pyramid_hover))
      
      perc <- round(sum(
        pyramid_data()$percentage_Female[pyramid_data()$age == input$pyramid_hover],
        abs(pyramid_data()$percentage_Male[pyramid_data()$age == input$pyramid_hover])
      ), digits = 1)
      
      paste0(perc, "% of total population")
      
    })
    

    
    ############################################.
    # charts -----
    ############################################.
    
    
    
    # pyramid chart -----
    output$pop_pyramid_chart <- renderHighchart({

      # only render chart once - all subsequent updates done via proxy functions
      isolate({
        create_pyramid_chart(data = pyramid_data()) |>
          # format bars
          hc_plotOptions(
            series = list(
              point = list(
                events = list(
                
                # when user hovers over an age group
                # update 'input$pyramid_hover' with the 
                # name of the hovered age group (note this input doesn't exist in the UI -
                # only here in the server)
                mouseOver = JS(sprintf("
                function () {
                clearTimeout(window.pyramidHoverTimeout);
                Shiny.setInputValue('%s', this.name, {priority: 'event'});
                }", ns("pyramid_hover"))),
                
                # when user removes mouse from an age group
                # update 'input$pyramid_hover' to NULL (but put a 75 millisec delay
                # on it as there's tiny spaces between the age group bars, so avoids
                # input$pyramid hover momentarily switching to NULL
                mouseOut = JS(sprintf("
                function () {
               window.pyramidHoverTimeout = setTimeout(function() {
               Shiny.setInputValue('%s', null, {priority: 'event'});
               }, 75);}", ns("pyramid_hover")))
                  
                )
              )
            )
          ) |>
          # downloaded chart options
            hc_exporting(
              filename = "ScotPHO Population Pyramid",
              chartOptions = list(
                title = list(text = paste0(first(pyramid_data()$areaname))),
                subtitle = list(text = paste0(first(pyramid_data()$year)))
              )
            )
        
        })
      
    })
    
    # update y-axis min and max each time user changes global geography selection
    observeEvent(dataset(), {
      
      # find highest % in the area filtered dataset
      axis_value <- round(max(abs(dataset()$percentage_Male),dataset()$percentage_Female),0)
      
      highchartProxy(ns("pop_pyramid_chart")) |>
        hcpxy_update(yAxis = list(min = -axis_value , max = axis_value))
    })
    
    
    
    # each time reactive dataset updates, update the pyramid chart
    observeEvent(pyramid_data(), {
      
      # Update bars
      hc <- highchartProxy(ns("pop_pyramid_chart")) |>
        hcpxy_update_series(
          id = "m_series",
          data = list_parse2(pyramid_data()[, .(age, percentage_Male)])
        ) |>
        hcpxy_update_series(
          id = "f_series",
          data = list_parse2(pyramid_data()[, .(age, percentage_Female)])
        )
      
      # update comparator lines (if applicable)
      if(isTRUE(input$comp_filter)){
        hc <- hc |>
          hcpxy_update_series(
            id = "m_comp_series",
            data = list_parse2(comparator_data()[, .(age, percentage_Male)])
          ) |>
          hcpxy_update_series(
            id = "f_comp_series",
            data = list_parse2(comparator_data()[, .(age, percentage_Female)])
          )
        
      }
      
      hc <- hc |>
        # update titles/subtitles in chart download
        hcpxy_update(
          exporting = list(
            chartOptions = list(
              title = list(text = paste0(first(pyramid_data()$areaname))),
              subtitle = list(text = paste0(first(pyramid_data()$year)))
            )
          )
        )
      
    }, ignoreInit = TRUE)
    
    
    
    # add/remove scotland comparator lines
    observeEvent(input$comp_filter, {
      
      hc <- highchartProxy(ns("pop_pyramid_chart"))
      
      if(isTRUE(input$comp_filter)){
        hc <- hc |>
          hcpxy_add_series(
            id = "m_comp_series", 
            name = "males - Scotland",
            type = "line",
            color = "black",
            data = list_parse2(comparator_data()[, .(age, percentage_Male)])
          ) |>
          hcpxy_add_series(
            id = "f_comp_series", 
            name = "females - Scotland",
            type = "line",
            color = "black",
            data = list_parse2(comparator_data()[, .(age, percentage_Female)])
          )
        
      } else {
        hc <- hc |>
          hcpxy_remove_series(id = "f_comp_series") |>
          hcpxy_remove_series(id = "m_comp_series")
      }
      
      hc
    }, ignoreInit = TRUE)
    
    
    
    # create sex bar chart for the value box
    # wrapping in isolate to ensure only renders once on initial load - subsequent updates done via proxy functions
    output$bars <- renderHighchart({
      isolate({
        
        # create axis categories 
        m_label <- paste0(format(totals_data()$population_Male, big.mark = ","), " males (", round(totals_data()$percentage_Male, digits = 1), "%)")
        f_label <- paste0(format(totals_data()$population_Female, big.mark = ","), " females (", round(totals_data()$percentage_Female, digits = 1), "%)")
        
        # create bar chart 
        highchart() |>
          hc_chart(type = "bar") |>
          hc_xAxis(
            categories = c(m_label, f_label), 
            title = list(text = NULL), 
            lineWidth = 0,
            tickLength = 0,
            labels = list(
              style = list(whiteSpace = "normal", fontSize = "14px", fontFamily = "Arial, sans-serif",color = "#333333"),
              useHTML = TRUE
            )
          ) |>
          hc_add_series(id = "perc_series", data = c(totals_data()$percentage_Male, totals_data()$percentage_Female), colorByPoint = TRUE) |>
          hc_colors(c("#3F3685", "#9B4393")) |>
          hc_yAxis(title = list(text = ""), labels = list(enabled = FALSE), gridLineWidth = 0) |>
          hc_legend(enabled = FALSE) |>
          hc_tooltip(enabled = FALSE) |>
          hc_add_theme(theme) |>
          hc_plotOptions(bar = list(pointPadding = 0, # Smaller value = fatter bars
                                    groupPadding = 0))
        
      })
    })
    
    
    # update sex bars in value box 
    observe({
      
      # if input$pyramid_hover is not NULL (i.e. user is hovered on an age group) then use the hover_data() df
      # otherwise if input$pyramid hover is NULL (i.e. user NOT hovered) use the totals_data() df
      data <- if(is.null(input$pyramid_hover)) totals_data() else hover_data()
      
      # axis cateogories 
      m_label <- paste0(format(data$population_Male, big.mark = ","), " males (", round(data$percentage_Male, digits = 1), "%)")
      f_label <- paste0(format(data$population_Female, big.mark = ","), " females (", round(data$percentage_Female, digits = 1), "%)")
      
      # update chart
      highchartProxy(ns("bars")) |>
        hcpxy_update_series(id = "perc_series", data = c(data$percentage_Male, data$percentage_Female)) |>
        hcpxy_update(xAxis = list(categories = c(m_label, f_label)))
      
      
    })
    

    # ~~~~~~~~~~~~~~~~~~~~~
    # data table ----
    # ~~~~~~~~~~~~~~~~~~~~~
    output$pyramid_table <- renderReactable({
      req(pyramid_data())
      
      data <- pyramid_data() |>
        select(year, code, areaname, age,population_Male,population_Female) 
      
      reactable(data,
                defaultExpanded = T,
                defaultPageSize = nrow(data),
                columns = list(
                  year = colDef("Year"),
                  code = colDef("Area code"),
                  areaname = colDef(name = "Area name"),
                  age= colDef("Age band (years)"),
                  population_Male = colDef(name = "Male population"),
                  population_Female = colDef(name = "Female population")
                ))
    }) #close pyramid mini data table
    
    
    
    
    ######################################.
    # Downloads -------
    ######################################.
    
    # server for chart and data downloads
    download_chart_mod_server(id = "save_pyramid_chart", chart_id = ns("pop_pyramid_chart"))
    
    download_data_btns_server(id = "download_pyramid_data", 
                              data = pyramid_data, 
                              file_name = "Pyramid_ScotPHO_data_extract", 
                              selected_columns = c("year",
                                                   "code",
                                                   "areaname",
                                                   "areatype",
                                                   "age band (years)" = "age",
                                                   "male population" = "population_Male",
                                                   "male percentage" = "percentage_Male",
                                                   "female population" = "population_Female",
                                                   "female percentage" = "percentage_Female"))
      
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Share card buttons ------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    #add ability to share card - might need to adapt the card share module to cope with sub-tab logic rather than LTMHI layout
    #share_button_mod_Server(id = "demog_pyramid_share", card_id = ns("demog_navset_card_pill"))
    
    
    
  }) #close moduleServer
} # close server function