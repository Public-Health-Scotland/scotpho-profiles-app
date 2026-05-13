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
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content)
                        # # time period filter 
                        # selectizeInput(
                        #   inputId = ns("period_filter"),
                        #   label = "Select time period:",
                        #   choices = NULL # choices dynamically updated in server depending on selected indicator
                        # ),
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
                        )
                        ),
      
      # create a multi-tab card 
      div(id = ns("demog_card_wrapper"),
          navset_card_pill(
            id = ns("demog_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("demog_chart_tab"), #id for guided tour
                      uiOutput(ns("pyramid_title")), # title 
                      highchartOutput(outputId = ns("pop_pyramid_chart")) |> # chart
                        withSpinner() |> 
                        bslib::as_fill_carrier() #required to ensure chart fills panel
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
      ), # close navset card pill
      
      # accordion panel with metadata table 
      div(id = ns("metadata_section"),
          div(uiOutput(ns("sex_split_text")), # chart header 
              p("metadata here")
          ))

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
    pyramid_data <- reactive({
      dataset() |>
        filter(year== input$period_filter & areaname == geo_selections()$areaname & areatype == geo_selections()$areatype) 
    })
    
    
    # comparator data (for lines on pop pyramid)
    comparator_data <- reactive({
      req(isTRUE(input$comp_filter))
      dataset() |>
        filter(year == input$period_filter & areatype == "Scotland")
    })
    
    # calculate total male/female population split
    sex_ratio_data <- reactive({
      dataset() |>
        filter(areaname == geo_selections()$areaname & areatype == geo_selections()$areatype) |>
        group_by(year,areaname)|>
        summarise(pmale=abs(sum(percentage_Male)),
                  pfemale=sum(percentage_Female),.groups = "drop")
    })
    
    
    ## year filter ----
    observe({
      
      # get available def periods for selected indicator AND selected area
      # (important as e.g. Scotland may have more up to date data
      # or have different level of aggregation than selected area)
      choices <- sort(unique(dataset()$year[dataset()$areaname == geo_selections()$areaname]),decreasing = FALSE)
      
      ##FIGURE OUT HOW TO SORT IN DESCENDING ORDER SO MOST RECENT YEAR PLOTTED FIRST
      
      # # avoid transient invalid values while updating filter
      # shiny::freezeReactiveValue(input, "period_filter")
      # 
      # # update filter choices
      # updateSelectizeInput(
      #   session = session,
      #   inputId = "period_filter",
      #   choices = choices,
      #   selected = if (length(choices)) choices[[1]] else NULL
      # )
      
      
      shinyWidgets::updateSliderTextInput(
        session = session,
        inputId = "period_filter",
        choices = choices
      )
      
    })
    
    
    #####################################.
    # DYNAMIC TEXT ----
    ####################################.
    
    # render sentence that details male/female sex split
    output$sex_split_text <- renderUI({
      paste0("Percentage of the population: Male ",round(sex_ratio_data()$pmale[1],digits=1),"% Female ",round(sex_ratio_data()$pfemale[1],digits=1),"%")
    })
    
    
    # Population Pyramid Title
    output$pyramid_title <- renderUI({
      req(pyramid_data())
      
      # create dynamic text if no indicators available for selected profile
      # and geography
      shiny::validate(
        need( nrow(pyramid_data()) > 0, "No population data available area type and year. Try selecting another geography type or time period")
      )
      
      # display 3 x titles
      div(
        tags$h5(first(pyramid_data()$areaname), class = "chart-header"), # year
        tags$h5(first(pyramid_data()$year), class = "chart-header"), # year
        tags$p("Source: NRS population estimates") # source of populations
      )
    })
    
    
    ############################################.
    # charts -----
    ############################################.
    output$pop_pyramid_chart <- renderHighchart({

      # only render chart once - all subsequent updates done via proxy functions
      isolate({
        create_pyramid_chart(data = pyramid_data()) |>
            hc_exporting(
              filename = "ScotPHO Population Pyramid",
              chartOptions = list(
                title = list(text = paste0(first(pyramid_data()$areaname))),
                subtitle = list(text = paste0(first(pyramid_data()$year)))
              )
            )
        
        })
      
    })
    
    
    # each time reactive dataset updates, update the chart
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