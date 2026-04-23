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
                        tags$b("A new tab designed to illustrate various asepects of demography e.g. age sex struture through pop pyramids,
                          Could also add SIMD deprivation breakdowns and ethnicity. would have radio buttons to switch between."),
                        # time period filter 
                        selectizeInput(
                          inputId = ns("period_filter"),
                          label = "Select time period:",
                          choices = NULL # choices dynamically updated in server depending on selected indicator
                        ),
                        
                        tags$b("Ideally we would add ability to overlay Scotland percentages as comparator (just lines with no fill")),
      
      # create a multi-tab card 
      div(id = ns("demog_card_wrapper"),
          navset_card_pill(
            id = ns("demog_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("demog_chart_tab"), #id for guided tour
                      uiOutput(ns("pyramid_title")), # title 
                      #uiOutput(ns("trend_caveats")), # caveats
                      highchartOutput(outputId = ns("pop_pyramid_chart")) |> # chart
                        withSpinner() |> 
                        bslib::as_fill_carrier() #required to ensure chart fills panel
            ),
            
            # data tab
            nav_panel("Data",
                      value = ns("demog_data_tab"), #id for guided tour
                      reactableOutput(ns("pyramid_table")) # table
            ),
            
            # add space
            bslib::nav_spacer()),
          
          # footer with download buttons
          footer = card_footer(class = "d-flex justify-content-left",
                               p("download buttons")
                               #  div(id = ns("trend_download_chart"), download_chart_mod_ui(ns("download_trends_chart"))),
                               # div(id = ns("trend_download_data"), download_data_btns_ui(ns("download_trends_data"))))
          )
      ), # close navset card pill
      
      # accordion panel with metadata table 
      div(id = ns("metadata_section"),
          div(uiOutput(ns("sex_split_text")), # chart header 
              p("metadata here")
          ))
      
      #metadata_panel_UI(ns("metadata_table")))
    ) # close
  )  # close layout sidebar)                 
  
  # nav_panel("Charts",
  #   p("hello")
  # )
  
  
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
        filter(year== input$period_filter) 
    })
    
    # calculate total male/female population split
    sex_ratio_data <- reactive({
      dataset() |>
        group_by(year,areaname)|>
        summarise(pmale=abs(sum(percentage_Male)),
                  pfemale=sum(percentage_Female),.groups = "drop")
    })
    
    
    ## year filter ----
    observe({
      
      # get available def periods for selected indicator AND selected area
      # (important as e.g. Scotland may have more up to date data
      # or have different level of aggregation than selected area)
      choices <- sort(unique(dataset()$year[dataset()$areaname == geo_selections()$areaname]),decreasing = TRUE)
      
      ##FIGURE OUT HOW TO SORT IN DESCENDING ORDER SO MOST RECENT YEAR PLOTTED FIRST
      
      # avoid transient invalid values while updating filter
      shiny::freezeReactiveValue(input, "period_filter")
      
      # update filter choices
      updateSelectizeInput(
        session = session,
        inputId = "period_filter",
        choices = choices,
        selected = if (length(choices)) choices[[1]] else NULL
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
        #tags$h5(selected_indicator(), class = "chart-header"), # selected indicator
        #tags$h6(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis)), # time range
        #tags$p(trend_data()$type_definition[1]) # type definition
      )
    })
    
    
    ############################################.
    # charts -----
    ############################################.
    output$pop_pyramid_chart <- renderHighchart({
      
      shiny::validate(
        need( nrow(pyramid_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area.")))
      
      create_pyramid_chart(
        data = pyramid_data())
      
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
    }) #close pyramid table
    
  }) #close moduleServer
} # close server function