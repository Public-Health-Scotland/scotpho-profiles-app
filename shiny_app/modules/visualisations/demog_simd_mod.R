###########################################################################.
# MODULE: demog_simd_mod ---- 
# prepares the nav_panel layout for page showing how datazone count and population (count & %)
# are distributed across SIMD qunatiles.
###########################################################################.


# id = unique id 
demog_simd_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 250,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content)

                       # year filter - maybe better as a simple drop down? but can't get it to work!
                       shinyWidgets::sliderTextInput(
                         inputId = ns("period_filter"),
                         label = "Select year:",
                         grid = TRUE,
                         animate = TRUE,
                         choices = c("2023")
                       ),
                        
                        # measure filter
                        div(id = ns("demog_item_wrapper"),
                            radioButtons(
                              inputId = ns("demog_simd_metric"),
                              label = "Select metric",
                              choices = c("Populations","Datazone geographies"),
                              selected = c("Populations")
                            )),
                       
                       # filter for age group - only applied is selection is to view populations
                       shinyjs::hidden(
                         selectizeInput(inputId = ns("age_filter"), 
                                        label = "Select agegroup:", 
                                        choices = c("all", "u26", "working"), 
                                        selected = "all")
                       )

      ),
      
      # create a multi-tab card 
      div(id = ns("demog_simd_card_wrapper"),
          navset_card_pill(
            id = ns("demog_simd_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("demog_simd_chart_tab"), #id for guided tour
                      # div(
                      #   h4(textOutput(ns("areaname"), inline = TRUE), " population/datazone count, by SIMD decile", class = "text-header"),
                      #   textOutput(ns("year")),
                      #   textOutput(ns("agegrp")),
                      # ),
                      uiOutput(ns("title_count")), # title 
                      reactableOutput(ns("count_table_top")) # table
                      #uiOutput(ns("pyramid_title")), # title 
                      #highchartOutput(outputId = ns("pop_pyramid_chart")) |> # chart
                       # withSpinner() |> 
                        #bslib::as_fill_carrier() #required to ensure chart fills panel
            ),
            
            # data tab
            nav_panel("Data",
                      value = ns("demog_data_tab") #id for guided tour
                      #reactableOutput(ns("count_table")) # table
            ),
            
            # footer with download buttons
            footer = card_footer(class = "d-flex justify-content-left",
                                 p("space to add download/share buttons")
                          #       div(id = ns("pyramid_download_chart"), download_chart_mod_ui(ns("save_pyramid_chart"))),
                           #      div(id = ns("pyramid_download_data"), download_data_btns_ui(ns("download_pyramid_data")))
            )
          ), # close navset card pill
          
          # accordion panel with metadata table 
          div(id = ns("metadata_section"),
            #  div(uiOutput(ns("sex_split_text")), # chart header 
                  p("metadata here")
              )
          
      ) # close
    ))  # close layout sidebar)                 
  
} #close ui function

############




demog_simd_mod_server <- function(id, dataset, geo_selections, selected_profile, root_session){
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns

    # create demographic simd data filtered for the selected geography - only the first step in dataset filtering
    # ds =  demography_simd
    ds_filtered_geo_data <- reactive({
      dataset() |>
        filter(year== input$period_filter & areaname == geo_selections()$areaname & areatype == geo_selections()$areatype)
    })
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # dynamic filters ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # filter choices updated dynamically depending on the selected
    # indicator. Take data filtered by indicator (ind_data) and find
    # unique values from specific columns to set as choices
    
    

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
    
    
    observe({
      if (input$demog_simd_metric == "Populations") {
        shinyjs::show("age_filter")
      } else {
        shinyjs::hide("age_filter")
      }
    })
    
    
    
    # 
    # #######################################################.
    # ## Reactive data / values ----
    # #######################################################.
    # 
    # create demographic simd data filtered for the selected geography - only the first step in dataset filtering
    # ds =  demography_simd
    ds_filtered_geo_data <- reactive({
      dataset() |>
        filter(year== input$period_filter & areaname == geo_selections()$areaname & areatype == geo_selections()$areatype)
    })

    
    # filtered data to metric of interest - need to add a mechanism to provide count/percentage e.g. left/right
    ds_filtered_data <- reactive({
      req(ds_filtered_geo_data()) 
    
    data<-  switch(input$demog_simd_metric,
               
             "Datazone geographies" = list(
              
               top_data = ds_filtered_geo_data() |>
               filter(pop_grp=="dz") |>
               filter(measure_type=="count")|>
               select(simd_domain, D1:Total) |>
               rename_with(~ sub("^D?", "", .), D1:D10), #COULD SEE IF NAMING FIELDS WITH NUMBERS IS OK IN DATA PREP
             
               
               bottom_data =ds_filtered_geo_data() |>
                 filter(pop_grp=="dz") |>
                 filter(measure_type=="percent")|>
                 select(simd_domain, D1:Total) |>
                 rename_with(~ sub("^D?", "", .), D1:D10)
               
             ),
             
             "Populations" = list(
               
               top_data = ds_filtered_geo_data() |>
               filter(pop_grp==input$age_filter) |>
               filter(measure_type=="count") |>
               select(simd_domain, D1:Total) |>
               rename_with(~ sub("^D?", "", .), D1:D10),
               
               bottom_data = ds_filtered_geo_data() |>
                 filter(pop_grp==input$age_filter) |>
                 filter(measure_type=="percent") |>
                 select(simd_domain, D1:Total) |>
                 rename_with(~ sub("^D?", "", .), D1:D10)
               )
             )
      })
            
             
    # ds_filtered_data <- reactive({
    #   req(ds_filtered_geo_data()) 
    #   
    #   data<-  switch(input$demog_simd_metric,
    #                  
    #                  "Datazone geographies" = ds_filtered_geo_data() |>
    #                    filter(pop_grp=="dz") |>
    #                    filter(measure_type=="count")|>
    #                    select(simd_domain, D1:Total) |>
    #                    rename_with(~ sub("^D?", "", .), D1:D10), #COULD SEE IF NAMING FIELDS WITH NUMBERS IS OK IN DATA PREP
    #                  
    #                  "Populations" = ds_filtered_geo_data() |>
    #                    filter(pop_grp==input$age_filter) |>
    #                    filter(measure_type=="count") |>
    #                    select(simd_domain, D1:Total) |>
    #                    rename_with(~ sub("^D?", "", .), D1:D10)
    #   )
    # })
    #         

    
    
    # ~~~~~~~~~~~~~~~~~~~~~
    # data table ----
    # ~~~~~~~~~~~~~~~~~~~~~
    output$count_table_top <- renderReactable({
      
      req(ds_filtered_data())
  
      reactable(ds_filtered_data()$top_data,
                defaultColDef = colDef(align = "center"),
                
                columns = list(
                  #could just rename this column in data prep to simplify - TO DO
                  simd_domain = colDef(name = "SIMD Domain", align= "left")
                  ),

                columnGroups = list(
                  colGroup("Population weighted deprivation decile", columns = c("1","2","3","4","5","6","7","8","9","10"))),
                bordered = TRUE,
                striped = TRUE,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(backgroundColor = "#3F3685", color = "white", textAlign="left"),
                  groupHeaderStyle = list(backgroundColor = "#3F3685", color = "white", textAlign = "center"))
      )
      
      
    }) #close pyramid mini data table
    
    #####################################.
    # DYNAMIC TEXT ----
    ####################################.
    
    output$title_count <- renderUI({

      # display 3 x titles
      div(tags$h5({geo_selections()$areaname}, " population by SIMD decile"),
          tags$h6({input$period_filter}," , ",{input$age_filter})
          
          )
        # tags$h5(output$areaname, class = "chart-header"), # selected indicator
        # tags$h6(" to ", output$year), # time range
        # tags$p(input$age_filter) # type definiton
      #)
      
    })

    
    # Dynamic elements of population pyramid title/subtitles
    output$areaname <- renderText({geo_selections()$areaname})
    output$year <- renderText({input$period_filter})
    output$agegrp <- renderText({input$age_filter})

    
    
    
    
  }) #close moduleServer
} # close server function


