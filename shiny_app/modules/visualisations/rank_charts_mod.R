# to do :
# sort time comparison dummbell chart colours?
# add mini app to show how module works

########################################################.
# MODULE: rank_charts_mod ---- 
# prepares the rank section 
########################################################.



#######################################################.
## MODULE UI ----
#######################################################.
# id = unique id 
rank_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    layout_sidebar(
      full_screen = FALSE,

      # sidebar for filters ------------------
      sidebar = sidebar(width = 300,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content
                        

                        # indicator filter (note this is a module)
                        div(id = ns("rank_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("indicator_filter"))),
                        
                        # button to scroll to metadata
                        div(id = ns("rank_scroll_button"), metadata_scroll_button_UI(id = ns("scroll_btn"), target_id = ns("metadata_section"))),
                        
                        # time period filter 
                        selectizeInput(
                          inputId = ns("period_filter"),
                          label = "Select time period:",
                          choices = NULL # choices dynamically updated in server depending on selected indicator
                        ),

                        
                        # comparator switch filter 
                        div(id = ns("rank_comparator_wrapper"),
                            bslib::input_switch(id = ns("comparator_switch"), 
                                            value = FALSE, 
                                            label = bslib::tooltip(placement = "bottom",trigger = list("Include comparator",icon("info-circle")),
                                                                   "Including a comparator will allow you to see whether each area
                                             within your chosen geography level (e.g. health boards) is statistically significantly
                                             better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago)."))
                        ),
                        
                        # additional hidden filters to display when comparator switch turned on
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
                            selectizeInput(inputId = ns("area_comparator"),
                                        label = "Select comparator area",
                                        choices = NULL,
                                        )
                          ),
                          conditionalPanel(
                            ns = NS(id),
                            condition = "input['comparator_type'] === 'Time'",
                            selectizeInput(inputId = ns("year_comparator"),
                                        label = "Select comparator year",
                                        choices = NULL)
                          )
                        ), # close hidden comparator filters panel
                        
                        # guided tour button
                        actionLink(inputId = ns("rank_tour_button"), label = "Take a guided tour of this page")
      ), # close sidebar
      
      layout_column_wrap(
        # bar chart card ----------------------
          
        div(id = ns("rank_chart_wrapper"),
        navset_card_pill(
            id = ns("rank_navset_card_pill"),
            full_screen = TRUE,
            height = 600,
            
            # charts tab
            nav_panel("Charts",
                      value = ns("rank_chart_tab"), #id for guided tour
                      uiOutput(ns("rank_title")), # title
                      highchartOutput(ns("rank_chart")) |> # chart
                        withSpinner() |> 
                        bslib::as_fill_carrier() 
            ),
            
            # data tab
            nav_panel("Data",
                      value = ns("rank_data_tab"), #id for guided tour
                      reactableOutput(ns("rank_table")) # table
            ),
     
            nav_spacer(),
            nav_item(
              div(id = ns("rank_popover"),
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("ci_switch"), label = " include confidence intervals", TRUE)
              )
            )
            ),
            footer = card_footer(class = "d-flex justify-content-left",
                        div(id = ns("rank_download_chart"), download_chart_mod_ui(ns("save_rank_chart"))),
                        div(id = ns("rank_download_data"), download_data_btns_ui(ns("rank_download"))))
          )),
       
        # map card -------------------
        
        div(id = ns("rank_map_wrapper"),
        card(
          height = 600,
          full_screen = TRUE,
          leafletOutput(ns("map")) |> # map
            withSpinner() |> 
            bslib::as_fill_carrier() 
        ))
        
      ), # close layout column wrap
      
      # accordion panel with metadata table 
      div(id = ns("metadata_section"), metadata_panel_UI(ns("metadata_table")))
    ) # close layout sidebar
  ) # close taglist
} # close ui function 


#######################################################.
## MODULE SERVER ----
#######################################################.

# id = unique id 
# profile_data = reactive dataframe created in main server script contains main data already filtered by profile
# geo_selections = reactive values in main server stores global geography selections
# selected_profile = name of reactive value stores selected profile from main server script

rank_mod_server <- function(id, profile_data, geo_selections, selected_profile, root_session) {
  moduleServer(id, function(input, output, session) {
    

    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Selected indicator -----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    selected_indicator <- indicator_filter_mod_server(
      id = "indicator_filter",
      profile_data, # data from app main server filtered by profile and areatype (+ Scotland)
      geo_selections, # reactive vals from main server storing geography selections (areaname, areatype, parent_area)
      selected_profile # reactive vals from main server storing details about selected profile
    )
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Indicator data ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    ind_data <- reactive({
      # only run when Scotland not selected
      # this prevents all other code in this module using ind_data() from being run.
      req(geo_selections()$areatype != "Scotland")
      
      profile_data() |>
        filter(indicator == selected_indicator()) |>
        arrange(desc(year))
      
    })
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # dynamic filters ----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # filter choices updated dynamically depending on the selected
    # indicator. Take data filtered by indicator (ind_data) and find
    # unique values from specific columns to set as choices
    
    
    ## year filter ----
    observeEvent(ind_data(), {
      
      # get available def periods for selected indicator AND selected area
      # (important as e.g. Scotland may have more up to date data
      # or have different level of aggregation than selected area)
      choices <- unique(ind_data()$def_period[ind_data()$areaname == geo_selections()$areaname])
      
      # avoid transient invalid values while updating filter
      shiny::freezeReactiveValue(input, "period_filter")
      
      # update filter choices
      updateSelectizeInput(
        session = session,
        inputId = "period_filter",
        choices = choices,
        selected = if (length(choices)) choices[[1]] else NULL
      )
      
    }, ignoreInit = FALSE)
    
    
    
    ## time comparator filter ----
    observe({
      
      # only run code when comparator switch turned on and time selected as comparator
      req(input$comparator_switch)
      req(input$comparator_type == "Time")
      
      # get choices 
      choices <- ind_data() |>
        filter(areatype == geo_selections()$areatype &
                 def_period != input$period_filter) |>
        pull(unique(def_period))
      
      
      # avoid transient invalid values while updating
      shiny::freezeReactiveValue(input, "year_comparator")
      
      # update filter choices 
      updateSelectizeInput(
        session = session,
        inputId = "year_comparator",
        choices = choices,
        selected = if (length(choices)) choices[[1]] else NULL
      )
      
    })
    
    
    
    ## area comparator filter ----
    observe({
      
      # only run code when comparator switch turned on and time selected as comparator
      req(input$comparator_switch)
      req(input$comparator_type == "Area")
      
      # get choices 
      choices <- ind_data() |>
        filter(def_period == input$period_filter) |>
        filter(areaname != geo_selections()$areaname) |>
        pull(unique(areaname))
      
      # avoid transient invalid values while updating
      shiny::freezeReactiveValue(input, "area_comparator")
      
      # update filter choices
      updateSelectizeInput(
        session = session,
        inputId = "area_comparator",
        choices = choices,
        selected = "Scotland"
      )
      
    })
    
    
    

    
    # disable confidence interval checkbox when time selected as comparator
    observe({
      if (input$comparator_switch == TRUE & input$comparator_type == "Time") {
        shinyjs::disable("ci_switch")
      } else {
        enable("ci_switch")
      }
      
      
    })
    

    
    
    
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # visualisation data -----
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    rank_data <- reactive({
      
      # set indicator data to data.table format
      ind_data <- setDT(ind_data()) 
      
      
      # filter by selected areatype
      dt <- ind_data[areatype == geo_selections()$areatype]
      
      
      # additional filtering of parent area if IZ/HSCL selected
      if(geo_selections()$areatype %in% c("Intermediate zone", "HSC locality")) {
        dt <- dt[parent_area == geo_selections()$parent_area]
      }
    
    
      # get comparator values (if required)
      comp_vals <- NULL
      if(input$comparator_switch == TRUE){
        
        # if comparator is area then return one single value 
        # (the value of the chosen comparator area for the latest year)
        if(input$comparator_type == "Area"){
          comp_vals <- ind_data[areaname == input$area_comparator & def_period == input$period_filter]$measure
          
          # if comparator is time then return multiple values 
          # (one for each area for selected time period)
        } else if(input$comparator_type == "Time"){
          comp_vals <- ind_data[areatype == geo_selections()$areatype & def_period == input$year_comparator]
          comp_vals <- comp_vals[,c("code", "measure")]
          comp_vals <- setnames(comp_vals, "measure", "comp_vals")
        }
      }
      
      
      # filter by selected time period
      dt <- dt[def_period == input$period_filter]
      
      
      # attach comparator values as a column
      if(input$comparator_switch == TRUE){
        if(input$comparator_type == "Time") {
          dt <- dt[comp_vals, on = "code"]
          dt <- dt[, high := fifelse(measure < comp_vals, measure, comp_vals)]
          dt <- dt[, low := fifelse(measure > comp_vals, measure, comp_vals)]
        } else if(input$comparator_type == "Area"){
          dt <- dt[, comp_vals := comp_vals]
          dt <- dt[, diff := measure - comp_vals]
        }
      }
      

      
      # prepare colour palette if comparator is selected (i.e. orange, blue, grey, green)
      if(input$comparator_switch == TRUE) {
        
        dt <- dt[, colour_pal := fcase(interpret == "O", '#999966',
                                       is.na(lowci) | is.na(upci) | is.na(comp_vals) | is.na(measure) | measure == 0, '#999966',
                                       # lowci <= comp_vals & upci >= comp_vals, '#cccccc',
                                       (lowci > comp_vals & interpret == "H") |  (upci < comp_vals & interpret == "L"),'#4da6ff',
                                       (lowci > comp_vals & interpret == "L") | (upci < comp_vals & interpret == "H"), '#ffa64d',
                                       default = '#ccccff')]
      } else {
        dt <- dt[, colour_pal := fifelse(areaname == geo_selections()$areaname, phs_colors("phs-purple"), phs_colors("phs-blue"))]
      }
      
      # order by measure
      dt <- setorder(dt, measure)
      
      dt
      
    })
        
        
     
    
    # ~~~~~~~~~~~~~~~~~~~~~~~
    # get shapefile ----
    # ~~~~~~~~~~~~~~~~~~~~~~~
    
    shapefile <- eventReactive(geo_selections(), {
      
      # dont run if areatype is Scotland or ADP - no shapefiles for these
      req(!geo_selections()$areatype %in% c("Scotland", "Alcohol & drug partnership"))
      
      # get shapefile depending on selected areatype
      shp <- switch(geo_selections()$areatype, 
                    "Health board" = hb_bound,
                    "Council area" = ca_bound,
                    "HSC partnership" = hscp_bound,
                    "HSC locality" = hscloc_bound,
                    "Intermediate zone" = iz_bound,
                    "Police division" = pd_bound
      )
      
      # further filter shapefile by parent area if IZ/locality selected
      if(geo_selections()$areatype %in% c("Intermediate zone", "HSC locality")){
        shp <- shp |> 
          filter(parent_area == geo_selections()$parent_area)
      }
      
      shp
      
    }, ignoreInit = FALSE)
    
    
    
    
    
    
     #######################################################.
     ## Dynamic text  ----
     #######################################################.

     # title
     output$rank_title <- renderUI({
       req(rank_data())
       
       # create dynamic text if no indicators available for selected profile
       # and geography / if Scotland selected
       shiny::validate(
         need(nrow(rank_data()) > 0, "No indicators available for this profile and area type.")
       )
       
       # get definition period
       max_year <- rank_data()$def_period[1]
       
   # prepare areaname (including parent area if IZ/HSCL selected)
      area <- if(geo_selections()$areatype == "HSC locality") {
        paste("HSC Localities in ",geo_selections()$parent_area)
      } else if(geo_selections()$areatype == "Intermediate zone"){
        paste("Intermediate zones in ",geo_selections()$parent_area)
      } else {
        geo_selections()$areatype
      }
      
      # prepare description of what map/chart show depending on
      # whether comparator included (and if so which comparator)
      chart_desc <- if(input$comparator_switch == TRUE){
        if(input$comparator_type == "Area"){
          tags$p(paste(area,"comparison against",input$area_comparator, " - ", input$period_filter))
        } else if(input$comparator_type == "Time"){
          tags$p(area,"- ", input$period_filter,"compared to ",input$year_comparator)
        }
      } else {
        tags$p(area, " - ",input$period_filter)
      }
     
        # display 3 x titles
        div(
         tags$h5(selected_indicator(), class = "chart-header"), # selected indicator
         chart_desc, # chart description
         tags$p(rank_data()$type_definition[1]), # measure type
         
        )
        
     })
      

   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   # CHART  ----
   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # chart (barchart/dumbell chart)
    output$rank_chart <- renderHighchart({
      req(rank_data())
      
      
      # create dynamic text if no indicators available for selected profile
      # and geography / if Scotland selected
      shiny::validate(
        need(nrow(rank_data()) > 0, "Not all profiles have available indicators for all geography types. 
              The drugs profile has no available indicators for intermediate zones and the mental health 
              profile has no available indicators for intermediate zones or HSC localities. Please 
              select another profile or geography type to continue.")
      )
      
      
      
      # get definition period
      max_year <- rank_data()$def_period[1]
      
      # prepare areaname (including parent area if IZ/HSCL selected)
      area <- if(geo_selections()$areatype == "HSC locality") {
        paste("HSC Localities in ",geo_selections()$parent_area)
      } else if(geo_selections()$areatype == "Intermediate zone"){
        paste("Intermediate zones in ",geo_selections()$parent_area)
      } else {
        geo_selections()$areatype
      }
      
      # prepare description of what map/chart show depending on
      # whether comparator included (and if so which comparator)
      chart_desc <- if(input$comparator_switch == TRUE){
        if(input$comparator_type == "Area"){
          tags$p(paste(area,"comparison against",input$area_comparator, " - ", max_year))
        } else if(input$comparator_type == "Time"){
          tags$p(area,"- ",max_year,"compared to ",input$year_comparator)
        }
      } else {
        tags$p(area, " - ", max_year)
      }
      
      # if there' no comparator selected, or the selected comparator is "area" then create a bar chart
      if(input$comparator_switch == FALSE | (input$comparator_switch == TRUE & input$comparator_type == "Area")) {
        
        x <-  hchart(object = rank_data(), 
                     type = "bar", hcaes(x = areaname, y = measure, color = colour_pal)) |>
          hc_yAxis(gridLineWidth = 0) |>
          hc_xAxis(title = list(text = "")) |>
          hc_yAxis(title = list(text = "")) |>
          hc_chart( backgroundColor = 'white') |>
          hc_plotOptions(series = list(animation = FALSE)) |>
          hc_tooltip(
            headerFormat = "<table>",
            pointFormat = paste(
              '<tr><th colspan="2"><p>{point.areaname}</p></th></tr>',
              "<tr><th>{point.type_definition}</th><td>{point.measure}</td></tr>",
              "<tr><th>upper ci:</th><td>{point.upci}</td></tr>",
              "<tr><th>lower ci:</th><td>{point.lowci}</td></tr>"
            ),
            footerFormat = "</table>",
            useHTML = TRUE
          ) |>
          # title for downloaded version
          hc_exporting(
            filename = paste0("ScotPHO rank - ", selected_indicator(), " - ", geo_selections()$areatype),
            chartOptions = list(
              title = list(text = paste0(selected_indicator(), " split by ", geo_selections()$areatype)),
              subtitle = list(text = paste0(chart_desc)),
              yAxis = list(title = list(text = paste0(unique(rank_data()$type_definition))))
            )
          )
        
        
        
        # add red comparator line if "area" selected as comparator
        if(input$comparator_switch == TRUE & input$comparator_type == "Area"){
          x <- x |>
            hc_yAxis(plotLines = list(list(color = "red", width = 3, value = rank_data()$comp_vals[1], zIndex = 10)))
        }
        
        
        
        # include confidence intervals when ci switch is turned on on
        if(input$ci_switch == TRUE) {
          x <- x |>
            hc_add_series(rank_data(), "errorbar", hcaes(x = areaname, low = lowci, high = upci), zIndex = 10)
        }
        
        
        # if the selected comparator is "Time" then build a dumbbell chart instead
      } else if (input$comparator_switch == TRUE & input$comparator_type == "Time") {
        
        x <- hchart(object = rank_data(), type = "dumbbell", hcaes(low = low, high = high, name = areaname)) |>
          hc_xAxis(type = "category") |>
          hc_tooltip(shared = TRUE) |>
          hc_legend(enabled = FALSE) |>
          hc_chart(inverted = TRUE)
      }
      
      
      x 
      
      
    })
    
    
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~
    # MAP -------
    # ~~~~~~~~~~~~~~~~~~~~~~~
    
  

    # create base map ----
    # This is only re-rendered when a user switches areatype or parent area
    # and a new shapefile needs to be plotted. It just creates the basic map without
    # any data points 
    output$map <- renderLeaflet({
      req(shapefile())
      
      # create basic map using shapefile
      map <- leaflet(shapefile()) |>
        addPolygons(
          layerId = ~code,
          group  = "areas",
          color = "white"
        )
      
      # If small areas selected (IZ/locality) then add provider tiles
      # so that map is detailed with streets
      if(!geo_selections()$areatype %in% c("Council area", "Health board", "HSC partnership")){
        map <- map |>
          addProviderTiles(provider = providers[["OpenStreetMap"]])
      }
      
      map
    })
    
    

    # update polygon fill and legend according to indicator selection
    # and whether comparator selected or not 
    observeEvent(rank_data(), {
      req(shapefile())
      
      # if there is data to plot
      if(nrow(rank_data()) > 0) {
        
        # get shapefile
        shp <- shapefile()
        
        # join data with shapefile
        shp <- left_join(shp, rank_data(), by = "code")
        
        # create colour palette
        palette <- if (input$comparator_switch) {
          colorFactor(palette = unique(shp$colour_pal), domain = shp$colour_pal)
        } else {
          colorNumeric(palette = "YlOrRd", domain = shp$measure)
        }
        
        # create label for tooltips
        shp$label <- paste0(
          "<b>Area name: </b> ", shp$areaname,"<br>",
          "<b>Measure type: </b>", shp$type_definition, "<br>",
          "<b>Rate: </b> ", format(shp$measure, nsmall=0, big.mark=","))
        
        # update map
        map <- leafletProxy("map", data = shp) |>
          clearMarkers() |>
          clearControls() |>
          setShapeStyle(
            layerId = ~code,
            fillColor = ~palette(if (input$comparator_switch) colour_pal else measure),
            fillOpacity = 0.7,
            opacity = 1,
            label = shp$label,
            color = "white",
            weight = 1.2,
            stroke = TRUE
          )
        
        # legend if comparator selected
        if(input$comparator_switch){
          map <- map |>
            addLegend(
              colors = c("#4da6ff",  "#ffa64d", "#ccccff", "#999966"),
              labels = c("better than comparator", "worse than comparator", "no difference", "N/A"),
              position = "bottomleft"
            )
          # legend if no comparator selected
        } else {
          map <- map |>
            addLegend(
              position = "bottomleft",
              pal = palette,
              values = ~measure,
              title = "Measure",
              opacity = 1
            ) 
        }
        
        # if no data to plot then turn white 
      } else {
        map <- leafletProxy("map", data = shapefile()) |>
          clearMarkers() |>
          clearControls() |>
          setShapeStyle(
            layerId = ~code,
            fillColor = 'white'
          )
        
      }
    }, ignoreInit = FALSE)
    
    
    # update map when geography type changes regardless of whether
    # user on rank tab or not - ensures map is ready when user goes on tab.
    outputOptions(output, "map", suspendWhenHidden = FALSE)
    
    
 
    # ~~~~~~~~~~~~~~~~~~~~~
    # data table ----
    # ~~~~~~~~~~~~~~~~~~~~~
    output$rank_table <- renderReactable({
      req(rank_data())
      
      data <- rank_data() |>
        select(areaname, measure, upci, lowci) |>
        arrange(Measure)
      
      reactable(data,
                defaultExpanded = T,
                defaultPageSize = nrow(data),
                cols = list(
                  areaname = colDef(name = "Area name"),
                  measure = colDef(name = "Measure"),
                  upci = colDef(name = "Upper CI"),
                  lowci = colDef(name = "Lower CI")
                  )
                )
    })
    
  
    #########################.
    # Metadata ----
    #########################.
    indicator_metadata <- filter_metadata_Server("metadata", r_indicator = selected_indicator) # techdoc filtered by selected indicator 
    btn_click <- metadata_scroll_button_Server("scroll_btn") # tracking when metadata scroll button clicked 
    metadata_panel_Server("metadata_table", r_event = btn_click, r_metadata = indicator_metadata, parent_session = root_session) # panel with metadata table
    
    
     
     ######################################.
     # Downloads -------
     ######################################.
     
     # note these are both modules 
     download_chart_mod_server(id = "save_rank_chart", 
                               chart_id = ns("rank_chart"), 
                               height = if(geo_selections()$areatype == "Intermediate zone") {
                              1200 } else if(geo_selections()$areatype %in% c("Health board", "Police division")) {
                               600 } else if(geo_selections()$areatype %in% c("Council area", "HSC partnership", "Alcohol & drug partnership", "HSC locality")) {
                               700 } else {
                                 500
                               }) # save chart as png
     
     download_data_btns_server(id = "rank_download", 
                               data = rank_data, 
                               file_name = "Rank_ScotPHO_data_extract",
                               selected_columns = c("code", 
                                                    "areatype", 
                                                    "areaname", 
                                                    "indicator", 
                                                    "type_definition", 
                                                    "definition_period" = "def_period",
                                                    "numerator", 
                                                    "measure", 
                                                    "upper_confidence_interval" = "upci", 
                                                    "lower_confidence_interval" = "lowci"))
     
     
     ############################################.
     # Guided tour ----
     ############################################.
     
     guide_rank<- Cicerone$
       new()$
       step(
         ns("rank_chart_wrapper"), #chart tab
         "Chart Tab",
         "These charts allow areas to be ranked against others of the same type.<br>
     You can also add a baseline comparator to assess whether each area in your chosen geography level is statistically significantly better or worse than your comparator.<br>
     For example, you may want to assess whether each  is significantly higher or lower than a particular geographical area (for instance, the national average) or whether there are particular
     areas in your chosen geography level that are significantly higher or lower than they were at another point in time (e.g. a decade ago)",
         position = "right"
       )$
       step(
         ns("rank_popover"), # popover icon
         "Adjust Chart Settings",
         "Click here to add error bars to the chart."
       )$
       step(
         ns("rank_navset_card_pill"), #table tab
         "Other views",
         "You can switch between viewing the chart or data for your selected indicator using the buttons highlighted.",
         position = "right"
       )$
       step(
         ns("rank_map_wrapper"), # map tab
         "Compare Areas Spatially",
         "This map allows spatial comparison of areas. Darker shading represents higher values for the selected indicator with lighter shading representing lower values.<br> 
     Hover over an area of the map to view the name of the area and its value.<br>
     Please note that the shading is relative to other areas of the same type; therefore two areas of different shades may have similar absolute values for the indicator if variability between areas is low.",
         position = "left"
       )$
       step(
         ns("rank_indicator_filter_wrapper"), #Select indicator
         "Indicator Filter",
         "First select an indicator.<br>
     The list has been filtered based on profile and area type selected at the top of the page.<br>
     The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
         position = "bottom"
       )$
       step(
         ns("rank_scroll_button"), #Scroll to metadata button
         "Scroll to Metadata Button",
         "Click here to scroll to the metadata panel below.<br>
        It contains information about the selected indicator, including indicator definition, 
        data source, notes and caveats and links to relevant publications and pages on the ScotPHO website. "
       )$
       step(
         ns("rank_comparator_wrapper"), #Include comparator switch
         "Select a Comparator",
         "Select a comparator to see whether each area
    within your chosen geography level (e.g. health boards) is statistically significantly
    better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago).",
         position = "bottom"
       )$
       step(
         ns("rank_download_chart"), #Download chart
         "Download Chart Button",
         "Click here to download this chart as a PNG.",
         position = "bottom"
       )$
       step(
         ns("rank_download_data"), #Download data
         "Download Data Button",
         "Click here to download the selected data as a CSV, RDS or JSON file.",
         position = "left"
       )
     

     #initiate the guide
     guide_rank$init()
     
     #when guided tour button is clicked, start the guide
     observeEvent(input$rank_tour_button, {
       guide_rank$start()
     })
     
     
     }) # close moduleServer

} # close server function



