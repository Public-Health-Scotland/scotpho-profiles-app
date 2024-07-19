# to do :
# sort time comparison dummbell chart colours?
# add mini app to show how module works


################################
# MODULE: rank_charts_mod ---- 
# prepares the rank section 
################################

## ui function -----------------------------------------------------------------------
# id = unique id 
rank_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    layout_sidebar(
      full_screen = FALSE,
      #height = 650,
      
      # enable guided tour
      use_cicerone(),
      
      # sidebar for filters ------------------
      sidebar = sidebar(width = 300,
                        
                        # guided tour button
                        actionButton(inputId = ns("rank_tour_button"),
                                     label = "Click here for a guided tour of this page"),

                        # indicator filter (note this is a module)
                        div(id = ns("rank_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("indicator_filter"))),
                        
                        # indicator definition button
                        indicator_definition_btn_ui(ns("rank_ind_def"),class="act-btn"),
                        
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
                                        choices = rank_area_comparators_list,
                                        selected = "Scotland")
                          ),
                          conditionalPanel(
                            ns = NS(id),
                            condition = "input['comparator_type'] === 'Time'",
                            selectizeInput(inputId = ns("year_comparator"),
                                        label = "Select comparator year",
                                        choices = NULL)
                          )
                        ) # close hidden comparator filters panel
      ), # close sidebar
      
      layout_column_wrap(
        # bar chart card ----------------------
        # NOTE: the 'footer' argument for navset_card_pill() is currently not working
        # package maintainers are aware and working on a fix
        # using the card_footer argument for card() in the meantime and suppressing warnings until bug fixed
        suppressWarnings(
          navset_card_pill(
            id = ns("rank_navset_card_pill"),
            full_screen = TRUE,
            height = 550,
            
            # charts tab
            nav_panel("Charts",
                      value = ns("rank_chart_tab"), #id for guided tour
                      uiOutput(ns("rank_title")), # title
                      highchartOutput(ns("rank_chart")) |> # chart
                        withSpinner() |> (\(x) {
                          x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
                          x})()
            ),
            
            # data tab
            nav_panel("Data",
                      value = ns("rank_data_tab"), #id for guided tour
                      reactableOutput(ns("rank_table")) # table
            ),
            
            # metadata tab
            nav_panel("Metadata",
                    value = ns("rank_metadata_tab"), #id for guided tour
                    uiOutput(ns("rank_metadata"))
          ),
     
            nav_spacer(),
            nav_item(
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("ci_switch"), label = " include confidence intervals", TRUE)
              )
            ),
            card_footer(class = "d-flex justify-content-between",
                        div(id = ns("rank_download_chart"), download_chart_mod_ui(ns("save_rank_chart"))),
                        div(id = ns("rank_download_data"), download_data_btns_ui(ns("rank_download"))))
          )),
       
        # map card -------------------
        
        div(id = ns("rank_map_wrapper"),
        card(
          height = 550,
          full_screen = TRUE,
          leafletOutput(ns("rank_map")) |> # map
            withSpinner() |> (\(x) {
              x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
              x})()
        )
        
      ) # close layout column wrap
    ) # close layout sidebar
  ) # close taglist
} # close ui function 





# server function:
# id = unique id 
# profile_data = reactive df in main server
# geo_selections <- reactive values in main server storing global geography selections
rank_mod_server <- function(id, profile_data, geo_selections, techdoc) {
  moduleServer(id, function(input, output, session) {
    

    # permits compatibility between shiny and cicerone tours
    # req(active_nav() == nav_id)

    ns <- session$ns
    
    #######################################################
    # Dynamic filters
    #######################################################
    
    # update choices in years filter if "time" selected as comparator
    observe({
      req(profile_data())
      req(selected_indicator())
      
      x <- profile_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype)
      
      updateSelectizeInput(session, inputId = "year_comparator",
                        choices = unique(x$def_period))
    })
    

    # disable confidence interval checkbox when time selected as comparator
    observe({
      if (input$comparator_switch == TRUE & input$comparator_type == "Time") {
        shinyjs::disable("ci_switch")
      } else {
        enable("ci_switch")
      }
      
      
    })
    
    
    #######################################################
    ## Reactive data / values ----
    #######################################################
    
    # stores selected indicator ----------------------------------------------
    # (note this is a module )
    selected_indicator <- indicator_filter_mod_server(id = "indicator_filter", 
                                                      profile_data, geo_selections)
    
    # calls definition button module server script and passes the actual indicator selected)
    indicator_definition_btn_server("rank_ind_def", selected_indicator = selected_indicator)  
    
    # prepares data to be plotted --------------------------------------------
    rank_data <- reactive({
      req(profile_data())
      req(selected_indicator())
      
      profile_data <- setDT(profile_data()) # set profile data to data.table format
      
      # filter by selected areatype
      if(!(geo_selections()$areatype == "Scotland")){
        dt <- profile_data[areatype == geo_selections()$areatype]
      } 
      else {
        dt <- profile_data[areatype != "Scotland"][areatype == geo_selections()$areatype]
      }
      
      
      # additional filtering of parent area if IZ/HSCL selected
      if(geo_selections()$areatype %in% c("Intermediate zone", "HSC locality")) {
        dt <- dt[parent_area == geo_selections()$parent_area]
      }
      
      # filter by selected indicator
      dt <- dt[indicator == selected_indicator()]
      
      # get comparator values (if required)
      comp_vals <- NULL
      if(input$comparator_switch == TRUE){
        #if comparator is area then return one single value (the value of the chosen comparator area for the latest year)
        if(input$comparator_type == "Area"){
          comp_vals <- profile_data[indicator == selected_indicator() & areaname == input$area_comparator,
                                    .SD[year == max(year)], by = indicator]$measure
        } else if(input$comparator_type == "Time"){
          # if comparator is time then return a column with values (one for each area for selected time period)
          comp_vals <- profile_data[indicator == selected_indicator() & areatype == geo_selections()$areatype & def_period == input$year_comparator]
          comp_vals <- comp_vals[,c("code", "measure")]
          comp_vals <- setnames(comp_vals, "measure", "comp_vals")
        }
      }
      
      # filter by latest year
      dt <- dt[year == max(year)]
      
      
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
                                       lowci <= comp_vals & upci >= comp_vals, '#cccccc',
                                       (lowci > comp_vals & interpret == "H") |  (upci < comp_vals & interpret == "L"),'#4da6ff',
                                       (lowci > comp_vals & interpret == "L") | (upci < comp_vals & interpret == "H"), '#ffa64d',
                                       default = '#ccccff')]
      } else {
        dt <- dt[, colour_pal := fifelse(areaname == geo_selections()$areaname, phs_colors("phs-purple"), phs_colors("phs-blue"))]
      }
      
      # order by measure
      dt <- setorder(dt, measure)
      
    })
    
    
    
    
    # map data --------------------------------------
    # dynamically selects shapefile and joins with map data
    map_data <- reactive({
      req(rank_data())
      
      # create dynamic text to explain map unavailability if ADP selected
      shiny::validate(
        need( !(geo_selections()$areatype == "Alcohol & drug partnership"), "Please note that the map is currently unavailable when Alcohol & drug partnership is selected.")
      )
      
      # create dynamic text to explain map unavailability if Scotland selected
      # brief message as accompanied by further detail in chart panel
      shiny:: validate(
        need( !(geo_selections()$areatype == "Scotland"), "Please note that map data is unavailable for Scotland.")
      )
      
      # create dynamic text to explain map unavailability if no available indicators for selected geography and profile
      # brief message as accompanied by further detail in chart panel
      shiny:: validate(
        need(nrow(rank_data()) > 0, "Map data unavailable.")
      )
      
      # don't create map data if Alcohol & drug partnership selected in global filters
      req(geo_selections()$areatype != "Alcohol & drug partnership")
      
      # get correct shapefile
      x <- switch(geo_selections()$areatype,
                  "Health board" = hb_bound,
                  "Council area" = ca_bound,
                  "HSC partnership" = hscp_bound,
                  "HSC locality" = hscloc_bound,
                  "Intermediate zone" = iz_bound
      )
      
      # further filter if HSCL or IZ selected, 
      if(geo_selections()$areatype == "HSC locality"){
        x <- x |> filter(parent_area == geo_selections()$parent_area)
      } else if(geo_selections()$areatype == "Intermediate zone"){
        x <- x |> filter(council == geo_selections()$parent_area)
      } else{ 
        x
      }
      x <- x |> left_join(rank_data(), by = join_by(code))
      
    })

     
     #######################################################
     ## Dynamic text  ----
     #######################################################

     # title ---------
     output$rank_title <- renderUI({
       req(rank_data())
       
       # create dynamic text if no indicators available for selected profile
       # and geography / if Scotland selected
       shiny::validate(
         need( nrow(rank_data()) > 0, "No indicators available for this profile and area type.")
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
     
        # display 3 x titles
        tagList(
         tags$h5(selected_indicator(), class = "chart-header"), # selected indicator
         chart_desc, # chart description
         tags$p(rank_data()$type_definition[1]), # measure type
         
        )
      
     })
    
    # info to display when user clicks metadata tab
    output$rank_metadata <- renderUI({
      
      #create dataframe containing only notes_caveats column for selected indicator from techdoc
      indicator_caveats <- techdoc |> 
        filter(indicator_name == selected_indicator()) |> 
        select(notes_caveats)
      
      #print notes and caveats for selected indicator
      tags$p(indicator_caveats)
    })

    # # info to display when user clicks help button (explains how to interpret)
    # output$rank_help <- renderUI({ 
    #   req(selected_indicator())
    #   tagList(
    #     tags$h5("How to use this chart"),
    #     p(paste0("The charts below allow you rank each ", geo_selections()$areatype, " for your selected indicator (in this case, ", selected_indicator(), ").")),
    #     p("You can also choose to add a baseline comparator, to assess whether each area in your chosen geography level is statistically significantly better or worse than your comparator."),
    #     p("For example, you may want to assess whether each ", geo_selections()$areatype, " is significantly higher or lower than a particular geographical area (for instance, the national average) or
    #              whether there are particular areas in your chosen geography level that are significantly higher or lower than they were at another point in time (e.g. a decade ago)"),
    #     br(),
    #     p("If a comparator is selected, the chart and the map will be colour coded using the key below:"),
    #     fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:orange; border-radius:50%; display:inline-block; margin:5px;"), "orange - statistically significantly better")),
    #     fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:blue; border-radius:50%; display:inline-block; margin:5px;"), "blue - statistically significantly worse")),
    #     fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:gray; border-radius:50%; display:inline-block; margin:5px;"), "grey - not statistically different to Scotland")),
    #     fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:white; border:1px solid black; outline-color:black; border-radius:50%; display:inline-block; margin:5px;"), "white - no difference to be calculated"))
    #     
    #   )
    # })
    
    
    ############################################
    # Visualisations / data table  ----
    #############################################
    
    # chart (barchart/dumbell chart)
    output$rank_chart <- renderHighchart({
      req(rank_data())
      
      
      # create dynamic text if no indicators available for selected profile
      # and geography / if Scotland selected
      shiny::validate(
        need( nrow(rank_data()) > 0, "Please note that rank data is unavailable at Scotland-level for all profiles.
          
Not all profiles have available indicators for all geography types. The drugs profile has no available indicators for intermediate zones and the mental health profile has no available indicators for intermediate zones or HSC localities. Please select another profile or geography type to continue.")
      )
      
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
            chartOptions = list(
              title = list(text = paste0(selected_indicator()))
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
    
    
    # leaflet map -------
    
    # Global definition of value_palette
    value_palette <- reactive({
      req(map_data())
      if(length(unique(map_data()$measure)) > 1) {
        colorNumeric(palette = "Blues", domain = map_data()$measure)
      } else {
        function(x) { phs_colors(colourname = "phs-purple") }
      }
    })
    
    
    
    output$rank_map <- renderLeaflet({
      req(map_data())
      leaflet(map_data()) |>
        addProviderTiles(provider = providers[["OpenStreetMap"]]) |>
        addPolygons(weight = 1, 
                    color = "black",
                    fillColor = ~value_palette()(measure),
                    fillOpacity = 0.5, 
                    smoothFactor = 0.5, 
                    opacity = 1, 
                    label = ~paste0(map_data()$areaname, ": ", map_data()$measure),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "13px",
                      direction = "auto"
                    ),
                    highlightOptions = highlightOptions(
                      color = "white", 
                      weight = 2, 
                      bringToFront = TRUE
                    )
        ) |>
        
        # add option to save chart as png
        onRender(
          "function(el, x) {
            L.easyPrint({
              sizeModes: ['Current'],
              filename: 'scotpho-map',
              exportOnly: true,
              hideControlContainer: false
            }).addTo(this);
            }"
        )
    })
    
    # update map when comparator toggled on/off
    observe({
      if(input$comparator_switch == TRUE) {
        leafletProxy("rank_map", session) |>
          clearShapes() |>
          clearControls() |>
          addPolygons(data = map_data(), 
                      weight = 1, 
                      color = "black", fill = TRUE, fillColor = ~colour_pal,
                      fillOpacity = 0.5, smoothFactor = 0.5, opacity = 0.8,
                      highlightOptions = highlightOptions(color = "white", weight = 2, opacity = 1, bringToFront = FALSE)) |>
          addLegend(colors = c("#4da6ff",  "#ffa64d", "#ccccff", "#999966"),
                    labels = c("better than comparator", "worse than comparator", "no difference", "N/A"),
                    position = "bottomright")
        
      } else {
        leafletProxy("rank_map", session) |>
          clearShapes() |>
          clearControls() |>
          addPolygons(data = map_data(), weight = 1, color = "black",
                      fillColor = ~value_palette()(measure),
                      fillOpacity = 0.5, 
                      smoothFactor = 0.5, 
                      opacity = 1, 
                      label = ~paste0(map_data()$areaname, ": ", map_data()$measure),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
      }
    })
    
    
    
    # data table ----
    output$rank_table <- renderReactable({
      req(rank_data())
      
      data <- rank_data() |>
        mutate(`Area name` = areaname,
               Measure = measure,
               `Upper CI` = upci,
               `Lower CI` = lowci) |>
        select(`Area name`, Measure, `Upper CI`, `Lower CI`) |>
        arrange(Measure)
      
      reactable(data,
                defaultExpanded = T,
                defaultPageSize = nrow(data))
    })
    
    
     
     ######################################
     # Downloads -------
     ######################################
     
     # note these are both modules 
     download_chart_mod_server(id = "save_rank_chart", chart_id = session$ns("rank_chart")) # save chart as png
     
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
     
     
     ############################################
     # Guided tour
     ###########################################
     
     guide_rank<- Cicerone$
       new()$
       step(
         ns("rank_chart"), #chart tab
         "Chart Tab",
         "These charts allow areas to be ranked against others of the same type.<br>
     You can also add a baseline comparator to assess whether each area in your chosen geography level is statistically significantly better or worse than your comparator.<br>
     For example, you may want to assess whether each  is significantly higher or lower than a particular geographical area (for instance, the national average) or whether there are particular 
     areas in your chosen geography level that are significantly higher or lower than they were at another point in time (e.g. a decade ago)",
         position = "right",
         tab_id = ns("rank_navset_card_pill"),
         tab = ns("rank_chart_tab")
       )$
       step(
         ns("rank_table"), #table tab
         "Data Tab",
         "The data tab shows the figures underpinning the chart for the selected indicator.",
         position = "right",
         tab_id = ns("rank_navset_card_pill"),
         tab = ns("rank_data_tab")
       )$
       step(
         ns("rank_metadata"), #metadata tab
         "Metadata Tab",
         "The metadata tab shows information about the data source, methodological information, advice on interpretation, and any known data quality issues.",
         position = "right",
         tab_id = ns("rank_navset_card_pill"),
         tab = ns("rank_metadata_tab")
       )$
       step(
         ns("rank_map_wrapper"),
         "Compare Areas Spatially",
         "This map allows spatial comparison of areas. Darker shading represents higher values for the selected indicator with lighter shading representing lower values.<br> 
     Hover over an area of the map to view the name of the area and its value.<br>
     Please note that the shading is relative to other areas of the same type; therefore two areas of different shades may have similar absolute values for the indicator if variability between areas is low.",
         position = "left"
       )$
       step(
         ns("rank_indicator_filter_wrapper"), 
         "Indicator Filter",
         "First select an indicator.<br>
     The list has been filtered based on profile and area type selected at the top of the page.<br>
     The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
         position = "bottom"
       )$
       step(
         ns("rank_comparator_wrapper"),
         "Select a Comparator",
         "Select a comparator to see whether each area
    within your chosen geography level (e.g. health boards) is statistically significantly
    better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago).",
         position = "bottom"
       )$
       step(
         ns("rank_download_chart"),
         "Download Chart Button",
         "Click here to download this chart as a PNG.",
         position = "bottom"
       )$
       step(
         ns("rank_download_data"),
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



