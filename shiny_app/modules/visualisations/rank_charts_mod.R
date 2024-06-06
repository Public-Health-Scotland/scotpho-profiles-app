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
      
      # sidebar for filters ------------------
      sidebar = sidebar(width = 300,
                        # help buttons
                        layout_columns(
                          actionButton(ns("rank_help"), label = "Help", class="act-btn"),
                          indicator_definition_btn_ui(ns("rank_ind_def"),class="act-btn")
                        ),
                        
                        # indicator filter (note this is a module)
                        indicator_filter_mod_ui(ns("indicator_filter")),
                        
                        # comparator switch filter 
                        bslib::input_switch(id = ns("comparator_switch"), 
                                            value = FALSE, 
                                            label = bslib::tooltip(placement = "bottom",trigger = list("Include comparator",icon("info-circle")),
                                                                   "Including a comparator will allow you to see whether each area
                                             within your chosen geography level (e.g. health boards) is statistically significantly
                                             better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago).")
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
                        ) # close hidden comparator filters panel
      ), # close sidebar
      
      layout_column_wrap(
      # bar chart card ----------------------
      navset_card_pill(
        full_screen = TRUE,
        nav_panel("chart",
                  uiOutput(ns("rank_title")), # title
                  highchartOutput(ns("rank_chart")) # chart
        ),
        nav_panel("data",
                  reactableOutput(ns("rank_table")) # table
        ),
        nav_spacer(),
        nav_item(
          bslib::popover(
            title = "Filters",
            bsicons::bs_icon("gear", size = "1.7em"),
            checkboxInput(ns("ci_switch"), label = " include confidence intervals", TRUE)
          )
        ),
        footer = card_footer(class = "d-flex justify-content-between",
                             download_chart_mod_ui(ns("save_rank_chart")),
                             download_data_btns_ui(ns("rank_download")))
      ),
      
      # map card -------------------
      
      card(
        full_screen = TRUE,
        leafletOutput(ns("rank_map")) # map
        )

      ) # close layout column wrap
  ) # close layout sidebar
  ) # close taglist
} # close ui function 








# server function:
# id = unique id 
# profile_data = reactive df in main server
# geo_selections <- reactive values in main server storing global geography selections
rank_mod_server <- function(id, profile_data, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    
    #######################################################
    # Dynamic filters
    #######################################################
    
    # update choices in years filter if "time" selected as comparator
    observe({
      
      x <- profile_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype)
      
      updateSelectInput(session, inputId = "year_comparator",
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
                                                       filtered_data = profile_data, 
                                                       geo_selections = geo_selections)
     
    # calls definition button module server script and passes the actual indicator selected)
    indicator_definition_btn_server("rank_ind_def", selected_indicator = selected_indicator)  
     
     # prepares data to be plotted --------------------------------------------
     rank_data <- reactive({

       profile_data <- setDT(profile_data()) # set profile data to data.table format

       # filter by selected areatype
       dt <- profile_data[areatype == geo_selections()$areatype]

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
      # get correct shapefile
      x <- switch(geo_selections()$areatype,
                  "Health board" = hb_bound,
                  "Council area" = ca_bound,
                  "HSC partnership" = hscp_bound,
                  "HSC locality" = hscloc_bound,
                  "Intermediate zone" = iz_bound,
                  "Scotland" = scot_bound
      )
      # further filter if HSCL or IZ selected
      if(geo_selections()$areatype == "HSC locality"){
        x <- x |> filter(hscp2019name == geo_selections()$parent_area)
      } else if(geo_selections()$areatype == "Intermediate zone"){
        x <- x |> filter(council == geo_selections()$parent_area)
      } else{
        x
      }
      x <- x |> left_join(rank_data(), by = join_by(code))
    })
     
     
     
     #######################################################
     ## dynamic text  ----
     #######################################################

     # title ---------
     output$rank_title <- renderUI({
       
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
     
  
     # info to display when user clicks help button (explains how to interpret)
     observeEvent(input$rank_help, {
       showModal(modalDialog(
         title = "How to interpret results",
         tagList(
           paste0("The charts below allow you rank each ", geo_selections()$areatype, " for your selected indicator (in this case, ", selected_indicator(), "). You can also
          choose to add a baseline comparator, to assess whether each area in your chosen geography level is statistically significantly better or worse than your comparator.
          For example, you may want to assess whether each ", geo_selections()$areatype, " is significantly higher or lower than a particular geographical area (for instance, the national average) or
                 whether there are particular areas in your chosen geography level that are significantly higher or lower than they were at another point in time (e.g. a decade ago)"),
           br(),
           p("If a comparator is selected, the chart and the map will be colour coded using the key below:"),
           fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:orange; border-radius:50%; display:inline-block; margin:5px;"), "orange - statistically significantly better")),
           fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:blue; border-radius:50%; display:inline-block; margin:5px;"), "blue - statistically significantly worse")),
           fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:gray; border-radius:50%; display:inline-block; margin:5px;"), "grey - not statistically different to Scotland")),
           fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:white; border-radius:50%; display:inline-block; margin:5px;"), "white - no difference to be calculated"))
           
         )
       ))
     })
     
     
     ############################################
     # Visualisations / data table  ----
     #############################################
     
     # chart (barchart/dumbell chart)
      output$rank_chart <- renderHighchart({

       # if there' no comparator selected, or the selected comparator is "area" then create a bar chart
       if(input$comparator_switch == FALSE | (input$comparator_switch == TRUE & input$comparator_type == "Area")) {

         x <-  hchart(object = rank_data(), 
                      type = "bar", hcaes(x = areaname, y = measure, color = colour_pal)) |>
           hc_yAxis(gridLineWidth = 0) |>
           hc_xAxis(title = list(text = "")) |>
           hc_yAxis(title = list(text = "")) |>
           hc_chart(margin = c(0, 0, 0, 150),
                    backgroundColor = 'white') |>
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
               title = list(text = selected_indicator())
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
       if(length(unique(map_data()$measure)) > 1) {
         colorNumeric(palette = "Blues", domain = map_data()$measure)
       } else {
         function(x) { phs_colors(colourname = "phs-purple") }
       }
     })


     
     output$rank_map <- renderLeaflet({
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
       
       data <- rank_data() |>
         select(areaname, measure, upci, lowci) |>
         arrange(measure)
       
       reactable(data,
                 defaultExpanded = T,
                 defaultPageSize = nrow(data))
     })

     
 
     
     ######################################
     # downloads -------
     ######################################
     
     # note these are both modules 
     download_chart_mod_server(id = "save_rank_chart", chart_id = session$ns("rank_chart")) # save chart as png
     download_data_btns_server(id = "rank_download", data = rank_data()) # download data in selected format
     
     
     }
    )
} # close server function


