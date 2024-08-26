################################.
# MODULE: simd_navpanel_mod ---- 
# prepares the nav_panel layout displaying SIMD based deprivation data
################################.

#######################################################.
## MODULE UI
#######################################################.

## ui function -----------------------------------------------------------------------
# id = unique id 
simd_navpanel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      
      # sidebar for filters -----------------------------
      sidebar = sidebar(width = 300,
                        
                        # indicator filter (note this is a module)
                        div(id = ns("deprivation_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("simd_indicator_filter"))),

                        # filter to include/exclude averages from charts
                        div(id = ns("deprivation_avg_switch_wrapper"), checkboxInput(ns("average_switch"), label = " include averages",FALSE)),
                        
                        
                        # quint type filter 
                        div(id = ns("deprivation_quintile_type_wrapper"), 
                            radioButtons(inputId = ns("quint_type"), 
                                     label = "Quintile Type",
                                     choices = c("Scotland", "Local"), 
                                     selected = "Scotland")),
                      
                        
                        
                        #guided tour button
                        actionLink(inputId = ns("deprivation_tour_button"),
                                   label = "Take a guided tour of this page"),
                        
                        actionLink(ns("simd_help"), label = "What is SIMD?", icon = icon("info-circle"))
                        
      ), # close sidebar
      
      
      
      
      
      layout_column_wrap(
        1/2,
        
        # Bar chart card ------------------------------------------

        bslib::navset_card_pill(
          id = ns("deprivation_navset_card_pill_barchart"),
          height = 550,
          full_screen = TRUE,
          
          # tab 1: bar chart 
              bslib::nav_panel("Chart",
                           value = ns("deprivation_barchart_tab"), #id for guided tour
                           uiOutput(ns("barchart_title")), # title 
                           highchartOutput(ns("simd_barchart")) # chart 
          ),
          
          # tab 2: data table
          bslib::nav_panel("Table",
                           value = ns("deprivation_data_tab"), #id for guided tour
                           reactableOutput(ns("bar_table")) # table
          ),
          
          
          # tab 3: metadata
          bslib::nav_panel("Metadata",
                           value = ns("deprivation_metadata_tab"), #id for guided tour
                           metadata_table_mod_UI(ns("indicator_metadata"))
          ),
          
          
          bslib::nav_spacer(),

        # extra controls for bar chart 
            bslib::nav_item(
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("bar_ci_switch"), label = " include confidence intervals", FALSE),
                selectInput(ns("simd_years_filter"), label = "select year", choices = NULL)
              ) #close popover
            ), #close popover panel
            # card footer - download buttons
            card_footer(class = "d-flex justify-content-between",
                        div(id = ns("deprivation_download_chart"), download_chart_mod_ui(ns("save_simd_barchart"))),
                        div(id = ns("deprivation_download_data"), download_data_btns_ui(ns("simd_barchart_download"))))
          ), # close bar chart card
        
   
               suppressWarnings(
          bslib::navset_card_pill(
            id = ns("deprivation_navset_card_pill_linechart"),
            height = 550,
            full_screen = TRUE,
            
            # tab 1: trend chart 
            bslib::nav_panel("Chart",
                             value = ns("deprivation_linechart_tab"), #id for guided tour
                             uiOutput(ns("trendchart_title")), # title
                             highchartOutput(ns("simd_trendchart")) |># chart
                               withSpinner() |> (\(x) {
                                 x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
                                 x})()
            ),
            
            # tab 2: data table
            bslib::nav_panel("Table",
                             value = ns("deprivation_table_linechart_tab"), #id for guided tour
                             reactableOutput(ns("trend_table"))
            ),
            
            bslib::nav_spacer(),

            # extra controls for filters
            bslib::nav_item(
              div(id = ns("depr_popover"),
              bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("trend_ci_switch"), label = " include confidence intervals", FALSE)
              ))
            ),
            # card footer - download buttons
            card_footer(class = "d-flex justify-content-between",
                        div(id = ns("deprivation_download_chart"), download_chart_mod_ui(ns("save_simd_trendchart"))),
                        div(id = ns("deprivation_download_data"), download_data_btns_ui(ns("simd_trendchart_download"))))
          )

        ) # close trend card
      ) # close layout column wrap
      
    ) # close sidebar layout
  ) # close taglist
} # close ui function 


#######################################################.
## MODULE SERVER
#######################################################.

simd_navpanel_server <- function(id, simd_data, geo_selections){
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    #######################################################.
    ## Dynamic filters -----
    #######################################################.
    
    # update years choices for bar chart filter, depending on indicator selected
    observe({
      

      available_years <- simd_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
        arrange(desc(year)) |>
        pull(unique(def_period))
      
      updateSelectInput(session, inputId = "simd_years_filter",
                        choices = available_years, selected = available_years[1])
    })
    
    
    
    observe({
      
      # if local quintiles are not available for the selected indicator and geography
      # then set selected quintile to "Scotland" and disable the filter
      
      available_quints <- simd_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
        select(quint_type) %>%
        unique()

      # If Scotland is selected, only scotland-level quintiles are appropriate (disable the radio buttons)
      # If another geography is selected but local-level quintiles aren't available do the same:
      if (length(available_quints)==1 & "sc_quin" %in% available_quints){
          updateRadioButtons(session, "quint_type", selected = "Scotland")
          shinyjs::disable("quint_type")

      # otherwise, if the areatype is local set the quintile to "Local" by default
            } else if (geo_selections()$areatype != "Scotland"){
              updateRadioButtons(session, "quint_type", selected = "Local")
              
              if(selected_indicator() %in% c("Mortality amenable to healthcare",
                                       "Repeat emergency hospitalisation in the same year",
                                       "Preventable emergency hospitalisation for a chronic condition",
                                       "Life expectancy, females",
                                       "Life expectancy, males")) {
                
                shinyjs::disable("quint_type")
                } else {
                  shinyjs::enable("quint_type")
                } 
              }
      })
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # generate list of indicators (from the simd indicators dataset) available 
    selected_indicator <- indicator_filter_mod_server(id="simd_indicator_filter", simd_data, geo_selections)
    
    
    geography_data <- reactive({
      dt <- setDT(simd_data())
      dt <- dt[areatype == geo_selections()$areatype & areaname == geo_selections()$areaname]
      
    })
    
    # creates trend data
    trend_data <- reactive({
      
      
      
      # filter by selected indicator
      dt <- geography_data()[indicator == selected_indicator()]
      
      # filter by quint type 
      if(input$quint_type == "Scotland"){
        dt <- dt[quint_type == "sc_quin"]
      } else {
        dt <- dt[quint_type != "sc_quin"]
      }
      
      #dt <- dt[quint_type == "sc_quin"]
      
      # get totals and rename them ready for joining
      totals <- dt[quintile == "Total"]
      setnames(totals, old = c("measure", "upci", "lowci"), new = c("avg", "avg_upci", "avg_lowci"))
      totals <- totals[, c("trend_axis", "avg", "avg_upci", "avg_lowci")]
      
      
      # remove totals from simd data
      dt <- dt[quintile != "Total"]
      
      # add the totals back on as a column 
      dt <- dt[totals, on = "trend_axis"]
      # create colour palette
      dt <- dt[, colour_pal := fcase(quintile == "1 - most deprived", phs_colors(colourname = "phs-blue"),
                                     quintile == "2", colour = "#c8c6d1", # phs graphite (need colours that are unique for line chart but effectively not noticible when rendered)
                                     quintile == "3", colour = "#c8c6d2", # phs graphite-ish
                                     quintile == "4", colour = "#c8c6d3", # phs graphite -ish#2
                                     default = phs_colors(colourname = "phs-magenta"))]
      dt
      
    })
    
    
    
    bar_data <- reactive({
      req(trend_data())
      trend_data()[def_period == input$simd_years_filter]
    })
    
    
    #######################################################.
    ## Dynamic text  ----
    #######################################################.
    
    
    # bar chart title ---------
    output$barchart_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(trend_data()) > 0, "No indicators available")
      )
      
      # if data is available display chart title
      div(
        tags$h5(selected_indicator(), " by SIMD quintile - snapshot", class = "chart-header"),
        tags$h6(input$simd_years_filter), # time period
        tags$p(trend_data()$type_definition[1]) # measure type
      )
    })
    
    
    # trend chart title  ----
    output$trendchart_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(trend_data()) > 0, "No indicators available")
      )
      
      # if data is available display chart title
      div(
        tags$h5(selected_indicator(), " by SIMD quintile - trend", class = "chart-header"), # selected indicator
        tags$h6(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis)), # time period 
        tags$p(trend_data()$type_definition[1]) # measure type
      )
    })
    
    observeEvent(input$simd_help, {
      showModal(modalDialog(
        title = "About SIMD",
        tagList(
          #simd explanation
          p("To prepare the charts shown we divide geographical areas into five groups (also known as quintiles) based on their relative levels of deprivation, as measured by the ",
            tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).")),
          p("Those living in areas assigned to quntile 1 experience the highest levels of relative deprivation and those living in quintile 5 the lowest relative deprivation."),
          p("Geogrpahical areas are assigned to a within Scotland quintile or a local quintile (e.g. within NHS board or within local authority quintile) based on the SIMD ranking."),
          p("Indicator data for an NHS board or council area is presented by local deprivation quintiles by default. This tool allows users to switch from the default local quintiles to view the same data according to Scotland quintiles.")
        ) #close taglist
      ))
    })
    
    # populate metadata tab ------------
    output$deprivation_metadata <- renderUI({
      
      #create dataframe containing only notes_caveats column for selected indicator from techdoc
      indicator_caveats <- techdoc |> 
        filter(indicator_name == selected_indicator()) |> 
        select(notes_caveats)
      
      #print notes and caveats for selected indicator
      tags$p(indicator_caveats)
    })
    
    #############################################.
    # Charts -----
    #############################################.
    
    # trend chart ---------------
    output$simd_trendchart <- renderHighchart({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(trend_data()) > 0, paste0("SIMD data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      # if there is data, plot trend chart 
      x <- hchart(trend_data(), 
                  "line", 
                  hcaes(x = trend_axis, y = measure, group = quintile), 
                  color = unique(trend_data()$colour_pal)) |>
        hc_yAxis(gridLineWidth = 0) |> # remove gridlines 
        hc_xAxis(title = list(text = "")) |>
        hc_yAxis(title = list(text = "")) |>
        # style xaxis labels - keeping only first and last label
        hc_xAxis(labels = list(
          rotation = 0,
          style = list(
            whiteSpace = 'nowrap',
            textOverflow = 'none'
          ),
          formatter = JS("function() {
               if (this.isFirst || this.isLast) {
                 return this.value;
               } else {
                 return '';
               }
             }"))) |>
        hc_chart(backgroundColor = 'white') |>
        hc_plotOptions(series = list(animation = FALSE)) |>
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 1,
          table = TRUE
        ) |>
        # add extra bits to chart for downloaded version
        hc_exporting(
          filename = paste0("ScotPHO SIMD trend - ", selected_indicator()),
          chartOptions = list(
            title = list(text = paste0(selected_indicator(), " split by SIMD Quintile")),
            subtitle = list(text = paste0(first(trend_data()$trend_axis)," to ",last(trend_data()$trend_axis))),
            yAxis = list(title = list(text = paste0(unique(trend_data()$type_definition))))
          )
        )
      
      # add averae line if switch turned on 
      if(input$average_switch == TRUE){
        
        x <- x |> hc_add_series(
          trend_data(),
          "line",
          name = "Average",
          color = "#C73918",
          hcaes(x = trend_axis, y = avg)
        )
        
      }
      
      
      # if the confidence interval switch turned on, plot cis
      if(input$trend_ci_switch == TRUE){
        
        x <- x |>
          hc_add_series(trend_data(), 
                        type = "arearange", 
                        hcaes(x = trend_axis, low = lowci, high = upci, group = quintile),  
                        color = hex_to_rgba("grey", 0.2), 
                        linkedTo = ":previous",
                        showInLegend = FALSE,
                        enableMouseTracking = FALSE,
                        zIndex = -1, # plots the CI series behind the line series
                        marker = list(enabled = FALSE, # removes the markers for the CI series
                                      states = list(
                                        hover = list(
                                          enabled = FALSE))))
      }
      
      x
      
    })
    
    
    # bar chart -------------
    output$simd_barchart <- renderHighchart({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(bar_data()) > 0, paste0("SIMD data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      # if there is data available, plot bar chart 
      x <- hchart(object = bar_data(), 
                  type = "column", hcaes(x = quintile, y = measure, color = colour_pal)) |>
        hc_yAxis(gridLineWidth = 0) |>
        hc_chart(backgroundColor = 'white') |>
        hc_xAxis(title = list(text = "")) |>
        hc_yAxis(title = list(text = "")) |>
        hc_plotOptions(series = list(animation = FALSE),
                       column= list(groupPadding  = 0)) |>  # Reduce padding between groups of columns
        hc_exporting(
          filename = paste0("ScotPHO SIMD - ", selected_indicator()),
          chartOptions = list(
            title = list(text = paste0(selected_indicator(), " split by SIMD Quintile")),
            subtitle = list(text = paste0(unique(bar_data()$trend_axis))),
            yAxis = list(title = list(text = paste0(unique(trend_data()$type_definition))))
          )
        )
      
      
      # incude average line if switch turned on 
      if(input$average_switch == TRUE){
        
        x <- x |> hc_add_series(
          name = "Average",
          data = bar_data()$avg,
          type = "line",
          color = "#C73918", #red colour for average line
          marker = list(enabled = FALSE),
          enableMouseTracking = FALSE) #turns off mouse tracking on average line only
      }
      
      # add confidence intervals if switch turned on
      if(input$bar_ci_switch == TRUE) {
        x <- x |>
          hc_add_series(bar_data(), "errorbar", hcaes(x = quintile, low = lowci, high = upci), zIndex = 10)
      }
      
      x
    })
    
    
    ###########################################.
    # Tables ----
    ###########################################.
    
    # trend data table -------
    output$trend_table <- renderReactable({
      
      data <- trend_data() |>
        select(def_period, quintile, measure)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data),
                # rename some columns 
                columns = list(
                  def_period = colDef(name = "Time Period"),
                  quintile = colDef(name = "SIMD Quintile"),
                  measure = colDef(name = "Measure")
                )
      )
    })
    
    # trend data table -------
    output$bar_table <- renderReactable({
      
      data <- bar_data() |>
        select(quintile, measure)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data),
                # rename some columns 
                columns = list(
                  quintile = colDef(name = "SIMD Quintile"),
                  measure = colDef(name = "Measure")
                )
      )
    })
    
    
    # metadata table (note this is a module)
    metadata_table_mod_Server("indicator_metadata", selected_indicator)
    
    
    #############################################.
    # Downloads  ----
    #############################################.
    
    download_chart_mod_server(id = "save_simd_barchart", chart_id = ns("simd_barchart"))
    download_chart_mod_server(id = "save_simd_trendchart", chart_id = ns("simd_trendchart"))
    
    download_data_btns_server(id = "simd_barchart_download", 
                              data = bar_data, 
                              file_name = "SIMD_ScotPHO_data_extract",
                              selected_columns = c("code", 
                                                   "areatype", 
                                                   "areaname", 
                                                   "indicator", 
                                                   "type_definition", 
                                                   "definition_period" = "def_period",
                                                   "quintile_type" = "quint_type",
                                                   "quintile", 
                                                   "numerator", 
                                                   "measure", 
                                                   "upper_confidence_interval" = "upci", 
                                                   "lower_confidence_interval" = "lowci"))
    
    
    
    
    download_data_btns_server(id = "simd_trendchart_download", 
                              data = trend_data, 
                              file_name = "SIMD_Trend_ScotPHO_data_extract",
                              selected_columns = c("code", 
                                                   "areatype", 
                                                   "areaname", 
                                                   "indicator", 
                                                   "type_definition", 
                                                   "definition_period" = "def_period",
                                                   "trend_axis",
                                                   "quintile_type" = "quint_type",
                                                   "quintile", 
                                                   "numerator", 
                                                   "measure", 
                                                   "upper_confidence_interval" = "upci", 
                                                   "lower_confidence_interval" = "lowci"))
    
    
    ############################################
    # Guided tour
    ###########################################
    
    guide_deprivation <- Cicerone$
      new()$
      step(
        ns("simd_barchart"),
        "Bar Chart",
        "This chart shows how the selected measure varies according to the relative deprivation of an area of residence, 
        It allows comparison of the most and least deprived areas.",
        position = "right",
        tab_id = ns("deprivation_navset_card_pill_barchart"),
        tab = ns("deprivation_barchart_tab")
      )$
      step(
        ns("simd_trendchart"),
        "Linechart Tab",
        "This chart shows the value of a measure by deprivation quintile over time. 
        It allows comparison of the most and least deprived areas over time.",
        position = "left",
        tab_id = ns("deprivation_navset_card_pill_linechart"),
        tab = ns("deprivation_linechart_tab")
      )$
      step(
        ns("deprivation_navset_card_pill_linechart"),
        "Other views",
        "You can switch between viewing charts, the data or the metadata using the buttons above each chart."
      )$
      step(
        ns("depr_popover"),
        "Chart controls",
        "You can switch between viewing charts, the data or the metadata using the buttons above each chart."
      )$
      step(
        ns("deprivation_indicator_filter_wrapper"), 
        "Indicator Filter",
        "First select an indicator.<br>
        The indicator list has been filtered based on profile and area type selected at the top of the page.<br>
        The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
        position = "right"
      )$
      step(
        ns("simd_help"),
        "Information on SIMD",
        "Click here for more information on SIMD",
        position = "right"
      )$
      step(
        ns("deprivation_avg_switch_wrapper"),
        "Average Line",
        "Click to add or remove a trend line showing the average for the measure across all quintiles.",
        position = "right"
      )$
      step(
        ns("deprivation_quintile_type_wrapper"),
        "Quintile Type",
        "When an area other than Scotland is selected, click here to toggle between local and Scottish quintiles.",
        position = "right"
      )$
      step(
        ns("deprivation_download_chart"),
        "Download Chart Button",
        "Click here to download this chart as a PNG.",
        position = "above"
      )$
      step(
        ns("deprivation_download_data"),
        "Download Data Button",
        "Click here to download the selected data as a CSV, RDS or JSON file.",
        position = "above")
    #add step for tooltips
    
    
    #initiate the guide
    guide_deprivation$init()
    
    #when guided tour button is clicked, start the guide
    observeEvent(input$deprivation_tour_button, {
      guide_deprivation$start()
    })
    
    
    
  }) #close moduleServer
 } # close server function
