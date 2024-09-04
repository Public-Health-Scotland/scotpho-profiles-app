################################
# MODULE: Population module ---- 
# prepares the layout displaying the population group splits

################################

# to do:
# remove (Total) from titles if no male/female (could be sex-split indicator already)
# disable averages if not available
# not finding avg for MH depression at first pass, but does find shortly after. 
# colour coding for CI bands
# suicide rate not available by SIMD on app...



#######################################################
## MODULE UI
#######################################################

## ui function -----------------------------------------------------------------------
# id = unique id 
pop_groups_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      # sidebar for filters -----------------------------
      sidebar = sidebar(width = 300,
                        useShinyjs(), # Set up shinyjs
                        
                        # indicator filter (note this is a module)
                        div(id = ns("popgroup_indicator_filter_wrapper"), indicator_filter_mod_ui(ns("indicator_filter"))),

                        # filter to select pop split (set choices to NULL 
                        # as they are updated dynamically on the server side, depending on selected indicator)
                        div(id = ns("popsplit_filter_wrapper"), 
                            selectInput(
                              inputId = ns("split_filter"),
                              label = "Select population split:",
                              selectize = TRUE,
                              choices = NULL)),
                        
                        div(id = ns("popsplit2_filter_wrapper"), 
                            selectizeInput(inputId = ns("split2_filter"), # this is disabled if no 2nd splits are available
                                                          label = "Select 2nd population split:",
                                                          choices = "Total",
                                                          selected = "Total")),
                        
                        # quint type filter 
                        div(id = ns("deprivation_quintile_type_wrapper"),
                            uiOutput(ns("ui_quint_button"))), # placeholder for the radio buttons (shown if split==SIMD)
                                             

                        # filter to include/exclude averages from charts (this will apply to all splits, not just deprivation)
                        div(id = ns("avg_switch_wrapper"), 
                            checkboxInput(ns("average_switch"), label = " include averages",FALSE)),

                        # guided tour button
                        actionLink(inputId = ns("popgroup_tour_button"),
                                   label = "Take a guided tour of this page"),
                        
                        actionLink(ns("simd_help"), label = "What is SIMD?", icon = icon("info-circle"))
      ), # close sidebar
      
      
      layout_column_wrap(
        1/2,
        
        # Bar chart card ------------------------------------------

        # footer with download buttons
        # NOTE: the 'footer' argument for navset_card_pill() is currently not working
        # package maintainers are aware and working on a fix
        # using the card_footer argument for card() in the meantime and suppressing warnings until bug fixed
        suppressWarnings(
          bslib::navset_card_pill(
            id = ns("pop_navset_card_pill_barchart"),
            height = 550,
            full_screen = TRUE,
            
            # tab 1: bar chart 
            bslib::nav_panel("Chart",
                             value = ns("pop_bar_tab"), #id for guided tour
                             uiOutput(ns("pop_bar_title")), # title 
                             highchartOutput(ns("pop_bar_chart"))|> # chart 
                               withSpinner() |> (\(x) {
                                 x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
                                 x})()
            ),
            
            # tab 2: data table
            bslib::nav_panel("Table",
                             value = ns("pop_bar_data_tab"), #id for guided tour
                             reactableOutput(ns("pop_bar_table")) # table
            ),
            
            # tab 3: indicator metadata
            bslib::nav_panel("Metadata",
                             value = ns("pop_metadata_tab"), #id for guided tour
                             metadata_table_mod_UI(ns("indicator_metadata"))
            ),
            
            
            bslib::nav_spacer(),
            
            # extra controls for bar chart 
            bslib::nav_item(
              bslib::popover(
                id = ns("popgroup_popover"),
                title = "Filters",
                chart_controls_icon(),
                checkboxInput(ns("ci_switch"), label = " include confidence intervals", FALSE),
                selectInput(ns("pop_years_filter"), label = "select year", choices = NULL)
              )
            ),
            
            # card footer - download buttons
            card_footer(class = "d-flex justify-content-between",
                        div(id = ns("deprivation_download_chart"), download_chart_mod_ui(ns("save_pop_barchart"))),
                        div(id = ns("deprivation_download_data"), download_data_btns_ui(ns("pop_bar_download"))))
          )), # close bar chart card
        
        
        ######  based on deprivation trend card added "pop_" to distinguish the two

        suppressWarnings(
          bslib::navset_card_pill(
            id = ns("pop_navset_card_pill_linechart"),
            height = 550,
            full_screen = TRUE,
            
            # tab 1: trend chart 
            bslib::nav_panel("Chart",
                             value = ns("pop_linechart_tab"), #id for guided tour
                             uiOutput(ns("pop_trend_title")), # title
                             highchartOutput(ns("pop_trend_chart")) |> # chart
                               # issue described here: https://github.com/daattali/shinycssloaders/issues/76 
                               # solution posted here: https://stackoverflow.com/questions/77184183/how-to-use-shinycssloaders-withspinner-with-a-plot-output-in-a-bslib-card 
                               withSpinner() |> (\(x) {
                                 x[[4]] <- x[[4]] |> bslib::as_fill_carrier() 
                                 x})()
            ),
            # tab 2: data table 
            bslib::nav_panel("Table",
                             value = ns("pop_table_linechart_tab"), #id for guided tour
                             reactableOutput(ns("pop_trend_table"))
            ),
            
            bslib::nav_spacer(),
            
            # extra controls for filters
            bslib::nav_item(
              div(id = ns("pop_popover"),
                  bslib::popover(
                title = "Filters",
                chart_controls_icon(),
                # too many CI for age split, removed at this stage
                checkboxInput(ns("trend_ci_switch"), label = " include confidence intervals", FALSE) 
              )
            )),
            # card footer - download buttons
            card_footer(class = "d-flex justify-content-between",
                        div(id = ns("deprivation_download_chart"), download_chart_mod_ui(ns("save_pop_trendchart"))),
                        div(id = ns("deprivation_download_data"), download_data_btns_ui(ns("pop_trend_download"))))
          )
        ) # close trend card
      ) # close layout column wrap
      
    ) # close sidebar layout
  ) # close taglist
} # close ui function 



#######################################################
## MODULE SERVER
#######################################################


pop_groups_server <- function(id, dataset, geo_selections, selected_profile) {
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    #######################################################
    ## Dynamic filters -----
    ######################################################
    
    ## update choices for population split filter, depending on what indicator was selected
    observe({
      req(popgroup_filtered_data())
      available_splits <- popgroup_filtered_data() |>
        pull(unique(split_name))
      
      updateSelectInput(session, inputId = "split_filter", choices = available_splits)

    })
    
    # update years choices for bar chart filter, depending on indicator selected
    observe({
      req(popgroup_filtered_data())
      available_years <- popgroup_filtered_data() |>
        filter(split_name == input$split_filter) |> # filter by selected split
        arrange(desc(year)) |>
        pull(unique(def_period))
      
      updateSelectInput(session, inputId = "pop_years_filter",
                        choices = available_years, selected = available_years[1])
    })
    
    ## update the splits available for the 2nd split filter
    observe( {
      req(popgroup_filtered_data())

      # stores available 2nd splits (is empty if no 2nd splits available)
      # clunky code: needs work
      available_2nd_splits <- popgroup_filtered_data() %>% 
        filter(split_name == input$split_filter & !is.na(split_value2)) %>%
        select(split_value2) %>%
        group_by(split_value2) %>%
        summarise() %>%
        arrange(desc(split_value2)) %>%  # to give Total first, and then Male and Female if available (could factorise when more options)
        pull(split_value2)

      # If 2nd splits are available, enable split2_filter, otherwise disable it
      if(length(available_2nd_splits)>0) {
        updateSelectizeInput(session,
                             inputId = "split2_filter",
                             options = list(placeholder = NULL),
                             choices = available_2nd_splits)
        shinyjs::enable("split2_filter")
      } else {
        updateSelectizeInput(session,
                             inputId = "split2_filter",
                             options = list(placeholder = "Unavailable"),
                             choices = character(0),
                             selected = character(0))
        shinyjs::disable("split2_filter")
      }
    })
    
   # # ## Show radio buttons for selection of the quintile type, if SIMD is the chosen split
   # #  # If geog==Scotland then quintile can only be sc_quin, so select this and disable the buttons
   # #  # If only one quint_type option then select this and disable the radio buttons
   # #  # If two quint_type options then enable the radio buttons
    output$ui_quint_button <- renderUI({
      
      req(popgroup_filtered_data())
      req(input$split_filter)
      
      if (input$split_filter == "Deprivation (SIMD)") {
      # stores available quint types 
      available_quints <- popgroup_filtered_data() |>
        filter(split_name == "Deprivation (SIMD)") |>
        pull(unique(quint_type))
      
      # Change selected options and disable the radio buttons, depending on the indicator selected
      if("sc_quin" %in% available_quints & ("hb_quin" %in% available_quints | "ca_quin" %in% available_quints)) {
        radioButtons(inputId = ns("quint_button"), #WORKING
                                    label = "Deprivation quintile type:",
                                    choices = c("Scotland", "Local"),
                                    selected = "Local")
        
        
      } else if("sc_quin" %in% available_quints & !("hb_quin" %in% available_quints | "ca_quin" %in% available_quints)) {
        shinyjs::disable("quint_button") # NOT WORKING (DISABLING) AT SCOTLAND LEVEL
        radioButtons(inputId = ns("quint_button"),
                     label = "Deprivation quintile type:",
                     choices = c("Scotland", "Local"),
                     selected = "Scotland")
        # then disable it
        
      } else if(!("sc_quin" %in% available_quints) & ("hb_quin" %in% available_quints | "ca_quin" %in% available_quints)) {
        shinyjs::disable("quint_button")
        radioButtons(inputId = ns("quint_button"),
                     label = "Deprivation quintile type:",
                     choices = c("Scotland", "Local"),
                     selected = "Local")
        # then disable it
      }
      }
    })
    
    # enable/disable average button depending on the selected indicator
    observeEvent(selected_indicator(), {
      req(popgroup_filtered_data())
      
      # stores available split_values
      available_splits <- popgroup_filtered_data() |>
        filter(split_name == input$split_filter) |>
        pull(unique(split_value))
      
      # If 'Total' is available, enable avg button, otherwise disable it
      if("Total" %in% available_splits) {
        shinyjs::enable("average_switch")
      } else {
        shinyjs::disable("average_switch") # DISABLING WHEN AVAILABLE, E.G., MENTAL WELLBEING. SOMETIMES DISABLES WHEN TICKED
        updateCheckboxInput(session, "average_switch", value=NULL)
      }
    })
      
    
   #   # Have all of the conditions previously used in the quint_type radio button logic been accounted for here?
   #   #   # if scotland is selected or the selected indicator is patients per GP OR the profile==MEN (not available at local quintiles) 
   #   #   # then set selected quintile to "Scotland" and disable the filter
   #   #         if(geo_selections()$areatype == "Scotland" | selected_indicator() == "Patients per general practitioner" | selected_profile() == "MEN"){
   #   #     updateRadioButtons(session, "quint_type", selected = "Scotland")
   #   #     shinyjs::disable("quint_type")
   #   #     
   #   #     # otherwise, if the areatype is local set the quintile to "Local" by default
   #   #   } else if (geo_selections()$areatype != "Scotland"){
   #   #     updateRadioButtons(session, "quint_type", selected = "Local")
   #   #     
   #   #     if(selected_indicator() %in% c("Mortality amenable to healthcare",
   #   #                                    "Repeat emergency hospitalisation in the same year",
   #   #                                    "Preventable emergency hospitalisation for a chronic condition",
   #   #                                    "Life expectancy, females",
   #   #                                    "Life expectancy, males")) {
   #   #       
   #   #       shinyjs::disable("quint_type")
   #   #     } else {
   #   #       shinyjs::enable("quint_type")
   #   #     }

     
   # enable/disable the average switch depending on the selected indicator and splits
   observe({
     req(popgroup_filtered_data())
     
     available_split_values <- popgroup_filtered_data() |>
       pull(unique(split_value))
  
     
     # If 'Total' is available, enable the 'include averages' button, otherwise disable it
     if("Total" %in% available_split_values) {
       shinyjs::enable("average_switch")
     } else {
       shinyjs::disable("average_switch")
     }
   })

    #######################################################
    ## Reactive data / values ----
    #######################################################
    
    # generate list of indicators (from the popgroup dataset) available 
    selected_indicator <- indicator_filter_mod_server(id = "indicator_filter", dataset, geo_selections)

    
    # create reactive data - filtering by selected indicator
    popgroup_filtered_data <- reactive({
      
      req(dataset())
      
      dataset() |>
        filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>  # filter by selected geography
        filter(indicator == selected_indicator()) # filter by selected indicator
    })

    # creates trend data
    pop_trend_data <- reactive({

      req(popgroup_filtered_data())
      
      # If 2nd splits are available (i.e., only for SIMD currently), filter the data on the selected split2
      if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available

      df <- popgroup_filtered_data() %>%
        filter(split_name == input$split_filter) %>% # filter by selected split
        filter(split_value2 == input$split2_filter) %>% # filter by the 2nd split value
        arrange(year, split_value2) 
      
      if(input$quint_button == "Scotland"){
        df <- df[quint_type == "sc_quin"]
      } else {
        df <- df[quint_type != "sc_quin"]
      }
      
      } else {
        
      df <- popgroup_filtered_data() %>%
          filter(split_name == input$split_filter) %>% # filter by selected split only
          arrange(year)
      
      } 
      
      # don't require this step: not all have totals, so this crashes (e.g., exp of harassment)
      if ("Total" %in% unique(df$split_value)) {
      
      # get totals and rename them ready for joining back in as 'average' columns
      totals <- df[split_value == "Total"]
      setnames(totals, old = c("measure", "upci", "lowci"), new = c("avg", "avg_upci", "avg_lowci"))
      totals <- totals[, c("trend_axis", "avg", "avg_upci", "avg_lowci")]
      
      # remove totals from popgroup data
      df <- df[split_value != "Total"]
      
      # add the totals back on as a column 
      df <- df[totals, on = "trend_axis"]
      }
      
      # apply colour schemes
      df <- df %>%
        mutate(colour_pal = case_when(split_value %in% c("1 - most deprived", "5 - lowest income") ~ "#0078D4", # phs_colors(colourname = "phs-blue")
                                    split_value == "2" ~ "#c8c6d1", # phs graphite (need colours that are unique for line chart but effectively not noticible when rendered)
                                    split_value == "3" ~ "#c8c6d2", # phs graphite-ish
                                    split_value == "4" ~ "#c8c6d3", # phs graphite -ish#2
                                    split_value %in%  c("5 - least deprived", "1 - highest income") ~ "#9B4393", # phs_colors(colourname = "phs-magenta")
                                    TRUE ~ "#80BCEA"))  # phs_colors("phs-blue-50")
        
    })
    

    # create single year data for the bar chart 
    pop_bar_data <- reactive({
      req(pop_trend_data())
      pop_trend_data() |>
        filter(def_period == input$pop_years_filter) 
    })
    
    #######################################################
    ## dynamic text  ----
    #######################################################
    
    output$pop_bar_title <- renderUI({
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need(nrow(pop_trend_data()) > 0, "No indicators available")
      )
      
      # if data is available display chart title
      # If 2nd splits are available (i.e., only for SIMD currently), filter the data on the selected split2
      if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
        if(grepl("males", selected_indicator())) { # don't give 'Total' in title if males/females already in indicator name
          div(
            tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
            tags$h6(pop_bar_data()$trend_axis[1]), # time period 
            tags$p(pop_bar_data()$rate_type[1]) # measure type
          )
        } else {
          div(
          tags$h5(selected_indicator(), " (", input$split2_filter, "); split by ", input$split_filter, class = "chart-header"),
          tags$h6(pop_bar_data()$trend_axis[1]), # time period 
          tags$p(pop_bar_data()$rate_type[1]) # measure type
          )
      }
        } else {
        div(
          tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
          tags$h6(pop_bar_data()$trend_axis[1]), # time period 
          tags$p(pop_bar_data()$rate_type[1]) # measure type
        )
      }
      
    })
    
    # need to add pop-trend title stuff
    
    output$pop_trend_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need(nrow(pop_trend_data()) > 3, "There are insufficent data points for this indicator to create a trend chart")
      )
      
      # if data is available display chart title
      # If 2nd splits are available (i.e., only for SIMD currently), filter the data on the selected split2
      if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
        if(grepl("males", selected_indicator())) { # don't give 'Total' in title if males/females already in indicator name
          div(
            tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
            tags$h6(first(pop_trend_data()$trend_axis)," to ",last(pop_trend_data()$trend_axis)), # time period 
            tags$p(pop_trend_data()$rate_type[1]) # measure type
          )
        } else {
        div(
          tags$h5(selected_indicator(), " (", input$split2_filter, "); split by ", input$split_filter, class = "chart-header"),
          tags$h6(first(pop_trend_data()$trend_axis)," to ",last(pop_trend_data()$trend_axis)), # time period 
          tags$p(pop_trend_data()$rate_type[1]) # measure type
        )
      }
        } else {
        div(
          tags$h5(selected_indicator(), "; split by ", input$split_filter, class = "chart-header"),
          tags$h6(first(pop_trend_data()$trend_axis)," to ",last(pop_trend_data()$trend_axis)), # time period 
          tags$p(pop_trend_data()$rate_type[1]) # measure type
        )
        }
    })
    
    observeEvent(input$simd_help, {
      showModal(modalDialog(
        title = "About SIMD",
        tagList(
          #simd explanation
          p("To prepare the charts shown we divide geographical areas into five groups (also known as quintiles) based on their relative levels of deprivation, as measured by the ",
            tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).")),
          p("Those living in areas assigned to quntile 1 experience the highest levels of relative deprivation and those living in quintile 5 the lowest relative deprivation."),
          p("Geographical areas are assigned to a within Scotland quintile or a local quintile (e.g. within NHS board or within local authority quintile) based on the SIMD ranking."),
          p("Indicator data for an NHS board or council area is presented by local deprivation quintiles by default. This tool allows users to switch from the default local quintiles to view the same data according to Scotland quintiles.")
        ) #close taglist
      ))
    })
    
    
    
    
    ############################################
    # charts -----
    #############################################
    
    # pop bar chart  ---------------
    
    output$pop_bar_chart <- renderHighchart({
      
      shiny::validate(
        need( nrow(pop_bar_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select a different area type (of either Scotland, Health board or Council area)."))
      )
      
      # if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
      #   bar_data <- simd_pop_bar_data()
      # } else {
      #   bar_data <- pop_bar_data()
      # }
      bar_data <- pop_bar_data()
      
      
      x <- hchart(bar_data, 
                  type = "column", hcaes(x = split_value, y = measure, color = colour_pal)) %>% 
        hc_yAxis(gridLineWidth = 0) %>%
        hc_chart(backgroundColor = 'white') %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_plotOptions(series = list(animation = FALSE),
                       column = list(groupPadding = 0))

      # incude average line if switch turned on
      if(input$average_switch == TRUE){

        x <- x |> hc_add_series(
          name = "Average",
          data = bar_data$avg,
          type = "line",
          color = "#C73918", #red colour for average line
          marker = list(enabled = FALSE),
          enableMouseTracking = FALSE) #turns off mouse tracking on average line only
      }
      
      if(input$ci_switch) {
        x <- x |>
          hc_add_series(bar_data, "errorbar", hcaes(x = split_value, low = lowci, high = upci), zIndex = 10)
      }
        
        if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
          x <- x |>
            # add extra bits to chart for downloaded version
            hc_exporting(
              filename = paste0("ScotPHO ", selected_indicator(), " - ", input$split2_filter , " - split by ", input$split_filter), 
              chartOptions = list(
                title = list(text = paste0(selected_indicator(), " - ", input$split2_filter , " - split by ", input$split_filter)), 
                subtitle = list(text = paste0(bar_data$trend_axis[1])),
                yAxis = list(title = list(text = paste0(bar_data$rate_type[1])))
              )
            )  
        } else {
          x <- x |>
            # add extra bits to chart for downloaded version
            hc_exporting(
              filename = paste0("ScotPHO ", selected_indicator(), " split by ", input$split_filter), 
              chartOptions = list(
                title = list(text = paste0(selected_indicator(), " split by ", input$split_filter)), 
                subtitle = list(text = paste0(bar_data$trend_axis[1])),
                yAxis = list(title = list(text = paste0(bar_data$rate_type[1])))
              )
            )
        }
        
      
      x

      
    }) # end pop_bar_chart
    
    # pop trend bar chart  ---------------
    
    output$pop_trend_chart <- renderHighchart({
      
      # if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
      #   trend_data <- simd_pop_trend_data()
      # } else {
      #   trend_data <- pop_trend_data()
      # }
      trend_data <- pop_trend_data()
      
      # create vector of colours - needs to be the same length as the 
      # number of lines that need to be plotted otherwise CI colours (the lighter colour plotted behind the main line)
      # wont match up properly
      purple_and_blues <- unname(phs_colours()[grepl("blue|purple", names(phs_colours()))])
      colours <- head(purple_and_blues, length(unique(trend_data$split_value)))
      
      
      x <- hchart(trend_data, 
                  "line",
                  hcaes(x = trend_axis, y = measure, group = split_value),
                  marker = list(enabled = TRUE)) |>
        hc_yAxis(gridLineWidth = 0) |> # remove gridlines 
        hc_xAxis(title = list(text = "")) |>
        hc_yAxis(title = list(text = "")) |>
        hc_xAxis(categories = unique(trend_data$trend_axis)) |>
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
        hc_plotOptions(series = list(animation = FALSE,
                                     connectNulls=TRUE)) |>
        hc_tooltip(
          crosshairs = TRUE,
          borderWidth = 1,
          table = TRUE
        ) 
      
      # if the confidence interval switch turned on, plot cis
      if(input$trend_ci_switch == TRUE){
        
        x <- x |>
          hc_add_series(trend_data, 
                        type = "arearange", 
                        hcaes(x = trend_axis, low = lowci, high = upci, group = split_value),  
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
      
      if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
        x <- x |>
          # add SIMD colour pal
          hc_colors(unique(trend_data$colour_pal)) |>
          # add extra bits to chart for downloaded version
          hc_exporting(
            filename = paste0("ScotPHO trend - ", selected_indicator(), " - ", input$split2_filter , " - split by ", input$split_filter),
            chartOptions = list(
              title = list(text = paste0(selected_indicator(), " - ", input$split2_filter , " - split by ", input$split_filter)),
              subtitle = list(text = paste0(first(trend_data$trend_axis)," to ",last(trend_data$trend_axis))),
              yAxis = list(title = list(text = paste0(trend_data$rate_type[1])))
            )
          )     
        } else {
        x <- x |>
          # add phs colours
          hc_colors(colours) |>
          # add extra bits to chart for downloaded version
          hc_exporting(
            filename = paste0("ScotPHO trend - ", selected_indicator(), " split by ", input$split_filter),
            chartOptions = list(
              title = list(text = paste0(selected_indicator(), " split by ", input$split_filter)),
              subtitle = list(text = paste0(first(trend_data$trend_axis)," to ",last(trend_data$trend_axis))),
              yAxis = list(title = list(text = paste0(trend_data$rate_type[1])))
            )
          )
        }
      
      # add average line if switch turned on 
      if(input$average_switch == TRUE){
        
        x <- x |> hc_add_series(
          trend_data,
          "line",
          name = "Average",
          color = "#C73918",
          hcaes(x = trend_axis, y = avg),
          marker = list(enabled = TRUE)
        )
      } 
      
      x
      
      
    }) #end  pop trend chart
    
    
    
    ##########################################
    # Tables ---------
    ###########################################
    
    # bar data table -------
    output$pop_bar_table <- renderReactable({
      
      if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
        data <- pop_bar_data() |>
          select(def_period, split_value2, split_value, measure)
        
        reactable(data = data,
                  defaultExpanded = TRUE,
                  defaultPageSize = nrow(data),
                  # rename some columns 
                  columns = list(
                    def_period = colDef(name = "Time Period"),
                    split_value2 = colDef(name = "Population Group 1"),
                    split_value = colDef(name = "Population Group 2"),
                    measure = colDef(name = "Measure")
                  )
        )
        
      } else {
        data <- pop_bar_data() |>
          select(def_period, split_value, measure)
        
        reactable(data = data,
                  defaultExpanded = TRUE,
                  defaultPageSize = nrow(data),
                  # rename some columns 
                  columns = list(
                    def_period = colDef(name = "Time Period"),
                    split_value = colDef(name = "Population Group"),
                    measure = colDef(name = "Measure")
                  )
        )
      }
      
    })
    
    # trend data table -------
    output$pop_trend_table <- renderReactable({
      
      if(input$split_filter == "Deprivation (SIMD)") { #will need generalising if other 1st splits are available
        
        data <- pop_trend_data() |>
          select(def_period, split_value2, split_value, measure)
        
        reactable(data = data,
                  defaultExpanded = TRUE,
                  defaultPageSize = nrow(data),
                  # rename some columns 
                  columns = list(
                    def_period = colDef(name = "Time Period"),
                    split_value2 = colDef(name = "Population Group 1"),
                    split_value = colDef(name = "Population Group 2"),
                    measure = colDef(name = "Measure")
                  )
        )
      } else {
        
      data <- pop_trend_data() |>
        select(def_period, split_value, measure)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data),
                # rename some columns 
                columns = list(
                  def_period = colDef(name = "Time Period"),
                  split_value = colDef(name = "Population Group"),
                  measure = colDef(name = "Measure")
                )
      )
      }
      
    })
    
    
    
    # metadata table
    metadata_table_mod_Server("indicator_metadata", selected_indicator)
    
    ### need to create pop trend table
    
    ############################################
    # Downloads  ----
    #############################################
    download_chart_mod_server(id = "save_pop_barchart", chart_id = ns("pop_bar_chart"))
    download_data_btns_server(id = "pop_bar_download", data = pop_trend_data, file_name = "Popgroup_ScotPHO_data_extract")
    
    download_chart_mod_server(id = "save_pop_trendchart", chart_id = ns("pop_trend_chart"))
    download_data_btns_server(id = "pop_trend_download", data = pop_trend_data, file_name = "Popgroup_ScotPHO_data_extract")
  
    
    ############################################
    # Guided tour
    ###########################################
    
    guide_popgroup <- Cicerone$
      new()$
      step(
        ns("pop_bar_chart"),
        "Bar Chart",
        "This chart shows how the selected measure varies according to the chosen split groups, at the chosen point in time.",
        position = "right",
        tab_id = ns("pop_navset_card_pill_barchart"),
        tab = ns("pop_bar_tab")
      )$
      step(
        ns("pop_trend_chart"),
        "Linechart Tab",
        "This chart shows the value of the indicator for the chosen population groups over time.",
        position = "left",
        tab_id = ns("pop_navset_card_pill_linechart"),
        tab = ns("pop_linechart_tab")
      )$
      step(
        ns("pop_navset_card_pill_barchart"),
        "Other views",
        "You can switch between viewing charts, the data or the metadata using the buttons above each chart."
      )$
      step(
        ns("popgroup_popover"),
        "Chart controls",
        "You can change how the data are presented using the cog symbol button above each chart."
      )$
      step(
        ns("popgroup_indicator_filter_wrapper"), 
        "Indicator Filter",
        "First select an indicator.<br>
        The indicator list has been filtered based on profile and area type selected at the top of the page.<br>
        The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
        position = "right"
      )$
      step(
        ns("popsplit_filter_wrapper"), 
        "Population group split filter",
        "Select from the available population splits.",
        position = "right"
      )$
      step(
        ns("popsplit2_filter_wrapper"), 
        "2nd population group split filter",
        "Select a second population split, if available.",
        position = "right"
      )$
      step(
        ns("avg_switch_wrapper"),
        "Average Line",
        "Click to add or remove a line showing the average for the measure across all split values.",
        position = "right"
      )$
      step(
        ns("deprivation_quintile_type_wrapper"), # not visible by default. will this work?
        "Quintile Type",
        "When an area other than Scotland is selected, click here to toggle between local and Scottish quintiles.",
        position = "right"
      )$
      step(
        ns("simd_help"), # not visible by default. will this work?
        "Information on SIMD",
        "Click here for more information on SIMD",
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
    guide_popgroup$init()
    
    #when guided tour button is clicked, start the guide
    observeEvent(input$popgroup_tour_button, {
      guide_popgroup$start()
    })
  
    
    } # module server
  )# module server
} # pop groups server