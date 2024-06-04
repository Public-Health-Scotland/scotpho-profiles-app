################################
# MODULE: inequality_mod ---- 
# prepares the layout displaying the inequalities splitss
################################

#######################################################
## MODULE UI
#######################################################

###  use these themes temporatily
# 6. Highcharter theme --------------------------------------------------------------

chart_theme <- hc_theme(
  
  colors = c("#000000", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
             "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#b15928"),
  
  chart = list(backgroundColor = NULL,
               style = list(fontSize = "14px",
                            fontFamily = "Arial")),
  
  title = list(
    align = "left",
    style = list(fontSize = "16px",
                 fontWeight = "bold",
                 color = "black",
                 fontFamily = "Arial")), 
  
  subtitle = list(
    align = "left",
    style = list(color = "black",
                 fontFamily = "Arial")),
  
  xAxis = list(
    labels = list(
      style = list(
        fontSize = "14px"))),
  
  yAxis = list(
    labels = list(
      style = list(
        fontSize = "14px")),
    min = 0),
  
  legend = list(
    itemStyle = list(fontFamily = "Arial",
                     color = "black"),
    itemHoverStyle = list(color = "black")),
  
  tooltip = list(
    shadow = FALSE,
    crosshairs = TRUE,
    style = list(
      fontFamily = "Arial",
      fontSize = "14px"))
)

###



## ui function -----------------------------------------------------------------------
# id = unique id 
pop_groups_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      # sidebar for filters -----------------------------
      sidebar = sidebar(width = 300,
                        # 2 x help buttons
                        layout_column_wrap(
                          1/2,
                          actionButton(ns("help"), label = "Help", class = "btn-sm"), # help button
                          indicator_definition_btn_ui(ns("ind_def")) # indicator definitions button
                        ),
                        
                        # indicator filter (note this is a module)
                        indicator_filter_mod_ui(ns("indicator_filter")),
                        
                        # filter to select pop split (set choices to NULL 
                        # as they are updated dynamically on the server side, depending on selected indicator)
                        selectInput(
                          inputId = ns("split_filter"),
                          label = "Select equality split:",
                          selectize = TRUE,
                          choices = NULL
                        )
      ), # close sidebar
      
      
      layout_column_wrap(
        1/2,
        
        # Bar chart card ------------------------------------------
        bslib::navset_card_pill(
          height = 550,
          full_screen = TRUE,
          
          # tab 1: bar chart 
          bslib::nav_panel("Chart",
                           uiOutput(ns("pop_rank_title")), # title 
                           highchartOutput(ns("pop_rank_chart")) # chart 
          ),
          
          # tab 2: data table
          bslib::nav_panel("Table",
                           reactableOutput(ns("pop_rank_table")) # table
          ),
          
          
          bslib::nav_spacer(),
          
          # extra controls for bar chart 
          bslib::nav_item(
            bslib::popover(
              title = "Filters",
              bsicons::bs_icon("gear", size = "1.7em"),
              checkboxInput(ns("ci_switch"), label = " include confidence intervals", FALSE),
              selectInput(ns("pop_years_filter"), label = "select year", choices = NULL)
            )
          ),
          
          # card footer - download buttons
          card_footer(class = "d-flex justify-content-between",
                      download_chart_mod_ui(ns("save_pop_rankchart")),
                      download_data_btns_ui(ns("pop_rank_download")))
        ), # close bar chart card
        
        ######  take monica's trend card and adapt
        bslib::navset_card_pill(
          height = 550,
          full_screen = TRUE,
          
          # tab 1: trend chart 
          bslib::nav_panel("Chart",
                           uiOutput(ns("pop_trend_title")), # title
                           highchartOutput(ns("pop_trend_chart")) # chart
          ),
          # to do IM
          # tab 2: data table # still need to creat this
          bslib::nav_panel("Table",
                           reactableOutput(ns("trend_table"))
          ),
          
          bslib::nav_spacer(),
          
          # extra controls for filters
          bslib::nav_item(
            bslib::popover(
              title = "Filters",
              bsicons::bs_icon("gear",size = "1.7em"),
              checkboxInput(ns("trend_ci_switch"), label = " include confidence intervals", FALSE)
            )
          ),
          # card footer - download buttons
          card_footer(class = "d-flex justify-content-between",
                      download_chart_mod_ui(ns("save_simd_trendchart")),
                      download_data_btns_ui(ns("simd_trendchart_download")))
        ) # close trend card
        
        
      ) # close layout column wrap
      
    ) # close sidebar layout
  ) # close taglist
} # close ui function 



#######################################################
## MODULE SERVER
#######################################################


pop_groups_server <- function(id, dataset, geo_selections) {
  moduleServer(id, function(input, output, session) {
    
    
    #######################################################
    ## Dynamic filters -----
    ######################################################
    
    ## update choices for inequalities split filter, depending on what indicator was selected
    observe({
      
      available_splits <- dataset() |>
        filter(indicator == selected_indicator()) |>
        pull(unique(split_name))
      
      
      updateSelectInput(session, inputId = "split_filter", choices = available_splits)
      
      
    })
    
    
    # update years choices for bar chart filter, depending on indicator selected
    observe({
      
      available_years <- pop_rank_data() |>
        filter(indicator == selected_indicator() & areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>
        arrange(desc(year)) |>
        pull(unique(year))
      
      updateSelectInput(session, inputId = "pop_years_filter",
                        choices = available_years, selected = available_years[1])
    })
    
    
    
    #######################################################
    ## Reactive data / values ----
    #######################################################
    
    # generate list of indicators (from the simd indicators dataset) available 
    selected_indicator <- indicator_filter_mod_server(id = "indicator_filter", dataset, geo_selections)
    
    
    # calls definition button module server script and passes the actual indicator selected)
    indicator_definition_btn_server("ind_def", selected_indicator = selected_indicator)  
    
    
    # creates trend data
    pop_trend_data <- reactive({
      dataset() |>
        mutate(colour_pal= case_when(split_value== "16-24"~ phs_colors(colourname = "phs-purple"),
                                     split_value == "25-34"~ phs_colors(colourname = "phs-magenta"),
                                     split_value == "35-44"~ phs_colors(colourname = "phs-teal"),
                                     split_value == "45-54"~ phs_colors(colourname = "phs-blue"),
                                     split_value == "55-64"~ phs_colors(colourname = "phs-green"),
                                     split_value == "65-74"~ phs_colors(colourname = "phs-purple-50"),
                                     split_value == "75+"~ phs_colors(colourname = "phs-purple-30"),
                                     split_value == "Total ages"~ phs_colors(colourname = "phs-purple-80"),
                                     #
                                     split_value== "All sex"~ phs_colors(colourname = "phs-purple"),
                                     split_value == "Female"~ phs_colors(colourname = "phs-magenta"),
                                     split_value == "Male"~ phs_colors(colourname = "phs-teal"),
                                     #
                                     split_value== "limiting_li"~ phs_colors(colourname = "phs-purple"),
                                     split_value == "no_li"~ phs_colors(colourname = "phs-magenta"),
                                     split_value == "non_limiting_li"~ phs_colors(colourname = "phs-teal"),
                                     TRUE ~ phs_colors(colourname = "phs-liberty")        ))|>
        filter(areatype == geo_selections()$areatype & areaname == geo_selections()$areaname) |>  # filter by selected geography
        filter(indicator == selected_indicator() & split_name == input$split_filter) # filter by selected indicator and selected split
   
      # dt <- setDT(dataset())
      # 
      # dt <- dt[, colour_pal := fcasesplit_value == "16-24", phs_colors(colourname = "phs-purple"),
      #                             split_value == "25-34", phs_colors(colourname = "phs-magenta"),
      #                             split_value == "35-44", phs_colors(colourname = "phs-teal"),
      #                             split_value == "35-44", phs_colors(colourname = "phs-blue"),
      #                             split_value == "45-54", phs_colors(colourname = "phs-green"),
      #                             #
      #                             split_value == "55-64", phs_colors(colourname = "phs-purple-50"),
      #                             split_value == "65-74", phs_colors(colourname = "phs-rust"),
      #                             split_value == "75+", phs_colors(colourname = "phs-liberty"),
      #                             split_value == "Total ages", phs_colors(colourname = "phs-blue"),
      #                             #
      #                             split_value == "All sex", phs_colors(colourname = "phs-purple"),
      #                             split_value == "Female", phs_colors(colourname = "phs-teal"),
      #                             split_value == "Male", phs_colors(colourname = "phs-blue"),
      #                             #
      #                             split_value == "limiting_li", phs_colors(colourname = "phs-purple"),
      #                             split_value == "no_li", phs_colors(colourname = "phs-teal"),
      #                             split_value == "non_limiting_li", phs_colors(colourname = "phs-blue")]
       })
    
    
    # take trend data and create single year data (for rank bar chart)
    # pop_rank_data <- reactive({
    #   pop_trend_data() |>
    #     filter(year == max(year))
    #   
    #   
    # })
    # 
    
    pop_rank_data <- reactive({
      pop_trend_data() |>
        filter(year == input$pop_years_filter)
    })
    
    # pop_rank_data <- reactive({
    #   pop_trend_data()[year == input$pop_years_filter]
    # })
    
    #######################################################
    ## dynamic text  ----
    #######################################################
    
    output$pop_rank_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(pop_trend_data()) > 0, "No indicators available")
      )
      
      # if data is available display chart title
      tagList(
        tags$h5(selected_indicator(), " split by ", input$split_filter, class = "chart-header"),
        tags$h6(pop_rank_data()$year[1]), # time period
        tags$p("Percentage meeting the MVPA") # measure type
      )
    })
    
    # need to add pop-trend title stuff
    
    output$pop_trend_title <- renderUI({
      
      # ensure there is data available, otherwise show message instead
      shiny::validate(
        need( nrow(pop_trend_data()) > 3, "There are insufficent data points for this indicator to create a trend chart")
      )
      
      # if data is available display chart title
      tagList(
        tags$h5(selected_indicator(), " split by ", input$split_filter, class = "chart-header"),
        tags$h6(pop_trend_data()$year[1]), # time period
        tags$p("Percentage meeting the MVPA") # measure type
      )
    })
    
    ############################################
    # charts -----
    #############################################
    
    # pop rank bar chart  ---------------
    
    output$pop_rank_chart <- renderHighchart({
      
      shiny::validate(
        need( nrow(pop_rank_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      x <- hchart(pop_rank_data(), 
                  type = "column", hcaes(x = sub_code, y = rate, color = colour_pal)) %>%
        hc_yAxis(gridLineWidth = 0) %>%
        hc_chart(backgroundColor = 'white') %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_yAxis(title = list(text = "")) %>%
        hc_plotOptions(series = list(animation = FALSE),
                       column = list(groupPadding = 0))
      
      
      
      if(input$ci_switch) {
        x <- x |>
          hc_add_series(pop_rank_data(), "errorbar", hcaes(x = sub_code, low = lowci, high = upci), zIndex = 10)
      }
      
      x
    }) # end pop_rank_chart
    
    # pop trend bar chart  ---------------
    
    output$pop_trend_chart <- renderHighchart({
      
      # define objects for chart titles and labels
      # selected_area <- unique(pop_trend_data()$location_name)
      # definition <- unique(pop_trend_data()$def_period)
      # split_name <- input$split_filter
      # chart_title <- paste(definition, "in", selected_area, "by", split_name)
      
      
      # generate name value for line chart
      selected_split <- unique(pop_trend_data()$split_value)
      x <- hchart(pop_trend_data(), 
                  "line", 
                  hcaes(x = trend_axis, y =rate, group = split_value),
                  color = unique(pop_trend_data()$colour_pal)) |>
                    
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
          table = TRUE)  #|> add back on if extending options
        # add extra bits to chart for downloaded version (still need to add subtitles?)
        # hc_exporting(
        #   chartOptions = list(
        #     title = list(text = paste0(selected_indicator(), " split by SIMD Quintile"))
        #   )
       # )
      
      # add averae line if switch turned on 
    #  if(input$average_switch == TRUE){

        # x <- x |> hc_add_series(
        #   trend_data(),
        #   "line",
        #   name = "Average",
        #   color = "#FF0000",
        #   hcaes(x = trend_axis, y = avg)
        # )
      
    }) #end  pop trend chart
    
    ##########################################
    # Tables ---------
    ###########################################
    
    # trend data table -------
    output$pop_rank_table <- renderReactable({
      
      data <- pop_trend_data() |>
        select(year, sub_code, rate)
      
      reactable(data = data,
                defaultExpanded = TRUE,
                defaultPageSize = nrow(data)
      )
    })
    
    ### need to create pop trend table
    
    ############################################
    # Downloads  ----
    #############################################
    # needs addressed
    download_chart_mod_server(id = "save_simd_barchart", chart_id = session$ns("pop_trend_chart"))
    download_data_btns_server(id = "simd_trendchart_download", data = pop_trend_data())
    
    
  }
  )
}