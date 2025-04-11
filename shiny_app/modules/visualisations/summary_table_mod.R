### to do 
# get small example app working properly

###############################################################################.
# MODULE: summary_table_mod ---- 
###############################################################################.

# prepares summary data for each profile and creates a table containing the latest data for each indicator in a profile, for the chosen geography
# within this table, there is a spine chart rendered one each row
# note for the charts to work you need to add a link to the highchart js library in the UI script (highcharts is not free - ScotPHO have a licence for this)
# module also includes download options:
# option to download as pdf (requires a separate rmarkdown file to re-create the summary table)
# option to download data in various formats using another module which is nested in this module (see download_data_mod.R)

###############################################################################.
# UI function ----
###############################################################################.

# id = unique id 

summary_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
    # enable guided tour
    # use_cicerone(),
    
    br(),
    hidden(
      card(
        id = ns("spine_chart_explanation"),
        card_header("How to interpret table results"),
        card_body(
          p("The results below provide a snapshot of the latest data for each indicator in this profile at your selected geography level. If you have selected a local area from the geography filter above, you will see a 'spine chart' for each indicator. 
                 These charts show where your selected local area fits in amongst the range of values and the national average. For example, comparing a particular health board against all other health boards. Results can be interpreted using the key below:"),
          layout_columns(
            col_widths = c(4, 8),
            p(tags$img(src='spine_chart.png', width = "350px", height = "auto", role="img",
                       alt = "Image to illustrate how to interpret the bars presented in the local area profile table. 
                                        Shows a grey horizontal bar with labels. The left end is labelled \"value for \'worst\' area\". 
                                        The right end is labelled \"value for \'best\' area\". A darker grey central portion is labelled \"the middle 50% of areas\". 
                                        A red central line is labelled \"Scotland average (mean)\". A coloured circle on the bar is labelled \"value for selected area\".
                                        The text to the right of the image explains the meaning of the three possible circle colours.")), 
            layout_columns(
              span(tags$div(style = "width:20px; height:20px; background-color:orange; border-radius:50%; display:inline-block; margin:5px;"), "orange - worse than national average"),
              span(tags$div(style = "width:20px; height:20px; background-color:blue; border-radius:50%; display:inline-block; margin:5px;"), "blue - better than national average"),
              span(tags$div(style = "width:20px; height:20px; background-color:gray; border-radius:50%; display:inline-block; margin:5px;"), "grey - not statistically different to Scotland"),
              span(tags$div(style = "width:20px; height:20px; background-color:white; border:1px solid black; outline-color:black; border-radius:50%; display:inline-block; margin:5px;"), "white - no difference to be calculated")
            ))))
    ),
    
    bslib::card(
      bslib::card_header(
        class = "d-flex flex-row-reverse",
        layout_columns(
          downloadButton(ns("download_summary_pdf"), "Download PDF report", class = "btn-sm btn-download", icon = icon("file-pdf")),
          download_data_btns_ui(ns("download_summary_data")),
        )
      ),
      card_body(
        withSpinner(reactableOutput(ns("summary_table"))) # summary table
      )
    )
  )
}


###############################################################################.
# SERVER function 
###############################################################################.

# id = unique id
# selected_geo = name of the reactive value storing selected areaname and selected areatype
# selected_profile = name of reactive value storing selected profile
# filtered_data = name of reactive dataframe where data has already been filtered by profile 

summary_table_server <- function(id, selected_geo, selected_profile, filtered_data) {
  
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    
    # show spine chart explanation when any areatype other than Scotland has been selected, otherwise hide it
    observe({
      if (selected_geo()$areatype != "Scotland") {
        shinyjs::show("spine_chart_explanation")
      } else {
        shinyjs::hide("spine_chart_explanation")
      }
    })
    
    
    # prepare local summary data ----
    local_summary <- reactive({
      req(selected_geo()$areatype != "Scotland")
      
      shiny::validate(
        need(selected_geo()$areatype %in% unique(filtered_data()$areatype), 
             paste0("Currently, there are no indicators in this profile available at ", selected_geo()$areatype, "level. Please select another geography level to view indicators in this profile."))
      )
      
      # convert to data.table format (using data.table package) to run quicker 
      dt <- setDT(filtered_data())
      
      # filter data by areatype and areaname
      dt <- dt[areaname == selected_geo()$areaname & areatype == selected_geo()$areatype]
      
      # remove archived indicators
      dt <- dt[!(ind_id %in% archived_indicators)]

      # get latest data for each indicator
      chosen_area <- dt[type_definition != "Number",
                        .SD[!is.na(measure) & year == max(year)], by = indicator]

      # include scotland figures for comparison
      scotland <- main_dataset[areaname == "Scotland"]
      chosen_area <- chosen_area[scotland, on = c("ind_id", "year"), scotland_value := scotland$measure]
      
      # calculate quantiles for each indicator within chosen geography level for spine chart
      chosen_areatype <- unique(chosen_area$areatype)
      other_areas <- main_dataset[areatype == chosen_areatype][chosen_area, on = .(ind_id, year), nomatch = 0][,
                                                                                                               .(Q0 = quantile(measure, probs = 0, na.rm = TRUE),
                                                                                                                 Q100 = quantile(measure, probs = 1, na.rm = TRUE),
                                                                                                                 Q25 = quantile(measure, probs = 0.25, na.rm = TRUE),
                                                                                                                 Q75 = quantile(measure, probs = 0.75, na.rm = TRUE)),
                                                                                                               by = .(ind_id, year)]
      
      # add quantile values for each indicator to table
      chosen_area <- chosen_area[other_areas, on = c("ind_id", "year"),
                                 c("Q0", "Q100", "Q25", "Q75") := .(other_areas$Q0, other_areas$Q100 ,other_areas$Q25, other_areas$Q75)]
      
      
      # if the selected profile has a particular order the domains should appear in the table
      # (i.e. the selected profile's domain order isn't NULL in the 'profiles_list' from the global script)
      # then covert the domain column to factor and set levels to ensure the data is ordered accordingly
      if(!is.null(selected_profile()$domain_order)){
        chosen_area <- chosen_area[, domain := factor(domain, levels = selected_profile()$domain_order)]
      }
      
      chosen_area <- setorder(chosen_area, domain)

      # assign colours to values depending on statistical significance
      final <- chosen_area %>%
        mutate(marker_colour = case_when(lowci <= scotland_value & upci >= scotland_value & interpret %in% c("H", "L") ~'#6A6C6D',
                                         lowci > scotland_value & interpret == "H" ~ '#1B7CED',
                                         lowci > scotland_value & interpret == "L" ~ '#FFA500',
                                         upci < scotland_value & interpret == "L" ~ '#1B7CED',
                                         upci < scotland_value & interpret == "H" ~ '#FFA500',
                                         interpret == "O" ~ '#FFFFFF', TRUE ~ '#FFFFFF'))
      
      
      # creating spine chart data ----
      final <- final %>%
        # duplicate chosen area value in another column so one can be used in the table and one can be used for spine chart
        mutate(chosen_value = measure) %>%
        
        mutate(scale_min = case_when(scotland_value - Q0 > Q100 - scotland_value ~ Q0,
                                     TRUE ~ scotland_value - (Q100 - scotland_value))) %>%
        mutate(scale_max = case_when(scale_min == Q0 ~ scotland_value + (scotland_value - Q0), TRUE ~ Q100)) %>%
        mutate(across(c(chosen_value, Q0, Q25, Q75, Q100), ~ (. - scale_min) / (scale_max - scale_min)))
      
      
      final <- final %>%
        mutate(across(c("Q0", "Q25", "Q75", "Q100", "chosen_value"), ~ case_when(interpret == "L" ~ 1 - ., TRUE ~ .)))
      
      
      # conditionally calculating worst to best, depending on whether a lower is value is better or a higher value is better
      # this ensures that 'worst' is always to the left of the spine, and 'best' is always to the right
      final <- final %>%
        mutate(worst = case_when(interpret == "L" ~ Q0 - Q25, TRUE ~ Q100 - Q75), # worst
               p25 = case_when(interpret == "L" ~ Q25 - Q75, TRUE ~ Q75 - Q25), # 25th percentile
               p75 = case_when(interpret == "L" ~ Q75 - Q100, TRUE ~ Q25 - Q0), # 75th percentile
               best = case_when(interpret == "L" ~ Q100, TRUE ~ Q0) # best
        )
      
      final$chart <- NA # create empty column to populate with in-line highcharts
      
      # create id to use for the charts (each chart needs a unique id otherwise chart won't display)
      final <- final |> 
        mutate(unique_id = paste0("highchart-", indicator, "-", chosen_value, Sys.time()))
      
      
      # selecting columns required for table ----
      final <- final %>%
        select(code,
               areaname,
               areatype,
               domain, # required for domain column 
               indicator, # required for indicator column 
               measure, # required for for selected area column 
               scotland_value, # required for scotland column 
               type_definition, # required for indicator column 
               def_period, # required for indicator column 
               chart, # required for spine chart 
               worst, # required for building spine chart 
               p25, # required for building spine chart 
               p75, # required for building spine chart 
               best, # required for building spine chart 
               chosen_value, # required for building spine chart 
               marker_colour, # required for building spine chart 
               unique_id) # required for building spine chart )
       
      final
      
    })
    
    
    # prepare scotland summary data ----
    scotland_summary <- reactive({
      req(selected_geo()$areatype == "Scotland")
      
      # set the profile data to data.table format
      dt <- setDT(filtered_data())
      
      # filter on scotland 
      dt <- dt[areaname == "Scotland" & type_definition != "number"]
      
      # remove archived indicators
      dt <- dt[!(ind_id %in% archived_indicators)]
      
      # order the data before grouping
      setorder(dt, indicator, year)
      
      # aggregate to 1 row per indicator (this step is equivalent using group_by() and summarise() from dplyr)
      dt <- dt[,.(measures = list(measure), # for the trend chart 
                  years = list(year), # for the trend chart 
                  domain = first(domain), # for the table 
                  trend_min = first(trend_axis), # for the trend chart label
                  trend_max = last(trend_axis), # for the trend
                  def_period = last(def_period), # for the table
                  type_definition = first(type_definition), # for the table
                  measure = last(measure), # for the table
                  code = "S00000001", # for data download
                  areatype = "Scotland", # for data download
                  areaname = "Scotland" # for data download
                  ),
               by = indicator]
      
      # create some additional cols
      dt <- dt[, trend := NA] # empty column to place chart in 
      dt <- dt[, unique_id := paste0(indicator, Sys.time())] # unique id for each chart 
      
      # set domain column as the first in the table
      setcolorder(dt, "domain")
      

      # if the selected profile has a particular order the domains should appear in the table
      # (i.e. the selected profile's domain order isn't NULL in the 'profiles_list' from the global script)
      # then covert the domain column to factor and set levels to ensure the data is ordered accordingly
      if(!is.null(selected_profile()$domain_order)){
        dt <- dt[, domain := factor(domain, levels = selected_profile()$domain_order)]
      }
      
      dt <- setorder(dt, domain)
      
      dt
      
      
    })
    
    
    # prepare data for download extract ----
    data_download <- reactive({
      if(selected_geo()$areatype == "Scotland"){
        df <- scotland_summary() |>
          select("code",
                   "areaname",
                   "areatype",
                   "domain", 
                   "indicator", 
                   "definition_period" = "def_period", 
                   "type_definition", 
                   "measure")
      } else {
        df <- local_summary() |>
          select("code",
               "areaname",
                "areatype",
                "domain",
                "indicator",
                "type_definition",
                "definition_period" = "def_period",
                "measure",
                "scotland_value")
      }
      
      df
    })
    

    
    
    output$summary_table <- renderReactable({
      
      # determine which dataset to use
      data <- if(selected_geo()$areatype == "Scotland"){
        scotland_summary()
      } else {
        local_summary()
      }
      
      shiny::validate(
        need( nrow(data) > 0, "No indicators available")
      )
      
      # domain column ----
      domain =  colDef(
        name = "Domain",
        maxWidth = 120,
        # this JS function hides domain name from appearing on every row
        # i.e. gives appearance of 'merged' cells
        style = JS("function(rowInfo, column, state) {
                                         const prevRow = state.pageRows[rowInfo.viewIndex - 1]
                                         if (prevRow && rowInfo.values['domain'] === prevRow['domain']) {
                                           return {visibility: 'hidden'}
                                         }
                                       }
                                     "))
      
      
      # indicator column ----
      indicator = colDef(
        name = "Indicator",
        minWidth = 320,
        html = TRUE,
        
        # this JS function creates clickable links to view trend data
        # when user clicks an indicator, it navigates to 'trends' tab
        # and updates filters on that tab to that particular indicator and users chosen geography area
        cell = JS(" function(rowInfo){
                                              return `<div>
                                                      <div style =  'font-weight: bold;'>${rowInfo.values['indicator']}</div>
                                                      <div style = 'margin-top: 3px;'>
                                                      <span style = 'margin-right: 0.25rem; padding: 2px; background-color:#F5F5F5;
                                                                      border: 1px solid hsl(0, 0%, 75%);
                                                                      border-radius: 2px;'>${rowInfo.values['type_definition']}</span>
                                                                  <span> • </span><span>${rowInfo.values['def_period']}
                                                                  </span>
                                                                     </div>
                                                                   </div>`;}"))
      
      # Scotland column ----
      scotland_value = colDef(
        maxWidth = 80,
        name = "Scotland",
        cell = function(value){
          div(style = "margin-top: 19px;", value)
        }
      )
      
      
      
      # Chosen area column ----
      measure = colDef(
        maxWidth = 80,
        name = as.character(selected_geo()$areaname),
        cell = function(value){
          div(style = "margin-top: 19px;", value)
        }
      )
      
      # spine chart column ----
      chart = colDef(name = "Chart",
                     html = T,
                     minWidth = 200,
                     cell = JS("
              function(rowInfo) {
                var containerId = rowInfo.row.unique_id;
                var rowData = rowInfo.row;

                var chartHTML = '<div id=\"' + containerId + '\" style=\"height: 70px; width: 100%\"></div>';

                setTimeout(function() {
                  Highcharts.chart(containerId, {
                    chart: {
                      type: 'bar',
                      backgroundColor:'transparent',
                      animation: false
                    },
                    title: {
                      text: ''
                    },
                    xAxis: {
                      categories: [''],
                      title: { text: null },
                      lineWidth: 0,
                      lineColor: 'transparent',
                      gridLineColor: 'transparent'
                    },
                    yAxis: {
                      min: 0,
                      max: 1,
                      title: { text: null },
                      labels: { enabled: false },
                      startOnTick: false,
                      endOnTick: false,
                      gridLineColor: 'transparent',
                      plotLines:[{
                      color: 'red',
                      width: 3,
                      value: 0.5,
                      zIndex: 1000
                      }],
                      lineColor: 'transparent'
                      },
                    legend: { enabled: false },
                    plotOptions: {
                      series: {
                        animation: false,
                        stacking: 'normal',
                        dataLabels: {
                          enabled: false
                        },
                        enableMouseTracking: false
                      }
                    },
                    series: [{
                      name: 'worst',
                      data: [{y: rowData.worst, color: '#D3D3D3'}]
                    }, {
                      name: '25th Percentile',
                    data: [{y: rowData.p25, color: '#A4A4A4'}]
                    }, {
                      name: '75th Percentile',
                      data: [{y: rowData.p75, color: '#D3D3D3'}]
                    }, {
                      name: 'best',
                      data: [{y: rowData.best, color: 'white'}]
                    }, {
                      type: 'scatter',
                      data: [{
                        x: 0,
                        y: rowData.chosen_value,
                        marker: {
                          radius: 8,
                          lineWidth: 1,
                          lineColor: 'black',
                          fillColor: rowData.marker_colour
                        }
                      }],
                      marker: {symbol: 'circle'},
                      zIndex: 5
                    }],
                    credits: {
                      enabled: false
                    },
                    exporting: {
                      enabled: false
                    },
                    tooltip: {
                      enabled: false
                    }
                  });
                }, 20); // add brief delay before rendering charts 

                return chartHTML;
              }
            "))
      
      
      
      trend = colDef(
        name = "Trend",
        html = TRUE,
        cell = JS("
            function(rowInfo) {
              var containerId = rowInfo.row.unique_id;
              var chartHTML = '<div id=\"' + containerId + '\" style=\"height: 100px; width: 100%\"></div>';
              var time_period = rowInfo.row.trend_min + ' to ' + rowInfo.row.trend_max;
              
              setTimeout(function() {
                Highcharts.chart(containerId, {
                  chart: {
                    type: 'area',
                    backgroundColor:'transparent',
                    animation: false
                  },
                  title: {
                    text: time_period,
                     style: {
                      fontSize: '12px'
                    }
                  },
                  xAxis: {
                    categories: rowInfo.row.years,
                    labels: {
                      enabled: false
                    },
                    tickLength: 0,
                    lineWidth: 0
                  },
                  yAxis: {
                    title: {
                      text: null,
                      
                    },
                    labels: {
                      enabled: false
                    },
                    gridLineWidth: 0
                  },
                  series: [{
                    name: '',
                    data: rowInfo.row.measures,
                    color: '#0078D4',
                    fillColor: {
                    linearGradient: {
                    x1: 0,
                    y1: 0,
                    x2: 0,
                    y2: 1
                    },
                    stops: [
                    [0, '#B3D7F2'],
                    [1, '#E6F2FB'] 
                    ]
                    },
                    marker: {
                    enabled: true,
                    radius: 3 
                    }
                    
                  }],
                  credits: {
                      enabled: false
                    },
                    exporting: {
                      enabled: false
                    },
                    tooltip: {
                      enabled: false
                    },
                    legend: { 
                    enabled: false 
                    },
                    plotOptions: {
                      series: {
                        animation: false,
                        connectNulls: true,
                        dataLabels: {
                          enabled: false
                        },
                        enableMouseTracking: false
                      }
                    }
                });
              }, 20); // add brief delay before rendering charts
              
              return chartHTML;
            }
          ")
      )
      
      
      
      if(selected_geo()$areatype == "Scotland"){
        cols <- list(domain = domain, 
                     indicator = indicator,
                     measure = measure, 
                     trend = trend,
                     def_period = colDef(show = FALSE),
                     years = colDef(show = FALSE),
                     measures = colDef(show = FALSE),
                     type_definition = colDef(show = FALSE),
                     unique_id = colDef(show = FALSE),
                     trend_min = colDef(show = FALSE),
                     trend_max = colDef(show = FALSE),
                     code = colDef(show = FALSE),
                     areatype = colDef(show = FALSE),
                     areaname = colDef(show = FALSE)
                     
                     )
      } else {
        cols <- list(domain = domain, 
                     indicator = indicator, 
                     scotland_value = scotland_value, 
                     measure = measure, 
                     chart = chart,
                     code = colDef(show = FALSE),
                     areaname = colDef(show = FALSE),
                     areatype = colDef(show = FALSE),
                     worst = colDef(show = FALSE),
                     p25 = colDef(show = FALSE),
                     p75 = colDef(show = FALSE),
                     best = colDef(show = FALSE),
                     chosen_value = colDef(show = FALSE),
                     def_period = colDef(show = FALSE),
                     marker_colour = colDef(show = FALSE),
                     type_definition = colDef(show = FALSE),
                     unique_id = colDef(show = FALSE))
      }
      
      
      reactable(data,
                compact = TRUE,
                defaultPageSize = nrow(data),
                defaultExpanded = T,
                sortable = F,
                highlight = FALSE,
                theme = reactableTheme(
                  headerStyle = list(backgroundColor = "#ECECEC")
                ),
                columns = cols)
      
 
      
      
    })

    
    # download data module ----
    download_data_btns_server(id = "download_summary_data", 
                              file_name = "ScotPHO_summary_data_extract",
                              data = data_download
                              )

    
    # download PDF logic ----
    output$download_summary_pdf <- downloadHandler(
      
      # name of file when downloaded
      filename = function() {
        paste(selected_profile()$full_name, "-summary-", selected_geo()$areaname, ".pdf", sep="")
      },
      
      content = function(file) {
        
        
        td <- tempdir() # create a temporary directory
        
        markdown_doc <- ifelse(selected_geo()$areatype == "Scotland", "scotland_summary_table_pdf.Rmd", "spinecharts.Rmd")
        
        tempReport <- file.path(td, markdown_doc) # rmarkdown file 
        tempLogo <- file.path(td, "scotpho_reduced.png") # scotpho logo to add to pdf
        
        file.copy(markdown_doc, tempReport, overwrite = TRUE) 
        file.copy("scotpho_reduced.png", tempLogo, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          reactive_df = if (selected_geo()$areatype == "Scotland") {
          scotland_summary()
        } else {
          local_summary()
        },
         chosen_area = selected_geo()$areaname,
         chosen_profile = selected_profile()$full_name,
         chosen_geography_level = selected_geo()$areatype
        )
        
        # create the pdf report 
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
        
        # unload package after each time report generated
        # otherwise users can only download 1 pdf, and any other download attempts will fail
        # details of issue here: https://stackoverflow.com/questions/46080853/why-does-rendering-a-pdf-from-rmarkdown-require-closing-rstudio-between-renders
        detach("package:kableExtra", unload=TRUE)
      }
    )


    
    
    ###########################################.
    # Guided tour ----
    ###########################################.
    
    guide_summary <- Cicerone$
      new()$
      step(
        ns("summary_download_pdf_wrapper"),
        "Download PDF",
        "Click here to download a PDF containing all available indicators for the selected area.",
        position = "below"
      )$
      step(
        ns("summary_download_data_wrapper"),
        "Download Data Button",
        "Click here to download the selected data as a CSV, RDS or JSON file.",
        position = "below"
      )
    
    #initiate the guide
    guide_summary$init()
    
    #when guided tour button is clicked, start the guide
    observeEvent(input$summary_tour_button, {
      guide_summary$start()
    })
    
 
       

  }) # close module server
} # close module




##############################################################################.
# example usage - repex of how this module works can be generated by running 
# the functions declared above then uncommenting the code below.
##############################################################################.
# library(bslib)
# library(shiny)
# 
# 
# source("modules/download_data_mod.R")
# 
# 
# main_dataset <- data.frame(profile = c("P1", "P1", "P1", "P1"),
#                          domain = c("domain 1", "domain 1", "domain 1", "domain 1"),
#                          indicator = c("ind 1", "ind 1", "ind 1", "ind 1"),
#                          ind_id = c(1, 1, 1, 1),
#                          type_definition = c("percent", "percent", "percent", "percent"),
#                          def_period = c("2021-2023: 3 year aggregates", "2021-2023: 3 year aggregates", "2021-2023: 3 year aggregates", "2021-2023: 3 year aggregates"),
#                          areatype = c("Scotland", "Health board", "Health board", "Health board"),
#                          areaname = c("Scotland", "NHS Borders", "NHS GGC", "NHS FV"),
#                          interpret = c("H", "H", "H", "H"),
#                          measure = c(20, 30, 10, 40),
#                          upci = c(20, 35, 15, 45),
#                          lowci = c(20, 25, 5, 35),
#                          year = c(2023, 2023, 2023, 2023))
# 
# 
# ui <- page_navbar(title = "Summary table module example",
#                   tags$script(src = "https://code.highcharts.com/highcharts.js"),
#                   nav_panel(title = "Profile 1",
#                             layout_column_wrap(
#                             selectInput("profile", label = "profile:", choices = c("P1"), selected = "P1"),
#                             selectInput("areatype", label = "areatype: ", choices = c("Health board"), selected = "Health board"),
#                             selectInput("areaname", label = "areaname: ", choices = c("NHS Borders", "NHS GGC", "NHS FV"), selected = "NHS Borders")
#                             ),
#                             summary_table_ui("P1_summary"))
#                   )
# 
# 
# 
# 
# server <- function(input, output, session) {
# 
# 
# 
# 
# 
#   chosen_geographies <- reactive({
#     list(areatype = input$areatype, areaname = input$areaname)
#   })
# 
# 
# chosen_profile <- reactiveVal()
# 
# observe({
#   chosen_profile(input$profile)
# })
# 
# 
#   filtered <- reactive({
#     main_dataset |>
#       filter(profile == input$profile)
#   })
# 
# 
# 
#   summary_table_server(id = "P1_summary",
#                        selected_geo = chosen_geographies,
#                        selected_profile  = chosen_profile,
#                        filtered_data = filtered)
# 
# 
# }
# 
# 
# shinyApp(ui, server)
