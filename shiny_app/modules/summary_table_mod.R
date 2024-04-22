### to do 
# - decide how to present scotland data since spine chart doesn't make sense?
# get small example app working properly



###############################################################################.
# MODULE: summary_table_mod ---- 
# prepares summary data for each profile and creates a table containing the latest data for each indicator in a profile, for the chosen geography
# within this table, there is a spine chart rendered one each row
# note for the charts to work you need to add a link to the highchart js library in the UI script (highcharts is not free - ScotPHO have a licence for this)
# module also includes download options:
# option to download as pdf (requires a separate rmarkdown file to re-create the summary table)
# option to download data in various formats using another module which is nested in this module (see download_data_mod.R)


# UI function:
# id = unique id 
summary_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
            layout_columns(
              col_widths = c(5, 2, 3, 2),
              NULL,
              actionButton(ns("help"), label = "Help"), 
              downloadButton(ns("download_summary_pdf"), "Download PDF report"),
              download_data_btns_ui(ns("download_summary_data"))
              )
            ),
      card_body(
        withSpinner(reactableOutput(ns("summary_table"))) # summary table
       )
    )
)

}

# server function 
# id = unique id
# selected_geo = name of the reactive value storing selected areaname and selected areatype
# selected_profile = name of reactive value storing selected profile
# filtered_data = name of reactive dataframe where data has already been filtered by profile 

summary_table_server <- function(id, selected_geo, selected_profile, filtered_data) {
  
  moduleServer(id, function(input, output, session) {
    
    
    summaryData <- reactive({
      
      # filter data by areatype and areaname
      dt <- filtered_data() |>
        filter(areaname == selected_geo()$areaname & areatype == selected_geo()$areatype)
      
      
      # convert to data.table format (using data.table package) to run quicker 
      dt <- as.data.table(dt)
      
      
      # filter by chosen area and get latest data for each indicator
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
      
      # assign colours to values depending on statistical significance
      final <- chosen_area %>%
        mutate(marker_colour = case_when(lowci <= scotland_value & upci >= scotland_value & interpret %in% c("H", "L") ~'#6A6C6D',
                                         lowci > scotland_value & interpret == "H" ~ '#1B7CED',
                                         lowci > scotland_value & interpret == "L" ~ '#FFA500',
                                         upci < scotland_value & interpret == "L" ~ '#1B7CED',
                                         upci < scotland_value & interpret == "H" ~ '#FFA500',
                                         interpret == "O" ~ '#FFFFFF', TRUE ~ '#FFFFFF'))
      
      
      # creating spine chart data
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
               p25 = case_when(interpret == "L" ~ Q75 - Q100, TRUE ~ Q25 - Q0), # 25th percentile
               p75 = case_when(interpret == "L" ~ Q25 - Q75, TRUE ~ Q75 - Q25), # 75th percentile
               best = case_when(interpret == "L" ~ Q100, TRUE ~ Q0) # best
        )
      
      
      final$chart <- NA # create empty column to populate with in-line highcharts
      
      # create id to use for the charts (each chart needs a unique id otherwise chart won't display)
      final <- final |> 
        mutate(unique_id = paste0("highchart-", indicator, "-", chosen_value, Sys.time()))
      
      
      
      # selecting columns required for table
      final <- final %>%
        select(domain, # required for domain column 
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

      
    })
    
    
    output$summary_table <- renderReactable({
      
      
      reactable(summaryData(),
                compact = TRUE,
                defaultExpanded = T,
                sortable = F,
                highlight = TRUE,
                theme = reactableTheme(
                  headerStyle = list(backgroundColor = "#ECECEC")
                ),
                columns = list(
                  
                  # domain column 
                  domain = colDef(
                    name = "domain",
                    maxWidth = 120,
                    # this JS function hides domain name from appearing on every row
                    # i.e. gives appearance of 'merged' cells
                    style = JS("function(rowInfo, column, state) {
                                         const prevRow = state.pageRows[rowInfo.viewIndex - 1]
                                         if (prevRow && rowInfo.values['domain'] === prevRow['domain']) {
                                           return {visibility: 'hidden'}
                                         }
                                       }
                                     ")),
                  
                  # indicator column --------
                  indicator = colDef(
                    name = "indicator",
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
                                                                   </div>`;}")),
                  
                  
                  
                  # Scotland column -------
                  scotland_value = colDef(
                    maxWidth = 80,
                    name = "Scotland",
                    cell = function(value){
                      div(style = "margin-top: 19px;", value)
                    }
                  ),
                  
                  # Chosen area column -------
                  measure = colDef(
                    maxWidth = 80,
                    name = as.character(selected_geo()$areaname),
                    cell = function(value){
                      div(style = "margin-top: 19px;", value)
                    }
                  ),
                  
                  
                  # spine chart column
                  chart = colDef(html = T,
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
            ")),
            # hidden columns that are used for other parts of the summary table (i.e. within other columns or for the spine chart )
                  worst = colDef(show = FALSE),
                  p25 = colDef(show = FALSE),
                  p75 = colDef(show = FALSE),
                  best = colDef(show = FALSE),
                  chosen_value = colDef(show = FALSE),
                  def_period = colDef(show = FALSE),
                  marker_colour = colDef(show = FALSE),
                  type_definition = colDef(show = FALSE),
                  unique_id = colDef(show = FALSE)
                ), defaultPageSize = nrow(summaryData()))
      
      
    })
    
    
    
    # download data module 
    download_data_btns_server("download_summary_data", summaryData())
    
    
    
    
    # download PDF logic
    output$download_summary_pdf <- downloadHandler(
      
      # name of file when downloaded
      filename = function() {
        paste(selected_profile(), "-summary-", selected_geo()$areaname, ".pdf", sep="")
      },
      
      content = function(file) {
        
        
        td <- tempdir() # create a temporary directory
        
        tempReport <- file.path(td, "spinecharts.Rmd") # rmarkdown file 
        tempLogo <- file.path(td, "scotpho_reduced.png") # scotpho logo to add to pdf
        
        file.copy("spinecharts.Rmd", tempReport, overwrite = TRUE) 
        file.copy("scotpho_reduced.png", tempLogo, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(reactive_df = summaryData(),
                       chosen_area = selected_geo()$areaname,
                       chosen_profile = selected_profile(),
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
    
    
    
    # info to display when user clicks help button (explains how to interpret the spine chart)
    observeEvent(input$help, {
      showModal(modalDialog(
        title = "How to interpret table results",
        tagList(
          paste0("The results below provide a snapshot of the latest data for the ", selected_profile() , " profile in ", selected_geo()$areaname, " compared to Scotland. 
        The spine charts show where ", selected_geo()$areaname, " fits in amongst the range of values (i.e. all other, ", selected_geo()$areatype, "s), as explained in 
        the key below."),
          br(),
          tags$img(src = "spinechart.PNG", style = "width:100%; height:auto;"),
          
          fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:orange; border-radius:50%; display:inline-block; margin:5px;"), "orange - statistically significantly better")),
          fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:blue; border-radius:50%; display:inline-block; margin:5px;"), "blue - statistically significantly worse")),
          fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:gray; border-radius:50%; display:inline-block; margin:5px;"), "grey - not statistically different to Scotland")),
          fluidRow(span(tags$div(style = "width:30px; height:30px; background-color:white; border-radius:50%; display:inline-block; margin:5px;"), "white - no difference to be calculated"))
          
        )
      ))
    })
 
       
  }) # close module server
} # close module








##############################################################################
# example usage - repex of how this module works can be generated by running 
# the functions declared above then uncommenting the code below.
##############################################################################
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
