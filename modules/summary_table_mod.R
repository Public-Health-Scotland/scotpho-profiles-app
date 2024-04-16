### to do 
# - add in download data button - DONE 
# - add in download pdf button - DONE
# - decide how to present scotland data since spine chart doesn't make sense?
# - add in other comparators (larger job?)
# - add in help text
# - formatting of table 
# - add example shiny app


###############################################################################.
# MODULE: summary_table_mod ---- 
# prepares summary data for each profile and renders spine chart on each row of profile table
# note for this to work you need to add a link to the highchart js library in the UI script 





# ui function 
summary_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("download_summary_pdf"), "Download summary as PDF"), # download pdf button
    download_data_btns_ui(ns("download_summary_data")), # download data buttons
    withSpinner(reactableOutput(ns("summary_table"))) # summary table
)

}



# server function
summary_table_server <- function(id, selected_areaname, selected_areatype, selected_profile, filtered_data) {
  
  moduleServer(id, function(input, output, session) {
    

    summaryData <- reactive({
      
      # filter data by areatype and areaname 
      dt <- filtered_data() |>
        filter(areaname == selected_areaname() & areatype == selected_areatype())
      
      
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
                                 c("Q0", "Q100", "Q25", "Q75") := .(other_areas$Q0,
                                                                    other_areas$Q100,
                                                                    other_areas$Q25,
                                                                    other_areas$Q75)]
      
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
        mutate(one = case_when(interpret == "L" ~ Q0 - Q25, TRUE ~ Q100 - Q75), # worst
               two = case_when(interpret == "L" ~ Q75 - Q100, TRUE ~ Q25 - Q0), # 25th percentile
               three = case_when(interpret == "L" ~ Q25 - Q75, TRUE ~ Q75 - Q25), # 75th percentile
               four = case_when(interpret == "L" ~ Q100, TRUE ~ Q0) # best
        )
      
      
      final$chart <- NA # create empty column to populate with in-line highcharts
      
      # create id to use for the charts (each chart needs a unique id otherwise chart won't display)
      final <- final |> 
        mutate(unique_id = paste0("highchart-", indicator, "-", chosen_value, Sys.time()))
      
      
      
      # selecting columns required for table
      final <- final %>%
        select(domain,
               indicator,
               measure,
               scotland_value,
               type_definition,
               def_period,
               chart,
               one,
               two,
               three,
               four,
               chosen_value,
               marker_colour,
               unique_id)
      
      
      final
      
      
    })
    
    
    output$summary_table <- renderReactable({
      
      
      
      reactable(summaryData(),
                compact = TRUE,
                defaultExpanded = T,
                sortable = F,
                highlight = TRUE,
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
                    name = as.character(selected_areaname()),
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
                      name: 'One',
                      data: [{y: rowData.one, color: '#D3D3D3'}]
                    }, {
                      name: 'Two',
                    data: [{y: rowData.two, color: '#A4A4A4'}]
                    }, {
                      name: 'Three',
                      data: [{y: rowData.three, color: '#D3D3D3'}]
                    }, {
                      name: 'Four',
                      data: [{y: rowData.four, color: 'white'}]
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
                }, 20); // A brief delay to ensure the DOM elements are ready

                return chartHTML;
              }
            ")),
                  one = colDef(show = FALSE),
                  two = colDef(show = FALSE),
                  three = colDef(show = FALSE),
                  four = colDef(show = FALSE),
                  chosen_value = colDef(show = FALSE),
                  def_period = colDef(show = FALSE),
                  marker_colour = colDef(show = FALSE),
                  type_definition = colDef(show = FALSE),
                  unique_id = colDef(show = FALSE)
                ), defaultPageSize = nrow(summaryData()))
      
      
    })
    
     download_data_btns_server("download_summary_data", summaryData())
    
    output$download_summary_pdf <- downloadHandler(

      filename = function() {
        paste(selected_profile(), "-summary-", selected_areaname(), ".pdf", sep="")
      },

      content = function(file) {


        td <- tempdir()

        tempReport <- file.path(td, "spinecharts.Rmd")
        tempLogo <- file.path(td, "scotpho_reduced.png")

        file.copy("spinecharts.Rmd", tempReport, overwrite = TRUE)
        file.copy("scotpho_reduced.png", tempLogo, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(reactive_df = summaryData(),
                       chosen_area = selected_areaname(),
                       chosen_profile = selected_profile(),
                       chosen_geography_level = selected_areatype()
        )

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
    
  })
}


##############################################################################
# example usage - repex of how this module works can be generated by running 
# the functions declared above then uncommenting the code below.
##############################################################################

