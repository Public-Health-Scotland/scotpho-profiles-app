#' Trend Chart Module
#'
#' A Shiny module that creates a trend chart using **highcharter** that is updated
#' # using highcharter proxy functions
#'
#' @name trend_chart_mod



#' Trend Chart Module UI
#'
#' Creates a UI container for a bar chart
#'
#' @param id Character string. 
#'
#' @return A `highchartOutput` element
#'
#' @examples
#' # In UI:
#' trend_chart_mod_UI("chart")
#'

trend_chart_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("trend_chart"))
  )
}

#' Trend Chart Module Server
#'
#' Server logic for rendering and updating a highcharter trend chart.
#'
#' The chart is rendered once on initial load. All subsequent updates 
#' (changes in the dataset or chart control options) are done using
#' **highcharter proxy functions**, without re-rendering the entire chart.
#'
#' @param id Character string. The module's namespace ID (should match the UI function).
#'
#' @param r_data A **reactive** dataset to be plotted.
#'   The dataset must contain the columns specified in `x_col`, `y_col`,
#'   and optionally `lowci_col` / `upci_col`.
#'
#' @param x_col Character string. Name of the column used for the x-axis. 
#' @param y_col Character string. Name of the column used for the y-axis. Must be one of: `"measure"`, `"rii_int"`, `"sii"`, `"par"`.
#' @param lowci_col,upci_col Character strings or `NULL`.Column names for the lower/upper CIs. Required if CIs enabled in `r_chart_controls`.
#'@param avg_col Character string or `NULL`. Column name for the average line. Required if average enabled in`r_chart_controls`.
#'
#'
#' @param r_chart_controls A **reactiveValues** object containing chart control options, e.g.:
#'   - `ci_switch` (logical): add/remove confidence intervals
#'   - `zero_yaxis_switch` (logical): force y-axis to start at zero
#'   - `avg_switch` (logical): add/remove average line
#'
#' @return None. This server function is called for its side-effects
#'   (it renders and updates a highchart in the UI).
#'
#' @examples
#' # In server:
#' trend_chart_mod_Server(
#'   id = "bar_chart",
#'   r_data = simd_data,
#'   x_col = "quintiles"
#'   y_col = "measure",
#'   r_chart_controls = sii_controls
#' )



trend_chart_mod_Server <- function(id,
                               r_data,
                               x_col = "trend_axis",
                               y_col = c("measure", "rii_int", "sii", "par"),
                               r_chart_controls,
                               lowci_col = NULL,
                               upci_col = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # render chart once on initial load -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Chart is created based on default selections when the app loads
      # wrapping in isolate() ensures this code doesn't run again -
      # all subsequent updates are done using highcharter proxy functions
      output$trend_chart <- renderHighchart({
        
        isolate({
          
          # data to plot
          df <- r_data()
          
          # create line chart
          hc <- highchart() |>
            hc_add_series(id = "line_series", type = "line", name = y_col, data = df[[y_col]]) |>
            hc_xAxis(
              categories = unique(df[[x_col]]),
              # only plot first and last year
              labels = list(
                rotation = 0, 
                style = list(
                  textOverflow = 'none'
                  ),
                formatter = JS("function() {
               if (this.isFirst || this.isLast) {
                 return this.value;
               } else {
                 return '';
               }
             }")
              )) |>
            hc_colors(c(phsstyles::phs_colors("phs-magenta"))) |>
            hc_chart(marginRight = 15) |>
            hc_legend(enabled = FALSE) |>
            hc_add_theme(theme) # theme is from global script
          
          
          # add confidence intervals if switched on by default
          if("ci_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$ci_switch)){
              
              hc <- hc |>
                hc_add_series(
                  id = "ci_series",
                  linked_to = "line_series",
                  data = purrr::map2(r_data()[[lowci_col]], r_data()[[upci_col]], ~ c(.x, .y)),
                  type = "arearange",
                  fillOpacity = 0.2,
                  enableMouseTracking = FALSE, 
                  showInLegend = FALSE,
                  zIndex = -1, 
                  marker = list(
                    enabled = FALSE,
                    states = list(
                      hover = list(
                        enabled = FALSE
                      )
                    )
                  )
                  
                )
              
            }
          }
          
          # start y-axis at 0 if switched on by default
          if("zero_yaxis_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$zero_yaxis_switch)){
              hc <- hc|>
                hc_yAxis(min = 0)
            }
          }
          
          
          # remove animation
          hc <- hc |>
            hc_plotOptions(series = list(animation = FALSE)) |>
            hc_chart(animation = FALSE)
          
          hc
          
          
        })
      })
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Update chart using proxy functions -------------
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # highcharter proxy functions let you update an already built chart without re-rendering
      # from scratch - faster way update a chart in response to user input: 
      # https://jkunst.com/highcharter/reference/index.html#shiny-and-proxy-functions
      
      
      
      ## a. Updates in response to r_data changing -----
      # i.e. updating the data that is plotted
      
      # whenever the reactive dataset is updated, update the chart:
      observeEvent(r_data(), {
        
        # update the x-axis categories
        hc <- highchartProxy(ns("trend_chart")) |>
          hcpxy_update(xAxis = list(categories = unique(r_data()[[x_col]])), redraw = FALSE)
        
        # update the y-axis values
        hc <- hc|>
          hcpxy_update_series(id = "line_series", data = r_data()[[y_col]], redraw = FALSE)
        
        
        # update ci_switch series (if ci_switch is on)
        if("ci_switch" %in% names(r_chart_controls)){
          if(isTRUE(r_chart_controls$ci_switch)){
            
            hc <- hc |>
              hcpxy_update_series(
                id = "ci_series",
                data = purrr::map2(r_data()[[lowci_col]], r_data()[[upci_col]], ~ c(.x, .y)), redraw = FALSE)
            
          }
        }
        
        # redraw once all chart updates done
        hcpxy_redraw(hc)
        
        
      }, ignoreInit = TRUE)
      
      
      
      ## b. Updates in response to r_chart_controls changing -----
      # i.e. adding/removing parts of the chart
      
      
      # adding/removing ci series
      observeEvent(r_chart_controls$ci_switch, {
        
        hc <- highchartProxy(ns("trend_chart"))
        
        if(isTRUE(r_chart_controls$ci_switch)){
          
          hcpxy_add_series(
            hc,
            id = "ci_series",
            linked_to = "line_series",
            data = purrr::map2(r_data()[[lowci_col]], r_data()[[upci_col]], ~ c(.x, .y)),
            type = "arearange",
            fillOpacity = 0.2,
            enableMouseTracking = FALSE, 
            showInLegend = FALSE,
            zIndex = -1, 
            marker = list(
              enabled = FALSE,
              states = list(
                hover = list(
                  enabled = FALSE
                )
              )
            )
          )
        } else {
          hcpxy_remove_series(hc, id = "ci_series")
        }
        
      }, ignoreInit = TRUE)
      
      
      
      # starting y-axis at 0
      observeEvent(r_chart_controls$zero_yaxis_switch, {
        highchartProxy(ns("trend_chart")) |>
          hcpxy_update(
            yAxis = list(min = if (isTRUE(r_chart_controls$zero_yaxis_switch)) 0 else NULL)
          )
      }, ignoreInit = TRUE)
      
      
    }
  )
}