#' Bar Chart Module
#'
#' A Shiny module that creates a bar chart using **highcharter** that is updated
#' # using highcharter proxy functions
#'
#' @name bar_chart_mod



#' Bar Chart Module UI
#'
#' Creates a UI container for a bar chart
#'
#' @param id Character string. 
#'
#' @return A `highchartOutput` element
#'
#' @examples
#' # In UI:
#' bar_chart_mod_UI("chart")
#'


bar_chart_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("bar_chart"))
  )
}



#' Bar Chart Module Server
#'
#' Server logic for rendering and updating a highcharter bar chart.
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
#' bar_chart_mod_Server(
#'   id = "bar_chart",
#'   r_data = simd_data,
#'   x_col = "quintiles"
#'   y_col = "measure",
#'   r_chart_controls = sii_controls
#' )


bar_chart_mod_Server <- function(id,
                             r_data,
                             r_chart_controls,
                             x_col,
                             y_col,
                             avg_col = NULL,
                             upci_col = NULL,
                             lowci_col = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      
      
      # colour palette
      simd_colours <- c("#0078D4", "#DFDDE3", "#DFDDE3", "#DFDDE3", "#9B4393")
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # render chart once on initial load -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Chart is created based on default selections when the app loads
      # wrapping in isolate() ensures this code doesn't run again -
      # all subsequent updates are done using highcharter proxy functions
      # note: each series must have it's own id for this to work!
      output$bar_chart <- renderHighchart({
        
        isolate({
          
          # data to plot
          df <- r_data()
          
          # create bar chart 
          hc <- highchart() |>
            hc_add_series(
              id = "bar_series", 
              type = "column",
              data = purrr::map2(df[[y_col]], simd_colours, ~ list(y = .x, color = .y))
            ) |>
            hc_xAxis(categories = unique(df[[x_col]])) |>
            hc_add_theme(theme) |>
            hc_legend(enabled = FALSE)
          
          # add confidence intervals if switched on by default
          if("ci_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$ci_switch)){
              
              hc <- hc |> 
                hc_add_series(
                  id = "ci_series",
                  type = "errorbar",
                  data = purrr::map2(df[[lowci_col]], df[[upci_col]], ~ c(.x, .y)),
                  zIndex = 10, stemColor = "#000000", whiskerColor = "#000000"
                )
              
            }
          }
          
          # start y-axis at 0 if switched on by default
          if("zero_yaxis_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$zero_yaxis_switch)){
              hc_yAxis(hc, min = 0)
            }
          }
          
          # add average line if switched on by default
          if("avg_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$avg_switch)){
              hc <- hc |>
                hc_add_series(
                  id = "avg_series",
                  type = "line",
                  data = r_data()[[avg_col]]
                )
            }
          }
          
          # remove animation
          hc |>
            hc_plotOptions(series = list(animation = FALSE)) |>
            hc_chart(animation = FALSE)
          
          
        })
      })
      
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # update chart when r_data is updated ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # whenever the reactive dataset is updated, update the chart.
      # Using proxy functions allows you to manipulate the chart 
      # without having to completely re-render it
      

      observeEvent(r_data(), {
        
        # get the already rendered bar chart 
        hc <- highchartProxy(ns("bar_chart"))
        
        # data to plot 
        df <- r_data()
        
        # update the x-axis
        hcpxy_update(hc, xAxis = list(categories = unique(df[[x_col]])), redraw = FALSE)
        
        # update the bars 
        hcpxy_update_series(hc, id = "bar_series", data = df[[y_col]], redraw = FALSE)
        
        # update the average line (if avg switch is on)
        if("avg_switch" %in% names(r_chart_controls)){
          if(isTRUE(r_chart_controls$avg_switch)){
            hcpxy_update_series(hc, id = "avg_series", data = df[[avg_col]], redraw = FALSE)
          }
        }
        
        # update ci_switch series (if ci_switch is on)
        if("ci_switch" %in% names(r_chart_controls)){
          if(isTRUE(r_chart_controls$ci_switch)){
            
            hcpxy_update_series(
              hc, 
              id = "ci_series",
              data = purrr::map2(df[[lowci_col]], df[[upci_col]], ~ c(.x, .y)), redraw = FALSE)
            
          }
        }
        
        # redraw once all updates done
        hcpxy_redraw(hc)
        
        
      }, ignoreInit = TRUE)
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Adding/removes series in response to chart control switches ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # adding/removing ci series
      observeEvent(r_chart_controls$ci_switch, {
        
        # get already rendered bar chart 
        hc <- highchartProxy(ns("bar_chart"))
        
        # add/remove series
        if(isTRUE(r_chart_controls$ci_switch)){
          hcpxy_add_series(
            hc,
            id = "ci_series",
            type = "errorbar",
            data = purrr::map2(r_data()[[lowci_col]], r_data()[[upci_col]], ~ c(.x, .y))
          )
        } else {
          hcpxy_remove_series(hc, id = "ci_series")
        }
        
      }, ignoreInit = TRUE)
      
      
      # adding/removing average line
      observeEvent(r_chart_controls$avg_switch, {
        
        # get already rendered bar chart
        hc <- highchartProxy(ns("bar_chart"))
        
        
        # add/remove series
        if(isTRUE(r_chart_controls$avg_switch)){
          hcpxy_add_series(
            hc,
            data = r_data()[[avg_col]],
            id = "avg_series",
            type = "line"
          )
        } else {
          hcpxy_remove_series(hc, id = "avg_series")
        }
        
      }, ignoreInit = TRUE)
      
      # starting y-axis at 0
      observeEvent(input$r_chart_controls$zero_yaxis_switch, {
        highchartProxy(ns("bar_chart")) |>
          hcpxy_update(
            yAxis = list(min = if (isTRUE(r_chart_controls$zero_yaxis_switch)) 0 else NULL)
          )
      }, ignoreInit = TRUE)
      
    }
  )
}


