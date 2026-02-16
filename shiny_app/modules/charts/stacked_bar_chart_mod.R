#' Stacked bar Chart Module
#'
#' A Shiny module that creates a stacked bar chart using **highcharter** that is updated
#' # using highcharter proxy functions
#'
#' @name stacked_bar_chart_mod



#' Stacked Bar chart Module UI
#'
#' Creates a UI container for a stacked bar chart
#'
#' @param id Character string. 
#'
#' @return A `highchartOutput` element
#'
#' @examples
#' # In UI:
#' stacked_bar_chart_mod_UI("chart")
#'


stacked_bar_chart_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
      highchartOutput(ns("stacked_bar")) 
  )

}



#' Stacked bar Chart Module Server
#'
#' Server logic for rendering and updating a highcharter stacked bar chart.
#'
#' The chart is rendered once on initial load. All subsequent updates 
#' (changes in the dataset or chart control options) are done using
#' **highcharter proxy functions**, without re-rendering the entire chart.
#'
#' @param id Character string. The module's namespace ID (should match the UI function).
#'
#' @param r_data A **reactive** dataset to be plotted.
#'   The dataset must contain the columns specified in `x_col`, `y_col`
#'
#' @param x_col Character string. Name of the column used for the x-axis. 
#' @param y_col Character string. Name of the column used for the y-axis. Must be one of: `"measure"`, `"rii_int"`, `"sii"`, `"par"`.
#' @param group_col Character string. Name of the categories for the bar stacks. 
#'
#' @return None. This server function is called for its side-effects
#'   (it renders and updates a highchart in the UI).
#'
#' @examples
#' # In server:
#' stacked_bar_chart_mod_Server(
#'   id = "chart",
#'   r_data = simd_data,
#'   x_col = "quintile"
#'   group_col = "measure_type",
#'   y_col = "value"
#' )

stacked_bar_chart_mod_Server <- function(id,
                                         r_data,
                                         x_col,
                                         y_col,
                                         group_col) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      # reactive value to store id for each stack
      curr_series_ids <- reactiveVal(character(0))
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # render chart once on initial load -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Chart is created based on default selections when the app loads
      # wrapping in isolate() ensures this code doesn't run again -
      # all subsequent updates are done using highcharter proxy functions
      # note: each series must have it's own id for this to work!
      output$stacked_bar <- renderHighchart({
        isolate({
      
      df  <- r_data() # data to plot 
      ids <- unique(df[[group_col]]) # stacks to plot 
      
      # update reactive value with id of stacks 
      # plotted on initial load
      curr_series_ids(ids)
      
      
      # create series data
      # For each group do the following:
      series_data <- map(ids, ~ {
        
        # a. get data for that particular group:
        bar_data <- df[df[[group_col]] == .x, ]
        
        # b. turn group data into list
        list(
          id = .x,
          name = .x,
          type = "column",
          data = list_parse2(bar_data[, c(x_col, y_col)])
        )
      })
      
      # create highchart
      hc <- highchart() |>
        hc_add_series_list(series_data) |>
        hc_plotOptions(series = list(stacking = "normal")) |>
        hc_xAxis(categories = unique(df[[x_col]])) |>
        hc_colors(c(phs_colors("phs-blue"), phs_colors("phs-purple"))) |>
        hc_add_theme(theme)
      
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
    
    # data to be plotted
    df <- r_data()
    
    old_group_ids <- isolate(curr_series_ids()) # old stacks that were plotted
    new_group_ids <- unique(df[[group_col]]) # new stacks to be plotted
    
    
    # get chart that's already rendered
    hc <- highchartProxy(ns("stacked_bar"))
    
    # check if there are any stacks to be removed
    # and if there are, remove them.
    groups_to_remove <- setdiff(old_group_ids, new_group_ids)
    
    if (length(groups_to_remove) > 0) {
      hcpxy_remove_series(hc, id = groups_to_remove)
    }
    
    
    # check if there are any stacks to be either updated
    # or added and if there are, update/add them.
    walk(new_group_ids, ~ {
      
      # get data for particular group:
      bar_data <- df[df[[group_col]] == .x, ]
      
      # turn group data into list
      bar_data <- list_parse2(bar_data[, c(x_col, y_col)])
      
      # if series id existed previously, update, otherwise add as new series
      if (.x %in% old_group_ids) {
        hcpxy_update_series(hc, id = .x, name = .x, data = bar_data, redraw = FALSE)
      } else {
        hcpxy_add_series(hc, id = .x, name = .x, data = bar_data, type = "column", redraw = FALSE)
      }
    })
    
    # update reactive val with series ids of new plotted stacks
    curr_series_ids(new_group_ids)

    
    # redraw chart once all updates done
    hc |> hcpxy_redraw()
    
  }, ignoreInit = TRUE)

  
    }
  )
}

