#' Multi Trend Chart Module
#'
#' A Shiny module that creates a trend chart using **highcharter** that is updated
#' # using highcharter proxy functions
#'
#' @name multi_trend_chart_mod



#' Multi Trend Chart Module UI
#'
#' Creates a UI container for a multi line trend chart
#'
#' @param id Character string. 
#'
#' @return A `highchartOutput` element
#'
#' @examples
#' # In UI:
#' multi_trend_chart_mod_UI("chart")
#'

multi_trend_chart_mod_UI <- function(id) {
  ns <- NS(id)
  tagList(
    highchartOutput(ns("multi_trend_chart"))
  )
}

#' Multi rend Chart Module Server
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
#' @param y_col Character string. Name of the column used for the y-axis.
#' @param lowci_col,upci_col Character strings or `NULL`.Column names for the lower/upper CIs. Required if CIs enabled in `r_chart_controls`.
#' @param avg_col Character string or `NULL`. Column name for the average line. Required if average enabled in`r_chart_controls`.
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
#' multi_trend_chart_mod_Server(
#'   id = "simd_trend_chart",
#'   r_data = simd_data,
#'   x_col = "trend_axis"
#'   y_col = "measure",
#'   group_col = "quintile",
#'   r_chart_controls = simd_controls
#' )
#' 
#' 
#' 




multi_trend_chart_mod_Server <- function(id,
                                   r_data,
                                   x_col = "trend_axis",
                                   y_col = "measure",
                                   group_col,
                                   r_chart_controls,
                                   avg_col = NULL,
                                   lowci_col = NULL,
                                   upci_col = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      
      # reactive value to store id for each line
      curr_series_ids <- reactiveVal(character(0))
      

      # chart colours (to be expanded for non SIMD charts)
      palette <- list(
        "1 - most deprived" =  "#9B4393",
        "5 - least deprived" =  "#0078D4",
        "10 - least deprived" = "#0078D4",
        "2" = "#DFDDE3",
        "3" = "#DFDDE3",
        "4" = "#DFDDE3",
        "5" = "#DFDDE3",
        "6" = "#DFDDE3",
        "7" = "#DFDDE3",
        "8" = "#DFDDE3",
        "9" = "#DFDDE3"
      )
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # render chart once on initial load -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # Chart is created based on default selections when the app loads
      # wrapping in isolate() ensures this code doesn't run again -
      # all subsequent updates are done using highcharter proxy functions
      output$multi_trend_chart <- renderHighchart({
        
        isolate({
          
          df <- r_data() # data to plot
          groups <- unique(df[[group_col]]) # lines to plot
          
          
          # update reactive val with the names of groups being plotted
          # on initial load 
          curr_series_ids(groups)
          
          # create line series data 
          line_series_data <- map(groups, function(group){
            list(
              id = group,
              name = group,
              type = "line",
              color = palette[[group]],
              # list_parse2 is a highcarter function that formats data into a list
              data = list_parse2(df[df[[group_col]] == group, c(x_col, y_col)])
            )
          })

          # create line chart
          hc <- highchart() |>
            hc_add_series_list(line_series_data) |> # pass line series data to chart
            hc_xAxis(
              type = "category",
              # only plot first and last year as labels on x-axis
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
            hc_chart(marginRight = 15) |>
            hc_legend(enabled = TRUE) |>
            hc_add_theme(theme) # theme is from global script
          
          
          # add confidence intervals if switched on by default
          if("ci_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$ci_switch)){

              # create ci series data 
              ci_series_data <- map(groups, function(group){
                list(
                  id = paste0("ci-", group),
                  name = paste0("ci-", group),
                  type = "arearange",
                  color = palette[[group]],
                  showInLegend = FALSE,
                  fillOpacity = 0.2,
                  enableMouseTracking = FALSE, 
                  zIndex = -1, 
                  data = list_parse2(df[df[[group_col]] == group, c(x_col, upci_col, lowci_col)])
                )
              })
              
              # pass ci series data to the chart 
              hc <- hc |>
                hc_add_series_list(ci_series_data)
              
            }
          }
          
          # start y-axis at 0 if switched on by default
          if("zero_yaxis_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$zero_yaxis_switch)){
              hc <- hc|>
                hc_yAxis(min = 0)
            }
          }
          
          # add average line if switched on by default
          if("zero_yaxis_switch" %in% names(r_chart_controls)){
            if(isTRUE(r_chart_controls$avg_switch)){
              hc <- hc |>
                hc_add_series(
                  type = "line",
                  showInLegend = TRUE,
                  id = "avg",
                  name = "Average",
                  color = "red",
                  data = list_parse2(df[, c(x_col, avg_col)])
                )
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

          # data to be plotted
          df <- r_data()

          old_group_ids <- isolate(curr_series_ids()) # old lines that were plotted
          new_group_ids <- unique(df[[group_col]]) # new lines to be plotted


          # get chart that's already rendered
          hc <- highchartProxy(ns("multi_trend_chart"))
          
          
          # update xaxis
          hc <- hcpxy_update(hc, xAxis = list(categories = unique(df[[x_col]])), redraw = FALSE)


          # check what's to be added, removed or updated
          remove <- setdiff(old_group_ids, new_group_ids)
          add <- setdiff(new_group_ids, old_group_ids)
          update <- intersect(old_group_ids, new_group_ids)

          
          # remove lines (if applicable)
          if (length(remove) > 0) {
            hc <- hcpxy_remove_series(hc, id = remove)
          }
          
          
          # add lines (if applicable)
          if(length(add) > 0){
            
            walk(add, ~ {
              hc <- hcpxy_add_series(
               hc,
               id = .x,
               name = .x,
               type = "line",
               color = palette[[.x]],
               data = list_parse2(df[df[[group_col]] == .x, c(x_col, y_col)]),
               redraw = FALSE
              )
            })
            
            # add matching CI series (if applicable)
            if (isTRUE(r_chart_controls$ci_switch)){
              walk(add, ~ {
                hc <- hcpxy_add_series(
                  hc,
                  id = paste0("ci-",.x),
                  name = paste0("ci-",.x),
                  type = "arearange",
                  color = palette[[.x]],
                  showInLegend = FALSE,
                  fillOpacity = 0.2,
                  enableMouseTracking = FALSE, 
                  zIndex = -1, 
                  data = list_parse2(df[df[[group_col]] == .x, c(x_col, upci_col, lowci_col)]),
                  redraw = FALSE
                )
              }) 
            }
          }
          
          
          # update lines (if applicable)
          if(length(update) > 0){
            walk(update, ~ {
              hc <- hcpxy_update_series(
                hc, 
                id = .x, 
                data = list_parse2(df[df[[group_col]] == .x, c(x_col, y_col)]), 
                redraw = FALSE
                )
            })
          }
            
            # update matching CI series (if applicable)
            if (isTRUE(r_chart_controls$ci_switch)){
              walk(update, ~ {
                hc <- hcpxy_update_series(
                  hc, 
                  id = paste0("ci-", .x), 
                  data = list_parse2(df[df[[group_col]] == .x, c(x_col, upci_col, lowci_col)]), 
                  redraw = FALSE
                )
              })
            }
              
              
              # update average line (if applicable)
                if(isTRUE(r_chart_controls$avg_switch)){
                  hc <- hc |>
                    hcpxy_update_series(
                      id = "avg",
                      data = list_parse2(df[, c(x_col, avg_col)])
                    )
                }


          # update reactive val with series ids of each line plotted
          curr_series_ids(new_group_ids)



        # redraw once all chart updates done
        hc |>
          hcpxy_redraw()


      }, ignoreInit = TRUE)


      
      ## b. Updates in response to chart controls changing -----
      # i.e. adding/removing bits from the chart

      # starting y-axis at 0
      observeEvent(r_chart_controls$zero_yaxis_switch, {
        highchartProxy(ns("multi_trend_chart")) |>
          hcpxy_update(
            yAxis = list(min = if (isTRUE(r_chart_controls$zero_yaxis_switch)) 0 else NULL)
          )
      }, ignoreInit = TRUE)
      
      
      # adding/removing CI series
      observeEvent(r_chart_controls$ci_switch, {
        
        hc <- highchartProxy(ns("multi_trend_chart"))
        groups <- curr_series_ids()
        
        if (isTRUE(r_chart_controls$ci_switch)){
           walk(groups, ~ {
            hc <- hcpxy_add_series(
              hc,
              id = paste0("ci-",.x),
              name = paste0("ci-",.x),
              type = "arearange",
              color = palette[[.x]],
              linkedTo = .x,
              showInLegend = FALSE,
              fillOpacity = 0.2,
              enableMouseTracking = FALSE, 
              zIndex = -1, 
              showInLegend = FALSE,
              data = list_parse2(r_data()[r_data()[[group_col]] == .x, c(x_col, upci_col, lowci_col)]),
              redraw = FALSE
            )
          })
        } else {
          hc <- hcpxy_remove_series(hc, id = paste0("ci-", groups))
        }
           
          hc
          
      }, ignoreInit = TRUE)
      
      
      # adding/removing average line
      observeEvent(r_chart_controls$avg_switch, {

          if(isTRUE(r_chart_controls$avg_switch)){
            highchartProxy(ns("multi_trend_chart")) |>
              hcpxy_add_series(
                type = "line",
                showInLegend = TRUE,
                id = "avg",
                name = "Average",
                color = "red",
                data = list_parse2(r_data()[, c(x_col, avg_col)])
              )
          } else {
            highchartProxy(ns("multi_trend_chart")) |>
              hcpxy_remove_series(id = "avg")
          }
        
        
      }, ignoreInit = TRUE)


    }
  )
}