###################.
# chart theme ----
##################.
theme <- hc_theme(
  chart = list(
    backgroundColor = "white"
  ),
  yAxis = list(
    gridLineWidth = 0
  ),
  xAxis = list(
    gridLineWidth = 0
  ),
  plotOptions = list(
    series = list(
      animation = FALSE,
      connectNulls=TRUE
    ),
    column = list(
      groupPadding = 0
    )
  )
)

############################.
# multi line chart ----
############################.
create_multi_line_trend_chart <- function(data, 
                                          xaxis_col = "trend_axis", 
                                          yaxis_col = "measure", 
                                          upci_col = "upci",
                                          lowci_col = "lowci",
                                          grouping_col,
                                          legend_position = c("bottom", "top"),
                                          reduce_xaxis_labels = FALSE,
                                          zero_yaxis = TRUE,
                                          include_confidence_intervals = FALSE,
                                          include_average = FALSE,
                                          chart_theme = theme,
                                          colour_palette = c("multi", "simd", "single")) {
  
  

  
  # create colour palette
  if(colour_palette == "multi"){
    colours <- head(unname(phs_colours()), length(unique(data[[grouping_col]])))
  } else if(colour_palette == "simd"){
    simd_colours <-  c("1 - most deprived" = "#0078D4",
                       "2" = "#DFDDE3",
                       "3" = "#DFDDE3",
                       "4" = "#DFDDE3",
                       "5 - least deprived" = "#9B4393")
    colours <- unname(simd_colours[names(simd_colours) %in% data[[grouping_col]]])
  } else {
    colours <- c(phs_colors("phs-blue-50"))
  }
  
  
  # create multi-line chart 
  hc <- hchart(
    data,
    "line", 
    hcaes(x = .data[[xaxis_col]], y = .data[[yaxis_col]], group = .data[[grouping_col]]), 
    marker = list(enabled = TRUE)) |>
    hc_xAxis(categories = unique(data[[xaxis_col]]), title = "") |>
    hc_colors(colours) |>
    hc_xAxis(title = "") |>
    hc_yAxis(title = "") |>
    hc_chart(marginRight = 15) |>
    hc_legend(verticalAlign = legend_position, align = "left") |>
    hc_tooltip(headerFormat = "", crosshairs = TRUE, shared = TRUE)
  
  
  
  # reduce axis labels
  if(reduce_xaxis_labels == TRUE){
    hc <- hc |>
      hc_xAxis(
        labels = list(
          rotation = 0, 
          style = list(
            whiteSpace = 'nowrap',
            textOverflow = 'none'),
          formatter = JS("function() {
               if (this.isFirst || this.isLast) {
                 return this.value;
               } else {
                 return '';
               }
             }")
        )
      )
  }
  
  
  
  
  # start y-axis at zero when checkbox ticked
  if(zero_yaxis == TRUE){
    hc <- hc |>
      hc_yAxis(min = 0)
  }
  
  # add confidence intervals when filter changes
  if(include_confidence_intervals == TRUE){
    hc <- hc |>
      hc_add_series(
        data,
        type = "arearange",
        name = "95% confidence interval",
        linked_to = ":previous",
        hcaes(x = .data[[xaxis_col]], low = .data[[lowci_col]], high = .data[[upci_col]], group = .data[[grouping_col]]),
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
  
  
  # add average line
  if(include_average == TRUE) {
    hc <- hc |>
    hc_add_series(
      data,
      "line",
      name = "Average",
      color = "#C73918",
      hcaes(x = trend_axis, y = total)
      )
  }
    
  
  # assign colour palette and theme 
  hc <- hc |>
    #hc_colors(colours) |>
    hc_add_theme(chart_theme)
  

  # return chart to display
  hc
  
  
}



############################.
# single line chart ----
############################.
create_single_line_trend_chart <- function(data, 
                                           xaxis_col = "trend_axis", 
                                           yaxis_col = "measure", 
                                           upci_col = "upci",
                                           lowci_col = "lowci",
                                           reduce_xaxis_labels = FALSE,
                                           include_confidence_intervals = FALSE,
                                           zero_yaxis = TRUE,
                                           chart_theme = theme) {
  
  
  
  
  # create line chart 
  hc <- hchart(
    data,
    "line", 
    hcaes(x = .data[[xaxis_col]], y = .data[[yaxis_col]]), 
    marker = list(enabled = TRUE)) |>
    hc_xAxis(title = "") |>
    hc_yAxis(title = "") |>
    hc_colors(c(phs_colors("phs-magenta"))) |>
    hc_chart(marginRight = 15) 
  
  
  # reduce axis labels
  if(reduce_xaxis_labels == TRUE){
    hc <- hc |>
      hc_xAxis(
        labels = list(
          rotation = 0, 
          style = list(
           # whiteSpace = 'nowrap',
            textOverflow = 'none'),
          formatter = JS("function() {
               if (this.isFirst || this.isLast) {
                 return this.value;
               } else {
                 return '';
               }
             }")
        )
      )
  }
  
  # add confidence intervals when filter changes
  if(include_confidence_intervals == TRUE){
    hc <- hc |>
      hc_add_series(
        data,
        type = "arearange",
        name = "95% confidence interval",
        linked_to = ":previous",
        hcaes(x = .data[[xaxis_col]], low = .data[[lowci_col]], high = .data[[upci_col]]),
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
  
  # start y-axis at zero when checkbox ticked
  if(zero_yaxis == TRUE){
    hc <- hc |>
      hc_yAxis(min = 0)
  }

  # add theme 
  hc <- hc |>
    hc_add_theme(chart_theme)
  
  
  # return chart to display
  hc
  
  
}


############################.
# bar chart chart ----
############################.
create_bar_chart <- function(data,
                             xaxis_col,
                             yaxis_col = "measure",
                             chart_theme = theme,
                             include_confidence_intervals = FALSE,
                             include_average = FALSE,
                             upci_col = "upci",
                             lowci_col = "lowci",
                             horizontal = TRUE,
                             colour_palette = c("simd", "single")){
  
  
  if(colour_palette == "simd"){
    
    # create simd colour palette
    simd_colours <- c("1 - most deprived" = "#0078D4",
                      "2" = "#DFDDE3",
                      "3" = "#DFDDE3",
                      "4" = "#DFDDE3",
                      "5 - least deprived" = "#9B4393")
    
    hc <- hchart(data, 
                 type = ifelse(horizontal == TRUE, "column", "bar"), 
                 hcaes(x = .data[[xaxis_col]], y = .data[[yaxis_col]]), colorByPoint = TRUE) |>
      hc_xAxis(title = list(text = "")) |>
      hc_yAxis(title = list(text = "")) |>
      hc_plotOptions(
        column = list(
          colors = unname(simd_colours[data$quintile])
        )
      )
    
  } else {
    
    hc <- hchart(data, 
                 type = ifelse(horizontal == TRUE, "column", "bar"), 
                 hcaes(x = .data[[xaxis_col]], y = .data[[yaxis_col]]), color = phs_colors("phs-blue-50")) |>
      hc_xAxis(title = list(text = "")) |>
      hc_yAxis(title = list(text = ""))
    
    
    
  }
  
  if(include_confidence_intervals == TRUE){
    hc <- hc |>
      hc_add_series(
        data, 
        type = "errorbar", 
        hcaes(x = .data[[xaxis_col]], low = .data[[lowci_col]], high = .data[[upci_col]]), 
        zIndex = 10,
        stemColor = "#000000", 
        whiskerColor = "#000000")
    
  }
  
  
  if(include_average == TRUE){
    hc <- hc |>
      hc_add_series(
        name = "Average",
        data = data$total,
        type = "line",
        color = "#C73918", #red colour for average line
        marker = list(enabled = FALSE),
        enableMouseTracking = FALSE) #turns off mouse tracking on average line only
    }

  hc <- hc |>
    hc_add_theme(chart_theme)
  
  hc 
  
}
