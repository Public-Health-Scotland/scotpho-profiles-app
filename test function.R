# pyramid <- highchart() %>%
#   hc_chart(object = data,type = "bar") %>%
#   hc_plotOptions(series = list(stacking = "normal",
#                                grouping = FALSE,
#                                pointPadding = 0, # Smaller value = fatter bars
#                                groupPadding = 0)) |>  # Smaller value = fatter bars
#   hc_xAxis(categories = data$age, 
#            title = list(text = "Age Group (years)"),
#            reversed=FALSE) %>% #reversing axis means that lower ages at the bottom rather than top
#   
#   # Add title
#   hc_title(text = "Population Pyramid") |>
#   hc_subtitle(text = "a subtitle - link to nrs?") |>
#   
#   # Add Series (mapping additional population and year columns which appear in tooltip alongside the % of population)
#   hc_add_series(name = "Male", data = data,type = "bar", hcaes(x = age, y = percentage_Male, pop_value = population_Male, year=year)) %>%
#   hc_add_series(name = "Female", data = data,type = "bar", hcaes(x = age, y = percentage_Female, pop_value = population_Female, year=year)) %>%
#   
#   # Tooltip
#   hc_tooltip(
#     shared = TRUE, # set to true to ensure both male and female values appear for each age category
#     formatter = JS("function() {
#       var s = '<b>Age: ' + this.x + '</b>';
#       $.each(this.points, function(i, point) {
#         var absVal = Math.abs(point.y);
#         s += '<br/>' + this.point.year + ' ' +
#         point.series.name + ' Population: '+  Highcharts.numberFormat(this.point.pop_value,0,',') +
#         ': ('+ Highcharts.numberFormat(absVal, 1) + '%)';
#       });
#       return s;
#     }")
#   ) %>%
#   
#   # Format Y-Axis (% population)
#   hc_yAxis(
#     min = -5,           # Fixed start
#     max = 5,            # Fixed end (must match min to be centered)
#     tickInterval = 1,   # Distance between labels
#     labels = list(formatter = JS("function() { return Math.abs(this.value); }")), #ensure axis labels show absolute values not negatives for the males
#     title = list(text = "Percentage of Population")
#   ) 
# 
# pyramid


create_pyramid_chart <- function(data){
  
  hc <- hchart(data,
              type = "bar") %>%
    hc_plotOptions(series = list(stacking = "normal",
                                 grouping = FALSE,
                                 pointPadding = 0, # Smaller value = fatter bars
                                 groupPadding = 0)) |>  # Smaller value = fatter bars
    hc_xAxis(categories = data$age, 
             title = list(text = "Age Group (years)"),
             reversed=FALSE) %>% #reversing axis means that lower ages at the bottom rather than top
    
    # Add title
    hc_title(text = "Population Pyramid") |>
    hc_subtitle(text = "a subtitle - link to nrs?") |>
    
    # Add Series (mapping additional population and year columns which appear in tooltip alongside the % of population)
    hc_add_series(name = "Male", data = data,type = "bar", hcaes(x = age, y = percentage_Male, pop_value = population_Male, year=year)) %>%
    hc_add_series(name = "Female", data = data,type = "bar", hcaes(x = age, y = percentage_Female, pop_value = population_Female, year=year)) %>%
    
    # Tooltip
    hc_tooltip(
      shared = TRUE, # set to true to ensure both male and female values appear for each age category
      formatter = JS("function() {
      var s = '<b>Age: ' + this.x + '</b>';
      $.each(this.points, function(i, point) {
        var absVal = Math.abs(point.y);
        s += '<br/>' + this.point.year + ' ' +
        point.series.name + ' Population: '+  Highcharts.numberFormat(this.point.pop_value,0,',') +
        ': ('+ Highcharts.numberFormat(absVal, 1) + '%)';
      });
      return s;
    }")
    ) %>%
    
    # Format Y-Axis (% population)
    hc_yAxis(
      min = -5,           # Fixed start
      max = 5,            # Fixed end (must match min to be centered)
      tickInterval = 1,   # Distance between labels
      labels = list(formatter = JS("function() { return Math.abs(this.value); }")), #ensure axis labels show absolute values not negatives for the males
      title = list(text = "Percentage of Population")
    ) 
  
  #return chart
  hc
  
  
}




pop_simd_centile<- demographic_simd_dataset |>
  filter(areatype == "Scotland" & areaname =="Scotland")|>
  filter(year==2023) |>
  filter(simd_domain=="overall") |>
  filter(centile_type=="quintile")











