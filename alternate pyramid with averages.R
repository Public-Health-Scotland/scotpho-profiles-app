pp_axis_min <- max(abs(demographic_dataset$percentage_Male))



max_val <- round(max(abs(demographic_dataset$percentage_Male),demographic_dataset$percentage_Female),0)


library(highcharter)
library(dplyr)

# 1. Prepare Dummy Data
categories <- c("0-14", "15-24", "25-44", "45-64", "65-84", "85+")
df <- data.frame(
  age = categories,
  Male = c(-100, -80, -60, -40, -20, -5), # Negative for left side
  Female = c(95, 78, 62, 42, 25, 10)
)

# 2. Create the Chart
hchart(df, "bar", hcaes(x = age, y = Male), name = "Male", color = "#4572A7") %>%
  hc_add_series(df, "bar", hcaes(x = age, y = Female), name = "Female", color = "#AA4643") %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Population Pyramid Example") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>% # Essential for stacking
  hc_xAxis(categories = df$age, reversed = TRUE) %>%
  hc_yAxis(
    title = list(text = "Population (Thousands)"),
    labels = list(
      formatter = JS("function(){ return Math.abs(this.value); }") # Absolute values
    )
  ) %>%
  hc_tooltip(
    formatter = JS("function(){
      return '<b>' + this.series.name + ', age ' + this.point.category + '</b><br/>' +
      'Population: ' + Highcharts.numberFormat(Math.abs(this.point.y), 0);
    }")
  )



library(highcharter)
library(dplyr)

# 1. Create Example Data
age_cats <- c('0-14', '15-24', '25-44', '45-64', '65+')

data <- data.frame(
  age = factor(age_cats, levels = age_cats),
  local_m = c(-50, -80, -120, -90, -40),    # Negative for left side
  local_f = c(48, 78, 125, 95, 45),
  nat_m   = c(-55, -75, -110, -85, -50),    # National Avg
  nat_f   = c(53, 73, 115, 88, 55),
  sub_m   = c(-45, -70, -100, -80, -35),    # Sub-national Avg
  sub_f   = c(43, 68, 105, 82, 40)
)

# 2. Build the Base Pyramid
highchart() %>%
  hc_chart(type = "bar") %>%
  hc_plotOptions(series = list(stacking = "normal")) %>%
  hc_xAxis(categories = age_cats) %>%
  hc_yAxis(labels = list(formatter = JS("function() { return Math.abs(this.value); }"))) %>%
  
  # Local Population (Bars)
  hc_add_series(data = data$local_m, name = "Local Male", color = "#3288bd") %>%
  hc_add_series(data = data$local_f, name = "Local Female", color = "#d53e4f") %>%
  
  # National Average Overlay (Solid Lines)
  hc_add_series(data = data$nat_m, type = "line", name = "National Avg", 
                color = "black", marker = list(enabled = FALSE)) %>%
  hc_add_series(data = data$nat_f, type = "line", name = "National Avg", 
                color = "black", marker = list(enabled = FALSE), linkedTo = ":previous") %>%
  
  # Sub-national Average Overlay (Dashed Lines)
  hc_add_series(data = data$sub_m, type = "line", name = "Sub-national Avg", 
                color = "#666", dashStyle = "Dash", marker = list(enabled = FALSE)) %>%
  hc_add_series(data = data$sub_f, type = "line", name = "Sub-national Avg", 
                color = "#666", dashStyle = "Dash", marker = list(enabled = FALSE), linkedTo = ":previous") %>%
  
  # Tooltip configuration to show absolute values
  hc_tooltip(shared = TRUE, formatter = JS("function() {
    var s = '<b>Age ' + this.x + '</b>';
    $.each(this.points, function(i, point) {
      s += '<br/>' + point.series.name + ': ' + Math.abs(point.y);
    });
    return s;
  }"))
