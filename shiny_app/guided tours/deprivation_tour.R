###########################################
# Cicerone Guided Tour for Deprivation Tab 
###########################################

guide_deprivation <- Cicerone$
  new()$
  step(
    "deprivation_barchart_wrapper",
    "Bar Chart",
    "This chart shows how the selected measure varies according to the relative deprivation of an area of residence, 
     It allows comparison of the most and least deprived areas.",
    position = "left"
  )$
  step(
    "deprivation_trendchart_wrapper",
    "Line Chart",
    "This chart shows the value of a measure by deprivation quintile over time. 
     It allows comparison of the most and least deprived areas over time.",
    position = "left"
  )$
  step(
    "deprivation_indicator_filter_wrapper", 
    "Indicator Filter",
    "First select an indicator.<br>
    The indicator list has been filtered based on profile and area type selected at the top of the page.<br>
    The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
    position = "right"
  )$
  step(
    "deprivation_avg_switch_wrapper",
    "Average Line",
    "Click to add or remove a trend line showing the average for the measure across all quintiles.",
    position = "right"
  )$
  step(
    "deprivation_quintile_type_wrapper",
    "Quintile Type",
    "When an area other than Scotland is selected, click here to toggle between local and Scottish quintiles.",
    position = "right"
  )$
  step(
    "deprivation_download_chart",
    "Download Chart Button",
    "Click here to download this chart as a PNG.",
    position = "above"
  )$
  step(
    "deprivation_download_data",
    "Download Data Button",
    "Click here to download the selected data as a CSV, RDS or JSON file.",
    position = "above")
#decide what to do for duplicate trend and chart download buttons