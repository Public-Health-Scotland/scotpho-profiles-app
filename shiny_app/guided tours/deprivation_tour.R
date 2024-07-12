###########################################
# Cicerone Guided Tour for Deprivation Tab 
###########################################

guide_deprivation <- Cicerone$
  new()$
  step(
    "deprivation_barchart_wrapper",
    "Bar Chart",
    "This chart shoes how the selected measure varies according to the relative deprivation of an area of residence. 
     It shows how the most and least deprived areas compare.",
    position = "below"
  )$
  step(
    "deprivation_trendchart_wrapper",
    "Line Chart",
    "This chart shows the value of a measure by deprivation quintile over time. 
    It shows how the most and least deprived areas compare over time.",
    position = "below"
  )$
  step(
    "deprivation_indicator_filter_wrapper", 
    "Indicator Filter",
    "First select an indicator.<br>
    The indicator list has been filtered based on profile and area type selected at the top of the page.<br>
    The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
    position = "bottom"
  )$
 #Add sidebar tickbox averages step here
 #Add quintile type step here
  step(
    "deprivation_download_chart",
    "Download Chart Button",
    "Click here to download this chart as a PNG.",
    position = "bottom"
  )$
  step(
    "deprivation_download_data",
    "Download Data Button",
    "Click here to download the selected data as a CSV, RDS or JSON file.",
    position = "left")
#decide what to do for duplicate trend and chart download buttons