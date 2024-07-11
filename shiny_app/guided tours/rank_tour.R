###########################################
# Cicerone Guided Tour for Rank Tab 
###########################################

#rank tab
guide_rank<- Cicerone$
  new()$
  step(
    "rank_indicator_filter_wrapper", 
    "Indicator Filter",
    "First select an indicator. The list has been filtered based on profile and area type selected. The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
    position = "bottom"
  )$
  step(
    "rank_indicator_definition_wrapper",
    "Indicator Definition Button",
    "Click here to see the definition of the selected indicator.",
    position = "bottom"
  )$
  step(
    "rank_comparator_wrapper",
    "Select a Comparator",
    "Select a comparator which will allow you to see whether each area
    within your chosen geography level (e.g. health boards) is statistically significantly
    better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago).",
    position = "bottom"
  )$
  step(
    "rank_card_wrapper",
    "Chart Tab",
    "These charts allow ranking of each area for your selected indicator against other areas of the same type. You can also choose to add a baseline comparator, 
    to assess whether each area in your chosen geography level is statistically significantly better or worse than your comparator. For example, you may want to 
    assess whether each  is significantly higher or lower than a particular geographical area (for instance, the national average) or whether there are particular 
    areas in your chosen geography level that are significantly higher or lower than they were at another point in time (e.g. a decade ago)",
    position = "right"
  )$
  step(
    "rank_download_chart",
    "Download Chart Button",
    "Click here to download the chart with all selected geographies as a PNG.",
    position = "bottom"
  )$
  step(
    "rank_download_data",
    "Download Data Button",
    "Click here to download the selected data as a CSV, RDS or JSON file.",
    position = "left"
  )$
  step(
    "rank_map_wrapper",
    "Compare Areas Spatially",
    "This map allows for spatial comparison of areas across Scotland for a selected indicator. Darker shading of an area represents higher values for the selected indicator with lighter shading representing lower values. 
     Hover over an area of the map to view the name of the area and its value.
     Please note that the shading of an area is relative to other areas of the same type; therefore two areas of different shades may have similar absolute values for the indicator in the event that variability between areas is small.",
    position = "left"
  )
