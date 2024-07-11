###########################################
# Cicerone Guided Tour for Trend Tab 
###########################################

guide_trend <- Cicerone$
  new(
    padding = 8
  )$
  step(
    "trend_indicator_filter_wrapper", # id of div wrapper - specified in trend module rather than indicator filter module
    "Indicator Filter",
    "First select an indicator. The list has been filtered based on profile and area type selected. The backspace can be used to remove the default selection. Indicators can then be searched by topic or name.",
    position = "bottom"
  )$
  step(
    "trend_indicator_definition_wrapper",
    "Indicator Definition Button",
    "Click here to see the definition of the selected indicator.",
    position = "bottom"
  )$
  step(
    "trend_geography_wrapper",
    "Geography Filters",
    "Next add one or more geographical areas of any type to the chart to compare with your selected geography. There may be some indicators for which data is not available for the full time series or at a particular geography level.
     If an area type other than Scotland is selected in the global options, the Scotland checkbox can be clicked to add or remove the trend line.",
    position = "right"
  )$
  step(
    "trend_card_wrapper",
    "Chart Tab",
    "The trend chart is designed to explore how a single indicator has changed over time for one or more geograpical area. Use the mouse to hover over a data point to see detailed information on its value, time period and area.
     The tabs at the top can be used to navigate between different views of the data and further information to aid interpretation. The data tab shows the figures underlying the chart and the notes and caveats tab contains further information about methodology and any data quality issues.",
    position = "left"
  )$
  step(
    "trend_table_wrapper",
    "Data Tab",
    "Insert text here",
    position = "left",
    tab = "trend_table_panel",
    tab_id = "trend_navset_card_pill"
  )$
  step(
    "trend_download_chart",
    "Download Chart Button",
    "Click here to download the chart with all selected geographies as a PNG.",
    position = "bottom"
  )$
  step(
    "trend_download_data",
    "Download Data Button",
    "Click here to download the selected data as a CSV, RDS or JSON file.",
    position = "left"
  )$
  step(
    "trend_popovers",
    "Adjust Chart Settings",
    "Click here to see chart settings. Confidence intervals (95%) can be added to the chart. They are shown as shaded areas and give an indication of the precision of a rate or percentage. The width of a confidence interval is related to sample size.
    The chart can also be switched from a measure (e.g. rate or percentage) to actual numbers (e.g. the number of births with a healthy birthweight)."
  )
