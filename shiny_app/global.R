###############################################################################.
#
# Global script ---- 
#
###############################################################################.


# 1. Required packages ----------------------------------------------------------
library(shiny) # for shiny functions
library(bslib) # app layout functions/theming
library(phsstyles) # for phs colour palette
library(shinyjs) # for various functions to expand/collapse geography filters 
library(htmltools) # for landing page template to read
library(purrr) # needed for sourcing modules with map
library(arrow) # for reading parquet files
library(reactable) # data tables
library(highcharter) # visualisations
library(data.table) # faster data wrangling
library(dplyr) # data wrangling
library(htmlwidgets) # for download buttons
library(shinycssloaders) # for spinners when ui loading
library(jsonlite) # for download data in json format/reading in .json shapefiles
library(reactable) # for data tables
library(leaflet) # for map
library(sf) # note: eventually remove this from here
library(jsTreeR) # for data tab geography filters
library(shinyWidgets)
library(bsicons) # for icons
library(cicerone) #for guided tours of tabs

library(readr) #im additiona will remove in future


# 2. Sourcing modules and narrative text -------------------------------------------
list.files("modules", full.names = TRUE, recursive = TRUE) |>
  map(~ source(.))

list.files("narrative", full.names = TRUE, recursive = TRUE) |>
  map(~ source(.))


# 3. Required datafiles ------------------------------------------------------------
main_dataset <- read_parquet("data/optdata") # main dataset (to do: rename optdata file in data prep script)
geo_lookup <- readRDS("data/geo_lookup.rds") # geography lookup
geo_lookup <- setDT(geo_lookup) 

main_data_geo_nodes <- readRDS("data/optdata_geography_nodes.rds") # geography nodes for data table tab

simd_dataset <- read_parquet("data/deprivation_data") # dataset behind simd panel

techdoc <- read_parquet("data/techdoc") # technical document

#temp data upload and simple wrangle
ineq_splits_data <- readr::read_csv("/PHI_conf/ScotPHO/Profiles/Data/Test Shiny Data/88007_meeting_mvpa_im.csv") |>
  rename(areatype = geography,
         areaname = location_name) |>
  mutate(areatype = case_when(areatype == "healthboard" ~ "Health board",
                              areatype == "scotland" ~ "Scotland",
                              areatype == "council" ~ "Council area", TRUE ~ areatype)) |>
  mutate(areaname = case_when(areaname == "scotland" ~ "Scotland", TRUE ~ areaname)) |>
  mutate(indicator = "Meets recommendations")|>
  filter(split_name!="simd")

# shapefiles (for map) 
ca_bound <- readRDS("data/CA_boundary.rds") # Council area
hb_bound <- readRDS("data/HB_boundary.rds") # Health board
hscp_bound <- readRDS("data/HSCP_boundary.rds")# HSC Partnerships
hscloc_bound <- readRDS("data/HSC_locality_boundary.rds") # HSC localities
iz_bound <- readRDS("data/IZ_boundary.rds") # Intermediate zone
# transform so in right format to join to main dataset 
# this should maybe  be done in data prep instead so don't need to load sf package into the app - just leaflet?)
ca_bound <- sf::st_as_sf(ca_bound)
hb_bound <- sf::st_as_sf(hb_bound)
hscp_bound <- sf::st_as_sf(hscp_bound)
hscloc_bound <- sf::st_as_sf(hscloc_bound)
iz_bound <- sf::st_as_sf(iz_bound)


# 4. lists ----------------------------------------------------------

# profile names list - for returning full profile name for tab header
profiles_list <- list(
  HWB = "Health and Wellbeing",
  CWB = "Care and Wellbeing",
  CYP = "Children and Young People",
  DRG = "Drugs",
  ALC = "Alcohol",
  POP = "Population",
  TOB = "Tobacco",
  MEN = "Mental Health",
  ALL = "All Indicators")



# Area names by geography type  including HB, CA, HSCP, alcohol and drugs partnership
# HSC partnership names - also used as the choices for an additional parent area filter 
# when intermediate zone/localities are selected to reduce the number of IZ/localities
hscp_list <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC partnership"]) 
hb_list <- sort(geo_lookup$areaname[geo_lookup$areatype=="Health board"])
ca_list <- sort(geo_lookup$areaname[geo_lookup$areatype=="Council area"])
adp_list <- sort(geo_lookup$areaname[geo_lookup$areatype=="Alcohol & drug partnership"])
hsc_loc_list <- sort(geo_lookup$areaname[geo_lookup$areatype== "HSC locality"])
imz_list <- sort(geo_lookup$areaname[geo_lookup$areatype== "Intermediate zone"])


# geography areatypes
areatype_list <- c("Alcohol & drug partnership", 
                   "Council area", 
                   "Health board",  
                   "HSC locality", 
                   "HSC partnership",  
                   "Intermediate zone",
                   "Scotland")


rank_area_comparators_list <- geo_lookup$areaname[geo_lookup$areatype %in% c("HSC partnership", "Scotland", "Health board")]



# 5. Dashboard theme ---------------------------------------------------------------

# see https://rstudio.github.io/bslib/articles/bs5-variables/ for list of all variables 
phs_theme <- bs_theme(version = 5, # bootstrap version 5
                      "nav-tabs-link-active-bg" = phs_colours(colourname = "phs-magenta"), # multi-tab cards colour when selected
                      "nav-tabs-link-active-color" = "white", # multi-tab cards font colour when selected
                      "form-label-font-weight" = "700") |> # filter labels font weight
  
  # adding custom styling for particular bits of ui (for instance making some bits of text purple without affecting all text)
  # note: could move over some stuff from css file into here i.e. for some of the landing page styling?
  bs_add_rules(
    list(
      ".geography-header { color: #9B4393; font-weight: 600 !important; }", # geography header light phs purple colour
      ".profile-header { color: #3F3685; font-weight: bold !important; }", # profile header darker phs purple colour
      ".btn-download_btns_menu { padding: 0}", # remove padding from download buttons menu so fits nicely in card footers
      ".chart-header { font-weight: bold !important;}", # make chart titles bold
      "strong { color: #9B4393 !important;}", # make the domain names purple for homepage
      ".btn-hero {color:black; background-color:#def4ff; border:none;}", # make buttons in the hero on landing page light blue
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }", # info box header lighter phs purple colour with white text
      ".metadata-header {font-weight: 600;}", # for indicator definitions tab - make headers in expandable rows bolder 
      ".rt-tr-details {padding: 24px; box-shadow: inset 0 1px 3px #dbdbdb; background: #FDFDFC ;}", # for indificator definitions tab - make expandable panel grey
      ".methodology-table th{border:thin solid black; background-color:purple; color:white; padding:3px; word-break: break-all;}", # for indicator def tab - make nested table headers purple
      ".methodology-table td{ border:thin solid black; padding:3px;}", # for indicator def tab - make nested table cells have black border
      ".shiny-output-error {color: white;}", # hiding auto-generated error messages
      ".shiny-output-error-validation {color: #8e8f90;}", # showing custom error messages
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }" # info box header lighter phs purple colour with white text

    )
  )

# phs colours for charts with dynamic number of lines/bars
phs_palette <- unname(unlist(phs_colours()))


# 6. Tab tours -----------------------------------------------------------------


#trend tab
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



  
