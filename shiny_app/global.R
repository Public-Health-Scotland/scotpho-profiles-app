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
library(jsTreeR) # for data tab geography filters
library(shinyWidgets)
library(bsicons) # for icons
library(cicerone) #for guided tours of tabs
library(sf)
library(DT)
library(tidyr) # for pivot longer used in meta data tab


library(readr) #im additiona will remove in future
library(tibble) #need for indicator_filter_mod fix: https://github.com/Rdatatable/data.table/issues/3745#issuecomment-1380723524

# 2. Sourcing modules, narrative text and guided tours  ------------------------
list.files("modules", full.names = TRUE, recursive = TRUE) |>
  map(~ source(.))

list.files("narrative", full.names = TRUE, recursive = TRUE) |>
  map(~ source(.))

list.files("guided tours", full.names = TRUE, recursive = TRUE) |> 
  map(~ source(.))

# 3. Required datafiles ------------------------------------------------------------
main_dataset <- read_parquet("data/main_dataset") # main dataset (to do: rename optdata file in data prep script)
geo_lookup <- readRDS("data/profiles_geo_lookup.rds") # geography lookup
geo_lookup <- setDT(geo_lookup) 

main_data_geo_nodes <- readRDS("data/main_dataset_geography_nodes.rds") # geography nodes for data table tab

simd_dataset <- read_parquet("data/deprivation_dataset") # dataset behind simd panel

techdoc <- read_parquet("data/techdoc") # technical document

popgroup_dataset <- read_parquet("data/popgroup_dataset") # dataset behind popgroup panel


# shapefiles (for map) 
ca_bound <- readRDS("data/CA_boundary.rds") # Council area
hb_bound <- readRDS("data/HB_boundary.rds") # Health board
hscp_bound <- readRDS("data/HSCP_boundary.rds")# HSC Partnerships
hscloc_bound <- readRDS("data/HSC_locality_boundary.rds") # HSC localities
iz_bound <- readRDS("data/IZ_boundary.rds") # Intermediate zone


# transform shapefiles - needs to be done here or else app doesn't work?!
# note: look into this at some point as wasn't required in old profiles tool
ca_bound <- sf::st_as_sf(ca_bound)
hb_bound <- sf::st_as_sf(hb_bound)
hscp_bound <- sf::st_as_sf(hscp_bound)
hscloc_bound <- sf::st_as_sf(hscloc_bound)
iz_bound <- sf::st_as_sf(iz_bound)


# 4. lists ----------------------------------------------------------

# profile names list - used for:
# - creating choices for the profile filter
# - filtering the dataset using the abbreviated profile name 
profiles_list <- list(
  "Health and Wellbeing" = "HWB",
  "Care and Wellbeing" = "CWB",
  "Children and Young People" = "CYP",
  "Drugs" = "DRG",
  "Alcohol" = "ALC",
  "Population" = "POP",
  "Tobacco" = "TOB",
  "Mental Health" = "MEN",
  "All Indicators" = "ALL"
  )

# archived indicators - for removing from the summary tab and separating indicators 
# into 'active' and 'archived' in the indicator filters across the other tabs
archived_indicators <- techdoc$ind_id[techdoc$active == "AR"]



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
                      "form-label-font-weight" = "550") |> # filter labels font weight
  
  # adding custom styling for particular bits of ui (for instance making some bits of text purple without affecting all text)
  # note: could move over some stuff from css file into here i.e. for some of the landing page styling?
  bs_add_rules(
    list(
      ".geography-header { color: #9B4393; font-weight: 600 !important; margin-right: 10px;}", # geography header light phs purple colour
      ".profile-header { color: #3F3685; font-weight: bold !important; margin-right: 10px}", # profile header darker phs purple colour
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
      ".info-box-header { background-color: #9B4393; color: #fff; font-size: 1.2em !important; }", # info box header for CWB profile- lighter phs purple colour with white text
      ".profile-btn:hover {cursor: pointer;background-color: #e0e0e0;}", # make profile buttons on landing page change colour when user hovers over it
      ".header-elements {display: flex; align-items: center;}", # make profile and geography headers side-by-side with buttons to open global filters
      ".global-filter {background-color: #ECECEC; color: black; padding: 5px;}", # make global filter buttons grey
      ".btn-apply-geo-filter {margin-top:20px; background-color: orange; font-weight: bold; border: none; border-radius: 0;}",
      ".chart-controls-icon {background-color:#0078D4; color:white; border-radius:5em; padding:5px;}" # styling of the chart controls icon

    )
  )

# phs colours for charts with dynamic number of lines/bars
phs_palette <- unname(unlist(phs_colours()))

# cog icon that goes on the top right-hand side of each chart with additional chart controls (icon links to bslib popover controls)
chart_controls_icon <- function(size = "2em") {
  bsicons::bs_icon(name = "gear-fill", 
                   size = size, 
                   title = "Click here to view customise chart options", # tooltip/for screenreaders
                   class = "chart-controls-icon")
}


  
