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
ineq_splits_data <- readr::read_csv("data/88007_meeting_mvpa_im.csv") |>
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


