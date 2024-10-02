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

# 2. Sourcing modules, narrative text and highchart functions  ------------------------
list.files("modules", full.names = TRUE, recursive = TRUE) |>
  map(~ source(.))

list.files("narrative", full.names = TRUE, recursive = TRUE) |>
  map(~ source(.))

source("highcharter functions.R")

# 3. Required datafiles ------------------------------------------------------------
main_dataset <- setDT(read_parquet("data/main_dataset")) # main dataset (to do: rename optdata file in data prep script)
simd_dataset <- setDT(read_parquet("data/deprivation_dataset")) # dataset behind simd panel
popgroup_dataset <- setDT(read_parquet("data/popgroup_dataset")) # dataset behind popgroup panel

# lookups
geo_lookup <- setDT(readRDS("data/profiles_geo_lookup.rds")) # geography lookup
main_data_geo_nodes <- readRDS("data/main_dataset_geography_nodes.rds") # geography nodes for data table tab
techdoc <- read_parquet("data/techdoc") # technical document



# shapefiles (for map) 
ca_bound <- readRDS("data/CA_boundary.rds") # Council area
hb_bound <- readRDS("data/HB_boundary.rds") # Health board
hscp_bound <- readRDS("data/HSCP_boundary.rds")# HSC Partnerships
hscloc_bound <- readRDS("data/HSC_locality_boundary.rds") # HSC localities
iz_bound <- readRDS("data/IZ_boundary.rds") # Intermediate zone
pd_bound <- readRDS("data/PD_boundary.rds") # Police divisions


# transform shapefiles - needs to be done here or else app doesn't work?!
# note: look into this at some point as wasn't required in old profiles tool
ca_bound <- sf::st_as_sf(ca_bound)
hb_bound <- sf::st_as_sf(hb_bound)
hscp_bound <- sf::st_as_sf(hscp_bound)
hscloc_bound <- sf::st_as_sf(hscloc_bound)
iz_bound <- sf::st_as_sf(iz_bound)
pd_bound <- sf::st_as_sf(pd_bound)


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

# there are some profiles where the domains should be ordered in a particular way 
# e.g. CWB profile should start with 'overarching indicators'
# to add a specified order for a profile domains all that should be required is to add to the list below. 
# The order specified will add (non-select-able) domains to the indicator filter dropdown and dictate the order of 
# domains wihtin profile summary table and within the indicator filter.
# If a profile does not have an domain ordering supplied domains and indicators will sort alphbetically.

profile_domain_order <- list(
  "Care and Wellbeing" = c("Over-arching indicators","Early years","Education","Work","Living standards",
                           "Healthy places", "Impact of ill health prevention","Discrimination and racism"),
  "Mental Health" =  c("Mental health outcomes", "Individual determinants",
                       "Community determinants", "Structural determinants"),
  "Children and Young People" =  c("Safe", "Healthy", "Achieving", "Nurtured", "Active", "Respected", "Responsible", "Included")
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
pd_list <- sort(geo_lookup$areaname[geo_lookup$areatype=="Police division"])


# geography areatypes
areatype_list <- c("Alcohol & drug partnership", 
                   "Council area", 
                   "Health board",  
                   "HSC locality", 
                   "HSC partnership",  
                   "Intermediate zone",
                   "Police division",
                   "Scotland")




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



# function to prepare datasets to be used for each sub-tab in the dashboard
prepare_profile_data <- function(dataset,
                                 selected_profile,
                                 selected_areaname = NULL, 
                                 selected_areatype = NULL){
  
  dt <- dataset
  
  # filter by areatype
  if(!is.null(selected_areatype)){
    dt <- dt[areatype == selected_areatype]
  }
  
  # filter by areaname
  if(!is.null(selected_areaname)){
    dt <- dt[areaname == selected_areaname]
  }
  
  # within the technical document indicator can be assigned to one or more profile
  # filter rows where profile abbreviation exists in one of the 3 profile_domain columns in the technical document
dt <- dt[substr(profile_domain1, 1, 3) == profiles_list[[selected_profile]] |
             substr(profile_domain2, 1, 3) == profiles_list[[selected_profile]] |
             substr(profile_domain3, 1, 3) == profiles_list[[selected_profile]]]

  #create a domain column - this ensures we return the correct domain for the chosen profile in cases where an indicator
  # is assigned to more than one profile (and therefore more than one domain)
  dt <- dt[, domain := fifelse(substr(profile_domain1, 1, 3) == profiles_list[[selected_profile]],
                             substr(profile_domain1, 5, nchar(as.vector(profile_domain1))),
                             fifelse(substr(profile_domain2, 1, 3) == profiles_list[[selected_profile]],
                                     substr(profile_domain2, 5, nchar(as.vector(profile_domain2))),
                                     substr(profile_domain3, 5, nchar(as.vector(profile_domain3)))))]
  
dt #returns a data table filtered to only contain indicators belonging to selected profile with column added for correct domain
  
}




  
