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

# see https://rstudio.github.io/bslib/articles/bs5-variables/ for more details
phs_theme <- bs_theme(
  # high level theming
  version = 5, # bootstrap v5 required to use bslib components (like cards etc.)
  bg = "white", # make background white
  fg = "#222", # make foreground darkgrey/black
  bootswatch = "shiny", # use default shiny theme
  primary = "#0078D4", # make primary colour blue - this will change i.e. active pill colour
  "form-label-font-weight" = "500"#, # font-weight for filter labels
  #"nav-tabs-link-active-bg" = phs_colours(colourname = "phs-magenta-10"), # multi-tab cards colour when selected
  #"nav-tabs-link-active-color" = "black" # multi-tab cards font colour when selected
  ) |>
  # create colour variables to use below
  bs_add_variables(
    "phs-purple" = "#3F3685",
    "phs-blue" = "#0078D4",
    "phs-magenta" = "#9B4393"
    )|>
  # lower level theming
  bs_add_rules(
    list(
      # header/text styling 
      "h1 {font-weight: 700;}",
      "h2 {font-weight: 700;}",
      "h3 {font-weight: bold;}",
      "h4 {font-weight: bold;}",
      "strong { color: $phs-magenta !important;}", # make the domain names magenta for homepage cards
      ".profile-header {color: $phs-purple;}", # make profile header purple
      ".geography-header {color: $phs-magenta;}", # make geography header magenta
      ".header-elements {display: flex; align-items: center;}", # make profile/geography headers sit on same line as button to open filters
      ".chart-header { font-weight: 700; font-size: 1.2rem;}", # make chart headers bold
      ".methodology-table th{border:thin solid black; background-color:$phs-magenta; color:white; padding:3px; word-break: break-all;}", # for indicator def tab - make nested table headers purple
 
      # buttons styling 
      ".btn-global {background-color: #E0E0E0; border:$phs-blue; color:black; border-radius:15px;}", #  global filter buttons
      ".btn-download {color:white; background-color:$phs-blue; border:solid;}", # data download buttons for card footers
      ".card-footer .btn-download {border:none; text-decoration:underline; color:$phs-blue; background-color:white}", # data download buttons for card footers
      ".btn-download:hover {background-color: #e0e0e0; color:black; border:none;}", # data download buttons on hover
      ".btn-hero {color:black; background-color:#def4ff; border:none;}", # 2 x landing page hero buttons
      ".profile-btn:hover {cursor: pointer;background-color: #e0e0e0;}", # hover colour for landing page profile buttons
      ".btn-apply-geo-filter {margin-top:20px; background-color: orange; font-weight: bold; border: none; border-radius: 0;}", # orange apply geographies button
      
      # other
      ".info-box-header { background-color: $phs-magenta; color: #FFF;}", # info box header lighter phs purple colour with white text
      ".methodology-table td{ border:thin solid black; padding:3px;}", # for indicator def tab - make nested table cells have black border
      ".rt-tr-details {padding: 24px; box-shadow: inset 0 1px 3px #dbdbdb; background: #FDFDFC;}", # for indicator definitions tab - make expandable panel grey
      ".chart-controls-icon {background-color:$phs-blue; color:white; border-radius:5em; padding:5px;}" # styling of the chart controls icon
      
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


# function to prepare datasets to be used for each sub-tab in the dashboard (called in server script when generating reactive datasets)
prepare_profile_data <- function(dataset, # a dataset (e.g. main,simd or pop_grp) that must be supplied when calling function 
                                 selected_profile, #reactive object created in server script contains name of selected profile
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




  
