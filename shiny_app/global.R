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
library(nanoparquet) # reading/writing parquet files


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

# the unique id for all the possible sub-tabs that can be displayed for a profile in the app
# these match the ids assigned to each sub-tab in the UI script (using the 'value' argument in nav_panel())
# this vector is used in the server script to determine which sub-tabs should be hidden/shown for a selected profile
# i.e. by going through each id and checking if it exists in the profiles_list below for the selected profile
all_subtabs <- c("summary_tab",
                 "trends_tab",
                 "rank_tab", 
                 "simd_tab", 
                 "pop_groups_tab", 
                 "about_profile_tab")


# this list contains information on each profile within the tool:
# - the name of the profile (this should be spelled how you want it to appear on the landing page button/profile filter)
# short_name =  3 letter profile abbreviation (as assigned in the tech doc) - this is used for filtering data by selected profile
# homepage_description = sentence or 2 to include in the homepage button (wrap specific words in ** **  if you want them to be purple)
# domain_order = use if there's a particular order domains should appear in the indicator filter/summary table (leave as NULL if no order required)
# active = either TRUE or FALSE. Set to FALSE if you want the homepage button to be disabled. Set to TRUE to make profile active for users to explore.
# subtabs = the subtabs you want to include for the profile - either pass 'all_subtabs' from vector above to include all, or select individual subtabs
# note: to add a new profile to the tool, just add a new section to the list
profiles_list <- list(
 
  # Health & wellbeing info
  "Health & Wellbeing" = list(
    short_name = "HWB",
    homepage_description = markdown("View indicators relating to **Behaviours**, **Crime**, **Economy**, **Life expectancy** and **Mortality, ill health and injury**."),
    domain_order = NULL,
    subtabs = all_subtabs,
    active = TRUE
  ),

  # Care & wellbeing portfolio info 
  "Population Health" = list(
    short_name = "CWB",
    homepage_description = markdown("View indicators through the lens of the **Marmot principles**. Supporting Scotland's **Population Health Framework**, and 
                                    the **Collaboration for Health Equity in Scotland**"),
    domain_order = c("Over arching indicators","Early years","Education","Work","Living standards",
                     "Healthy places", "Impact of ill health prevention","Discrimination and racism", "Environmental sustainability and health equity"),
    subtabs = all_subtabs,
    active = TRUE
  ),
  
  # Adult Mental health info
  "Adult Mental Health" = list(
    short_name = "MEN",
    homepage_description = markdown("View indicators relating to **Mental health outcomes**, and **Individual**, **Community** and **Structural**  determinants."),
    domain_order = c("Mental health outcomes", "Individual determinants",
                     "Community determinants", "Structural determinants"),
    subtabs = all_subtabs,
    active = TRUE
  ),

  # CYP Mental health info
  "Children & Young People Mental Health" = list(
    short_name = "CMH",
    homepage_description = markdown("View indicators relating to **Mental health outcomes**, and **Individual**, **Family and friends**, **Learning environment**, **Community** and **Structural**  determinants."),
    domain_order = c("Mental health outcomes", "Individual determinants",
                     "Family and friends", "Learning environment",
                     "Community determinants", "Structural determinants"),
    subtabs = all_subtabs,
    active = TRUE
  ),
  
  # Tobacco info
  "Tobacco" = list(
    short_name = "TOB",
    homepage_description = markdown("View indicators relating to **Adult prevalence**, **Smoking during and post pregnancy**, **Smoking attributable deaths and diseases** and **Smoking cessation and services.**"),
    domain_order = NULL,
    subtabs = c("summary_tab", "trends_tab", "rank_tab", "simd_tab"),
    active = TRUE
  ),
  
  # Alcohol info
  "Alcohol" = list(
    short_name = "ALC",
    homepage_description = markdown("View indicators relating to **Community safety**, **Environment**, **Health**, **Prevalence** and **Services**."),
    domain_order = NULL,
    subtabs = all_subtabs,
    active = TRUE
  ),
  
  # Drugs info
  "Drugs" = list(
    short_name = "DRG",
    homepage_description = markdown("View indicators relating to **Community safety**, **Environment**, **Health**, **Prevalence** and **Services**."),
    domain_order = NULL,
    subtabs = c("summary_tab", "trends_tab", "rank_tab", "simd_tab"),
    active = TRUE
  ),
  
  # Children and young people info
  "Children & Young People" = list(
    short_name = "CYP",
    homepage_description = markdown("View indicators relating to **Active**, **Healthy**, **Achieving**, **Safe** and **Nurtured**."),
    domain_order = c("Safe", "Healthy", "Achieving", "Nurtured", "Active", "Respected", "Responsible", "Included"),
    subtabs = c("summary_tab", "trends_tab", "rank_tab", "simd_tab", "about_profile_tab"),
    active = TRUE
  ),
  
  # Population info
  "Population" = list(
    short_name = "POP",
    homepage_description = markdown("View **population estimates** for different age groups."),
    domain_order = NULL,
    subtabs = c("summary_tab", "trends_tab", "rank_tab"),
    active = TRUE
  ),
  
  # All indicators info
  "All Indicators" = list(
    short_name = "ALL",
    homepage_description = markdown("View **all indicators** in this tool from across every profile."),
    domain_order = NULL,
    subtabs = c("trends_tab","rank_tab", "simd_tab", "pop_groups_tab"),
    active = TRUE
  ),
  
  # Physical Activity info
  "Physical Activity" = list(
    short_name = "PHY",
    homepage_description = markdown("Under development - not yet available"),
    domain_order = NULL,
    subtabs = c("trends_tab", "rank_tab", "simd_tab"),
    active = FALSE
  )
  
)


# store the names of active profiles
# this is used in the UI to create the choices for the profile filter
# and in the server to run the server logic for what happens when a user clicks an profile button on landing page
active_profiles <- names(Filter(function(x) x$active == TRUE, profiles_list))


  
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
  "form-label-font-weight" = "550"#, # font-weight for filter labels
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
      ".btn-download {color:white; background-color:$phs-blue; border:$phs-blue;}", # data download buttons for card footers
      ".card-footer .btn-download {border:none; text-decoration:underline; color:$phs-blue; background-color:white}", # data download buttons for card footers
      ".btn-download:hover {background-color: #e0e0e0; color:black; border:#e0e0e0;}", # data download buttons on hover
      ".profile-btn-disabled {background-color:#F4F4F4}",
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
                                 selected_profile, # reactive object created in server script contains short name of selected profile
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
  
  # get short name of the selected profile from the profiles list
  profile <- pluck(profiles_list, selected_profile, "short_name")
  
  
  # within the technical document indicator can be assigned to one or more profile
  #  filter if the 3-letter profile abbreviation is found in the profile_domain column
  dt <- dt[grepl(profile, profile_domain)]
  

  # create a domain column - this ensures we return the correct domain for the chosen profile in cases where an indicator
  # This code extracts the relevant profile and domain (it looks for the text after the XXX profile code and stops 
  # if a character that isn't a letter or a space is encountered, e.g., a ";")
  dt <- dt[, domain := regmatches(profile_domain, 
                                  regexpr(paste0(profile, "-([a-zA-Z*[[:blank:]]]*)*"), 
                                          profile_domain))
  ][, domain := substr(domain, 5, nchar(domain))] # gets rid of the XXX profile code
  
dt #returns a data table filtered to only contain indicators belonging to selected profile with column added for correct domain

}

  
