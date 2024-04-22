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
library(jsonlite) # for download data in json format



# 2. Sourcing modules --------------------------------------------------------------
list.files("modules") |> 
  map(~ source(paste0("modules/", .)))


# 3. Required datafiles ------------------------------------------------------------
main_dataset <- read_parquet("data/optdata") # main dataset (to do: rename optdata file in data prep script)
geo_lookup <- readRDS("data/geo_lookup.rds") # geography lookup

# 4. Dashboard theme ---------------------------------------------------------------

# see https://rstudio.github.io/bslib/articles/bs5-variables/ for list of all variables 
phs_theme <- bs_theme(version = 5, # bootstrap version 5
                      "nav-tabs-link-active-bg" = phs_colours(colourname = "phs-magenta"), # multi-tab cards colour when selected
                      "nav-tabs-link-active-color" = "white", # multi-tab cards font colour when selected
                      "form-label-font-weight" = "700") |> # filter label font weight
  
  # adding custom styling for particular bits of ui (for instance making some bits of text purple without affecting all text)
  bs_add_rules(
    list(
      ".geography-header { color: #9B4393; font-weight: 600 !important; }", # geography header light phs purple colour
      ".profile-header { color: #3F3685; font-weight: bold !important; }"  # profile header darker phs purple colour
    )
  )
  


# profile names 
profiles_list <- list(
  HWB = "Health and Wellbeing",
  CWB = "Care and Wellbeing",
  CYP = "Children and Young People",
  DRG = "Drugs",
  ALC = "Alcohol",
  POP = "Population",
  TOB = "Tobacco",
  MEN = "Mental Health")


# partnership names 
partnership_name <- sort(geo_lookup$areaname[geo_lookup$areatype=="HSC partnership"]) 