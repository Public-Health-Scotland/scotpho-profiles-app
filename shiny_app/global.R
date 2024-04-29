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


# 2. Sourcing modules --------------------------------------------------------------
list.files("modules") |> 
  map(~ source(paste0("modules/", .)))


# 3. Required datafiles ------------------------------------------------------------
main_dataset <- read_parquet("data/optdata") # main dataset (to do: rename optdata file in data prep script)
