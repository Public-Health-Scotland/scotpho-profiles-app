###############################################################################.
#
# Global script ---- 
#
###############################################################################.


# 1. Required packages ----------------------------------------------------------
library(shiny) 
library(bslib) # app layout functions
library(phsstyles) #
library(shinyjs) # for useShinyjs() function expanding geography filters 
library(htmltools) # for landing page template to read
library(purrr) # needed for sourcing modules with walk


# 2. Sourcing modules --------------------------------------------------------------
list.files("modules") |> 
  map(~ source(paste0("modules/", .)))


# 3. Required datafiles ------------------------------------------------------------