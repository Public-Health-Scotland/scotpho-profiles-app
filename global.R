library(shiny) 
library(bslib)
library(phsstyles)
library(shinyjs) # for useShinyjs() function expaning geography filters so parent/child logic 
library(htmltools) # for landing page template to read

# modules ----------------------------------------------------------------------
list.files("modules") |> 
  map(~ source(paste0("modules/", .)))