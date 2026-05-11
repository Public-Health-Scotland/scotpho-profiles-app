# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Update geography lookups ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This script contains 3 functions which prepare geographic reference information required to run the profiles tool
# 1x geography lookup (which feeds geography filters within shiny app & also used in data file preparation)
# 1x geography hierachies - (used in data tab of shiny app to identify parent/child relationships of some geography levels)
# x6 shape files (used to construct maps in rank tab)

# It will save the files in a 'data' sub-folder WITHIN your version of the shiny app project
# These will then be ready to be read in via the 'Global.R' script of the shiny app

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Create geography lookup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is the apps main geography lookup 
# This look-up generate choices for any geography filters within the app
# (i.e. filters on the 'trend' tab and the apps main geography filters)

# It's also used within the data preparation script to 
# join with indicator datasets in order to add extra geography columns to them

create_geography_lookup <- function (folder){
  
  # check folder exists
  if (!dir.exists(folder)){
    cli_abort("{.val {folder}} does not exist.")
  }
  
  # get lookup 
  geo_lookup <- readRDS(file.path(scotpho_folder, "Lookups/Geography/opt_geo_lookup.rds"))
  
  # convert cols from factor to character and arrange alphabetically
  geo_lookup <- geo_lookup |>
    select(-areaname_full) |> # drop column - not use anywhere in shiny app
    mutate(across(everything(), ~ as.character(.))) |>
    arrange(areatype, parent_area, areaname)
  
  # save in local repo
  saveRDS(geo_lookup, file.path(project_folder, "profiles_geo_lookup.rds"))
  
  # return geo_lookup outside of function
  geo_lookup<<-geo_lookup
  
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. geography list -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This lookup is used only on the data table tab of the app, in order
# to create the hierarchical geography filter in the sidebar.

update_geography_hierachy <- function(folder){
  
  # Go through each geography type and do the following:
  geo_list <- map(c("Scotland", "Health board", "Council area", "Alcohol & drug partnership", 
                    "HSC partnership", "Police division", "HSC locality", "Intermediate zone"), ~ {

    
    # a. filter the geo_lookup by that areatype
    areatype_lookup <- geo_lookup[geo_lookup$areatype == .x, ]
    
    # b. created a list for that areatype, in the format
    # required for shinyWidgets::quercusInput() 
    # create_tree is a function from the shiny Widgets package that
    # creates choices for this input in the required format
    
    # if the areatype is scotland, create a list with just 1 level
    if (.x == "Scotland") {
      
      create_tree(
        data = areatype_lookup,
        levels = "areatype",
        levels_id = "code"
      )
    
    # if areatype is IZ/HSC locality, create list with 3 levels
    } else if (.x %in% c("Intermediate zone", "HSC locality")) {
      
      create_tree(
        data = areatype_lookup,
        levels = c("areatype", "parent_area", "areaname"),
        levels_id = c("areatype", "parent_area", "code")
      )
      
    } else {
      
    
      # otherwise if anything else (HB/CA etc) create list with 2 levels
      create_tree(
        data = areatype_lookup,
        levels = c("areatype", "areaname"),
        levels_id = c("areatype", "code")
      )
    }
  }) |> reduce(c) # reduce into one large nested list
    
  
  # save output
  saveRDS(geo_list, file.path(project_folder, "main_dataset_geography_nodes.rds"))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. shapefiles -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Update shapefiles used to draw the maps on the 'rank' tab of the app
# This function copies shapefiles from network to local shiny project folder. 

update_shapefiles <- function(folder){
  
  shapefiles <- c("CA_boundary.rds", 
                  "HB_boundary.rds", 
                  "HSCP_boundary.rds",
                  "HSC_locality_boundary.rds",
                  "IZ_boundary.rds",
                  "PD_boundary.rds")
  
  purrr::walk(shapefiles, ~ {
    readRDS(file.path(scotpho_folder, "Shapefiles", .x)) |>
      sf::st_as_sf() |>
      readr::write_rds(file.path(project_folder, .x))
    cli_alert_success("{.val {.x}} saved in `shiny_app/data` folder") # each file copy accompanied by message indicating if transfer successful
  })}



# clear global env
#rm(list = ls())

