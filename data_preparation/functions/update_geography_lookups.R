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
geo_lookup <- readRDS(file.path(scotpho_folder, "Lookups/Geography/opt_geo_lookup.rds")) |>
  select(-areaname_full)

# arrange areatypes
geo_lookup <- geo_lookup |>
  mutate(areatype = factor(
    areatype,
    levels = c(
      "Scotland",
      "Health board",
      "Council area",
      "Alcohol & drug partnership",
      "HSC partnership",
      "HSC locality",
      "Intermediate zone",
      "Police division"
    )
  )) |>
  arrange(areatype, parent_area, areaname)

# add additional column to the lookup that 
# contains the full geography path (e.g. Health board/NHS Ayrshire & Arran)
geo_lookup <- geo_lookup |>
  mutate(
    geo_path = paste(
      areatype,
      case_when(
        areatype %in% c("Intermediate zone", "HSC locality") ~ parent_area,
        TRUE ~ areaname
      ),
      case_when(
        areatype %in% c("Intermediate zone", "HSC locality") ~ areaname,
        TRUE ~ NA_character_
      ),
      sep = "/"
    ),
    geo_path = sub("/NA$", "", geo_path)
  )

saveRDS(geo_lookup, file.path(project_folder, "profiles_geo_lookup.rds"))

# return geo_lookup outside of function
geo_lookup<<-geo_lookup

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. geography list -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This lookup is used only on the data table tab of the app, in order
# to create the hierarchical geography filter in the sidebar.
# this code is lifted from the documentation for the 'jsTreeR' package 
# (which is the package used to create the geography filter in the data table tab)
# see examples here: https://www.rdocumentation.org/packages/jsTreeR/versions/1.1.0/topics/jstree-shiny 

update_geography_hierachy <-function(folder){

# get unique geographies from geo lookup
leaves <- geo_lookup$geo_path 

dfs <- lapply(strsplit(leaves, "/"), function(s){
  item <-Reduce(function(a,b) paste0(a,"/",b), s[-1], s[1], accumulate = TRUE)
  data.frame(
    item = item,
    parent = c("root", item[-length(item)]),
    stringsAsFactors = FALSE
  )
})


#dat <- dfs[[1]]
# for(i in 2:length(dfs)){
#   dat <- merge(dat, dfs[[i]], all = TRUE)
# }

# amending function using 2 lines below to use rbind instead of merge 
# This allows parent nodes to be ordered based on how data is arranged before being passed to this function
# instead of alphabetically
dat <- do.call(rbind, dfs)
dat <- dat[!duplicated(dat), ]

f <- function(parent){
  i <- match(parent, dat$item)
  item <- dat$item[i]
  children <- dat$item[dat$parent==item]
  label <- tail(strsplit(item, "/")[[1]], 1)
  if(length(children)){
    list(
      text = label,
      children = lapply(children, f),
      icon =FALSE,
      state = list(selected = FALSE, opened = FALSE )
    )
  }else{
    list(text = label, type = "child",icon = FALSE,
         state = list(selected = FALSE,opened = FALSE ))
  }
}

geo_list <- lapply(dat$item[dat$parent == "root"], f)

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