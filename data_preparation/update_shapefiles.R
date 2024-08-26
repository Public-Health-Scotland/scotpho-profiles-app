## FUNCTION: update_shapefiles.R ----

update_shapefiles <- function() {
  

library(sf) # required for formatting shapefiles

# shapefiles (for map) 
ca_bound <- readRDS(paste0(shape_files,"CA_boundary.rds")) # Council area
hb_bound<- readRDS(paste0(shape_files,"HB_boundary.rds")) # Health board
hscp_bound <- readRDS(paste0(shape_files,"HSCP_boundary.rds"))# HSC Partnerships
hscloc_bound <- readRDS(paste0(shape_files,"HSC_locality_boundary.rds")) # HSC localities
iz_bound <- readRDS(paste0(shape_files,"IZ_boundary.rds")) # Intermediate zone
#scot_bound <- readRDS("data/scot_bound.rds") # scotland
pd_bound <- readRDS(paste0(shape_files,"PD_boundary.rds")) # # Police divisions (for mental health profile only)


# transform so in right format to join to main dataset 
# this should maybe  be done in LOOKUPS repo

ca_bound <- sf::st_as_sf(ca_bound)
hb_bound <- sf::st_as_sf(hb_bound)
hscp_bound <- sf::st_as_sf(hscp_bound)
hscloc_bound <- sf::st_as_sf(hscloc_bound)
iz_bound <- sf::st_as_sf(iz_bound)
#scot_bound <- sf::st_as_sf(scot_bound)
pd_bound <- sf::st_as_sf(pd_bound)

# 
write_rds(ca_bound,"shiny_app/data/CA_boundary.rds")
write_rds(hb_bound,"shiny_app/data/HB_boundary.rds")
write_rds(hscp_bound,"shiny_app/data/HSCP_boundary.rds")
write_rds(hscloc_bound,"shiny_app/data/HSC_locality_boundary.rds")
write_rds(iz_bound,"shiny_app/data/IZ_boundary.rds")
write_rds(pd_bound,"shiny_app/data/PD_boundary.rds")

}




# Copy geography lookups  --------------------------------------------------------------
#  might want to flip back to the method below if moving the transformation to the lookups repo?

# Note: this step is only really necessary if you are running this script for the first time
# OR if there have been updates to the geography lookups
# 
# # Copy all shapefiles to your local repository
# map_lgl(c("CA_boundary.rds", 
#           "HB_boundary.rds", 
#           "HSCP_boundary.rds",
#           "HSC_locality_boundary.rds",
#           "IZ_boundary.rds"), ~ {
#             
#             file.copy(
#               paste0(shape_files, .x), # takes shape files from network directory
#               paste0("shiny_app/data/", .x),  # new destination
#               overwrite = TRUE
#             )
#           })
# 
# 
# # copy the main geography lookup to your local repo
# file.copy(
#   paste0(lookups, "Geography/opt_geo_lookup.rds"), # old file path
#   paste0("shiny_app/data/opt_geo_lookup.rds"),  # new destination
#   overwrite = TRUE
# )
