# PROFILE SUMMARY REPORTS

# This script produces summary tables detailing which indicators are included in the prepared profiles data files sitting in your personal shiny app 
# The summary tables detail indicators, which geography types are present and which population group splits will be available when tool deployed

# YOU NEED RUN THE APP LOCALLY FIRST BEFORE THIS SCRIPT WILL RUN
# running app locally ensures the data tables called in script are available - potentially could add in a step to source data from personal analyst shiny app folders at some point

######################################.
# Set up ----
######################################.

library(readr) # for writing csv
#library(stringr) # for reg expressions?

## Set file-paths
data_folder <- "/PHI_conf/ScotPHO/Profiles/Data/"


# SPECIFY WHICH PROFILE YOU WANT SUMMARY FOR (use 3 letter cypher)
profile_select <- "CWB" #you can manually change the profile you're interested in



######################################.
# MAIN DATA  ----
######################################.

main_summary <- main_dataset |>
  #select only indicators that are present in selected profile
  filter(grepl(paste0(profile_select,"-"),profile_list)) |>
  #extract the correct domain based on the profiles selected
  mutate(domain=regmatches(profile_list, regexpr(paste0(profile_select, "-([a-zA-Z*[[:blank:]]]*)*"),profile_list))) |>
  group_by(domain,areatype,indicator) |>
  summarise(count=n()) |>
  mutate(count=case_when(count>0 ~ "Y", TRUE ~ "")) |>
  arrange(areatype)|>
  ungroup() |>
  pivot_wider(names_from = areatype, values_from = count) |>
  arrange(domain, indicator)


write_csv(main_summary, file = paste0(data_folder, "Data to be checked/Main Summary_",profile_select,Sys.Date(),".csv"), na="")  


######################################.
# POP GROUPS DATA  ----
######################################.

popgroup_summary <- popgroup_dataset |>
  #select only indicators that are present in selected profile
  filter(grepl(paste0(profile_select,"-"),profile_list)) |>
  #extract the correct domain based on the profiles selected
  mutate(domain=regmatches(profile_list, regexpr(paste0(profile_select, "-([a-zA-Z*[[:blank:]]]*)*"),profile_list))) |>
  #     domain2=paste0(profile_select,"-",substr(domain, 5,nchar(domain))))
  #some splits can be grouped to make easier to track
  mutate(split_name2=case_when(split_name=="Gender" ~"Sex",
                               split_name=="Disability of household member(s)" ~"Disability",
                               split_name=="Limiting Longstanding Illness" ~"LTC/LLI",
                               split_name=="Long-term conditions" ~"LTC/LLI",
                               split_name=="Scottish Index of Multiple Deprivation"~"Deprivation (SIMD)",
                               TRUE ~split_name)) |>
  mutate(group=paste0(areatype,"-",split_name2)) |>
  group_by(domain,indicator,group) |>
  summarise(count=n()) |>
  mutate(count=case_when(count>0 ~ "Y", TRUE ~ "")) |>
  ungroup() |>
  arrange(group) |> #sort that ensures same geographies are next to each other after pivot
  #convert to pivot table which presents data by domain and indicator
  pivot_wider(names_from = group, values_from = count)|>
  arrange(domain, indicator)


write_csv(popgroup_summary, file = paste0(data_folder, "Data to be checked/Pop Group Summary_",profile_select,Sys.Date(),".csv"), na="")  


######################################.
# DEPRIVATION DATA  ----
######################################.

dep_summary <- simd_dataset |>
  #select only indicators that are present in selected profile
  filter(grepl(paste0(profile_select,"-"),profile_list)) |>
  #extract the correct domain based on the profiles selected
  mutate(domain=regmatches(profile_list, regexpr(paste0(profile_select, "-([a-zA-Z*[[:blank:]]]*)*"),profile_list))) |>
  group_by(domain,areatype,indicator) |>
  summarise(count=n()) |>
  mutate(count=case_when(count>0 ~ "Y", TRUE ~ "")) |>
  arrange(areatype)|>
  ungroup() |>
  pivot_wider(names_from = areatype, values_from = count) |>
  arrange(domain, indicator)

write_csv(dep_summary, file = paste0(data_folder, "Data to be checked/Deprivation Summary_",profile_select,Sys.Date(),".csv"), na="")  




  
