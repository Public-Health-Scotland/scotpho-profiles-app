
## FUNCTION: combine_popgroup_and_simd_data() ----

## Combines latest popgroup_dataset and deprivation_dataset to create a single parquet data file within local shiny app data folder.
## The new file - popgroup_dataset.R - will populate the population group tab in the app.
## This shows indicators split by various equality splits, including age/sex/disability/SIMD etc
## The new data format introduced here allows for a second split column (split_value2).
## Currently split_value2 is only used for sex split when the split_name is SIMD, but there is scope for other multi-way splits.

## DATA FORMAT

## split_name
## Same as in original popgroup_dataset: this populates the first split filter "Select population split"
## This script standardises the split_names used.
## Current split_name options: "Age", "Income (Equivalised)", "Long-term conditions", "Sex", "Deprivation (SIMD)", "Urban/Rural".
## Any rows with split_name=="Total" should be dropped.
## Deprivation should always be in split_name rather than split_value2, because its data needs to be treated differently.
## This means the other splits can be in either column, depending on whether also combined with SIMD or not.

## split_value2 
## A new variable, which populates the second split filter "Select 2nd population split:"
## Current values for split_value2 are "Male", "Female", "Total". 
## split_value2 should be NA if the data relate to a single split

## split_value
## These are the splits displayed in the charts (as in original popgroup_dataset), and the function standardises these:
## ages e.g., "0 to 4 years", "60+ years", "Children", "Working-age adults", "Pensioners" (latter 3 could do with age ranges being given too)
## income: "1 - highest income" to "5 - lowest income"
## Long-term conditions: "No", "Yes, but not limiting", "Yes, limiting"
## Sex: Female, Male, Total 
## SIMD: "1 - most deprived" to "5 - least deprived", and "Total"
## Urb/Rural: "1 Large urban areas" to "6 Remote rural"
## split_value == "Total" is what is used to plot the average on the charts, if selected. 
## In many cases this is not in the original data, so the function extracts the relevant "Total" split - by def_period, code, and ind_id - and appends this.

## quint_type 
## only applies to SIMD data, and has been assumed to be scotland (sc_quin) unless specified. 
## Check this assumption is correct. Applies to HWB and CWB profiles only (when SIMD data were included in their popgroup rather than deprivation data)

## PARAMETERS
# create_backup :  (default is FALSE ) option to set to TRUE when creating a distinct backup version desired e.g. when planning to deploy the app

combine_popgroup_and_simd_data <- function(create_backup = FALSE) {

  ############################################
  # Processing the popgroup_dataset
  ############################################
  
  # Original popgroup dataset:  
  popgroup <- read_parquet(paste0(test_shiny_files, "/processed_intermediate_datasets/popgroup_dataset")) # result of update_popgroup_data.R
  
  # standardise the splits:
  popgroup_std <- popgroup %>%
    mutate(split_name = case_when(split_name=="Gender" ~ "Sex", # some also have split_name == Total when split_value==All: how to use?
                                  split_name %in% c("Scottish Index of Multiple Deprivation", "SIMD") ~ "Deprivation (SIMD)",
                                  split_name %in% c("Long-term physical/mental health condition", "Longterm conditions") ~ "Long-term conditions",
                                  split_name=="Equivalised income" ~ "Income (Equivalised)",
                                  TRUE ~ split_name)) %>%
    mutate(split_value = case_when(split_value=="1st-Top quintile" ~ "1 - highest income",
                                   split_value=="2nd quintile" ~ "2",
                                   split_value=="3rd quintile" ~ "3",
                                   split_value=="4th quintile" ~ "4",
                                   split_value=="5th-Bottom quintile" ~ "5 - lowest income",
                                   split_value=="1st-Most deprived" ~ "1 - most deprived",
                                   split_value=="2nd" ~ "2",
                                   split_value=="3rd" ~ "3",
                                   split_value=="4th" ~ "4",
                                   split_value=="5th-Least deprived" ~ "5 - least deprived",
                                   split_value=="5 - most deprived" ~ "5 - least deprived", # checked the data to confirm this was coded wrong
                                   split_value=="All sexes" ~ "Total",
                                   split_value=="Males" ~ "Male",
                                   split_value=="Females" ~ "Female",
                                   split_name=="Long-term conditions" & split_value %in% c("Limiting long-term conditions", "Limiting long-term illness") ~ "Yes, limiting",
                                   split_name=="Long-term conditions" & split_value %in% c("Non-limiting long-term conditions", "Non-limiting long-term illness") ~ "Yes, but not limiting",
                                   split_name=="Long-term conditions" & split_value %in% c("No long-term conditions", "No long-term illness") ~ "No",
                                   split_value %in% c("Working to age adults", "Working age adults") ~ "Working-age adults",
                                   split_value %in% c("All ages", "All") ~ "Total",
                                   split_name=="Age" ~ gsub("-", " to ", split_value),
                                   TRUE ~ split_value)) %>%
    mutate(split_value = case_when(split_name=="Age" ~ gsub("Aged ", "", split_value),
                                   TRUE ~ split_value)) %>%
    mutate(split_value = case_when(split_name=="Age" & grepl("^[0-9]", split_value)==TRUE ~ paste0(split_value, " years"), 
                                   TRUE ~ split_value)) %>%
    mutate(split_value2 = case_when(split_name == "Deprivation (SIMD)" ~ "Total", # i.e., all sexes
                                    TRUE ~ as.character(NA))) %>%
    mutate(quint_type = case_when(split_name == "Deprivation (SIMD)" ~ "sc_quin", # an assumption: check
                                  TRUE ~ as.character(NA))) %>%
    filter(!split_name=="Total") #%>%
  # filter(!ind_id %in% c(30000:39999)) # remove original MHI data, as adding in more complete set (with SIMD x sex)
  
  # #check what splits are now used:
  # popgroup_std_splits <- popgroup_std %>% # 
  #   select(split_value, split_value2, 
  #          split_name) %>%
  #   unique()        
  
  # find which popgroup splits don't have total
  popgp_have_total <- popgroup_std %>%
    mutate(total_exists = ifelse(split_value=="Total", 1, 0)) %>%
    group_by(year, code, ind_id, def_period, split_name) %>%
    summarise(has_total = sum(total_exists)) %>%
    ungroup()
  
  # table(popgp_have_total$has_total)
  # #0    1     
  # #7379 2753    
  
  # get totals for those with has_total==0:
  no_totals <- popgp_have_total %>% # n=7379
    filter(has_total==0) %>%
    select(-has_total)
  
  totals <- read_parquet("shiny_app/data/main_dataset") %>% # main dataset 
    merge(y=no_totals, by=c("year", "def_period", "code", "ind_id"), all.y=T)
  
  # #some data with splits have no totals available in the main_dataset:
  # ind_w_no_totals <- totals %>% #n=204
  #   filter(is.na(indicator))
  # 
  # table(ind_w_no_totals$ind_id)
  # # 30057 99105 99106 99107 99108 99109 99116 99117 99118 99121 99123 
  # # 2    12    42    10    42    28     8     6    10    12    32 
  # # 10 CWB indicators: 99105-109, 99116-118, 99121, 99123 and 1 MHI: 30057 (violent crime, because SCJS pooled some years for police div estimates)
  
  
  # prep the total rows for appending to the rest of the data
  totals_4_popgroup_data <- totals %>%
    filter(!is.na(indicator)) %>% # drops those with no totals
    mutate(split_value="Total",
           split_value2=ifelse(split_name=="Deprivation (SIMD)", "Total", as.character(NA)),
           quint_type = ifelse(split_name=="Deprivation (SIMD)", "sc_quin", as.character(NA))) # all missing quint_types assumed to be sc_quin
  
  
  ############################################
  # Processing the deprivation_dataset
  ############################################
  
  # Original deprivation dataset:  
  simd <- read_parquet(paste0(test_shiny_files, "/processed_intermediate_datasets/deprivation_dataset")) %>% # result of update_deprivation_data.R
    mutate(split_value2=case_when(sex=="Male" ~ "Male",
                                  sex=="Female" ~ "Female",
                                  sex=="Total" ~ "Total",
                                  is.na(sex) ~ "Total"),
           #  quint_type = case_when(is.na(quint_type) ~ "sc_quin", # all have quint_type, so this assumption not needed
           #                         TRUE ~ quint_type),
           split_name = "Deprivation (SIMD)") %>%
    rename(split_value = quintile) 
  
  # # do any simd splits not have total?
  # have_total <- read_parquet(paste0(test_shiny_files, "/processed_deprivation_dataset/deprivation_dataset")) %>%
  #   mutate(total_quint = ifelse(quintile=="Total", 1, 0)) %>%
  #   group_by(year, def_period, code, ind_id, sex, quint_type) %>%
  #   summarise(has_total = sum(total_quint)) %>%
  #   ungroup()
  # all SIMD data in the deprivation data have quintile==Total (used for averages)
  
  # #check what splits are now used:
  # simd_splits <- simd %>% # 
  #   select(split_value, split_value2, 
  #          split_name) %>%
  #   unique()       
  
  
  ############################################
  # Appending the datasets
  ############################################
  
  popgroup_dataset <- bind_rows(popgroup_std, totals_4_popgroup_data, simd) %>% 
    arrange(ind_id, year, code, split_name, split_value, split_value2)
  
  # # what are the splits and names now?
  # popgroup_new_splits <- popgroup_new %>% # 42 vars
  #   select(split_value, split_value2, split_name) %>%
  #   unique()
  
  # make available in global environment for viewing what will be sent to shiny app
  popgroup_dataset <<- popgroup_dataset
  
  ## save final file to local repo
  write_parquet(popgroup_dataset, "shiny_app/data/popgroup_dataset")
  
  
  ## Optional: Create backup of from local repo -----
  ## Usually would only want to create a backup if you intend to update live tool
  ## This file will be stored in backups folder and could be used to roll back app to a particular date
  if (create_backup == TRUE) {
    
    file.copy(
      "/shiny_app/data/popgroup_dataset", 
      paste0(backups, "popgroup_dataset", Sys.Date()), 
      overwrite = TRUE
    )
  } 

} #close function

#END.


