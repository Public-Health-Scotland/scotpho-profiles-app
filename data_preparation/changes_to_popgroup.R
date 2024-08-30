# processing the popgroup data file for new tab.



popgroup_dataset <- read_parquet("data/popgroup_dataset") # dataset behind popgroup panel

popgroup_dataset2 <- read_parquet("data/popgroup_dataset2") # dataset behind popgroup panel



# The main popgroups dataset doesn't have a 'total' category for SIMD: will this be a problem?
# standardise the splits:
popgroup_dataset_std <- popgroup_dataset %>%
  mutate(split_name = case_when(split_name=="Gender" ~ "Sex", # some also have split_name == Total when split_value==All: how to use?
                                split_name=="Scottish Index of Multiple Deprivation" ~ "SIMD",
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
                                 split_value=="All sexes" ~ "All",
                                 split_value=="Males" ~ "Male",
                                 split_value=="Females" ~ "Female",
                                 split_name=="Long-term conditions" & split_value %in% c("Limiting long-term conditions", "Limiting long-term illness") ~ "Yes, limiting",
                                 split_name=="Long-term conditions" & split_value %in% c("Non-limiting long-term conditions", "Non-limiting long-term illness") ~ "Yes, but not limiting",
                                 split_name=="Long-term conditions" & split_value %in% c("No long-term conditions", "No long-term illness") ~ "No",
                                 split_value=="Working to age adults" ~ "Working age adults",
                                 split_value=="All ages" ~ "All",
                                 split_name=="Age" ~ gsub("-", " to ", split_value),
                                 TRUE ~ split_value)) %>%
  mutate(split_value = case_when(split_name=="Age" ~ gsub("Aged ", "", split_value),
                                 TRUE ~ split_value)) %>%
  mutate(split_value = case_when(split_name=="Age" & grepl("^[0-9]", split_value)==TRUE ~ paste0(split_value, " years"), 
                                 TRUE ~ split_value)) %>%
  mutate(split_value2 = case_when(split_name == "SIMD" ~ "Total", # i.e., all sexes
                                  TRUE ~ as.character(NA)))
popgroups_dataset_std_split <- popgroup_dataset_std %>% # 
  select(split_value, split_value2, 
         split_name) %>%
  unique()        


#flip the columns in the 2nd dataset, so SIMD coding is always first, and sex is 2nd if available
popgroup_dataset2_std <- popgroup_dataset2 %>%
  mutate(split_value2 = case_when(split_name=="Sex" ~ split_value,
                                  TRUE ~ split_value2)) %>%
  mutate(split_value = case_when(split_name=="Sex" ~ as.character(NA),
                                 TRUE ~ split_value)) %>%
  mutate(split_name = case_when(split_name=="Sex + SIMD" ~ "SIMD",
                                TRUE ~ split_name)) %>%
  mutate(temp = split_value,
         split_value = split_value2,
         split_value2 = temp) %>%
  select(-temp)

popgroup_dataset_new <- bind_rows(popgroup_dataset2_std, popgroup_dataset_std) #42 vars
## save final file to local repo
write_parquet(popgroup_dataset_new, "shiny_app/data/popgroup_dataset_new")

# # # what are the splits and names now?
# popgroup_dataset_new_split <- popgroup_dataset_new %>% # 42 vars
#   select(split_value, split_value2, split_name) %>%
#   unique()
# write.csv(popgroup_dataset_new_split, "popgroup_dataset_new_split.csv", row.names=FALSE)
