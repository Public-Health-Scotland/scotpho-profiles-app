# processing the popgroup data file for new tab.


popgroup_dataset_new <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/popgroup_dataset_new")  # dataset behind popgroup panel
# new data format:
# split_name is the same: this populates the first split filter "Select population split"
# split_names have been standardised throughout. 
# Current options: "Age", "Income (Equivalised)", "Long-term conditions", "Sex", "Deprivation (SIMD)", "Urban/Rural". 
# Any rows with split_name=="Total" should be dropped.
# Deprivation should always be in split_name rather than split_value2, because its data needs to be treated differently.
# This means the other splits can be in both columns, depending on whether also combined with SIMD or not.

# split_value2 = new variable, populates the second split filter "Select 2nd population split:"
# Current values for split_value2 are "Male", "Female", "Total". 
# split_value2 should be NA if the data relate to a single split

# split_values are what are displayed in the charts, and require some format standardising:
# ages e.g., "0 to 4 years", "60+ years", "Children", "Working-age adults", "Pensioners" (latter 3 could do with age ranges being given too) (All)
# income: "1 - highest income" to "5 - lowest income"
# Long-term conditions: "No", "Yes, but not limiting", "Yes, limiting"
# Sex: Female, Male, Total (All)
# SIMD: "1 - most deprived" to "5 - least deprived" (Total also given)
# Urb/Rural: "1 Large urban areas" to "6 Remote rural"
# split_value == "Total" is what is used to plot the average on the charts, if selected. 
# split_value == "Total" will currently need to be repeated in the data for each split_name (and split2_filter, if used), unless this can be included in the data prep
# I have extracted Totals for each year x code x ind_id without a split_value==Total (from main_dataset) and appended to the popgroup data to give averages.
# In some cases in CWB profile this has introduced an issue as the Scotland wide and the popgroup data have different def_periods:
# e.g., food insecurity has national data for 4 years aggregated (e.g., 2016-2019) while SIMD data are annual. 
# Even though the aggregated data will have a numeric 'year' value that matches an annual value they should not be plotted together, as this would be misleading.

# quint_type has been assumed to be scotland unless specified. 
# Check this assumption is correct. Applies to HWB and CWB profiles only (when SIMD data were included in the popgroups file)


# Original popgroup dataset: n = 26,563 
popgroup_dataset <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/popgroup_dataset") # dataset behind popgroup panel


# popgroup_dataset2 <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/popgroup_dataset2") # dataset behind popgroup panel
# #15955


# Original SIMD file: n = 258,329
simd_dataset2 <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/deprivation_dataset")
# sex included, NA for most

# The main popgroups dataset doesn't have a 'total' category for SIMD: will this be a problem?
# No, but won't show 'average' line unless total is given. Can extract from main dataset
# standardise the splits:
popgroup_dataset_std <- popgroup_dataset %>%
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
#check what splits are now used:
popgroups_dataset_std_split <- popgroup_dataset_std %>% # 
  select(split_value, split_value2, 
         split_name) %>%
  unique()        

# find which popgroup splits don't have total
popgp_have_total <- popgroup_dataset_std %>%
  mutate(total_exists = ifelse(split_value=="Total", 1, 0)) %>%
  group_by(year, code, ind_id, def_period, split_name) %>%
  summarise(has_total = sum(total_exists)) %>%
  ungroup()
table(popgp_have_total$has_total)
#0    1     
#7379 2753    

# get totals for those with 0 total data:
no_totals <- popgp_have_total %>% # n=7377
  filter(has_total==0) %>%
  select(-has_total)

totals <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/main_dataset") %>% # main dataset 
  merge(y=no_totals, by=c("year", "def_period", "code", "ind_id"), all.y=T)
#some dat with splits have no totals:
ind_w_no_totals <- totals %>% #n=204
  filter(is.na(indicator))

table(ind_w_no_totals$ind_id)
# 30057 99105 99106 99107 99108 99109 99116 99117 99118 99121 99123 
# 2    12    42    10    42    28     8     6    10    12    32 
# 10 CWB indicators: 99105-109, 99116-118, 99121, 99123 and 1 MHI: 30057 (violent crime, because SCJS pooled some years for police div estimates)


# prep the total rows for appending to the rest of the data
totals_4_popgroup_data <- totals %>%
  filter(!is.na(indicator)) %>%
  mutate(split_value="Total",
         split_value2=ifelse(split_name=="Deprivation (SIMD)", "Total", as.character(NA)),
         quint_type = ifelse(split_name=="Deprivation (SIMD)", "sc_quin", as.character(NA))) # all missing quint_types assumed to be sc_quin

# Existing SIMD file:
simd_dataset2 <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/deprivation_dataset") %>% # dataset behind simd panel
  mutate(split_value2=case_when(sex=="Male" ~ "Male",
                                sex=="Female" ~ "Female",
                                sex=="Total" ~ "Total",
                                is.na(sex) ~ "Total"),
       #  quint_type = case_when(is.na(quint_type) ~ "sc_quin", # all have quint_type
       #                         TRUE ~ quint_type),
         split_name = "Deprivation (SIMD)") %>%
  rename(split_value = quintile) 

# find which simd splits don't have total
have_total <- read_parquet("/conf/MHI_Data/Liz/repos/scotpho-profiles-app/shiny_app/data/deprivation_dataset") %>%
  mutate(total_quint = ifelse(quintile=="Total", 1, 0)) %>%
  group_by(year, def_period, code, ind_id, sex, quint_type) %>%
  summarise(has_total = sum(total_quint)) %>%
  ungroup()
# all SIMD data in the deprivation data has quintile==Total


#check what splits are now used:
simd_dataset2_std_split <- simd_dataset2 %>% # 
  select(split_value, split_value2, 
         split_name) %>%
  unique()       


popgroup_dataset_new <- bind_rows(popgroup_dataset_std, totals_4_popgroup_data, simd_dataset2) %>% #42 vars
  arrange(ind_id, year, code, split_name, split_value, split_value2)
## save final file to local repo
write_parquet(popgroup_dataset_new, "shiny_app/data/popgroup_dataset_new")

# # # what are the splits and names now?
popgroup_dataset_new_split <- popgroup_dataset_new %>% # 42 vars
  select(split_value, split_value2, split_name) %>%
  unique()
# write.csv(popgroup_dataset_new_split, "popgroup_dataset_new_split.csv", row.names=FALSE)
