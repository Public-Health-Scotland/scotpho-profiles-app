update_profiles_lookup <- function(techdoc){
  
  # get unique indicators
  indicators <- techdoc |>
    select(indicator_name, ind_id, active, profile_domain) |>
    unique()
  
  # pivot data longer by profile/domain 
  # so there's so more than 1 row per indicator if belongs to more than 1 profile/domain
  # and separate profile and domain into 2 different columns
  profiles_lookup <- indicators |>
    separate_rows(profile_domain, sep = ";") |>
    filter(profile_domain != "") |>
    separate(profile_domain, into = c("profile", "domain"), sep = "-")
  
  
  
  # create profile name column 
  profiles_lookup <- profiles_lookup |>
    mutate(profile_name = case_when(profile == "POP" ~ "Population",
                                    profile == "TOB" ~ "Tobacco",
                                    profile == "MEN" ~ "Adult Mental Health",
                                    profile == "ALC" ~ "Alcohol",
                                    profile == "DRG" ~ "Drugs",
                                    profile == "CWB" ~ "Population Health",
                                    profile == "HWB" ~ "Health & Wellbeing",
                                    profile == "CYP" ~ "Children & Young People",
                                    profile == "CMH" ~ "Children & Young People Mental Health",
                                    profile == "PAO" ~ "Physical Activity", 
                                    TRUE ~ "other"))
  
    # replace domain name with 'Archived indicators' if it's not active
    profiles_lookup <- profiles_lookup |>
      mutate(domain = if_else(active == "AR", domain, "Archived indicators"))
    
    
    # select final columns 
    profiles_lookup <- profiles_lookup |>
      select(ind_id, indicator = indicator_name, profile_name, domain)
   
    return(profiles_lookup)
  
}