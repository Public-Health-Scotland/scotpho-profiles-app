
## Deprivation Dataset ----

update_deprivation_data <- function() {
  
  ## generate list deprivation data files ----
  files_depr <- list.files(
    path = shiny_files, 
    pattern = "*_ineq.rds", 
    full.names = TRUE
  )
  
  ## combine into one deprivation dataset
  data_depr <- combine_files(files_depr) #call to one of the generic functions that combines files from list
  
  # for now skip loading andy pulford inequality indicators
  
  # attach technical document info ----
  data_depr <- left_join(x = data_depr, y = technical_doc, by = "ind_id")
  
  # attach geography info ----
  data_depr <- data_depr |>
    replace_old_geography_codes(col_name = "code") |>
    left_join(geography_lookup, by = "code")
  
  # formatting numbers ----
  data_depr <- data_depr |>
    mutate(quintile = recode(quintile,"1" = "1 - most deprived","5" = "5 - least deprived")) |>
    mutate_at(c("numerator", "measure", "lowci", "upci", "rii", "upci_rii",
                "lowci_rii", "sii", "lowci_sii", "upci_sii", "par", "abs_range",
                "rel_range", "rii_int", "lowci_rii_int", "upci_rii_int"),round, 1)
  
  
  ## apply suppression function ----
  data_depr <- data_depr |>
    apply_suppressions()
  
  
  ## create new fields ----
  data_depr <- data_depr |>
    group_by(ind_id, year, quint_type, code) |>
    # label if par, sii or rii  positive or negative (helps with health inequality dynamic summary text)
    mutate(across(c(sii, rii, par),
             ~ case_when(. > 0 ~ "positive", . < 0 ~ "negative",  . == 0 ~ "zero"),
             .names = "{.col}_gradient")) |>
    mutate(
      qmax = quintile[which.max(measure)], # which quintile contains highest rate/value
      qmin = quintile[which.min(measure)] # which quintile contains lowest rate/value
    ) |>
    ungroup() |>
    rename(indicator = indicator_name)
  
  # deselect columns not required and rename
  data_depr <<- data_depr 
  
  #|> # pass dataset from function so visible in data panel
    #select(-c("file_name", "parent_area", "areaname_full")) |>
  #  rename(indicator = indicator_name)
  
  
  #write parquet file to shiny app data folder
  write_parquet(data_depr, "shiny_app/data/deprivation_data")
  
}