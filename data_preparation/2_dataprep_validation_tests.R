
######################################################################################################################################.
# Data validation tests run against data file prepared for shiny app   ----
# These automated tests look for errors that have occurred within indicator dataframes that can be hard to spot
# Checks also use the techdoc as a reference looking for missing indicators or metadata

######################################################################################################################################.

## Test 1: Ensure no 2 files contain the same unique indicator ID
TEST_no_duplicate_ids <- function(data) {
  
  data <- data |> 
    distinct(ind_id, file_name) |>
    group_by(ind_id) |>
    add_tally() |>
    filter(n > 1)
  
  assert_that(nrow(data) == 0, 
              msg = paste0("The same indicator ID was found in more than 1 data file.", 
                           "\nThis could be because the previous file for this indicator was named slightly differently, and therefore wasn't overwritten OR because the wrong indicator ID has accidentally been used for a particular indicator.\n 
                Check the following files in the shiny folder: \n \n", 
                           paste(data$file_name, collapse = "\n")))
}


## Test 2: Ensure there are no indicator data files missing 
TEST_no_missing_indicators <- function(data) {
  
  data <- anti_join(technical_doc, data, by = c("ind_id"))
  
  assert_that(nrow(data) == 0, 
              msg = paste0("\nThe number of indicators in the dataset created DOES NOT match the number of indicators listed as 'Active' in the technical document. \n",
                           "This could be because there are indicators incorrectly listed as 'Active' in the technical document OR because there are data files missing from the shiny folder\n",
                           "Check the following indicator(s): \n",
                           paste(paste0("• ", data$indicator), collapse = "\n"))
  )
  
}


## Test 3: Ensure there are no indicators with missing metadata
TEST_no_missing_metadata <- function(data) {
  
  data <- data |>
    filter(is.na(indicator))
  
  assert_that(nrow(data) == 0, 
              msg = paste0("\n Metdata was not successfully joined for the following indicator(s) \n",
                           paste(paste0("• ", unique(data$ind_id)), collapse = "\n"))
  )
  
}


## Test 4: Ensure there are no indicators with missing geography info
TEST_no_missing_geography_info <- function(data) {
  
  data <- data |>
    filter(is.na(areaname_full))
  
  assert_that(nrow(data) == 0, 
              msg = paste0("\n The following geography code(s) were not found in the geography lookup, either because they are invalid OR because they are 'old' codes no longer in use\n",
                           paste(paste0("• ", unique(data$code)), collapse = "\n")
              )
  )
  
}


## Test 5: Ensure suppression has been applied
TEST_suppression_applied <- function(data) {
  
  # find indicators in tech doc requiring suppression and they're suppression threshhold
  techdoc <- technical_doc |>
    select(indicator_name, ind_id, supression, supress_less_than)
  
  # join information onto dataset, filter on indicator requiring suppression
  # and check if there are any rows where the numerator is less than the suppression threshold
  # i.e. if threshold is 10, should be no numerator figures less than 10.
  data <- data |>
    #   left_join(techdoc, by = c("indicator" = "indicator_name")) |>
    left_join(techdoc, by = "ind_id") |> #safer (because "Persistent poverty" was included twice, with slightly different breakdowns, so gave error if joined using name, but not using ind_id. Obviously we want to avoid having indicators with the same name too though... (I've renamed the MH indicator now, but still think this logic makes sense))
    filter(supression == "Y") |>
    subset(numerator < supress_less_than)
  
  
  assert_that(nrow(data) == 0, 
              msg = paste0("SUPPRESSION NOT APPLIED! Please run the suppression function before saving this data.\n",
                           "The following indicators still contain numbers that shouldn't be there:\n",
                           paste(paste0("• ", data$indicators), collapse = "\n"))
  )
  
}


## Test 6: Ensure there are no inequalities indicators missing
TEST_no_missing_ineq_indicators <- function(data) {
  
  # find indicators from tech doc with an inequalities label
  depr_indicators <- technical_doc |>
    filter(!is.na(label_inequality))
  
  # check what indicators are in deprivation dataset
  data <- data |>
    select(indicator, ind_id) |>
    unique()
  
  # check which are missing
  data <- anti_join(depr_indicators, data, by = c("ind_id"))
  
  assert_that(nrow(data) == 0, 
              msg = paste0(
                "\nThe number of indicators DOES NOT match the number of indicators in the technical doc with an inequalities label assigned to them. This could be because they should not be produced at this level OR because there are data files missing from the shiny folder\n",
                "Check the following indicator(s): \n",
                paste(paste0("• ", data$indicator), collapse = "\n"))
  )
  
  
}

# Test 7: Ensure deprivation files go up to the correct year
TEST_inequalities_trends <- function(data) {
  
  # get list of main indicators and they're max year
  main_indicators <- main_dataset |> 
    group_by(ind_id) |>
    summarise(year = max(year),
              ind_id = first(ind_id),
              indicator = first(indicator)) |>
    ungroup()
  
  
  # get list of inequalities indicators and they're max year
  depr_indicators <- data |> 
    group_by(ind_id) |>
    summarise(year = max(year),
              ind_id = first(ind_id),
              indicator = first(indicator)) |>
    ungroup() 
  
  # get indicators where max years don't match
  data <- left_join(depr_indicators, main_indicators, by = c("ind_id")) |>
    filter(year.x != year.y)
  
  
  assert_that(nrow(data) == 0, 
              msg = paste0("The max year in the inequalities dataset is not the same as the max year in the main dataset for the indicators listed below.This could be because the deprivation function wasn't updated/run when refreshing these indicators, 
                           or because the new inequalities file wasn't moved across to the shiny folder \n", 
                           paste("max year for ", paste0(data$indicator.x, " should be: ", data$year.y, " but is ", data$year.x), collapse = "\n"))
  )
  
}