
###############################################.
## Packages/Filepaths ----
###############################################.
library(dplyr)
library(tidyr) # long to wide format
library(httr) # api connection
library(jsonlite)  # transforming JSON files into dataframes
library(readr)
library(highcharter)
#library(data.table)
#library(phsstyles)
library(htmlwidgets)


# Filepaths 
pop_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Population/"
geo_lookup <- "/PHI_conf/ScotPHO/Profiles/Data/Lookups/Geography/"


#open population data 
pops_file <- readRDS(file=paste0(pop_lookup, 'testfile_population_breakdown_wide.rds'))

# # setting some parameters
# dz = "DZ11"
# lower = 0 
# upper = 200
# name = "pop_popgrp"

#need to convert age_gro to meaningful age categories
data <- pops_file  |>
  mutate(age = case_when(age_grp == 1 ~ "0-4",
                         age_grp == 2   ~ "5-9",
                         age_grp == 3   ~ "10-14",
                         age_grp == 4   ~ "15-19",
                         age_grp == 5   ~ "20-24",
                         age_grp == 6   ~ "25-29",
                         age_grp == 7   ~ "30-34",
                         age_grp == 8   ~ "35-39",
                         age_grp == 9   ~ "40-44",
                         age_grp == 10  ~ "45-49",
                         age_grp == 11  ~ "50-54",
                         age_grp == 12  ~ "55-59",
                         age_grp == 13  ~ "60-64",
                         age_grp == 14  ~ "65-69",
                         age_grp == 15  ~ "70-74",
                         age_grp == 16  ~ "75-79",
                         age_grp == 17  ~ "80-84",
                         age_grp == 18  ~ "85-89",
                         age_grp == 19  ~ "90+", TRUE ~"other")) |>
  filter(code=="S00000001",
         year==2023)


#set up population pyramid

pyramid <- highchart() %>%
  hc_chart(object = data,type = "bar") %>%
  hc_plotOptions(series = list(stacking = "normal",
                               grouping = FALSE,
                               pointPadding = 0, # Smaller value = fatter bars
                               groupPadding = 0)) |>  # Smaller value = fatter bars
  hc_xAxis(categories = data$age, 
           title = list(text = "Age Group (years)"),
           reversed=FALSE) %>% #reversing axis means that lower ages at the bottom rather than top
  
  # Add title
  hc_title(text = "Population Pyramid") |>
  hc_subtitle(text = "a subtitle - link to nrs?") |>
  
  # Add Series (mapping additional population and year columns which appear in tooltip alongside the % of population)
  hc_add_series(name = "Male", data = data,type = "bar", hcaes(x = age, y = percentage_Male, pop_value = population_Male, year=year)) %>%
  hc_add_series(name = "Female", data = data,type = "bar", hcaes(x = age, y = percentage_Female, pop_value = population_Female, year=year)) %>%

  # Tooltip
  hc_tooltip(
    shared = TRUE, # set to true to ensure both male and female values appear for each age category
    formatter = JS("function() {
      var s = '<b>Age: ' + this.x + '</b>';
      $.each(this.points, function(i, point) {
        var absVal = Math.abs(point.y);
        s += '<br/>' + this.point.year + ' ' +
        point.series.name + ' Population: '+  Highcharts.numberFormat(this.point.pop_value,0,',') +
        ': ('+ Highcharts.numberFormat(absVal, 1) + '%)';
      });
      return s;
    }")
  ) %>%

  # Format Y-Axis (% population)
  hc_yAxis(
    min = -5,           # Fixed start
    max = 5,            # Fixed end (must match min to be centered)
    tickInterval = 1,   # Distance between labels
    labels = list(formatter = JS("function() { return Math.abs(this.value); }")), #ensure axis labels show absolute values not negatives for the males
    title = list(text = "Percentage of Population")
  ) 
  
pyramid
  
