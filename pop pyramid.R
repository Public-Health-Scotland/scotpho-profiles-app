#https://www.highcharts.com/demo/highcharts/bar-negative-stack

# testing script to create a population pyramid that could be useful in redesigned population profile
# also thinking about indicators around ethnicity

#inserted line in global script so when you  run the app this file is available for testing purposes
#pops_file <- readRDS("data/testfile_population_breakdown_wide.rds") # full populations file created as an output from lookups repo (populations)


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


# setting some parameters
#create_pop(
dz = "DZ11"
lower = 0 
upper = 200
name = "pop_popgrp"



###############################################.
# This functions takes a basefile (DZ01 or DZ11), selects the age of interest and
# produces files for % and crude rates and for standard rates. It also allows you
# to create files only with council and higher geographies.
# Lower and upper are the age limits, name is the name of the output file,
# dx is what datazone you are using, council if you want council files and
# stdrate if you want pops used  for standarized rates
#create_pop <- function(lower, upper, name, dz) {

###############################################.
# Creating files for percentages crude rates 
#Reading file, aggregating and saving file for % and crude rates

# if(name="pop_grp"){

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
  
# 
# populations <- data_pop |>
#   # filter(code=="S00000001",
#   #        year==2023) |>
#   group_by(code,year) |>
#   mutate(pop_sum=sum(population),
#          sex_grp=case_when(sex_grp=="1" ~ "Male",sex_grp=="2" ~"Female", TRUE ~"other")) |>
#   ungroup()|> #ungroup to allow % with male/female within geocode
#   mutate(percentage=population/pop_sum*100) |>
#   pivot_wider(names_from = sex_grp, values_from = c(percentage,population))|>
#   mutate(percentage_Male=0-percentage_Male)


# data <- populations |>
#   filter(code=="S02001280",
#          year==2022)

#need axis labels
#reduce bar gap/padding
#tool tips
#age group labels
#colour scheme

x <-  hchart(object = data, 
             type = "bar", hcaes(x = age, y = percentage_Male)) |>
  hc_add_series(data, type = "bar", hcaes(x = age, y = percentage_Female))|>
  hc_plotOptions(stacking = "normal",
                 series = list(grouping = FALSE))|> # This allows the bars to overlap
  hc_title(text = "Population Pyramid") |>
  hc_subtitle(text = "a subtitle - link to nrs?") |>
  hc_xAxis(reversed=FALSE, #reversing axis means that lower ages at the bottom rather than top
    opposite=TRUE)|> #opposite means the axis label on right side of chart (not sure if we want left/right or both)
  hc_xAxis(title = list(text = "")) |> #removes the 'age' as title of x-axis
  hc_yAxis(title = list(text = "Percent of total population"),
           labels = list(
             # Use JavaScript Math.abs() to format labels so that male percentages show absolute numbers not negatives
             formatter = JS("function() { return Math.abs(this.value); }"))) |>
  hc_plotOptions(
    bar = list(
      pointPadding = 0, # Controls padding around a single bar (0 = no gap, 1 = full gap)
      groupPadding = 0  # Controls padding around groups of bars (0 = no gap, 1 = full gap)
    ))
x
           
           
           ) |> #removes the 'age' as title of x-axis
  hc_yAxis(labels=format(abs()))




  # hc_tooltip(
  #   headerFormat = "<table>",
  #   pointFormat = paste(
  #     # '<tr><th colspan="2"><p>{point.areaname}</p></th></tr>',
  #     # "<tr><th>{point.type_definition}</th><td>{point.measure}</td></tr>",
  #     "<tr><th>age:</th><td>{point.age}</td></tr>",
  #     "<tr><th>:</th><td>{point.age}</td></tr>",
  #     
  #     "<tr><th>code:</th><td>{point.code}</td></tr>"))

   # hc_yAxis(rangeDescription: 'Range: 0 to 5%')
#

x



labels = list(
  # Use JavaScript Math.abs() to format labels
  formatter = JS("function() { return Math.abs(this.value); }")
)





hc_xAxis(title = list(text = "")) |>
  hc_yAxis(title = list(text = "")) |>






# create multi-line chart 
hc <- hchart(
  data,
  "bar", 
  hcaes(x =age_grp, y = percentage_Male) |>
    hc_series(x =age_grp, y = percentage_Female))

hc

hc<- hchart (data,
             hc_chart(type = "bar") %>% 
               hc_title(text = "MyGraph") %>% 
               hc_plotOptions(column = list(
                 dataLabels = list(enabled = FALSE),
                 stacking = "normal",
                 enableMouseTracking = FALSE) %>% 
                   hc_series(list(name="A",data=mydata$percentage_Male),
                             list(name="B",data=mydata$percenteage_Female))
                 
                 
                 hc_plotOptions(stacking=normal))
             
             hc
             hc_series(list(name="A",data=mydata$A),
                       list(name="B",data=mydata$B),
                       
                       marker = list(enabled = TRUE)) |>
               hc_xAxis(categories = unique(data[[xaxis_col]]), title = "") |>
               hc_colors(colours) |>
               hc_xAxis(title = "") |>
               hc_yAxis(title = "") |>
               hc_chart(marginRight = 15) |>
               hc_legend(verticalAlign = legend_position, align = "left") |>
               hc_tooltip(crosshairs = TRUE, shared = TRUE)
             
             
             
             
             
             type = "bar", hcaes(x = areaname, y = measure, color = colour_pal)) |>
  
  
  
  
  
  
  
  
  
  
  
  
  
  } else{
    
    
    data_dz <- readRDS(file=paste0(pop_lookup, "basefile_", dz, ".rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code) %>% summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_dz, file=paste0(pop_lookup, dz, '_', name,'.rds'))
    
    # Creating file for indicators that need council as base
    data_ca <- data_dz %>% subset(substr(code,1,3) %in% c('S00', 'S08', 'S12', "S11", "S37", "S32"))
    
    saveRDS(data_ca, file=paste0(pop_lookup, 'CA_', name,'.rds'))
    
    ###############################################.
    # Creating files for standardized rates
    data_dz_sr <- readRDS(file=paste0(pop_lookup, "basefile_", dz, ".rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, sex_grp, age_grp) %>% #aggregating
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_dz_sr, file=paste0(pop_lookup, dz, '_', name,'_SR.rds'))
    
    # Creating file for indicators that need council as base
    data_ca_sr <- data_dz_sr %>% subset(substr(code,1,3) %in%  c('S00', 'S08', 'S12', "S11", "S37", "S32"))
    
    saveRDS(data_ca_sr, file=paste0(pop_lookup, 'CA_', name,'_SR.rds'))
    
    ###############################################.
    # Creating files for deprivation cases
    data_depr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, quintile, quint_type) %>% 
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_depr, file=paste0(pop_lookup, 'depr_', name,'.rds'))
    
    # Creating files for standardized rates
    data_depr_sr <- readRDS(file=paste0(pop_lookup, "basefile_deprivation.rds")) %>% 
      subset(age >= lower & age <= upper) %>% #selecting age of interest
      group_by(year, code, sex_grp, age_grp, quintile, quint_type) %>% 
      summarise(denominator=sum(denominator)) %>% ungroup()
    
    saveRDS(data_depr_sr, file=paste0(pop_lookup, 'depr_', name,'_SR.rds'))
    
  }