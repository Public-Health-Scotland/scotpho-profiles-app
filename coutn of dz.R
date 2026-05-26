# 
demographic_simd_dataset <- setDT(read_parquet("project_folder/demographic_simd_dataset.parquet")) # dataset behind demographics tab SIMD populations visualisations


input_period_filter <-2023
input_areaname <- "Scotland"
input_areatype <-  "Scotland"
input_pop_grp <- "dz"

input_demog_type <- "percent"

#filter data for time period and area
demog_simd_data  <- demographic_simd_dataset |>
  select(-total_dz) |>
  filter(year== input_period_filter & areaname == input_areaname & areatype == input_areaname)|>
  filter(grepl(input_pop_grp,population_group2))


# select columns for percent or count
if(input_demog_type == "count"){
  demog_simd_data <- demog_simd_data |>
    select(simd_domain, ends_with("count"))
} else {
  demog_simd_data <- demog_simd_data |>
    select(simd_domain, ends_with("percent"))
}


#function that detect which column names to expect (count or percent)

# custom_col_defs <-
# 
# 
# all_column_names <- names(demog_simd_data )
# metric_columns <- all_column_names[all_column_names != "simd_domain"]
# 
# custom_col_defs <- lapply(metric_columns, function(col_name) {
#   if (endsWith(col_name, "count")) {
#     colDef(name = "n", align = "center",format = colFormat(separators = TRUE))
#   } else {
#     colDef(name = "%", format = colFormat(digits = 1))
#   }
# })
# names(custom_col_defs) <- metric_columns



#count of datazones by decile for a particular geography
reactable(demog_simd_data,  
          
          columns = list(
            simd_domain = colDef(name = "SIMD Domain",      
                                 style = list(borderRight = "1px solid #eee"),
                                 headerStyle = list(borderRight = "1px solid #eee")),
            D1count=colDef(name = "1", format = colFormat(digits = 0,separators = TRUE)), #ideally want formatting for pops to be 0dp with separator and percent to be % with 1dp
            D2count=colDef(name = "2",format = colFormat(digits = 0,separators = TRUE)),
            D3count=colDef(name = "3",format = colFormat(digits = 0,separators = TRUE)),
            D4count=colDef(name = "4",format = colFormat(digits = 0,separators = TRUE)),
            D5count=colDef(name = "5",format = colFormat(digits = 0,separators = TRUE)),
            D6count=colDef(name = "6",format = colFormat(digits = 0,separators = TRUE)),
            D7count=colDef(name = "7",format = colFormat(digits = 0,separators = TRUE)),
            D8count=colDef(name = "8",format = colFormat(digits = 0,separators = TRUE)),
            D9count=colDef(name = "9",format = colFormat(digits = 0,separators = TRUE)),
            D10count=colDef(name = "10",format = colFormat(digits = 0,separators = TRUE)),
            total_count=colDef(name = "Total", format = colFormat(digits = 0,separators = TRUE))),
          
          columnGroups = list(
            colGroup("Population weighted deprivation decile", columns = c("D1count", "D2count", "D3count","D4count",
                                                                           "D5count","D6count","D7count","D8count","D9count","D10count"))),
          bordered = TRUE,
          striped = TRUE,
          highlight = TRUE,
          theme = reactableTheme(
            headerStyle = list(backgroundColor = "#3F3685", color = "white"),
            groupHeaderStyle = list(backgroundColor = "#3F3685", color = "white", textAlign = "center"))
          )
            


