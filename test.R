# 
demographic_simd_dataset <- setDT(read_parquet("project_folder/demographic_simd_dataset.parquet")) # dataset behind demographics tab SIMD populations visualisations



input_period_filter <-2023
input_areaname <- "Scotland"
input_areatype <-  "Scotland"
input_pop_grp <- "dz"
input_demog_type <- "percent"

demog_simd_data  <- demographic_simd_dataset |>
  select(-total_dz) |>
  filter(year== input_period_filter & areaname == input_areaname & areatype == input_areaname)|>
  filter(grepl(input_pop_grp,population_group2))


# filter by quint type 
if(input_demog_type == "count"){
  demog_simd_data <- demog_simd_data |>
    select(simd_domain, ends_with("count"))
} else {
  demog_simd_data <- demog_simd_data |>
    select(simd_domain, ends_with("percent"))
}



data <- switch(input_demog_type

if (input_measure_type == "count")
demog_simd_data <- demog_simd_data |>
  select(simd_domain,D1count:total_percent)


###########################################


reactable(
  demog_simd_data ,
  defaultColDef = colDef(
    minWidth = 75,
    name = function(column_name) {
      if (endsWith(column_name, "count")) {
        return("n")
      } else if (endsWith(column_name, "percent")) {
        return("b")
      } else {
        return(column_name) # Fallback for other columns
      }
    },
    cell = function(value, index, name) {
      if (endsWith(name, "count")) format(value, big.mark = ",") else format(value, nsmall = 1)
    }
  ), 

  columnGroups = list(
    # Top Master Headers
  #  colGroup(name = "Deprivation Decile", columns = names(df)[startsWith(names(df), "D")]),
  #  colGroup(name = "Deprivation Decile", columns = names(df)),
   # colGroup(name = "Deprivation Decile", columns = names(df)),
  #  colGroup(name = "Total", columns = c("total_count", "total_percent")),
    
    # Sub-Headers (Deciles 1 to 10)
    colGroup(name = "1", columns = c("D1count", "D1percent")),
    colGroup(name = "2", columns = c("D2count", "D2percent")),
    colGroup(name = "3", columns = c("D3count", "D3percent")),
    colGroup(name = "4", columns = c("D4count", "D4percent")),
    colGroup(name = "5", columns = c("D5count", "D5percent")),
    colGroup(name = "6", columns = c("D6count", "D6percent")),
    colGroup(name = "7", columns = c("D7count", "D7percent")),
    colGroup(name = "8", columns = c("D8count", "D8percent")),
    colGroup(name = "9", columns = c("D9count", "D9percent")),
    colGroup(name = "10", columns = c("D10count", "D10percent"))
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  theme = reactableTheme(
    headerStyle = list(backgroundColor = "#34495e", color = "white"),
    groupHeaderStyle = list(backgroundColor = "#2c3e50", color = "white")
  )
)




# 1. Dynamically build the exact list of column definitions for your 22 columns
# We can use lapply here safely since it outputs an explicitly named list of colDefs
all_column_names <- names(df)
metric_columns <- all_column_names[all_column_names != "simd_domain"]

custom_col_defs <- lapply(metric_columns, function(col_name) {
  if (endsWith(col_name, "count")) {
    colDef(
      name = "n",  # Pure string, no functions
      minWidth = 75,
      align = "center",
      format = colFormat(separators = TRUE)
    )
  } else {
    colDef(
      name = "%",  # Pure string, no functions
      minWidth = 75,
      align = "center",
      format = colFormat(digits = 1)
    )
  }
})
names(custom_col_defs) <- metric_columns

# 2. Append your metadata identifier column to the list
final_col_defs <- c(
  list(simd_domain = colDef(name = "SIMD Domain")),
  custom_col_defs
)

# 3. Dynamic target span for the master header
decile_span <- names(df)[match("D1count", names(df)):match("D10percent", names(df))]

# 4. Render the table
reactable(
  df,
  columns = final_col_defs, # Passes the pre-named list here
  columnGroups = list(
    # Master Headers
    colGroup(name = "Deprivation Deciles", columns = decile_span),
    colGroup(name = "Total Summary", columns = c("total_count", "total_percent")),
    
    # Sub-Headers
    colGroup(name = "1", columns = c("D1count", "D1percent")),
    colGroup(name = "2", columns = c("D2count", "D2percent")),
    colGroup(name = "3", columns = c("D3count", "D3percent")),
    colGroup(name = "4", columns = c("D4count", "D4percent")),
    colGroup(name = "5", columns = c("D5count", "D5percent")),
    colGroup(name = "6", columns = c("D6count", "D6percent")),
    colGroup(name = "7", columns = c("D7count", "D7percent")),
    colGroup(name = "8", columns = c("D8count", "D8percent")),
    colGroup(name = "9", columns = c("D9count", "D9percent")),
    colGroup(name = "10", columns = c("D10count", "D10percent"))
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  theme = reactableTheme(
    headerStyle = list(backgroundColor = "#34495e", color = "white", textAlign = "center"),
    groupHeaderStyle = list(backgroundColor = "#2c3e50", color = "white", textAlign = "center")
  )
)

  
  



  

#count of datazones by decile for a particular geography
reactable(demog_simd_data,          
          #defaultExpanded = TRUE, #set default expanded groups (not required if table isn't grouped)
          pagination = FALSE,
          # defaultPageSize = nrow(data),
          # groupBy = c("simd_domain"),
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
            total_count=colDef(format = colFormat(digits = 0,separators = TRUE))),
          columnGroups = list(
            colGroup("Population weighted deprivation decile", columns = c("D1count", "D2count", "D3count","D4count","D5count","D6count","D7count","D8count","D9count","D10count"))
          ))




