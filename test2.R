
#trying to get reactable to have merged column headers 
#headers with n & %
#still missing the decile 

# Extract column span from D1count to D10percent dynamically
#decile_span <- names(df)[match("D1count", names(df)):match("D10percent", names(df))]

# 1. Pre-build the 'n' and '%' column configurations
all_column_names <- names(df)
metric_columns <- all_column_names[all_column_names != "simd_domain"]

custom_col_defs <- lapply(metric_columns, function(col_name) {
  if (endsWith(col_name, "count")) {
    colDef(name = "n", align = "center",format = colFormat(separators = TRUE))
  } else {
    colDef(name = "%", format = colFormat(digits = 1))
  }
})
names(custom_col_defs) <- metric_columns

final_col_defs <- c(
  list(simd_domain = colDef(name = "SIMD Domain")),
  custom_col_defs
)

# 2. Render table with a styled, blank spacer group above SIMD Domain
reactable(
  df,
  columns = final_col_defs,
  columnGroups = list(
    # FAKE VERTICAL MERGE: Invisible spacer groups above 'simd_domain'
    colGroup(
      name = "", 
      columns = "simd_domain",
      headerStyle = list(backgroundColor = "transparent", borderBottom = "none")
    ),
    
    # Master Headers for metrics
    colGroup(name = "test", columns = decile_span),
    colGroup(name = "Total Summary", columns = c("total_count", "total_percent")),
    
    # Sub-Headers
    colGroup(name = "D1", columns = c("D1count", "D1percent")),
    colGroup(name = "D2", columns = c("D2count", "D2percent")),
    colGroup(name = "D3", columns = c("D3count", "D3percent")),
    colGroup(name = "D4", columns = c("D4count", "D4percent")),
    colGroup(name = "D5", columns = c("D5count", "D5percent")),
    colGroup(name = "D6", columns = c("D6count", "D6percent")),
    colGroup(name = "D7", columns = c("D7count", "D7percent")),
    colGroup(name = "D8", columns = c("D8count", "D8percent")),
    colGroup(name = "D9", columns = c("D9count", "D9percent")),
    colGroup(name = "D10", columns = c("D10count", "D10percent"))
  ),
  bordered = TRUE,
  striped = TRUE,
  highlight = TRUE,
  theme = reactableTheme(
    headerStyle = list(backgroundColor = "#34495e", color = "white", textAlign = "center"),
    groupHeaderStyle = list(backgroundColor = "#2c3e50", color = "white", textAlign = "center")
  )
)