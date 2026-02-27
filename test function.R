# pyramid <- highchart() %>%
#   hc_chart(object = data,type = "bar") %>%
#   hc_plotOptions(series = list(stacking = "normal",
#                                grouping = FALSE,
#                                pointPadding = 0, # Smaller value = fatter bars
#                                groupPadding = 0)) |>  # Smaller value = fatter bars
#   hc_xAxis(categories = data$age, 
#            title = list(text = "Age Group (years)"),
#            reversed=FALSE) %>% #reversing axis means that lower ages at the bottom rather than top
#   
#   # Add title
#   hc_title(text = "Population Pyramid") |>
#   hc_subtitle(text = "a subtitle - link to nrs?") |>
#   
#   # Add Series (mapping additional population and year columns which appear in tooltip alongside the % of population)
#   hc_add_series(name = "Male", data = data,type = "bar", hcaes(x = age, y = percentage_Male, pop_value = population_Male, year=year)) %>%
#   hc_add_series(name = "Female", data = data,type = "bar", hcaes(x = age, y = percentage_Female, pop_value = population_Female, year=year)) %>%
#   
#   # Tooltip
#   hc_tooltip(
#     shared = TRUE, # set to true to ensure both male and female values appear for each age category
#     formatter = JS("function() {
#       var s = '<b>Age: ' + this.x + '</b>';
#       $.each(this.points, function(i, point) {
#         var absVal = Math.abs(point.y);
#         s += '<br/>' + this.point.year + ' ' +
#         point.series.name + ' Population: '+  Highcharts.numberFormat(this.point.pop_value,0,',') +
#         ': ('+ Highcharts.numberFormat(absVal, 1) + '%)';
#       });
#       return s;
#     }")
#   ) %>%
#   
#   # Format Y-Axis (% population)
#   hc_yAxis(
#     min = -5,           # Fixed start
#     max = 5,            # Fixed end (must match min to be centered)
#     tickInterval = 1,   # Distance between labels
#     labels = list(formatter = JS("function() { return Math.abs(this.value); }")), #ensure axis labels show absolute values not negatives for the males
#     title = list(text = "Percentage of Population")
#   ) 
# 
# pyramid


create_pyramid_chart <- function(data){
  
  hc <- hchart(data,
              type = "bar") %>%
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
  
  #return chart
  hc
  
  
}




##Plan to convert these into a table showing count of datazone in each of the 5 deprivation quintiles for all domains (including overall) 

pop_simd_centile<- demographic_simd_dataset |>
  filter(areatype == "Scotland" & areaname =="Scotland")|>
  filter(year==2023) |>
  filter(simd_domain=="overall") |>
  filter(centile_type=="quintile")


pop_simd_centile_iz<- demographic_simd_dataset |>
  #filter(areatype == "Intermediate zone" & areaname =="Culter")|>
  filter(areatype == "Council area" & areaname =="Aberdeen City")|>
  #filter(year==2020) |>
  filter(centile_type=="quintile")|>
  group_by(year,code,simd_domain, areatype, areaname,centile_type,parent_area,areaname_full) |>
  ungroup()|>
  complete(quintile = c("1","2","3","4","5"), fill = list(pop_all_ages = 0))|>
  mutate(quintile=paste0("Q",quintile))
  
pop_simd_centile_iz_rearrange <-pop_simd_centile_iz |>
  select(-dz_count)|>
  pivot_longer(cols = c("pop_all_ages", "pop_u26", "pop_working","percent_all","percent_u26","percent_working"),
               names_to = "population_group",
               values_to = "population")|>
  pivot_wider(names_from = "quintile",
                values_from = "population") |>
  #mutate(across(c(Q1:Q5), ~ replace_na(.x, 0)))|>
  #ungroup() |>
  mutate(Total = rowSums(across(c(Q1,Q2,Q3,Q4,Q5)), na.rm = TRUE),
         measure_type=case_when(substr(population_group,1,3) =="pop" ~ "count",
                                substr(population_group,1,3) =="per" ~ "(%)",
                                TRUE ~ "other"))


    

# need to filter for single year 
table_data<-pop_simd_centile_iz_rearrange |>
  filter(year==2020)|>
  filter(population_group=="pop_all_ages") |>
  select(simd_domain,measure_type,population_group,Q1,Q2,Q3,Q4,Q5,Total)
  


#table showing population count and percent of population by each simd quintile grouped by simd domain
reactable(data=table_data,
  defaultExpanded = TRUE,
  #defaultPageSize = nrow(data),
  groupBy = "measure_type",
  columns = list(
    simd_domain = colDef(name = "SIMD Domain",      
                         style = list(borderRight = "1px solid #eee"),
                         headerStyle = list(borderRight = "1px solid #eee")),
    Q1=colDef(format = colFormat(digits = 0,separators = TRUE)), #ideally want formatting for pops to be 0dp with separator and percent to be % with 1dp
    Q2=colDef(format = colFormat(digits = 0,separators = TRUE)),
    Q3=colDef(format = colFormat(digits = 0,separators = TRUE)),
    Q4=colDef(format = colFormat(digits = 0,separators = TRUE)),
    Q5=colDef(format = colFormat(digits = 0,separators = TRUE)),
    Total=colDef(format = colFormat(digits = 0,separators = TRUE))
    ),
  columnGroups = list(
    #colGroup("", columns = c("population_group", "simd_domain")),
    colGroup("Deprivation Quintile (population weighted)", columns = c("Q1", "Q2", "Q3","Q4","Q5"))
  ))
    
  



#charting  

# would need to create input fitlers for population group and simd domain  
trend_chart_data<-pop_simd_centile_iz_rearrange |>
  filter(population_group=="pop_all_ages") |>
  filter(simd_domain=="overall")

data<-trend_chart_data

hc <- 
  
  hchart()|>
  hc_chart(object = data,type = "line") %>%
  #hcaes(x = .data[[xaxis_col]], y = .data[[yaxis_col]], group = .data[[grouping_col]]), 
  hc_xAxis(categories = data$year)|>
  hc_add_series(data = data$Q1, name = "Q1 Most Deprived",color = "#1f77b4") %>%
  # Add the second column
  hc_add_series(data = data$Q2, color = "#DFDDE3") |> 
  hc_add_series(data = data$Q3, color = "#DDDDE3") |>
  hc_add_series(data = data$Q4, color = "#DFDDE3") |> 
  hc_add_series(data = data$Q5, name = "Q5 Least Deprived",color = "#9b4393")


  marker = list(enabled = TRUE)) |>
  hc_colors(colours) |>
  hc_xAxis(title = "") |>
  hc_yAxis(title = "") |>
  hc_chart(marginRight = 15) |>
  hc_legend(verticalAlign = legend_position, align = "left") |>
  hc_tooltip(crosshairs = TRUE, shared = TRUE)



highchart() %>%
  hc_chart(type = "line") %>%
  hc_xAxis(categories = data$date_column) %>%
  # Add the first column
  hc_add_series(
    data = data$column1, 
    name = "Series A", 
    color = "#1f77b4"
  ) %>%
  # Add the second column
  hc_add_series(
    data = data$column2, 
    name = "Series B", 
    color = "#ff7f0e"
  ) %>%
  hc_tooltip(shared = TRUE

