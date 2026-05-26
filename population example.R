demographic_simd_dataset <- setDT(read_parquet("project_folder/demographic_simd_dataset.parquet")) # dataset behind demographics tab SIMD populations visualisations


input_period_filter <-2023
input_areaname <- "Scotland"
input_areatype <-  "Scotland"
input_pop_grp <- "all"


input_demog_type <- "percent"

#filter data for time period and area
demog_simd_data  <- demographic_simd_dataset |>
  filter(year== input_period_filter & areaname == input_areaname & areatype == input_areaname)|>
  filter(grepl(input_pop_grp,population_group2))

demog_simd_data  <- demog_simd_data  |>
  mutate('1'=paste0(D1count," (",D1percent,"%)"),
         '2'=paste0(D2count," (",D2percent,"%)"),
          '3'=paste0(D3count," (",D3percent,"%)"),
          '4'=paste0(D4count," (",D4percent,"%)"),
          '5'=paste0(D5count," (",D5percent,"%)"),
          '6'=paste0(D6count," (",D6percent,"%)"),  
          '7'=paste0(D7count," (",D7percent,"%)"),  
          '8'=paste0(D8count," (",D8percent,"%)"),  
          '9'=paste0(D9count," (",D9percent,"%)"),  
          '10'=paste0(D10count," (",D10percent,"%)"),
          'Total'=paste0(total_count," (",total_percent,"%)"))|>
    select(simd_domain,'1':'Total')

#count of datazones by decile for a particular geography

reactable(demog_simd_data,  
        defaultColDef = colDef(align = "center"),
            
        columns = list(
        simd_domain = colDef(name = "SIMD Domain", align= "left")
        
        ),

        columnGroups = list(
          colGroup("Population weighted deprivation decile", columns = c("1","2","3","4","5","6","7","8","9","10"))),
          bordered = TRUE,
          striped = TRUE,
          highlight = TRUE,
          theme = reactableTheme(
            headerStyle = list(backgroundColor = "#3F3685", color = "white", textAlign="left"),
            groupHeaderStyle = list(backgroundColor = "#3F3685", color = "white", textAlign = "center"))
)

