update_techdoc<- function(techdocs_list){
  
  # combine techdocs to create one "master copy" of metadata 
  # for all indicators in the dashboard
  techdocs_combined <- list_rbind(techdocs_list)
  
  
  # check if same ind_id exists in more than 1 techdoc: ind_ids should be 
  # unique to each indicator, not just indicators within a specific techdocs!
  duplicate_inds <- techdocs_combined |>
    group_by(ind_id) |>
    filter(n() > 1)
  
  
  # remove columns not required
  techdocs_combined <- techdocs_combined |>
    select(-c(supress_less_than, techdoc_path))
  
  
  # if no duplicates found then save the metadata in this projects
  # data folder ready to be used within the shiny app 
  if (nrow(duplicate_inds) == 0){
    write_parquet(techdocs_combined, file = file.path(project_folder, "techdoc.parquet"))
  }
  
  techdoc<<-techdocs_combined
  
}



