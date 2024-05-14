inequality_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      height = 600, 
      width = 350, 
      padding = 20,
      sidebar = sidebar(width = 300,
                        indicator_filter_mod_ui(ns("indicator_filter")),
                        paste0("some text"),
                        indicator_definition_btn_ui(ns("inequalities_ind_def"))
                        #actionButton(ns("indicator_definition"), label = "Definition", icon= icon('info'))
                        #indicator_definition_btn_ui(ns("hwb_inequality"))
      )# close sidebar

      )    
    
  #  textOutput("selected_var"),
    
  )
}

inequality_server <- function(id, profile_data, geo_selections) {
  moduleServer(id, function(input, output, session) {
      
     
      # return selected indicator
      selected_indicator <- indicator_filter_mod_server("indicator_filter", profile_data, geo_selections)
      

      indicator_definition_btn_server("inequalities_ind_def", selected_indicator = selected_indicator)   
     


      

    }
  )
}