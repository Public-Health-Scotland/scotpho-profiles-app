###########################################################################.
# MODULE: demographics mod ---- 
# prepares the nav_panel layout displaying demographics
###########################################################################.

#######################################################.
## MODULE UI ----
#######################################################.

# id = unique id 
demographics_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::layout_sidebar(
      full_screen = FALSE,
      height = "80%",
      # sidebar for filters ------------------
      sidebar = sidebar(width = 500,
                        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content)
                        p("some text")
                        ))
    
    
    ,
    nav_panel("Charts",
      p("hello")
    )
    )#tag list
  
  
  
} #close ui function

############




demographics_mod_server <- function(id, geo_selections, selected_profile, root_session){
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    
    
  }) #close moduleServer
} # close server function