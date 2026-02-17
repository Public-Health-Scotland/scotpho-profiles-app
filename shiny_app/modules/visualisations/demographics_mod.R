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
                        p("some text")),
      
      # create a multi-tab card 
      div(id = ns("demog_card_wrapper"),
          navset_card_pill(
            id = ns("demog_navset_card_pill"),
            full_screen = TRUE,
            
            # charts tab -----------------------
            nav_panel("Charts",
                      value = ns("demog_chart_tab"), #id for guided tour
                      # uiOutput(ns("trend_title")), # title 
                      #uiOutput(ns("trend_caveats")), # caveats
                      highchartOutput(outputId = ns("pop_pyramid_chart")) |> # chart
                        withSpinner() |> 
                        bslib::as_fill_carrier() #required to ensure chart fills panel
            ),
            
            # add space
            bslib::nav_spacer()),
          
          # footer with download buttons
          footer = card_footer(class = "d-flex justify-content-left",
                               p("download buttons")
                               #  div(id = ns("trend_download_chart"), download_chart_mod_ui(ns("download_trends_chart"))),
                               # div(id = ns("trend_download_data"), download_data_btns_ui(ns("download_trends_data"))))
          )
      ), # close navset card pill
            
          # accordion panel with metadata table 
          div(id = ns("metadata_section"),
              p("metadata here"),
              div(uiOutput(ns("sex_split_text")) # chart header 
              
              ))
      
              #metadata_panel_UI(ns("metadata_table")))
      ) # close
    )  # close layout sidebar)                 
                        
                        # nav_panel("Charts",
                        #   p("hello")
                        # )
     
  
} #close ui function

############




demographics_mod_server <- function(id, dataset, geo_selections, selected_profile, root_session){
  moduleServer(id, function(input, output, session) {
    
    # permits compatibility between shiny and cicerone tours
    ns <- session$ns
    
    
    #######################################################.
    ## Reactive data / values ----
    #######################################################.
    
    # create population pyramid dataframe
    pyramid_data <- reactive({
    dataset() |>
      filter(year==2024)
      })
    
    # calculate total male/female population split
    sex_ratio_data <- reactive({
      dataset() |>
        group_by(year,areaname)|>
        summarise(pmale=abs(sum(percentage_Male)),
               pfemale=sum(percentage_Female))|>
        ungroup()
    })
    
    
    #####################################.
    # DYNAMIC TEXT ----
    ####################################.
 
    # render sentence that details male/female sex split
    output$sex_split_text <- renderUI({
      paste0("Percentage of the population : Male ",round(sex_ratio_data()$pmale[1],digits=1),"% Female ",round(sex_ratio_data()$pfemale[1],digits=1),"%")
    })
    
    ############################################.
    # charts -----
    ############################################.
    output$pop_pyramid_chart <- renderHighchart({
    
      shiny::validate(
        need( nrow(pyramid_data()) > 0, paste0("Data is not available at ", geo_selections()$areatype, " level. Please select either Scotland, Health board or Council area."))
      )
      
      create_pyramid_chart(
        data = pyramid_data())
    
      })
    

    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }) #close moduleServer
} # close server function