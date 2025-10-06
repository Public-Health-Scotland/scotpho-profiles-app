rank_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      full_screen = FALSE,

      # ~~~~~~~~~~~~~~~~~~~
      # SIDEBAR -----
      # ~~~~~~~~~~~~~~~~~~~
      sidebar = sidebar(
        width = 300,
        open = list(mobile = "always-above"), # make contents of side collapse on mobiles above main content

        ## indicator filter ----
        div(
          id = ns("rank_indicator_filter_wrapper"),
          indicator_filter_mod_ui(ns("indicator_filter"))
        ),

        ## metadata button ----
        metadata_scroll_button_UI(
          id = ns("scroll_btn"),
          target_id = ns("metadata_section")
        ),

        ## year filter ----
        selectizeInput(
          inputId = ns("period_filter"),
          label = "Select time period:",
          choices = NULL # choices dynamically updated in server depending on selected indicator
        ),
        
        ## comparator switch ----
        div(
          id = ns("rank_comparator_wrapper"),
          input_switch(
            id = ns("comparator_switch"),
            value = FALSE,
            label = bslib::tooltip(
              placement = "bottom",
              trigger = list("Include comparator", icon("info-circle")),
              "Including a comparator will allow you to see whether each area
               within your chosen geography level (e.g. health boards) is statistically significantly
               better or worse than another area (e.g. Scotland) or another point in time (e.g. 10 years ago).")
          )
        ),
        
        
        ### hidden panel with extra filters ----
        # only displayed when comparator switch turned on
        conditionalPanel(
          ns=NS(id),
          condition = "input['comparator_switch'] === true",
          
          
          #### comparator type filter ----
          radioButtons(
            inputId = ns("comparator_type"),
            label = "Compare by: ",
            choices = c("Area", "Time"),
            selected = "Area"
          ),
          
          
          #### Area comparator filter -----
          # only displayed of comparator type is 'Area'
          conditionalPanel(
            ns = NS(id),
            condition = "input['comparator_type'] === 'Area'",
            
            selectizeInput(
              inputId = ns("area_comparator"),
              label = "Select comparator area",
              choices = NULL
            )
          ),
          
          
          #### Time comparator filter ----
          # only displayed if comparator type is 'Time'
          conditionalPanel(
            ns = NS(id),
            condition = "input['comparator_type'] === 'Time'",
            
            selectizeInput(
              inputId = ns("year_comparator"),
              label = "Select comparator year",
              choices = NULL
            )
          )
          
        ), # close hidden comparator filters panel
        
        ##  guided tour button -----
        actionLink(
          inputId = ns("rank_tour_button"),
          label = "Take a guided tour of this page"
        )
        
      ), # close sidebar

      
      # ~~~~~~~~~~~~~~~~~~~
      # MAIN PANEL ----
      # ~~~~~~~~~~~~~~~~~~
      
      layout_columns(
        
      ## chart card ----
      card(
        highchartOutput(ns("rank_chart"))
      ),

      ## map card ----
      div(id = ns("rank_map_wrapper"),
          card(
            full_screen = TRUE,
            height = 600,
            card_body(
              leafletOutput(ns("map"))
            ),
            card_footer(
              actionButton(
                inputId = ns("save_map_btn"),
                label = "Save Map (png)",
                class = "btn-sm me-2 btn-download",
                icon = icon("chart-simple")
              )
            )
          )
      ) # close map div
      ) # close layout cols
    ) # close layout sidebar
  ) # close taglist

}

rank_mod_server <- function(id, profile_data, geo_selections, selected_profile, root_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      ns <- session$ns

      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # selected indicator ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      selected_indicator <- indicator_filter_mod_server(
        id = "indicator_filter",
        profile_data, # data from app main server filtered by profile and areatype (+ Scotland)
        geo_selections, # reactive vals from main server storing geography selections (areaname, areatype, parent_area)
        selected_profile # reactive vals from main server storing details about selected profile
      )
      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # data filtered by indicator ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ind_data <- reactive({
        # only run when Scotland not selected
        # this prevents all other code in this module using ind_data() from being run.
        req(geo_selections()$areatype != "Scotland")
        
        profile_data() |>
          filter(indicator == selected_indicator()) |>
          arrange(desc(year))

      })
      




      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # dynamic filters ----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      # filter choices updated dynamically depending on the selected
      # indicator. Take data filtered by indicator (ind_data) and find
      # unique values from specific columns to set as choices



      ## year filter ----
      observeEvent(ind_data(), {

        # get available def periods for selected indicator AND selected area
        # (important as e.g. Scotland may have more up to date data
        # or have different level of aggregation than selected area)
        choices <- unique(ind_data()$def_period[ind_data()$areaname == geo_selections()$areaname])

        # avoid transient invalid values while updating filter
        shiny::freezeReactiveValue(input, "period_filter")

        # update filter choices
        updateSelectizeInput(
          session = session,
          inputId = "period_filter",
          choices = choices,
          selected = if (length(choices)) choices[[1]] else NULL
        )

      }, ignoreInit = FALSE)
      
      
      
      ## time comparator filter ----
      observe({
        
        # only run code when comparator switch turned on and time selected as comparator
        req(input$comparator_switch)
        req(input$comparator_type == "Time")
        
        # get choices 
        choices <- ind_data() |>
          filter(areatype == geo_selections()$areatype &
                   def_period != input$period_filter) |>
          pull(unique(def_period))
        
        
        # avoid transient invalid values while updating
        shiny::freezeReactiveValue(input, "year_comparator")
        
        # update filter choices 
        updateSelectizeInput(
          session = session,
          inputId = "year_comparator",
          choices = choices,
          selected = if (length(choices)) choices[[1]] else NULL
        )
        
      })
      
      
      
      ## area comparator filter ----
      observe({
        
        # only run code when comparator switch turned on and time selected as comparator
        req(input$comparator_switch)
        req(input$comparator_type == "Area")
        
        # get choices 
        choices <- ind_data() |>
          filter(def_period == input$period_filter) |>
          filter(areaname != geo_selections()$areaname) |>
          pull(unique(areaname))
        
        # avoid transient invalid values while updating
        shiny::freezeReactiveValue(input, "area_comparator")
        
        # update filter choices
        updateSelectizeInput(
          session = session,
          inputId = "area_comparator",
          choices = choices,
          selected = "Scotland"
        )
        
      })



      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # visualisation data -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      vis_data <- reactive({
        
        # set indicator data to data.table format
        ind_data <- setDT(ind_data()) 
        
        
        # filter by selected areatype
        dt <- ind_data[areatype == geo_selections()$areatype]
        
        
        # additional filtering of parent area if IZ/HSCL selected
        if(geo_selections()$areatype %in% c("Intermediate zone", "HSC locality")) {
          dt <- dt[parent_area == geo_selections()$parent_area]
        }
        
        # get comparator values (if required)
        comp_vals <- NULL
        if(input$comparator_switch == TRUE){
          
          # if comparator is area then return one single value 
          # (the value of the chosen comparator area for the latest year)
          if(input$comparator_type == "Area"){
            comp_vals <- ind_data[areaname == input$area_comparator & def_period == input$period_filter]$measure
            
            # if comparator is time then return multiple values 
            # (one for each area for selected time period)
          } else if(input$comparator_type == "Time"){
            comp_vals <- ind_data[areatype == geo_selections()$areatype & def_period == input$year_comparator]
            comp_vals <- comp_vals[,c("code", "measure")]
            comp_vals <- setnames(comp_vals, "measure", "comp_vals")
          }
        }
        
        # filter by selected time period
        dt <- dt[def_period == input$period_filter]
        
        
        # attach comparator values as a column
        if(input$comparator_switch == TRUE){
          if(input$comparator_type == "Time") {
            dt <- dt[comp_vals, on = "code"]
            dt <- dt[, high := fifelse(measure < comp_vals, measure, comp_vals)]
            dt <- dt[, low := fifelse(measure > comp_vals, measure, comp_vals)]
          } else if(input$comparator_type == "Area"){
            dt <- dt[, comp_vals := comp_vals]
            dt <- dt[, diff := measure - comp_vals]
          }
        }
        
        # prepare colour palette if comparator is selected (i.e. orange, blue, grey, green)
        if(input$comparator_switch == TRUE) {

          
          dt <- dt[, colour_pal := fcase(interpret == "O", '#999966',
                                         is.na(lowci) | is.na(upci) | is.na(comp_vals) | is.na(measure) | measure == 0, '#999966',
                                        # lowci <= comp_vals & upci >= comp_vals, '#cccccc',
                                         (lowci > comp_vals & interpret == "H") |  (upci < comp_vals & interpret == "L"),'#4da6ff',
                                         (lowci > comp_vals & interpret == "L") | (upci < comp_vals & interpret == "H"), '#ffa64d',
                                         default = '#ccccff')]
        } else {
          dt <- dt[, colour_pal := fifelse(areaname == geo_selections()$areaname, phs_colors("phs-purple"), phs_colors("phs-blue"))]
        }
        
        # order by measure
        dt <- setorder(dt, measure)
        
        dt
        
      })
      


      # ~~~~~~~~~~~~~~~~~~~~~~~
      # get shapefile ----
      # ~~~~~~~~~~~~~~~~~~~~~~~
      
      shapefile <- eventReactive(geo_selections(), {
        
        # dont run if areatype is Scotland or ADP - no shapefiles for these
        req(!geo_selections()$areatype %in% c("Scotland", "Alcohol & drug partnership"))
        
        # get shapefile depending on selected areatype
        shp <- switch(geo_selections()$areatype, 
                      "Health board" = hb_bound,
                      "Council area" = ca_bound,
                      "HSC partnership" = hscp_bound,
                      "HSC locality" = hscloc_bound,
                      "Intermediate zone" = iz_bound,
                      "Police division" = pd_bound
        )
        
        # further filter shapefile by parent area if IZ/locality selected
        if(geo_selections()$areatype %in% c("Intermediate zone", "HSC locality")){
          shp <- shp |> 
            filter(parent_area == geo_selections()$parent_area)
        }
        
        shp
        
      }, ignoreInit = FALSE)
      
      

      
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # create base map -----
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      # base map (no data assumptions)
      output$map <- renderLeaflet({
        req(shapefile())
        
        # create map 
        map <- leaflet(shapefile()) |>
          addPolygons(
            layerId = ~code,
            group  = "areas",
            color = "white"
          )
        
        
        # if small areas not selected (i.e. showing whole map of scotland) add box around Orkney and Shetland
        # to show they have been repositioned (note can't be done for polive divisions as islands + Highland are one division)
        if(geo_selections()$areatype %in% c("Council area", "Health board", "HSC partnership")){
          
        orkney_box <- st_as_sfc(st_bbox(orkney_bound))
        shetland_box <- st_as_sfc(st_bbox(shetland_bound))
        
        map <- map |>
          addPolygons(data = orkney_box, color = "black", weight = 1, fill = FALSE) |>
          addPolygons(data = shetland_box, color = "black", weight = 1, fill = FALSE) 
        
        }
        
        # If small areas selected (IZ/locality) then add provider tiles
        # so that map is detailed with streets
        if(!geo_selections()$areatype %in% c("Council area", "Health board", "HSC partnership")){
          map <- map |>
            addProviderTiles(provider = providers[["OpenStreetMap"]])
        }

        map
      })
      
      # update map when geography type changes regardless of whether
      # user on rank tab or not - ensures map is ready when user goes on tab.
      outputOptions(output, "map", suspendWhenHidden = FALSE)
      


      # update polygons according to indicator selection
      observeEvent(vis_data(), {
        req(shapefile())
        
        # if there is data to plot
        if(nrow(vis_data()) > 0) {
          
          # get shapefile
          shp <- shapefile()

          # join data with shapefile
          shp <- left_join(shp, vis_data(), by = "code")
          
          # create colour palette
          palette <- if (input$comparator_switch) {
            colorFactor(palette = unique(shp$colour_pal), domain = shp$colour_pal)
          } else {
            colorNumeric(palette = "YlOrRd", domain = shp$measure)
          }
          
          # create label for tooltips
          shp$label <- paste0(
            "<b>Area name: </b> ", shp$areaname,"<br>",
            "<b>Measure type: </b>", shp$type_definition, "<br>",
            "<b>Rate: </b> ", format(shp$measure, nsmall=0, big.mark=","))
          
          # update map
          map <- leafletProxy("map", data = shp) |>
            clearMarkers() |>
            clearControls() |>
            setShapeStyle(
              layerId = ~code,
              fillColor = ~palette(if (input$comparator_switch) colour_pal else measure),
              fillOpacity = 0.7,
              opacity = 1,
              label = shp$label,
              color = "white",
              weight = 1.2,
              stroke = TRUE
            )
          
          if(input$comparator_switch){
            map <- map |>
              addLegend(
                colors = c("#4da6ff",  "#ffa64d", "#ccccff", "#999966"),
                labels = c("better than comparator", "worse than comparator", "no difference", "N/A"),
                position = "bottomleft"
                )
          } else {
            map <- map |>
              addLegend(
                position = "bottomleft",
                pal = palette,
                values = ~measure,
                title = "Measure",
                opacity = 1
              ) 
          }
          
        # if no data to plot then turn white 
        } else {
          map <- leafletProxy("map", data = shapefile()) |>
            clearMarkers() |>
            clearControls() |>
            setShapeStyle(
              layerId = ~code,
              fillColor = 'white'
            )
          
        }
        
        map
        
      }, ignoreInit = FALSE)
      

  
    }
  )
}


